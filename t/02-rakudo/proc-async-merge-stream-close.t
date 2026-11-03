use Test;

# A process can close one output stream while the other keeps producing.
# The merged Supply must deliver the output that arrives after the first
# stream closed and complete only when both streams have ended.

plan 10;

sub merged-run($child-code) {
    my $proc = Proc::Async.new($*EXECUTABLE, '-e', $child-code);
    my @chunks;
    my $done = False;
    $proc.Supply.tap: { @chunks.push($_) }, done => { $done = True };
    my $finished = $proc.start;
    await Promise.anyof($finished, Promise.in(60));
    $proc.kill(SIGKILL) if $finished.status == Planned;
    (@chunks, $done)
}

my ($chunks, $done) = merged-run
    '$*ERR.print("early"); $*OUT.close; sleep 0.3; $*ERR.print("late")';
ok $chunks.join.contains('early'),
    'Merged output includes stderr output produced before stdout closed';
ok $chunks.join.contains('late'),
    'Merged output includes stderr output produced after stdout closed';
ok $chunks.all.defined,
    'Every merged chunk is defined after stdout closed early';
ok $done,
    'Merged Supply completes when stderr ends after stdout closed early';

($chunks, $done) = merged-run
    '$*OUT.print("early"); $*ERR.close; sleep 0.3; $*OUT.print("late")';
ok $chunks.join.contains('early'),
    'Merged output includes stdout output produced before stderr closed';
ok $chunks.join.contains('late'),
    'Merged output includes stdout output produced after stderr closed';
ok $chunks.all.defined,
    'Every merged chunk is defined after stderr closed early';
ok $done,
    'Merged Supply completes when stdout ends after stderr closed early';

($chunks, $done) = merged-run 'exit 0';
is $chunks.join, '',
    'A merged child that writes nothing produces no output';
ok $done,
    'Merged Supply completes for a child that writes nothing';

# The crashes guarded here kill a thread pool worker asynchronously. Give
# such a crash time to take the process down before the plan completes.
await Promise.in(0.5);

# vim: expandtab shiftwidth=4
