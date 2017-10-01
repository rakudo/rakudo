unit module Test::Helpers;
use Test;

sub is-run (
    Str() $code, $desc = "$code runs",
    Stringy :$in, :@compiler-args, :@args, :$out, :$err, :$status
) is export {
    with run :in, :out, :err,
        $*EXECUTABLE, @compiler-args, '-e', $code, @args
    {
        $in ~~ Blob ?? .in.write: $in !! .in.print: $in if $in;
        $ = .in.close;
        my $proc-out    = .out.slurp: :close;
        my $proc-err    = .err.slurp: :close;
        my $proc-status = .status;

        my $wanted-status = $status // 0;
        my $wanted-out    = $out    // '';
        my $wanted-err    = $err    // '';

        subtest $desc => {
            plan 3;
            cmp-ok $proc-out,    '~~', $wanted-out,    'STDOUT';
            cmp-ok $proc-err,    '~~', $wanted-err,    'STDERR';
            cmp-ok $proc-status, '~~', $wanted-status, 'Status';
        }
    }
}
