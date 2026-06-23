use Test;

plan 3;

# The compiler id keys every precompilation store, so each frontend must
# have its own: bytecode produced by one frontend must never be loaded
# by a process compiling with the other. Both frontends are pinned
# explicitly in subprocesses here, so the assertions hold no matter
# which frontend runs this file.

my $code = 'print $*RAKU.compiler.id';

my %with-rakuast = %*ENV;
%with-rakuast<RAKUDO_RAKUAST> = 1;
my %without-rakuast = %*ENV;
%without-rakuast<RAKUDO_RAKUAST>:delete;

my $rakuast-id = run($*EXECUTABLE.absolute, '-e', $code, :env(%with-rakuast), :out)
    .out.slurp(:close);
my $legacy-id = run($*EXECUTABLE.absolute, '-e', $code, :env(%without-rakuast), :out)
    .out.slurp(:close);

ok $rakuast-id ~~ /^ <[0..9 A..F]> ** 40 $/, 'RakuAST frontend compiler id is a 40 digit hex digest';
ok $legacy-id  ~~ /^ <[0..9 A..F]> ** 40 $/, 'legacy frontend compiler id is a 40 digit hex digest';
isnt $rakuast-id, $legacy-id, 'the two frontends have distinct compiler ids';

# vim: expandtab shiftwidth=4
