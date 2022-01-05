use Test;

plan 3;

{
    my $p := run $*EXECUTABLE, '-V', :out, :err;
    is $p.exitcode,                0,        '`raku -V` succeeds';
    like $p.out.slurp(:close),     / 'Raku::implementation=Rakudo' /, '`raku -V` prints configuration options';
    is $p.err.slurp(:close).chars, 0,        '`raku -V` doesn\'t print to STDERR';
}
