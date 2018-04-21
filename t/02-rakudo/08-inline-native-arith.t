use v6;
use Test;

plan 1;

my $p = run($*EXECUTABLE, '--target=optimize',
    '-e', 'my int $i = 1; for ^10 { $i = $i * 2 }', :out);
like $p.out.slurp(:close), /mul_i/,
    '$i * 2 inlines to mul_i when $i is declared as int';
