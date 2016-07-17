use v6;
use Test;
plan 2;

# RT#128098
{
    my $proc = run :out, :err, $*EXECUTABLE, '-e',
        q/use Test; plan 1; diag 'test message'; ok 1;/;

    subtest 'diag at the start of file shows up in non-verbose prove run', {
        like $proc.out.slurp-rest, /^ '1..1' \s+ 'ok 1 -' \s* $/, 'STDOUT';
        like $proc.err.slurp-rest, /'test message'/,              'STDERR';
    }
}

{
    my $proc = run :out, :err, $*EXECUTABLE, '-e',
        q/use Test; plan 1; todo 'meow'; diag 'test message'; ok 1;/;

    subtest 'using diag in the middle of TODO tests does not interfere', {
        my $out = $proc.out.slurp-rest;
        like   $out, /^ '1..1' \s+ 'ok 1 -' \s* '# TODO'/, 'STDOUT has TODO';
        unlike $out, /'test message'/, 'diag message is not in STDOUT';
        like   $proc.err.slurp-rest, /'test message'/,
               'diag message is in STDERR';
    }
}

done-testing;
