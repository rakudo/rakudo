use v6;
use Test;

plan 5;

# https://github.com/Raku/old-issue-tracker/issues/5258

{
    dies-ok { my $a = "a" x 2**30; my $b = "b" x 2**30; my $c = $a ~ $b; my $d = $b ~ $a; my $e = $c ~ $d; },
        'concatenating strings with `~` that would create a too large result dies';
    dies-ok { (('a' x 1000000) x 1000000) },
        'repeating strings with `x` that would create a too large result dies';
}

# https://github.com/Raku/old-issue-tracker/issues/5279
if $*VM.name eq 'jvm' {
    skip-rest 'OutOfMemoryError: Java heap space';
}
else {
    my $a;
    lives-ok({ $a = 'a' x 1073741824 }, 'repeat count equal to the NQP limit works');
    is($a.chars, 1073741824, 'correct result for count equal to the NQP limit');

    throws-like({ $a = 'a' x 9999999999999999999 }, Exception, 'too large repeat count throws instead of going negative');
}

# vim: expandtab shiftwidth=4
