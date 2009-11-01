#!./parrot perl6.pbc

# check inline PIR

use v6;

say '1..3';

## inline directly
Q:PIR { say 'ok 1' };

## assigned to a variable
my $a = Q:PIR { %r = box 'ok 2' };
say $a;

## within a subroutine
sub foo($x) {
    Q:PIR {
        $P0 = find_lex '$x'
        say $P0
    }
}
foo('ok 3');


