#!./parrot perl6.pbc

# check inline PIR

use v6;

say '1..4';

## inline directly
q:PIR { say 'ok 1' };

## assigned to a variable
my $a = q:PIR { %r = box 'ok 2' };
say $a;

## within a subroutine
sub foo($x) {
    q:PIR {
        $P0 = find_lex '$x'
        say $P0
    }
}
foo('ok 3');

## as the result of a return
sub bar() {
    return q:PIR { %r = box 'ok 4' };
}
say bar();

