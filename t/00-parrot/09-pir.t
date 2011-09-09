#!./parrot perl6.pbc

# check inline PIR

use v6;

say '1..3';

## inline directly
Q:PIR { say 'ok 1' };

## assigned to a variable
my $a = Q:PIR { %r = perl6_box_str 'ok 2' };
say $a;

## within a subroutine
sub foo($x) {
    Q:PIR {
        $P0 = find_lex '$x'
        $S0 = repr_unbox_str $P0
        say $S0
    };
    # a Q:PIR block returning nothing as last statement inside a block
    # is a bad idea, so end on a happy note instead:
    1;

}
foo('ok 3');


