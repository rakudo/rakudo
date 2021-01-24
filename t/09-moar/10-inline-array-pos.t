use Test;
use MoarVM::SIL;

# running the tests after the code has run
if SIL() -> $SIL {
    my @names =
      'AT-POS', 'ASSIGN-POS', 'ASSIGN_POS_SLOW_PATH', 'postcircumfix:<[ ]>'
    ;

    plan +@names;

    for @names {
        ok $SIL.inlined-by-name($_), "Was $_ inlined?";
    }
}

# running the code
else {
    my $i = 3;
    for ^100000 {
        my @a; @a[$i] = 42;
    }

    my @b = ^10;
    for ^100000 {
        my $b = @b[$i];
    }
}

# vim: expandtab shiftwidth=4
