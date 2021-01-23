use Test;
use MoarVM::SIL;

if SIL() -> $SIL {
    LEAVE $SIL.exit;

    my @names =
      'AT-POS', 'ASSIGN-POS', 'ASSIGN_POS_SLOW_PATH', 'postcircumfix:<[ ]>'
    ;

    plan +@names;

    for @names {
        ok $SIL.inlined-by-name($_), "Was $_ inlined?";
    }
}

my $i = 3;
for ^100000 {
    my @a; @a[$i] = 42;
}

my @b = ^10;
for ^100000 {
    my $b = @b[$i];
}

# vim: expandtab shiftwidth=4
