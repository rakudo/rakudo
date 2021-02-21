use Test;
use MoarVM::SIL;

%*ENV<MVM_SPESH_BLOCKING> = 1;

# running the tests after the code has run
if SIL() -> $SIL {
    my @names =
      'AT-KEY', 'ASSIGN-KEY', 'postcircumfix:<{ }>'
    ;

    plan +@names;

    for @names {
        ok $SIL.inlined-by-name($_), "Was $_ inlined?";
    }
}

# running the code
else {
    my $key = "foo";
    for ^750 {
        my %a; %a{$key} = 42;
    }

    my %b = foo => 42, bar => 666;
    for ^750 {
        my $b = %b{$key};
    }
}

# vim: expandtab shiftwidth=4
