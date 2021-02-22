use Test;
use MoarVM::SIL;

%*ENV<MVM_SPESH_BLOCKING> = 1;

# running the tests after the code has run
if SIL() -> $SIL {
    my @names =
      'infix:<eq>', 'infix:<ne>', 'infix:<gt>', 'infix:<ge>',
      'infix:<lt>', 'infix:<le>', 'infix:<leg>',
    ;

    plan +@names;

    for @names {
        ok $SIL.inlined-by-name($_), "Was $_ inlined?";
    }
}

# running the code
else {
    my $a = "foo";
    my $b = "bar";
    for ^1000 {
        my $c := $a eq $b;
    }
    for ^1000 {
        my $c := $a ne $b;
    }
    for ^1000 {
        my $c := $a gt $b;
    }
    for ^1000 {
        my $c := $a ge $b;
    }
    for ^1000 {
        my $c := $a lt $b;
    }
    for ^1000 {
        my $c := $a le $b;
    }
    for ^1000 {
        my $c := $a leg $b;
    }
}

# vim: expandtab shiftwidth=4
