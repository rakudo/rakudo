use Test;
use MoarVM::SIL;

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
    for ^1000000 {
        my $c;
        $c := $a eq $b;
        $c := $a ne $b;
        $c := $a gt $b;
        $c := $a ge $b;
        $c := $a lt $b;
        $c := $a le $b;
        $c := $a leg $b;
    }
}

# vim: expandtab shiftwidth=4
