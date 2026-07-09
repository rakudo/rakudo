use Test;

plan 7;

# A `my (...)` list declaration with no initializer, used as the right-hand
# side of a signature bind, produced a null and died at compile time with
# "No such method 'Capture' for invocant of type 'VMNull'".

{
    my (\a, \b) := my ($x, $y);
    ok a =:= $x, 'first sigilless target aliases the first declared container';
    ok b =:= $y, 'second sigilless target aliases the second declared container';
    a = 10;
    b = 20;
    is-deeply ($x, $y), (10, 20), 'assigning through the aliases writes the underlying containers';
}

{
    my @l = (my ($p, $q));
    is @l.elems, 2, 'a no-initializer list declaration used as an rvalue yields its containers';
}

lives-ok { my ($m, $n); $m = 1; $n = 2 }, 'a plain sunk list declaration statement still works';

{
    my ($a, $b) = 1, 2;
    is-deeply ($a, $b), (1, 2), 'list assignment still binds each element';
    my ($c, $d) := (3, 4);
    is-deeply ($c, $d), (3, 4), 'list bind to a literal list still works';
}

# vim: expandtab shiftwidth=4
