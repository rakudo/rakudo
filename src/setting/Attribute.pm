class Attribute {
    has $.name;
    has $.type;
    has $.build;
    has $.accessor;
    has $.rw;
    method readonly() { !$!rw }
}

# vim: ft=perl6
