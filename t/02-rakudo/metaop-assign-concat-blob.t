use Test;

plan 5;

# Assigning-concat with an undefined left side takes the identity path of
# METAOP_ASSIGN, which must concatenate like the defined path does.

{
    my $x;
    $x ~= 'abc'.encode;
    is $x, 'abc', 'undefined variable ~= a Blob stringifies the Blob';
}

{
    my Str $r;
    $r ~= 'abc'.encode;
    is $r, 'abc', 'undefined Str-typed variable ~= a Blob stringifies the Blob';
}

{
    my $x;
    &infix:<~=>($x, 'abc'.encode);
    is $x, 'abc', 'the indirect &infix:<~=> call form stringifies the Blob';
}

{
    my $x = 'x';
    $x ~= 'abc'.encode;
    is $x, 'xabc', 'defined variable ~= a Blob still concatenates';
}

{
    my $x;
    $x ~= 'abc';
    is $x, 'abc', 'undefined variable ~= a Str still works';
}

# vim: expandtab shiftwidth=4
