use Test;

plan 4;

{
    my $h;
    my $value = 1;
    $h<a> := $value;
    $value = 2;
    is $h<a>, 2,
        'binding a key of an undefined variable binds the container';
    ok $h<a>:exists,
        'binding a key of an undefined variable vivifies a hash holding the key';
}

{
    my %h;
    my $value = 1;
    %h<a><b> := $value;
    $value = 2;
    is %h<a><b>, 2,
        'binding a key of a hash that a key vivifies binds the container';
}

{
    my $h;
    $h<a> := Mu;
    ok $h<a> =:= Mu,
        'binding Mu to a key of an undefined variable binds it';
}

# vim: expandtab shiftwidth=4
