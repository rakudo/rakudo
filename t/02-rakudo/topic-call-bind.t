use Test;

plan 4;

# A topic call `.<k> := $v` binds the element like the explicit `$_<k> := $v`.

{
    my $x = 1;
    my %h;
    given %h { .<c> := $x }
    $x = 99;
    is %h<c>, 99, 'binding to a .<key> topic call aliases the hash element';
}

{
    my $x = 1;
    my %h;
    given %h { .{'c'} := $x }
    $x = 99;
    is %h<c>, 99, 'binding to a .{...} topic call aliases the hash element';
}

{
    my $x = 1;
    my @a;
    given @a { .[0] := $x }
    $x = 99;
    is @a[0], 99, 'binding to a .[i] topic call aliases the array element';
}

throws-like 'given (my $o) { .foo := 5 }', Exception,
    'binding to a methodish .foo topic call is a compile error',
    message => /'bind operator'/;

# vim: expandtab shiftwidth=4
