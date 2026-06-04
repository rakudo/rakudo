use Test;

plan 4;

# `my %h{K}` without an explicit value type should default the value type
# to Any, producing `Hash[Any, K]`. Without a shape the variable stays
# generic Hash. An explicit value type combines with the shape as
# `Hash[Of, K]`.

my %a{Mu};
is %a.WHAT.^name, 'Hash[Any,Mu]',
    'my %h{Mu} defaults value type to Any';

my %b{Str};
is %b.WHAT.^name, 'Hash[Any,Str]',
    'my %h{Str} defaults value type to Any';

my Int %c{Str};
is %c.WHAT.^name, 'Hash[Int,Str]',
    'my Int %h{Str} keeps the explicit value type';

my %d;
is %d.WHAT.^name, 'Hash',
    'my %h without shape stays the generic Hash';

# vim: expandtab shiftwidth=4
