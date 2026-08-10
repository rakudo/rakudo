use Test;

plan 8;

# An explicit container base type on a keyed hash, as in `my %h{C} is Hash`,
# must still be parameterized with the key type. Getting this wrong built a
# plain Hash that stringified its object keys.

my class C { }

{
    my %h{C} is Hash;
    is %h.^name, 'Hash[Any,C]', 'a keyed `is Hash` hash keeps its key type';
    my $c = C.new;
    %h{$c} = 42;
    is-deeply %h.keys.head, $c, 'a keyed `is Hash` hash stores object keys';
    is %h{$c}, 42, 'a keyed `is Hash` hash retrieves by object key';
}

{
    my Int %h{C} is Hash;
    is %h.^name, 'Hash[Int,C]', 'a typed keyed `is Hash` hash keeps both types';
    my $c = C.new;
    %h{$c} = 5;
    is %h{$c}, 5, 'a typed keyed `is Hash` hash stores and retrieves';
}

{
    my %h{C} is Hash[Int];
    is %h.^name, 'Hash[Int][Any,C]', 'a keyed hash parameterizes a parameterized base';
}

{
    my class A {
        has %.h{C} is Hash;
    }
    is A.new.h.^name, 'Hash[Any,C]', 'a keyed `is Hash` attribute keeps its key type';
}

# The unkeyed and untraited forms are unchanged.
{
    my %h{C};
    is %h.^name, 'Hash[Any,C]', 'a keyed hash without a base trait still keeps its key type';
}
