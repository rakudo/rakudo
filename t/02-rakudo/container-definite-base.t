use Test;

plan 5;

# `is Type:D` on a container names a definite type as the container base. A
# container cannot be an instance of a definite type, so the base type is used,
# the same as `is Type` (without :D). Getting this wrong died "You cannot create
# an instance of this type (List:D)".

{
    my @a is List:D = (1, 2, 3);
    is-deeply @a.List, (1, 2, 3), 'a `is List:D` array holds its values';
    is @a.WHAT.^name, 'List', 'a `is List:D` array is built as a List';
}

{
    my %h is Map:D = (:a(1), :b(2));
    is %h<a>, 1, 'a `is Map:D` hash holds its values';
    is %h.WHAT.^name, 'Map', 'a `is Map:D` hash is built as a Map';
}

# The non-definite form is unchanged.
{
    my @b is Array = (4, 5);
    is @b.WHAT.^name, 'Array', 'a `is Array` array is still an Array';
}
