use Test;

plan 15;

{
    my Int @a = 1, 2;
    throws-like { @a[2] = "s" }, X::TypeCheck::Assignment,
        'assigning a Str to a new element of an Int array dies';
    is @a.elems, 2, 'the failed assignment did not add an element';
    nok @a[2]:exists, 'the index of the failed assignment does not exist';
}

{
    my Int @a = 1, 2;
    my uint $i = 2;
    throws-like { @a[$i] = "s" }, X::TypeCheck::Assignment,
        'the same assignment with a uint index dies';
    is @a.elems, 2, 'the failed assignment with a uint index did not add an element';
}

{
    my Int @a;
    throws-like { @a[2] = "s" }, X::TypeCheck::Assignment,
        'assigning a Str to an element of an Int array that was never assigned to dies';
    is @a.elems, 0, 'the failed assignment left the untouched array empty';
}

{
    my Int @a = 1, 2;
    throws-like { @a[5] = "s" }, X::TypeCheck::Assignment,
        'assigning a Str far past the end of an Int array dies';
    is @a.elems, 2, 'the failed assignment did not extend the array';
    nok @a[5]:exists, 'the far index of the failed assignment does not exist';
}

{
    my Int @a = 1, 2, 3;
    @a[1]:delete;
    throws-like { @a[1] = "s" }, X::TypeCheck::Assignment,
        'assigning a Str into a hole of an Int array dies';
    nok @a[1]:exists, 'the hole is still a hole after the failed assignment';
    is @a.elems, 3, 'the failed assignment into a hole kept the number of elements';
}

{
    my Int:D @a = 1, 2;
    throws-like { @a[2] = Nil }, X::TypeCheck::Assignment,
        'assigning Nil to a new element of an Int:D array dies';
    is @a.elems, 2, 'the failed Nil assignment did not add an element';
}

# vim: expandtab shiftwidth=4
