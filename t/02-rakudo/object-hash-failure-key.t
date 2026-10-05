use Test;

plan 6;

{
    my Hash %h{Int(Str)};
    throws-like { %h<x><a> = 1 }, X::Str::Numeric,
        'taking an element of an object hash for a key that fails to coerce throws';
    is %h.elems, 0,
        'an element taken for a key that fails to coerce stores nothing';
}

{
    my %h{Mu};
    ok %h{Failure} === Any,
        'reading the Failure type object as a key of an object hash gives the default';
    %h{Failure} = 1;
    ok %h{Failure}:exists,
        'the Failure type object is assigned as a key of an object hash';
    %h{Failure}:delete;
    nok %h{Failure}:exists,
        'the Failure type object is deleted as a key of an object hash';
    %h{Failure} := 2;
    is %h{Failure}, 2,
        'the Failure type object is bound as a key of an object hash';
}

# vim: expandtab shiftwidth=4
