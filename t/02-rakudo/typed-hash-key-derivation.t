use Test;

plan 29;

# Hash::Typed derives a storage key from a concrete Str by its string value,
# which is how the AT-KEY, EXISTS-KEY and DELETE-KEY it inherits look one up.

{
    my Int %h;
    my $key = "a" but role { method Str { "b" } };
    %h{$key} = 1;
    is %h.keys.join(','), 'a',
        'a Str key whose .Str differs reaches the slot named by its string value';
    is %h{$key}, 1,
        'the assigned key reads back the value it was given';
    is %h{$key}:exists, True,
        'the assigned key reports itself present';
    %h{$key} := 2;
    is %h{$key}, 2,
        'binding that key replaces the value the assignment stored';
    is (%h{$key}:delete), 2,
        'the same key deletes the slot the assignment made';
}

{
    my Int %h;
    my $key = "a" but role { method Str { "b" } };
    %h{$key} := 7;
    is %h.keys.join(','), 'a',
        'binding a Str key whose .Str differs reaches the slot named by its string value';
    is %h{$key}, 7,
        'the bound key reads back the value it was bound to';
    my Int %g;
    %g{42} := 9;
    is %g<42>, 9,
        'binding a non-Str key reaches the slot named by its stringification';
}

{
    my $key = "a" but role { method Str { die "reached" } };
    my Int %h;
    lives-ok { %h{$key} = 1 }, 'assigning a Str key leaves its .Str alone';
    lives-ok { %h{$key} := 2 }, 'binding a Str key leaves its .Str alone';
}

{
    my Int %h;
    %h{Str} = 1;
    is %h.keys.join(','), '',
        'a Str type object key reaches the slot named by the empty string';
    my Int %g;
    %g{Str} := 2;
    is %g.keys.join(','), '',
        'binding a Str type object key reaches the same slot';
}

{
    my Int %h;
    %h{IntStr.new(1, 'one')} = 5;
    is %h.keys.join(','), 'one',
        'an allomorph key reaches the slot named by its string half';
    %h{42} = 7;
    is %h<42>, 7,
        'a non-Str key reaches the slot named by its stringification';
    %h{Version.new('1.2')} = 8;
    is %h{'1.2'}, 8,
        'an arbitrary object key reaches the slot named by its stringification';
}

{
    my Int %h;
    is (%h<a> = 1), 1, 'assigning a key it does not hold yet stores the value';
    is (%h<a> = 2), 2, 'assigning a key it already holds replaces the value';
    is %h<a>.VAR.of, Int, 'a new key gets a scalar built from the hash descriptor';

    my $slot := (%h<b> = 3);
    $slot = 9;
    is %h<b>, 9, 'assignment hands back the scalar the key holds';
}

{
    my Int %h;
    throws-like { %h<a> = 'oops' }, X::TypeCheck::Assignment,
        'a rejected value on a key it does not hold yet throws';
    is %h.keys.elems, 0,
        'a rejected value on a key it does not hold yet leaves the key absent';

    %h<b> = 1;
    throws-like { %h<b> = 'oops' }, X::TypeCheck::Assignment,
        'a rejected value on a key it already holds throws';
    is %h<b>, 1,
        'a rejected value on a key it already holds leaves the old value in place';
}

{
    my Int %h is default(42);
    is %h<missing>, 42, 'the hash default answers for a key never assigned';
    %h<a> = Nil;
    is %h<a>, 42, 'assigning Nil to a key it does not hold yet gives the default';
    %h<b> = 1;
    %h<b> = Nil;
    is %h<b>, 42, 'assigning Nil to a key it already holds returns it to the default';
}

{
    my Array %h;
    my @a;
    %h<a> := @a;
    %h<a> = 1, 2, 3;
    is @a.join(','), '1,2,3', 'assigning over a bound composite runs its STORE';
}

{
    my Int %h;
    %h<a> := 5;
    throws-like { %h<a> = 6 }, X::Assignment::RO,
        message => /'immutable Int (5)'/,
        'assigning over a bound immutable value names the value in the message';
}

# The invocant constraint is what reports a type object, so ASSIGN-KEY has to
# stay a single method. Candidates behind a proto report a dispatch failure.
{
    my %h := Hash[Int];
    throws-like { %h<a> = 1 }, X::Parameter::InvalidConcreteness,
        'assigning through the type object reports the invocant';
}

# vim: expandtab shiftwidth=4
