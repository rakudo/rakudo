use Test;

plan 3;

# `Nil` is a valid initializer for a typed attribute. The assignment
# resets the Scalar to its container default (set by `is default(...)`
# or the type-object default otherwise), regardless of the declared
# type's definedness.

lives-ok
    { EVAL 'class AttrNilTyped1 { has IO::Handle $.x = Nil }; AttrNilTyped1.new' },
    '`has TypedClass $.x = Nil` compiles and constructs';

is EVAL('class AttrNilTyped2 { has IO::Handle $.x = Nil }; AttrNilTyped2.new.x.^name'),
    'IO::Handle',
    '`= Nil` default yields the type object';

is EVAL('class AttrNilTyped3 { has Int $.x is default(42) is rw = Nil }; my $a = AttrNilTyped3.new; $a.x = Nil; ~$a.x'),
    '42',
    '`= Nil` honors `is default(...)`';

# vim: expandtab shiftwidth=4
