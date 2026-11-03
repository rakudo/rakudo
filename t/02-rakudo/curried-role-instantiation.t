use Test;

plan 5;

# Instantiating a generic curried role must preserve everything the curry
# was declared with: named and value arguments have to survive into the
# composed class, and the pun repr of the role group has to reach the
# instantiated curry.

{
    role Flagged[::T, :$flag] { method flag() { $flag } }
    role Wrap[::U] does Flagged[U, :flag(42)] { }
    class WrapInt does Wrap[Int] { }
    is WrapInt.new.flag, 42,
        'named curry arg survives generic instantiation';
    ok WrapInt ~~ Flagged,
        'class composing a curry with a named arg typechecks against the role group';
}

{
    role Valued[Str $s] { method s() { $s } }
    role UsesValued[::T] does Valued["x"] { }
    class ValuedInt does UsesValued[Int] { }
    is ValuedInt.new.s, "x",
        'value curry arg survives generic instantiation';
    ok ValuedInt ~~ Valued["x"],
        'class composing a curry with a value arg typechecks against the parameterization';
}

{
    role Struct[::T] is repr('CStruct') { has int32 $.x }
    role UsesStruct[::T] does Struct[T] { }
    is UsesStruct[Int].^roles(:!transitive)[0].^pun.REPR, 'CStruct',
        'pun repr of the role group reaches a curry instantiated from a generic';
}

# vim: expandtab shiftwidth=4
