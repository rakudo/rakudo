use Test;

plan 4;

# An is default of a role type parameter is instantiated with the
# container when the role is composed.

cmp-ok EVAL(q[
    my role TypedAttr[::T] { has Mu:U $.v is default(T) }
    my class TypedAttrC does TypedAttr[Int] { }
    TypedAttrC.new.v
]), &[===], Int, 'typed scalar attribute gets its default instantiated';

cmp-ok EVAL(q[
    my role TypedAttrNil[::T] { has Mu:U $.v is rw is default(T) }
    my class TypedAttrNilC does TypedAttrNil[Int] { }
    my $obj = TypedAttrNilC.new(v => Str);
    $obj.v = Nil;
    $obj.v
]), &[===], Int, 'assigning Nil to a typed scalar attribute gives its instantiated default';

cmp-ok EVAL(q[
    my role TypedVar[::T] { method m { my Mu:U $v is default(T); $v } }
    my class TypedVarC does TypedVar[Int] { }
    TypedVarC.new.m
]), &[===], Int, 'typed scalar variable gets its default instantiated';

is-deeply EVAL(q[
    my role TypedVarNil[::T] { method m { my Mu:U $v is default(T); $v = Num; $v = Nil; $v } }
    my class TypedVarNilInt does TypedVarNil[Int] { }
    my class TypedVarNilStr does TypedVarNil[Str] { }
    (TypedVarNilInt.new.m, TypedVarNilStr.new.m)
]), (Int, Str), 'assigning Nil to a typed scalar variable gives the default of each instantiation';

# vim: expandtab shiftwidth=4
