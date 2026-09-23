use Test;

plan 9;

# The declared type of an @, % or & attribute constrains its elements or
# its return value, and a variable only bounds the type of its value.

is-deeply EVAL(q[
    my @d = 1, 2;
    my class ArrayVarDefault { has Int @.x = @d }
    ArrayVarDefault.new.x
]), Array[Int].new(1, 2), 'typed @ attribute defaults to an array variable';

is-deeply EVAL(q[
    my @d = Int, Int;
    my class UndefinedArrayVarDefault { has Int:U @.x = @d }
    UndefinedArrayVarDefault.new.x
]), Array[Int:U].new(Int, Int), 'typed @ attribute of undefined elements defaults to an array of type objects';

is-deeply EVAL(q[
    my %d = a => 1;
    my class HashVarDefault { has Int %.x = %d }
    HashVarDefault.new.x
]), (my Int % = a => 1), 'typed % attribute defaults to a hash variable';

is EVAL(q[
    my &d = sub (--> Int) { 42 };
    my class CodeVarDefault { has Int &.x = &d }
    CodeVarDefault.new.x.()
]), 42, 'typed & attribute defaults to a code variable';

is EVAL(q[
    my Any $d = 42;
    my class WiderVarDefault { has Int $.x = $d }
    WiderVarDefault.new.x
]), 42, 'typed $ attribute defaults to a variable of a wider type';

is-deeply EVAL(q[
    my @d = 1, 2;
    my class PositionalVarDefault { has Array $.x = @d }
    PositionalVarDefault.new.x
]), [1, 2], 'Array $ attribute defaults to an array variable';

is EVAL(q[
    my class NativeVarDefault { has Int $.x = my int $ = 42 }
    NativeVarDefault.new.x
]), 42, 'Int $ attribute defaults to a native int variable';

cmp-ok EVAL(q[
    my role TypeParamDefault[::T] { has Mu:U $.x = T }
    my class TypeParamDefaultC does TypeParamDefault[Int] { }
    TypeParamDefaultC.new.x
]), &[===], Int, 'typed $ attribute defaults to a role type parameter';

throws-like q[
    my role TypeParamMismatch[::T] { has Str $.x = T }
    my class TypeParamMismatchC does TypeParamMismatch[Int] { }
    TypeParamMismatchC.new
], X::TypeCheck::Assignment,
  'role type parameter default is type checked when the object is built';

# vim: expandtab shiftwidth=4
