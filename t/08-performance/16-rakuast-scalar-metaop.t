use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
plan 16;

# A compound assignment on a boxed scalar inlines to a direct assignment of the
# operator's result, dropping the metaop dispatch.
{
    my $a = 10; $a += 5;
    is $a, 15, 'a plain compound assignment steps the value';
}
{
    my $a; $a //= 7;
    is $a, 7, 'a test assignment assigns when the left is undefined';
}
{
    my $a = 3; $a //= 7;
    is $a, 3, 'a test assignment keeps a defined left';
}
{
    my $a = 0; $a ||= 5;
    is $a, 5, 'an or assignment assigns a false left';
}
{
    my $a = 1; $a &&= 5;
    is $a, 5, 'an and assignment assigns a true left';
}

# A chain of compound assignments inlines in full and still chains correctly.
{
    my $a; ($a //= 0) += 1;
    is $a, 1, 'a chained compound assignment composes';
}
{
    my $a; (((($a //= 0) += 1) //= 0) += 1);
    is $a, 2, 'a longer chain composes';
}

# The right of a test assignment runs only when the left selects it.
{
    my $ran = 0; my $a = 5; $a //= do { $ran++; 9 };
    is "$a $ran", "5 0", 'a defined left does not run the right';
}

# An undefined left is not passed to the operator. The operator's identity
# stands in, matching the metaop, so there is no use-of-uninitialized warning.
{
    my Int $x; $x += 1;
    is $x, 1, 'an undefined typed scalar autovivifies';
}
{
    my $a; $a -= 1;
    is $a, -1, 'an undefined scalar autovivifies for a non-commutative operator';
}

# A nested metaop whose own left is not a scalar is not a scalar chain, so the
# outer assignment is left to the metaop.
{
    my @a = 20; (@a ||= 42) += 10;
    is-deeply @a, [11], 'a nested array metaop is left to the metaop';
}

# orelse, andthen, and notandthen compile to a call rather than a
# short-circuiting op, so they keep their laziness through the metaop.
{
    my $a = 5; $a orelse= 99;
    is $a, 5, 'orelse keeps a defined left';
}
{
    my $a; $a andthen= 99;
    nok $a.defined, 'andthen does not assign on an undefined left';
}

# A xor assignment with two true operands yields Nil. The operator compiles to
# the xor QAST op, whose neither-selected result is a VMNull, so it is left to
# the metaop, which calls the routine and returns a proper Nil there.
{
    my $a = 42; $a ^^= 15;
    is $a, Any, 'a xor assignment with two true operands yields Nil';
}

{
    sub infix:<+>(\a, \b) { 999 }
    my $a = 5; $a += 3;
    is $a, 999, 'a user-redefined operator is not inlined';
}

# The inlining drops the metaop dispatch. This observes the emitted QAST.
qast-is 'my $a; $a += 1', -> \v { not qast-contains-call v, /METAOP/ },
    'a compound assignment inlines the metaop away';

# vim: expandtab shiftwidth=4
