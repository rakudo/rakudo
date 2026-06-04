use Test;

plan 2;

# A constant declared with an assign meta-op like `=%=` has no explicit
# left operand; the parser synthesises an anonymous state variable on
# its behalf. The constant's value is computed by applying the wrapped
# infix between that synthesised state var and the right operand at
# BEGIN time. These cases exercise that the synthesised variable
# resolves correctly during the BEGIN-time evaluation.

is-deeply EVAL('my constant \X =%= %( a => 1, b => 2 ); X.Map'),
    Map.new((:a(1), :b(2))),
    '=%= constant produces a Map from its right operand';

is EVAL('my constant \A =%= %( a => 1 ); my constant \B =%= %( b => 2 ); "{A<a>}-{B<b>}"'),
    '1-2',
    'separate =%= constants get separate anonymous state vars';

# vim: expandtab shiftwidth=4
