use Test;
use MONKEY-SEE-NO-EVAL;

plan 6;

# A `my :(...)` signature literal declaration binds the right-hand side
# against the signature. It binds only (`:=`) and requires an initializer,
# unlike the list form `my (...)`.

{
    my :($a, $b) := (1, 2);
    is "$a $b", "1 2", 'my :(...) := list binds positionally';
}

{
    my :(:$x, :$y) := \(x => 5, y => 6);
    is "$x $y", "5 6", 'my :(...) := capture binds named arguments';
}

{
    our :($a, $b) := (1, 2);
    is "$a $b", "1 2", 'our :(...) := binds';
}

# The assignment check applies to the declaration's own initializer, not
# to a declaration nested in the bound expression.
{
    my :($a, $b) := (my $c = 3, 4);
    is "$a $b $c", "3 4 3", 'a plain assignment nested in the bound expression is not rejected';
}

# Assignment is not allowed for a signature literal: it binds, so `=` is an
# error pointing at `:=` or the list form.
throws-like {
    EVAL 'my :($a, $b) = (1, 2)'
}, X::Syntax::Variable::SignatureAssignment,
    'assignment to a signature literal declaration is rejected';

# A signature literal declaration requires an initializer.
throws-like {
    EVAL 'my :($a, $b)'
}, X::Syntax::Variable::SignatureWithoutInitializer,
    'signature literal declaration without an initializer is rejected';

# vim: expandtab shiftwidth=4
