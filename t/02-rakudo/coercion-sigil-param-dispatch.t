use Test;

plan 6;

# Behind an `@`/`%`/`&` sigil the parameter binds `Positional[T()]` (etc.), a
# parameterized role, not a coercion. A multi with such a parameter must build
# its dispatch info without treating the parameter itself as a coercion, or the
# builder looks for a coercion wrappee on the role and dies.

multi ga(Str $n, Int() @c) { 'coerced' }
multi ga(Str $n,        @c) { 'plain'   }
lives-ok { ga('x', [1, 2]) }, 'a multi with a coercive array parameter dispatches';
is ga('x', [1, 2]), 'plain', 'a plain Array selects the unconstrained candidate';
is ga('x', Array[Int].new(1, 2)), 'coerced',
    'a Positional[Int] selects the Int() @c candidate';

multi gh(Str $n, Int() %h) { 'coerced' }
multi gh(Str $n,        %h) { 'plain'   }
lives-ok { gh('x', { :a(1) }) }, 'a multi with a coercive hash parameter dispatches';

multi gc(Str $n, Int() &c) { 'coerced' }
multi gc(Str $n,        &c) { 'plain'   }
lives-ok { gc('x', { 42 }) }, 'a multi with a coercive callable parameter dispatches';

# A scalar coercion is still flagged coercive and still coerces.
multi gs(Int() $x) { "int $x" }
is gs("5"), 'int 5', 'a scalar coercion still coerces';

# vim: expandtab shiftwidth=4
