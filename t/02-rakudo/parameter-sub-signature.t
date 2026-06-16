use Test;

plan 15;

# Parameter sub signature destructure must compile under both
# frontends, including when the owning routine is compiled at BEGIN.
# A trait_mod multi applied to an attribute declaration is one such
# case.

# 1. trait_mod multi with a named array sub signature on attribute.
lives-ok {
    EVAL q:to/CODE/
        multi sub trait_mod:<is> (Attribute $a, :@spec! (Int $x, Int $y)) { }
        my class C { has Str $.tags is spec(1, 2) }
        CODE
}, 'trait_mod multi with named array sub signature on attribute compiles';

# 2. Regular sub call with a named array sub signature.
is EVAL(q:to/CODE/),
        sub foo(Int $x, :@a! ($p, $q)) { "$x|$p|$q" }
        foo(99, a => [10, 20])
        CODE
    '99|10|20', 'named array sub signature destructure binds';

# 3. Positional sub signature.
is EVAL(q:to/CODE/),
        sub foo($h ($x, $y)) { "$x|$y" }
        foo([10, 20])
        CODE
    '10|20', 'positional sub signature destructure binds';

# 4. Multi dispatch where one candidate has a sub signature.
is EVAL(q:to/CODE/),
        multi foo(Int $x) { "int $x" }
        multi foo(Int $x, :@a! ($p, $q)) { "$x|$p|$q" }
        foo(99, a => [10, 20])
        CODE
    '99|10|20', 'multi candidate with sub signature dispatches and binds';

# 5. Nested sub signature.
is EVAL(q:to/CODE/),
        sub foo(@a ($b, ($c, $d))) { "$b|$c|$d" }
        foo([1, [2, 3]])
        CODE
    '1|2|3', 'nested sub signature destructure binds';

# 6. trait_mod multi applied to a class declaration.
lives-ok {
    EVAL q:to/CODE/
        multi sub trait_mod:<is> (Mu:U $a, :@meta! (Int $x, Int $y)) { }
        my class C is meta(1, 2) { }
        CODE
}, 'trait_mod multi with named array sub signature on class compiles';

# 7. Slurpy positional with a sub signature.
is EVAL(q:to/CODE/),
        sub foo(*@a ($p, $q)) { "$p|$q" }
        foo([10, 20])
        CODE
    '10|20', 'slurpy positional with sub signature destructure binds';

# 8. Hash sub signature destructure.
is EVAL(q:to/CODE/),
        sub foo(%h (:$x, :$y)) { "$x|$y" }
        foo({:x(1), :y(2)})
        CODE
    '1|2', 'hash sub signature destructure binds';

# 9. Sub signature parameter with a default value. The default runs
# through the thunk path before custom-args gets propagated.
is EVAL(q:to/CODE/),
        sub foo(:@a ($p, $q) = [3, 4]) { "$p|$q" }
        foo()
        CODE
    '3|4', 'sub signature parameter default thunk runs';

# 10. Parametric role with a sub signature parameter, applied with an
# inline array literal as the parameterization argument.
is EVAL(q:to/CODE/),
        role R[@a ($x, $y)] { method values { "$x|$y" } }
        my class C does R[[7, 9]] { }
        C.new.values
        CODE
    '7|9', 'parametric role with sub signature param accepts inline array';

# 11. Parametric role with a named sub signature parameter, applied
# with a named argument whose value is an inline array literal.
is EVAL(q:to/CODE/),
        role R[:@a! ($x, $y)] { method values { "$x|$y" } }
        my class C does R[:a([7, 9])] { }
        C.new.values
        CODE
    '7|9', 'parametric role with named sub signature param accepts inline array via named arg';

# 12. Parameterization with a non-interpretable argument (`self` in a
# typename context with no enclosing object) must fail at compile
# time, not crash or silently proceed.
dies-ok {
    EVAL q:to/CODE/
        role R[$x] { method val { $x } }
        my class C does R[self] { }
        CODE
}, 'role parameterization with self outside object dies cleanly';

# 13. An untyped sub signature parameter takes the same default nominal
# type as a routine parameter, so an unfilled named key binds to Any.
is EVAL(q:to/CODE/),
        sub foo(%h (:$x!, :$y)) { $y.^name }
        foo({:x(1)})
        CODE
    'Any', 'unfilled named sub signature parameter binds to Any';

# 14. The same holds for a missing optional positional element.
is EVAL(q:to/CODE/),
        sub foo(@a ($p, $q?)) { $q.^name }
        foo([1])
        CODE
    'Any', 'unfilled positional sub signature parameter binds to Any';

# 15. Because the unfilled value is Any rather than Mu, a downstream
# call constrained to Any resolves rather than failing to find a
# candidate.
is EVAL(q:to/CODE/),
        my class C {
            multi method handle(Str:D $_) { "str" }
            multi method handle($)        { "any" }
            method run(%h (:$x!, :$comment)) { self.handle($comment) }
        }
        C.new.run({:x(1)})
        CODE
    'any', 'unfilled sub signature value resolves to an Any candidate';

# vim: expandtab shiftwidth=4
