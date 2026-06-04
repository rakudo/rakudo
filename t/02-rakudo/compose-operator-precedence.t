use Test;

plan 2;

# `∘` and `o` are concatenation-precedence infixes (looser than `+`),
# so `&sin ∘ * + 1` must curry as `&sin ∘ (* + 1)` and call sin on x+1.

my &uni = &sin ∘ * + 1;
is-approx uni(0), sin(1), '∘ binds looser than +';

my &ascii = &sin o * + 1;
is-approx ascii(0), sin(1), 'o binds looser than +';

# vim: expandtab shiftwidth=4
