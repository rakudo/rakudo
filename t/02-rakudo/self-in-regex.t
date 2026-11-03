use Test;

plan 7;

# `self` inside a regex code block or assertion resolves to the
# invocant, including when the regex is declared in a role. Where no
# invocant is available it is still rejected at compile time.

# These type declarations default to `our` scope and install into GLOBAL,
# so each EVAL uses a distinct name to avoid colliding with the others in
# the one process running the test.

lives-ok {
    EVAL q:to/CODE/
        role R1 { token t { . { my $ = self } } }
        CODE
}, 'self in a regex code block in a role compiles';

lives-ok {
    EVAL q:to/CODE/
        role R2 { token t { . <?{ self.defined }> } }
        CODE
}, 'self in a regex predicate assertion in a role compiles';

is EVAL(q:to/CODE/),
        role R3 { method tag { "Z" }; token t { . <?{ self.tag eq "Z" }> } }
        grammar G3 does R3 { token TOP { <t> } }
        G3.parse("x") ?? "matched" !! "no-match"
        CODE
    'matched', 'self in a role regex calls the right method';

is EVAL(q:to/CODE/),
        grammar G4 { has $.v = 42; token TOP { . <?{ self.v == 42 }> } }
        G4.new.parse("x") ?? "ok" !! "no"
        CODE
    'ok', 'self in a class/grammar regex still works';

dies-ok {
    EVAL q:to/CODE/
        sub no-self() { self }
        CODE
}, 'self with no available object still fails at compile time';

# A regex that is not a method has no invocant, so self there must still
# be rejected rather than falling back to a lexical lookup.
dies-ok {
    EVAL q:to/CODE/
        role RNoSelf { / . { self } / }
        CODE
}, 'self in a non-method regex in a role still fails at compile time';

dies-ok {
    EVAL q:to/CODE/
        role RMainline { my $ = self }
        CODE
}, 'self in a role body mainline still fails at compile time';

# vim: expandtab shiftwidth=4
