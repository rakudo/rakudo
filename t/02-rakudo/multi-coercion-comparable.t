use Test;

plan 2;

# Distinct coercion types are wrongly seen as equivalent by the
# duplicate-multi check. `use fatal` turns that spurious worry into a compile
# error, so a clean compile proves the check left these signatures alone.

lives-ok {
    EVAL q:to/CODE/
        use fatal;
        class Id {
            multi method COERCE(Cool $x) { Id }
            multi method COERCE(Capture $x) { Id }
        }
        multi to-sorter(Id(Cool) $x) { "cool" }
        multi to-sorter(Id(Capture) $x) { "capture" }
        CODE
}, 'distinct coercion types on a multi parameter are not a redeclaration';

# A coercion inside a sub-signature destructure is compared the same way.
lives-ok {
    EVAL q:to/CODE/
        use fatal;
        role Expr { }
        class Ident does Expr { multi method COERCE(Any $x) { Ident } }
        multi expand(Pair (Ident(Any) :$key, Mu :$value)) { "ident" }
        multi expand(Pair (Expr        :$key, Mu :$value)) { "expr" }
        CODE
}, 'a coercion inside a sub-signature is not a redeclaration';

# vim: expandtab shiftwidth=4
