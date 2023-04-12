# This "role" is created in the RakuAST bootstrap, and is augmented here
# to allow a lot of logic (which will **NOT** be needed to compile the
# Raku setting) to be written in Raku rather than in NQP.
augment class RakuAST::Doc::DeclaratorTarget {

    method foo() { "foo" }
}

# vim: expandtab shiftwidth=4
