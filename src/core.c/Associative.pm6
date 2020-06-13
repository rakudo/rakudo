my role Associative[::TValue = Mu, ::TKey = Str(Any)] {
    method of() { TValue }
    method keyof() { TKey }

# These methods must be implemented by any object performing the Associative
# role.  The reason this is not actually activated, is that there are some
# chicken-and-egg issues with building the core if we do.
#    method AT-KEY($)     { ... }
#    method EXISTS-KEY($) { ... }
}

# vim: expandtab shiftwidth=4
