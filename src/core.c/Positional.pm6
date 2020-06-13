my role Positional[::T = Mu] {
    method of() { T }

# These methods must be implemented by any object performing the Positional
# role.  The reason this is not actually activated, is that there are some
# chicken-and-egg issues with building the core if we do.
#    method elems()       { ... }
#    method AT-POS($)     { ... }
#    method EXISTS-POS($) { ... }

}

# vim: expandtab shiftwidth=4
