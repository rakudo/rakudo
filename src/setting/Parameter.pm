class Parameter {
    has $.name;
    has $.type;
    has $.constraints;
    has $.rw;
    has $.ref;
    has $.copy;
    method readonly() { !$!rw && !$!ref && !$!copy }
    has $.named;
    has $.named_names;
    has $.slurpy;
    has $.optional;
    has $.default;
    has $.invocant;
    has $.multi_invocant;
    has $.signature;
}

# vim: ft=perl6
