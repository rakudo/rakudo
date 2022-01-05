# A Sequence represents anything that can lazily produce a sequence of values.
# There are various concrete implementations of Sequence, the most common
# being Seq, which represents a sequentially produced sequence.
#
# Sequences are born in a state where iterating them will consume the values.
# However, calling .cache will return a List that will lazily reify to the
# values in the Sequence. The List is memoized, so that subsequent calls to
# .cache will always return the same List (safe as List is immutable). More
# than one call to .iterator throws an exception (and calling .cache calls the
# .iterator method the first time also). The memoization can be avoided by
# asking very specifically for the Seq to be coerced to a List (using .List
# or .list), a Slip (.Slip) or an Array (.Array).
#
# The actual memoization functionality is factored out into a role,
# PositionalBindFailover, which is used by the binder to identify types that,
# on failure to bind to an @-sigilled thing, can have .cache called on them
# and get memoization semantics. This decouples this functionality from the
# Sequence role, so other user-defined types can get access to this
# functionality.

my role PositionalBindFailover {
    has $!list;

    method cache() {
        nqp::isconcrete($!list)
          ?? $!list
          !! ($!list := List.from-iterator(self.iterator))
    }

    multi method list(::?CLASS:D:) {
        nqp::isconcrete($!list)
          ?? $!list
          !! List.from-iterator(self.iterator)
    }

    method iterator() { ... }
}
nqp::p6configposbindfailover(Positional, PositionalBindFailover); # Binder
Routine.'!configure_positional_bind_failover'(Positional, PositionalBindFailover); # Multi-dispatch

my role Sequence does PositionalBindFailover {
    multi method Array(::?CLASS:D:) { Array.from-iterator(self.iterator) }
    multi method List(::?CLASS:D:)  { self.list.List }
    multi method Slip(::?CLASS:D:)  { self.list.Slip }

    multi method Str(::?CLASS:D:) {
        self.cache.Str
    }

    multi method Stringy(::?CLASS:D:) {
        self.cache.Stringy
    }

    method Numeric(::?CLASS:D:) { self.cache.elems }

    multi method AT-POS(::?CLASS:D: Int:D $idx) is raw {
        self.cache.AT-POS($idx)
    }

    multi method AT-POS(::?CLASS:D: int $idx) is raw {
        self.cache.AT-POS($idx)
    }

    multi method EXISTS-POS(::?CLASS:D: Int:D $idx) {
        self.cache.EXISTS-POS($idx)
    }

    multi method EXISTS-POS(::?CLASS:D: int $idx) {
        self.cache.EXISTS-POS($idx)
    }

    multi method eager(::?CLASS:D:) { List.from-iterator(self.iterator).eager }

    method fmt(|c) {
        self.cache.fmt(|c)
    }

    multi method gist(::?CLASS:D:) {
        self.cache.gist
    }
}

# vim: expandtab shiftwidth=4
