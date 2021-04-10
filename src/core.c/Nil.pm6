my class Nil is Cool { # declared in BOOTSTRAP

    method !die(str $method) is hidden-from-backtrace {
        die "Use of Nil.$method not allowed";
    }
    method !warn(str $method) is hidden-from-backtrace {
        warn "Use of Nil.$method coerced to empty string";
    }

    # core functionality
    method sink(--> Nil) { }     # required by RESTRICTED setting
    method FALLBACK(| --> Nil) { }
    method STORE(|) { X::Assignment::RO.new(:value<Nil>).throw }

    # interface methods that should silently return Nil
    multi method new(Nil: *@ --> Nil) { }
    multi method iterator(Nil:) { Rakudo::Iterator.OneValue(Nil) }
    method AT-POS(| --> Nil) { }
    method AT-KEY(| --> Nil) { }
#    method ACCEPTS(*@ --> Nil) { }  # XXX spec says Nil, but makes install fail

    # interface methods that should fail
    multi method BIND-POS(Nil: |)   { Failure.new(X::Bind.new(:target<Nil>)) }
    multi method BIND-KEY(Nil: |)   { Failure.new(X::Bind.new(:target<Nil>)) }

    # interface methods that should throw
    multi method ASSIGN-POS(Nil: |) { self!die: 'ASSIGN-POS' }
    multi method ASSIGN-KEY(Nil: |) { self!die: 'ASSIGN-KEY' }
    multi method push(Nil: |)    { self!die: 'push' }
    multi method append(Nil: |)  { self!die: 'append' }
    multi method unshift(Nil: |) { self!die: 'unshift' }
    multi method prepend(Nil: |) { self!die: 'prepend' }

    # Cool methods that should just warn
    multi method wordcase(Nil:  --> '') { self!warn: 'wordcase' }
    multi method trans(Nil: |c  --> '') { self!warn: 'trans' }
    multi method indent(Nil: |c --> '') { self!warn: 'indent' }
    multi method uc(Nil:        --> '') { self!warn: 'uc' }
    multi method lc(Nil:        --> '') { self!warn: 'lc' }
    multi method tc(Nil:        --> '') { self!warn: 'tc' }
    multi method tclc(Nil:      --> '') { self!warn: 'tclc' }
    multi method flip(Nil:      --> '') { self!warn: 'flip' }
    multi method chop(Nil: |c   --> '') { self!warn: 'chop' }
    multi method chomp(Nil:     --> '') { self!warn: 'chomp' }

    # numeric coercions
    method Numeric( --> 0) { warn "Use of Nil in numeric context" }

    # string coercions
    multi method gist(Nil: --> "Nil") { }
    multi method Str(Nil:  --> "")    { warn "Use of Nil in string context" }
    multi method raku(Nil: --> "Nil") { }

    # QuantHash coercions
    multi method Set(Nil:)     { Set.new(Nil)     }
    multi method SetHash(Nil:) { SetHash.new(Nil) }
    multi method Bag(Nil:)     { Bag.new(Nil)     }
    multi method BagHash(Nil:) { BagHash.new(Nil) }
    multi method Mix(Nil:)     { Mix.new(Nil)     }
    multi method MixHash(Nil:) { MixHash.new(Nil) }
}

# vim: expandtab shiftwidth=4
