my class Nil is Cool { # declared in BOOTSTRAP

    method !die(str $method) is hidden-from-backtrace {
        nqp::istype(self,Failure)
          ?? self.throw
          !! die "Use of Nil.$method not allowed";
    }
    method !warn(str $method) is hidden-from-backtrace {
        nqp::istype(self,Failure)
          ?? self.throw
          !! warn "Use of Nil.$method coerced to empty string";
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
    multi method chars(Nil:         --> '') { self!warn: 'chars' }
    multi method chomp(Nil:         --> '') { self!warn: 'chomp' }
    multi method chop(Nil: |        --> '') { self!warn: 'chop' }
    multi method codes(Nil: |       --> '') { self!warn: 'codes' }
    multi method comb(Nil: |        --> '') { self!warn: 'comb' }
    multi method contains(Nil: |    --> '') { self!warn: 'contains' }
    multi method ends-with(Nil: |   --> '') { self!warn: 'ends-with' }
    multi method flip(Nil:          --> '') { self!warn: 'flip' }
    multi method Int(Nil: |         --> '') { self!warn: 'Int' }
    multi method indent(Nil: |      --> '') { self!warn: 'indent' }
    multi method index(Nil: |       --> '') { self!warn: 'index' }
    multi method indices(Nil: |     --> '') { self!warn: 'indices' }
    multi method lc(Nil:            --> '') { self!warn: 'lc' }
    multi method lines(Nil: |       --> '') { self!warn: 'lines' }
    multi method tc(Nil:            --> '') { self!warn: 'tc' }
    multi method tclc(Nil:          --> '') { self!warn: 'tclc' }
    multi method rindex(Nil: |      --> '') { self!warn: 'rindex' }
    multi method starts-with(Nil: | --> '') { self!warn: 'starts-with' }
    multi method trans(Nil: |       --> '') { self!warn: 'trans' }
    multi method substr(Nil: |      --> '') { self!warn: 'substr' }
    multi method subst(Nil: |       --> '') { self!warn: 'subst' }
    multi method substr-eq(Nil: |   --> '') { self!warn: 'substr-eq' }
    multi method substr-rw(Nil: |   --> '') { self!warn: 'substr-rw' }
    multi method wordcase(Nil:      --> '') { self!warn: 'wordcase' }
    multi method words(Nil: |       --> '') { self!warn: 'words' }
    multi method uc(Nil:            --> '') { self!warn: 'uc' }

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
