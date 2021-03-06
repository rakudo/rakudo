my class Pair does Associative {
    has $.key is default(Nil);
    has $.value is rw is default(Nil);
    has ObjAt $!WHICH;

    proto method new(|) {*}
    # This candidate is needed because it currently JITS better
    multi method new(Pair: Cool:D \key, Mu \value) {
        my \p := nqp::p6bindattrinvres(
          nqp::create(self),Pair,'$!key',nqp::decont(key));
        nqp::bindattr(p,Pair,'$!value',value);
        p
    }
    multi method new(Pair: Mu \key, Mu \value) {
        my \p := nqp::p6bindattrinvres(
          nqp::create(self),Pair,'$!key',nqp::decont(key));
        nqp::bindattr(p,Pair,'$!value',value);
        p
    }
    multi method new(Pair: Mu :$key!, Mu :$value!) {
        my \p := nqp::p6bindattrinvres(
          nqp::create(self),Pair,'$!key',$key);
        nqp::bindattr(p,Pair,'$!value',$value);
        p
    }

    multi method clone(Pair:D:) {
        nqp::p6bindattrinvres(self.Mu::clone, Pair, '$!WHICH', nqp::null)
    }
    multi method WHICH(Pair:D: --> ObjAt:D) {
        nqp::isconcrete($!WHICH) ?? $!WHICH !! self!WHICH
    }

    method !WHICH() {
        $!WHICH := nqp::if(
          nqp::iscont($!value)
            || nqp::not_i(nqp::istype((my $VALUE := $!value.WHICH),ValueObjAt)),
          self.Mu::WHICH,
          nqp::box_s(
            nqp::concat(
              nqp::if(
                nqp::eqaddr(self.WHAT,Pair),
                'Pair|',
                nqp::concat(self.^name,'|')
              ),
              nqp::sha1(nqp::concat(nqp::concat($!key.WHICH,"\0"),$VALUE))
            ),
            ValueObjAt
          )
        )
    }

    multi method ACCEPTS(Pair:D: %h) {
        $!value.ACCEPTS(%h.AT-KEY($!key));
    }
    multi method ACCEPTS(Pair:D: Pair:D $p) {
        $!value.ACCEPTS(nqp::getattr(nqp::decont($p),Pair,'$!value'));
    }
    multi method ACCEPTS(Pair:D: Mu $other) {
        nqp::can($other,(my $method := $!key.Str))
          ?? ($other."$method"().Bool === $!value.Bool)
          !! X::Method::NotFound.new(
               invocant => $other,
               method   => $method,
               typename => $other.^name,
               addendum => "Or did you try to smartmatch against a Pair specifically?  If so, then the key of the Pair should be a valid method name, not '$method'."
             ).throw
    }

    method Pair() { self }
    method antipair(Pair:D:) { self.new($!value,$!key) }
    method freeze(Pair:D:) { $!value := nqp::decont($!value) }

    method iterator(Pair:D:) {
        Rakudo::Iterator.OneValue(self)
    }
    multi method keys(Pair:D:) {
        Seq.new(Rakudo::Iterator.OneValue($!key))
    }
    multi method kv(Pair:D:) {
        Seq.new(Rakudo::Iterator.TwoValues($!key,$!value))
    }
    multi method values(Pair:D:) {
        Seq.new(Rakudo::Iterator.OneValue($!value))
    }
    multi method pairs(Pair:D:) {
        Seq.new(Rakudo::Iterator.OneValue(self))
    }
    multi method antipairs(Pair:D:) {
        Seq.new(Rakudo::Iterator.OneValue(self.new($!value,$!key)))
    }
    multi method invert(Pair:D:) {
        Seq.new(Rakudo::Iterator.Invert(self.iterator))
    }

    multi method Str(Pair:D:) { $!key ~ "\t" ~ $!value }

    multi method gist(Pair:D:) {
        self.gistseen('Pair', {
            nqp::istype($!key, Pair)
              ?? '(' ~ $!key.gist ~ ') => ' ~ $!value.gist
              !! $!key.gist ~ ' => ' ~ $!value.gist;
        })
    }

    proto sub allowed-as-bare-key(|) {*}
    multi sub allowed-as-bare-key(Mu \key --> False) { }
    multi sub allowed-as-bare-key(Str:D \key) {
        my int $i;
        my int $pos;

        while $i < nqp::chars(key) {
            return False                            # starts with numeric
              if nqp::iscclass(nqp::const::CCLASS_NUMERIC,key,$i);

            $pos = nqp::findnotcclass(
              nqp::const::CCLASS_WORD,key,$i,nqp::chars(key)
            );

            if $pos == nqp::chars(key) {
                return True;                        # reached end ok
            }
            elsif nqp::eqat(key,'-',$pos) || nqp::eqat(key,"'",$pos) {
                return False
                  if $pos == $i                     # - or ' at start
                  || $pos == nqp::chars(key) - 1;   # - or ' at end
            }
            else {
                return False;                       # not a word char
            }
            $i = $pos + 1;                          # more to parse
        }

        False                                       # the empty string
    }

    multi method raku(Pair:D: :$arglist = False) {
        self.rakuseen:
          self.^name,
          {
              nqp::isconcrete($!key)
                ?? nqp::istype($!key,Str)
                     && nqp::not_i($arglist)
                     && allowed-as-bare-key($!key)
                  ?? nqp::eqaddr($!value,True) || nqp::eqaddr($!value,False)
                    ?? nqp::concat(':',
                         nqp::concat(nqp::x('!',nqp::not_i($!value)),
                           $!key))
                    !! nqp::concat(':',
                         nqp::concat($!key,
                           nqp::concat('(',
                             nqp::concat($!value.raku,
                               ')'))))
                  !! nqp::istype($!key,Numeric)
                       && nqp::not_i(
                            nqp::istype($!key,Num) && nqp::isnanorinf($!key)
                          )
                    ?? nqp::concat($!key.raku,
                         nqp::concat(' => ',
                           $!value.raku))
                    !! nqp::istype($!key,Pair)
                      ?? nqp::concat('(',
                           nqp::concat($!key.raku,
                             nqp::concat(') => ',
                               $!value.raku)))
                      !! nqp::concat($!key.raku,
                           nqp::concat(' => ',
                             $!value.raku))
                !! nqp::concat('(',
                     nqp::concat($!key.^name,
                       nqp::concat(') => ',
                         $!value.raku)))
          }
    }

    method fmt($format = "%s\t%s") {
        sprintf($format, $!key, $!value);
    }

    multi method AT-KEY(Pair:D: $key)     { $key eq $!key ?? $!value !! Nil }
    multi method EXISTS-KEY(Pair:D: $key) { $key eq $!key }

    method FLATTENABLE_LIST() is implementation-detail {
        nqp::list()
    }
    method FLATTENABLE_HASH() is implementation-detail {
        nqp::hash($!key.Str, $!value)
    }
}

multi sub infix:<eqv>(Pair:D \a, Pair:D \b --> Bool:D) {
    nqp::hllbool(
      nqp::eqaddr(nqp::decont(a),nqp::decont(b))
        || (nqp::eqaddr(a.WHAT,b.WHAT)
             && a.key   eqv b.key
             && a.value eqv b.value)
    )
}

multi sub infix:<cmp>(Pair:D \a, Pair:D \b) {
    nqp::eqaddr((my $cmp := a.key cmp b.key),Order::Same)
      ?? (a.value cmp b.value)
      !! $cmp
}

proto sub infix:«=>»(Mu, Mu, *%) is pure {*}
multi sub infix:«=>»(Mu $key, Mu \value) { Pair.new($key, value) }

proto sub pair(Mu, Mu, *%) is pure {*}
multi sub pair(Mu \key, Mu \value) { Pair.new(key, value) }

# vim: expandtab shiftwidth=4
