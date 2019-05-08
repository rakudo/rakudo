my class Pair does Associative {
    has $.key is default(Nil);
    has $.value is rw is default(Nil);
    has Mu $!WHICH;

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
        nqp::unless(
          $!WHICH,
          ($!WHICH := nqp::if(
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
          ))
        )
    }

    multi method ACCEPTS(Pair:D: %h) {
        $!value.ACCEPTS(%h.AT-KEY($!key));
    }
    multi method ACCEPTS(Pair:D: Pair:D $p) {
        $!value.ACCEPTS(nqp::getattr(nqp::decont($p),Pair,'$!value'));
    }
    multi method ACCEPTS(Pair:D: Mu $other) {
        nqp::if(
          nqp::can($other,(my $method := $!key.Str)),
          ($other."$method"().Bool === $!value.Bool),
          X::Method::NotFound.new(
            invocant => $other,
            method   => $method,
            typename => $other.^name,
            addendum => "Did you try to smartmatch against a Pair specifically?  If so, then the
key of the Pair should be a valid method name, not '$method'."
          ).throw
        )
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

    multi method perl(Pair:D: :$arglist) {
        self.perlseen('Pair', -> :$arglist {
            nqp::istype($!key, Str) && nqp::isconcrete($!key)
              ?? !$arglist && $!key ~~ /^ [<alpha>\w*] +% <[\-']> $/
                ?? nqp::istype($!value,Bool) && nqp::isconcrete($!value)
                   ?? ':' ~ '!' x !$!value ~ $!key
                   !! ':' ~ $!key ~ '(' ~ $!value.perl ~ ')'
                !! $!key.perl ~ ' => ' ~ $!value.perl
              !! nqp::istype($!key, Numeric)
                   && nqp::isconcrete($!key)
                   && !(nqp::istype($!key,Num) && nqp::isnanorinf($!key))
                ?? $!key.perl ~ ' => ' ~ $!value.perl
                !! '(' ~ $!key.perl ~ ') => ' ~ $!value.perl
        }, :$arglist)
    }

    method fmt($format = "%s\t%s") {
        sprintf($format, $!key, $!value);
    }

    multi method AT-KEY(Pair:D: $key)     { $key eq $!key ?? $!value !! Nil }
    multi method EXISTS-KEY(Pair:D: $key) { $key eq $!key }

    method FLATTENABLE_LIST() { nqp::list() }
    method FLATTENABLE_HASH() { nqp::hash($!key.Str, $!value) }
}

multi sub infix:<eqv>(Pair:D \a, Pair:D \b) {
    nqp::hllbool(
      nqp::eqaddr(a,b)
        || (nqp::eqaddr(a.WHAT,b.WHAT)
             && a.key   eqv b.key
             && a.value eqv b.value)
    )
}

multi sub infix:<cmp>(Pair:D \a, Pair:D \b) {
    (a.key cmp b.key) || (a.value cmp b.value)
}

proto sub infix:«=>»(Mu, Mu, *%) is pure {*}
multi sub infix:«=>»(Mu $key, Mu \value) { Pair.new($key, value) }

proto sub pair(Mu, Mu, *%) is pure {*}
multi sub pair(Mu \key, Mu \value) { Pair.new(key, value) }

# vim: ft=perl6 expandtab sw=4
