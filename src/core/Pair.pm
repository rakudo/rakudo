my class Pair does Associative {
    has $.key is default(Nil);
    has $.value is rw is default(Nil);

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
    multi method new(Pair: Mu :$key, Mu :$value) {
        my \p := nqp::p6bindattrinvres(
          nqp::create(self),Pair,'$!key',$key);
        nqp::bindattr(p,Pair,'$!value',$value);
        p
    }

    multi method WHICH(Pair:D:) {
        nqp::iscont($!value)
          ?? nextsame()
          !! "Pair|" ~ $!key.WHICH ~ "|" ~ $!value.WHICH
    }

    multi method ACCEPTS(Pair:D: %h) {
        $!value.ACCEPTS(%h.AT-KEY($!key));
    }
    multi method ACCEPTS(Pair:D: Pair:D $p) {
        $!value.ACCEPTS(nqp::getattr(nqp::decont($p),Pair,'$!value'));
    }
    multi method ACCEPTS(Pair:D: Mu $other) {
        $other."$!key"().Bool === $!value.Bool
    }

    method antipair(Pair:D:) { self.new($!value,$!key) }
    method freeze(Pair:D:) { $!value := nqp::decont($!value) }

    multi method keys(Pair:D:)      { ($!key,) }
    multi method kv(Pair:D:)        { $!key, $!value }
    multi method values(Pair:D:)    { ($!value,) }
    multi method pairs(Pair:D:)     { (self,) }
    multi method antipairs(Pair:D:) { (self.new($!value,$!key),) }
    multi method invert(Pair:D:)    { $!value »=>» $!key }

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
            nqp::istype($!key, Str)
              ?? !$arglist && $!key ~~ /^ [<alpha>\w*] +% <[\-']> $/
                ?? nqp::istype($!value,Bool)
                   ?? ':' ~ '!' x !$!value ~ $!key
                   !! ':' ~ $!key ~ '(' ~ $!value.perl ~ ')'
                !! $!key.perl ~ ' => ' ~ $!value.perl
              !! nqp::istype($!key, Numeric)
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
    nqp::p6bool(
      nqp::eqaddr(a,b)
        || (nqp::eqaddr(a.WHAT,b.WHAT)
             && a.key   eqv b.key
             && a.value eqv b.value)
    )
}

multi sub infix:<cmp>(Pair:D \a, Pair:D \b) {
    (a.key cmp b.key) || (a.value cmp b.value)
}

sub infix:«=>»(Mu $key, Mu \value) is pure {
    Pair.new($key, value)
}

sub pair(Mu $key, \value) is pure {
    Pair.new($key, value)
}

# vim: ft=perl6 expandtab sw=4
