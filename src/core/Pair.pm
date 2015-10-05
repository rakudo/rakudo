my class Pair does Associative {
    has $.key is default(Nil);
    has $.value is rw is default(Nil);

    multi method new(Mu $key, Mu \value) {
        nqp::create(self).BUILD($key, value)
    }
    multi method new(Mu :$key, Mu :$value) {
        nqp::create(self).BUILD($key, $value)
    }
    method BUILD(Mu $!key, Mu \value) {
        nqp::bindattr(self, Pair, '$!value', value);
        self
    }

    multi method ACCEPTS(Pair:D: %h) {
        $.value.ACCEPTS(%h{$.key});
    }

    multi method ACCEPTS(Pair:D: Mu $other) {
        $other."$.key"().Bool === $.value.Bool
    }

    method antipair(Pair:D:) { self.new(key => $!value, value => $!key) }
    method freeze(Pair:D:) { $!value := nqp::decont($!value) }

    multi method keys(Pair:D:)      { ($!key,).list }
    multi method kv(Pair:D:)        { $!key, $!value }
    multi method values(Pair:D:)    { ($!value,).list }
    multi method pairs(Pair:D:)     { (self,).list }
    multi method antipairs(Pair:D:) { self.new(key => $!value, value => $!key) }
    multi method invert(Pair:D:)    { $!value »=>» $!key }

    multi method Str(Pair:D:) { $!key ~ "\t" ~ $!value }

    multi method gist(Pair:D:) {
        my $result;
        if not %*gistseen<TOP> { my %*gistseen = :TOP ; return self.gist }
        if %*gistseen{self.WHICH} { %*gistseen{self.WHICH} = 2; return "Pair_{self.WHERE}" }
        %*gistseen{self.WHICH} = 1;
        if nqp::istype($!key, Pair) {
            $result = '(' ~ $!key.gist ~ ') => ' ~ $!value.gist;
        } else {
            $result = $!key.gist ~ ' => ' ~ $!value.gist;
        }
        $result = "(\\Pair_{self.WHERE} = $result)" if %*gistseen{self.WHICH}:delete == 2;
        $result;
    }

    multi method perl(Pair:D: :$arglist) {
        my $result;
        if not %*perlseen<TOP> { my %*perlseen = :TOP ; return self.perl(:$arglist) }
        if %*perlseen{self.WHICH} { %*perlseen{self.WHICH} = 2; return "Pair_{self.WHERE}" }
        %*perlseen{self.WHICH} = 1;
        if nqp::istype($!key, Pair) {
            $result = '(' ~ $!key.perl ~ ') => ' ~ $!value.perl;
        } elsif nqp::istype($!key, Str) and !$arglist and $!key ~~ /^ [<alpha>\w*] +% <[\-']> $/ {
            if nqp::istype($!value,Bool) {
                $result = ':' ~ '!' x !$!value ~ $!key;
            } else {
                $result = ':' ~ $!key ~ '(' ~ $!value.perl ~ ')';
            }
        } else {
            $result = $!key.perl ~ ' => ' ~ $!value.perl;
        }
        $result = "(my \\Pair_{self.WHERE} = $result)" if %*perlseen{self.WHICH}:delete == 2;
        $result;
    }

    method fmt($format = "%s\t%s") {
        sprintf($format, $!key, $!value);
    }

    multi method AT-KEY(Pair:D: $key)     { $key eq $!key ?? $!value !! Mu }
    multi method EXISTS-KEY(Pair:D: $key) { $key eq $!key }

    method FLATTENABLE_LIST() { nqp::list() }
    method FLATTENABLE_HASH() { nqp::hash($!key, $!value) }
}

multi sub infix:<eqv>(Pair:D $a, Pair:D $b) {
    $a.WHAT === $b.WHAT && $a.key eqv $b.key && $a.value eqv $b.value
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
