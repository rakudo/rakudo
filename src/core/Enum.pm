my class Enum does Associative {
    has $.key;
    has $.value;

    method new(:$key, Mu :$value) { nqp::create(self).BUILD($key, $value) }
    method BUILD($!key, Mu $!value) { self }

    multi method ACCEPTS(Enum:D: Associative:D $topic) {
        $topic{$!key} ~~ $!value
    }

    multi method ACCEPTS(Enum:D: Mu $topic) {
        my $method = $!key;
        $topic."$method"() === $!value;
    }

    method antipair(Enum:D:) { self.new(key => $!value, value => $!key) }

    multi method keys(Enum:D:)      { ($!key,).list }
    multi method kv(Enum:D:)        { $!key, $!value }
    multi method values(Enum:D:)    { ($!value,).list }
    multi method pairs(Enum:D:)     { (self,).list }
    multi method antipairs(Enum:D:) { self.new(key => $!value, value => $!key) }
    multi method invert(Enum:D:)    { $!value »=>» $!key }

    multi method Str(Enum:D:) { $!key ~ "\t" ~ $!value }

    multi method gist(Enum:D:) {
        my $result;
        if not %*gistseen<TOP> { my %*gistseen = :TOP ; return self.gist }
        if %*gistseen{self.WHICH} { %*gistseen{self.WHICH} = 2; return "Pair_{self.WHERE}" }
        %*gistseen{self.WHICH} = 1;
        if nqp::istype($!key, Enum) {
            $result = '(' ~ $!key.gist ~ ') => ' ~ $!value.gist;
        } else {
            $result = $!key.gist ~ ' => ' ~ $!value.gist;
        }
        $result = "(\\Pair_{self.WHERE} = $result)" if %*gistseen{self.WHICH}:delete == 2;
        $result;
    }

    multi method perl(Enum:D: :$arglist) {
        my $result;
        if not %*perlseen<TOP> { my %*perlseen = :TOP ; return self.perl(:$arglist) }
        if %*perlseen{self.WHICH} { %*perlseen{self.WHICH} = 2; return "Pair_{self.WHERE}" }
        %*perlseen{self.WHICH} = 1;
        if nqp::istype($!key, Enum) {
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

    multi method AT-KEY(Enum:D: $key)     { $key eq $!key ?? $!value !! Mu }
    multi method EXISTS-KEY(Enum:D: $key) { $key eq $!key }

    method FLATTENABLE_LIST() { nqp::list() }
    method FLATTENABLE_HASH() { nqp::hash($!key, $!value) }
}

multi sub infix:<eqv>(Enum:D $a, Enum:D $b) {
    $a.WHAT === $b.WHAT && $a.key eqv $b.key && $a.value eqv $b.value
}

multi sub infix:<cmp>(Enum:D \a, Enum:D \b) {
    (a.key cmp b.key) || (a.value cmp b.value)
}

# vim: ft=perl6 expandtab sw=4
