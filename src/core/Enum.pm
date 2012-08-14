my class Enum does Associative {
    has $.key;
    has $.value;

    method new(:$key, Mu :$value) { nqp::create(self).BUILD($key, $value) }
    method BUILD(\key, Mu \value) { $!key = key; $!value = value; self }

    multi method ACCEPTS(Enum:D: Associative:D $topic) { 
        $topic{$.key} ~~ $.value 
    }

    multi method ACCEPTS(Enum:D: Mu $topic) {
        my $method = $.key;
        $topic."$method"() === $.value;
    }
    
    method invert() {
        Enum.new(key => $.value, value => $.key);
    }

    method key(Enum:D:)   { $!key }
    method kv(Enum:D:)    { $!key, $!value }
    method value(Enum:D:) { $!value }

    method keys(Enum:D:)  { ($!key,).list }
    method values(Enum:D:){ ($!value,).list }
    method pairs(Enum:D:) { (self,).list }

    multi method Str(Enum:D:) { $.key ~ "\t" ~ $.value }
    multi method perl(Enum:D:) { $.key.perl ~ ' => ' ~ $.value.perl }

    method fmt($format = "%s\t%s") {
        sprintf($format, $.key, $.value);
    }
    
    method at_key($key) {
        $key eq $!key ?? $!value !! Mu
    }
    
    method FLATTENABLE_LIST() { nqp::list() }
    method FLATTENABLE_HASH() { nqp::hash($!key, $!value) }
}

multi sub infix:<eqv>(Enum:D $a, Enum:D $b) {
    $a.WHAT === $b.WHAT && $a.key eqv $b.key && $a.value eqv $b.value
}

multi infix:<cmp>(Enum:D \a, Enum:D \b) {
    (a.key cmp b.key) || (a.value cmp b.value)
}
