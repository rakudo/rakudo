my class Enum {
    has $!key;
    has $!value;

    method new(:$key, Mu :$value) { self.CREATE.BUILD($key, $value) }
    method BUILD(\$key, Mu \$value) { $!key = $key; $!value = $value; self }

    multi method ACCEPTS(Enum:D: Associative:D: $topic) { 
        $topic{$.key} ~~ $.value 
    }

    multi method ACCEPTS(Enum:D: Mu $topic) {
        my $method = $.key;
        $topic."$method"() === $.value;
    }

    method key(Enum:D:)   { $!key }
    method kv(Enum:D:)    { $!key, $!value }
    method value(Enum:D:) { $!value }

    method keys(Enum:D:)  { ($!key,).list }
    method values(Enum:D:){ ($!value,).list }
    method pairs(Enum:D:) { (self,).list }

    multi method Str(Enum:D:) { $.key ~ "\t" ~ $.value }
    multi method perl(Enum:D:) { $.key.perl ~ ' => ' ~ $.value.perl }
}

