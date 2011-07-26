my class Enum {
    has $!key;
    has $!value;

    method new(:$key, Mu :$value) { self.CREATE.BUILD($key, $value) }
    method BUILD(\$key, Mu \$value) { $!key = $key; $!value = $value; self }

    method key()   { $!key }
    method kv()    { $!key, $!value }
    method value() { $!value }

    method keys()  { ($!key,).list }
    method values(){ ($!value,).list }
    method pairs() { (self,).list }

    multi method Str(Enum:D:) { $.key ~ "\t" ~ $.value }
    multi method perl(Enum:D:) { $.key.perl ~ ' => ' ~ $.value.perl }
}

