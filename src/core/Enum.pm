my class Enum {
    has $!key;
    has $!value;

    method new(:$key, :$value) { self.CREATE.BUILD($key, $value) }
    method BUILD(\$key, \$value) { $!key = $key; $!value = $value; self }

    method key()   { $!key }
    method kv()    { $!key, $!value }
    method value() { $!value }

    multi method Str(Enum:D:) { $.key ~ "\t" ~ $.value }
}

