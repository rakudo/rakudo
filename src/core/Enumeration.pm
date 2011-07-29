# Method that we have on enumeration types.
my role Enumeration {
    has $.key;
    has $.value;
    
    method Numeric(Enumeration:D:) {
        $!value.Numeric
    }
    
    method enums() {
        self.^enum_values
    }
    
    multi method gist(::?CLASS:D:) {
        self.^name ~ '::' ~ $!key
    }
    
    method kv(Enumeration:D:) { ($!key, $!value) }
    
    method pair(Enumeration:D:) { $!key => $!value }
    
    method perl() {
        self.defined ??
            (self.^name ~ '::' ~ $!key) !!
            self.^name;
    }
    
    method pick(*@pos, *%named) {
        self.^enum_value_list.pick(|@pos, |%named)
    }

    method Int(::?CLASS:D:) {
        self.value.Int
    }
}

# Methods that we also have if the base type of an enumeration is
# Numeric.
my role NumericEnumeration {
    multi method Str(::?CLASS:D:) {
        self.key
    }
}
