# Method that we have on enumeration types.
my role Enumeration {
    has $.key;
    has $.value;
    
    method Numeric() {
        $!value.Numeric
    }
    
    method enums() {
        self.^enum_values
    }
    
    multi method gist(::?CLASS:D:) {
        self.^name ~ '::' ~ $!key
    }
    
    method kv() { ($!key, $!value) }
    
    method pair() { $!key => $!value }
    
    method perl() {
        self.defined ??
            (self.^name ~ '::' ~ $!key) !!
            self.^name;
    }
    
    method pick(*@pos, *%named) {
        self.^enum_value_list.pick(|@pos, |%named)
    }
}

# Methods that we also have if the base type of an enumeration is
# Numeric.
my role NumericEnumeration {
    multi method Str(::?CLASS:D:) {
        self.key
    }
}
