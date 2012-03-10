# Method that we have on enumeration types.
my role Enumeration {
    has $.key;
    has $.value;
    
    multi method Numeric(::?CLASS:D:) { $!value.Numeric }
    
    method enums() {
        self.^enum_values
    }
    
    multi method gist(::?CLASS:D:) {
        $!key
    }
    
    method kv(::?CLASS:D:) { ($!key, $!value) }
    
    method pair(::?CLASS:D:) { $!key => $!value }
    
    method perl() {
        self.defined ??
            (self.^name ~ '::' ~ $!key) !!
            self.^name;
    }
    
    method pick(*@pos, *%named) {
        self.^enum_value_list.pick(|@pos, |%named)
    }
    method roll(*@pos, *%named) {
        self.^enum_value_list.roll(|@pos, |%named)
    }

    method Int(::?CLASS:D:) {
        self.value.Int
    }

    method postcircumfix:<( )>($x) {
        self.^enum_from_value($x)
    }
}

# Methods that we also have if the base type of an enumeration is
# Numeric.
my role NumericEnumeration {
    multi method Str(::?CLASS:D:) {
        self.key
    }
}

sub ANON_ENUM(*@args) {
    my Mu $prev = -1;
    my %res;
    for @args {
        if .^isa(Enum) {
            %res{.key} = .value;
        }
        else {
            %res{$_} = $prev.=succ;
        }
    }
    my $r := nqp::create(EnumMap);
    nqp::bindattr($r, EnumMap, '$!storage',
        nqp::getattr(%res, EnumMap, '$!storage'));
    $r;
}
