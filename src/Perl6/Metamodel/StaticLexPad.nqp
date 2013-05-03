my $SLP_FLAG_IS_CLONED := 1;
my $SLP_FLAG_IS_STATE  := 2;
class Perl6::Metamodel::StaticLexPad {
    # Hash of name to static values.
    has %!static_values;
    
    # Hash of name to integer flags.
    has %!flags;
    
    # Has anything changed since the guts last memoized it?
    has int $!changed;
    
    # Does this block get fresh magicals?
    has int $!fresh_magicals;
    
    # Adds a static value (which may actually be a container) to the static
    # lexpad.
    method add_static_value($name, $value, $is_cloned, $is_state) {
        %!static_values{$name} := $value;
        my $flags := 0;
        if $is_cloned { $flags := $flags + $SLP_FLAG_IS_CLONED }
        if $is_state  { $flags := $flags + $SLP_FLAG_IS_STATE  }
        %!flags{$name} := $flags;
        $!changed := 1;
        $value
    }
    
    # Flag that the block gets fresh magicals.
    method set_fresh_magicals() {
        $!fresh_magicals := 1;
        $!changed := 1;
    }
}
