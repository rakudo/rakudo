# Note: this file probably wants to be in some Perl6::CompilerGuts namespace.

our sub SETUP_NAMED_ENUM($name, $values) {
    # For now, just install EnumMap under the main name.
    my @full_ns = Perl6::Grammar::parse_name($name);
    my ($shortname, @base_ns) = @full_ns;
    pir::set_hll_global__vPSP(@base_ns, $shortname, $values);
    
    for $values.kv -> $key, $value {
        pir::set_hll_global__vPSP(@full_ns, $key, $value);
        pir::set_hll_global__vPSP(@base_ns, $key, $value);
    }
}


