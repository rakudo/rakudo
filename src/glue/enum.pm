# Note: this file probably wants to be in some Perl6::CompilerGuts namespace.

our sub SETUP_NAMED_ENUM($name, $values) {
    # For now, just install EnumMap under the main name.
    my @full_ns = Perl6::Grammar::parse_name($name);
    my ($shortname, @base_ns) = @full_ns;
    my $enumeration-object = (class {
        method WHAT { $enumeration-object }
        method enums { $values }
        method Str { $name }
        multi method roll($num = 1) { $values.roll($num) }
        multi method pick($num = 1) { $values.pick($num) }
        method ACCEPTS($topic) { $topic eqv any $values.values }
        method invert() { $values.invert }
    });
    pir::set_hll_global__vPSP(@base_ns, $shortname, $enumeration-object);

    for $values.kv -> $key, $value {
        my $enum-object = $value but role {
            method WHAT { $enumeration-object }
            method perl { $name ~ '::' ~ $key }
            method Str { $name ~ '::' ~ $key }
            method Stringy { $key }
            method key { $key }
            method value { $value }
            method pair { $key => $value }
            method kv { $key, $value }
            method defined { True }
        };
        pir::set_hll_global__vPSP(@full_ns, $key, $enum-object);
        pir::set_hll_global__vPSP(@base_ns, $key, $enum-object);
    }
}


