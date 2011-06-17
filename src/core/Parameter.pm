my class Parameter {
    # XXX constant...
    my $SIG_ELEM_BIND_CAPTURE       := 1;
    my $SIG_ELEM_BIND_PRIVATE_ATTR  := 2;
    my $SIG_ELEM_BIND_PUBLIC_ATTR   := 4;
    my $SIG_ELEM_SLURPY_POS         := 8;
    my $SIG_ELEM_SLURPY_NAMED       := 16;
    my $SIG_ELEM_SLURPY_BLOCK       := 32;
    my $SIG_ELEM_INVOCANT           := 64;
    my $SIG_ELEM_MULTI_INVOCANT     := 128;
    my $SIG_ELEM_IS_RW              := 256;
    my $SIG_ELEM_IS_COPY            := 512;
    my $SIG_ELEM_IS_PARCEL          := 1024;
    my $SIG_ELEM_IS_OPTIONAL        := 2048;
    my $SIG_ELEM_ARRAY_SIGIL        := 4096;
    my $SIG_ELEM_HASH_SIGIL         := 8192;
    my $SIG_ELEM_IS_CAPTURE         := 32768;
    my $SIG_ELEM_UNDEFINED_ONLY     := 65536;
    my $SIG_ELEM_DEFINED_ONLY       := 131072;

    method name() {
        $!variable_name
    }
    
    method positional() {
        pir::perl6_booleanize__PI(
            ($!flags +& ($SIG_ELEM_SLURPY_POS +| $SIG_ELEM_SLURPY_NAMED)) == 0 &&
            pir::isnull__IP($!named_names)
         )
    }
    
    method optional() {
        ?($!flags +& $SIG_ELEM_IS_OPTIONAL)
    }
    
    # XXX TODO: Many more bits :-)
    multi method perl(Parameter:D:) {
        my $perl = $!nominal_type.HOW.name($!nominal_type);
        if $!variable_name {
            $perl = $perl ~ " " ~ $!variable_name;
        }
        $perl
    }
}
