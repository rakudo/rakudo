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
    
    method constraint_list() {
        pir::isnull($!post_constraints) ?? () !!
            pir::perl6ize_type__PP($!post_constraints)
    }
    
    method constraints() {
        all(pir::isnull($!post_constraints) ?? () !!
            pir::perl6ize_type__PP($!post_constraints))
    }

    method type() {
        $!nominal_type
    }

    method named() {
        !nqp::p6bool(nqp::isnull($!named_names)) ||
            nqp::p6bool($!flags +& $SIG_ELEM_SLURPY_NAMED)
    }

    method named_names() {
        if !pir::isnull($!named_names) {
            my Int $count = nqp::p6box_i(nqp::elems($!named_names));
            my Int $i = 0;
            my @res;
            while $i < $count {
                @res.push: nqp::p6box_s(nqp::atpos($!named_names, nqp::unbox_i($i)));
                $i++;
            }
            @res;
        } else {
            ().list
        }
    }
    
    method positional() {
        nqp::p6bool(
            ($!flags +& ($SIG_ELEM_SLURPY_POS +| $SIG_ELEM_SLURPY_NAMED)) == 0 &&
            nqp::isnull($!named_names)
         )
    }

    method slurpy() {
        nqp::p6bool(
            $!flags +& ($SIG_ELEM_SLURPY_POS
                        +| $SIG_ELEM_SLURPY_NAMED
                        +| $SIG_ELEM_SLURPY_BLOCK)
        )
    }
    
    method optional() {
        ?($!flags +& $SIG_ELEM_IS_OPTIONAL)
    }
    
    method parcel() {
        ?($!flags +& $SIG_ELEM_IS_PARCEL)
    }
    
    method capture() {
        ?($!flags +& $SIG_ELEM_IS_CAPTURE)
    }
    
    method rw() {
        ?($!flags +& $SIG_ELEM_IS_RW)
    }
    
    method copy() {
        ?($!flags +& $SIG_ELEM_IS_COPY)
    }
    
    method readonly() {
        !($.rw || $.copy || $.parcel)
    }
    
    method invocant() {
        ?($!flags +& $SIG_ELEM_INVOCANT)
    }
    
    method default() {
        nqp::isnull($!default_value) ?? Any !!
            $!default_value ~~ Code ?? $!default_value !! { $!default_value }
    }
    
    method type_captures() {
        if !pir::isnull($!type_captures) {
            my Int $count = nqp::p6box_i(nqp::elems($!type_captures));
            my Int $i = 0;
            my @res;
            while $i < $count {
                @res.push: nqp::p6box_s(nqp::atpos($!type_captures, nqp::unbox_i($i)));
                $i++;
            }
            @res;
        } else {
            ().list
        }
    }
    
    # XXX TODO: A few more bits :-)
    multi method perl(Parameter:D:) {
        my $perl = '';
        my $type = $!nominal_type.HOW.name($!nominal_type);
        if $!flags +& $SIG_ELEM_ARRAY_SIGIL {
            # XXX Need inner type
        }
        elsif $!flags +& $SIG_ELEM_HASH_SIGIL {
            # XXX Need inner type
        }
        else {
            $perl = $type;
            if $!flags +& $SIG_ELEM_DEFINED_ONLY {
                $perl ~= ':D';
            } elsif $!flags +& $SIG_ELEM_UNDEFINED_ONLY {
                $perl ~= ':U';
            }
            $perl ~= ' ';
        }
        if $!variable_name {
            my $name = $!variable_name;
            if $!flags +& $SIG_ELEM_IS_CAPTURE {
                $perl ~= '|' ~ $name;
            } elsif $!flags +& $SIG_ELEM_IS_PARCEL {
                $perl ~= '\\' ~ $name;
            } else {
                my $default = self.default();
                if self.named {
                    my @names := self.named_names;
                    my $/ := $name ~~ / ^^ $<sigil>=<[$@%&]> $<desigil>=(@names) $$ /;
                    $name = ':' ~ $name if $/;
                    for @names {
                        next if $/ and $_ eq $<desigil>;
                        $name = ':' ~ $_ ~ '(' ~ $name ~ ')';
                    }
                    $name ~= '!' unless self.optional;
                } elsif self.optional && !$default {
                    $name ~= '?';
                } elsif self.slurpy {
                    $name = '*' ~ $name;
                }
                $perl ~= $name;
                if $!flags +& $SIG_ELEM_IS_RW {
                    $perl ~= ' is rw';
                } elsif $!flags +& $SIG_ELEM_IS_COPY {
                    $perl ~= ' is copy';
                }
                $perl ~= ' = { ... }' if $default;
                unless nqp::isnull($!sub_signature) {
                    $perl ~= ' ' ~ $!sub_signature.perl();
                }
            }
        }
        $perl
    }
}
