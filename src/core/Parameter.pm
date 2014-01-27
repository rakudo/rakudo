my class Parameter { # declared in BOOTSTRAP
    # class Parameter is Any {
    #     has str $!variable_name
    #     has Mu $!named_names
    #     has Mu $!type_captures
    #     has int $!flags
    #     has Mu $!nominal_type
    #     has Mu $!post_constraints
    #     has Mu $!coerce_type
    #     has str $!coerce_method
    #     has Mu $!sub_signature
    #     has Mu $!default_value
    #     has Mu $!container_descriptor;
    #     has Mu $!attr_package;

    my constant $SIG_ELEM_BIND_CAPTURE       = 1;
    my constant $SIG_ELEM_BIND_PRIVATE_ATTR  = 2;
    my constant $SIG_ELEM_BIND_PUBLIC_ATTR   = 4;
    my constant $SIG_ELEM_SLURPY_POS         = 8;
    my constant $SIG_ELEM_SLURPY_NAMED       = 16;
    my constant $SIG_ELEM_SLURPY_BLOCK       = 32;
    my constant $SIG_ELEM_INVOCANT           = 64;
    my constant $SIG_ELEM_MULTI_INVOCANT     = 128;
    my constant $SIG_ELEM_IS_RW              = 256;
    my constant $SIG_ELEM_IS_COPY            = 512;
    my constant $SIG_ELEM_IS_PARCEL          = 1024;
    my constant $SIG_ELEM_IS_OPTIONAL        = 2048;
    my constant $SIG_ELEM_ARRAY_SIGIL        = 4096;
    my constant $SIG_ELEM_HASH_SIGIL         = 8192;
    my constant $SIG_ELEM_IS_CAPTURE         = 32768;
    my constant $SIG_ELEM_UNDEFINED_ONLY     = 65536;
    my constant $SIG_ELEM_DEFINED_ONLY       = 131072;

    method name() {
        $!variable_name
    }
    
    method constraint_list() {
        nqp::isnull($!post_constraints) ?? () !!
            nqp::hllize($!post_constraints)
    }
    
    method constraints() {
        all(nqp::isnull($!post_constraints) ?? () !!
            nqp::hllize($!post_constraints))
    }

    method type() {
        $!nominal_type
    }

    method named() {
        !nqp::p6bool(nqp::isnull($!named_names)) ||
            nqp::p6bool($!flags +& $SIG_ELEM_SLURPY_NAMED)
    }

    method named_names() {
        if !nqp::isnull($!named_names) {
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
            ($!flags +& ($SIG_ELEM_SLURPY_POS +| $SIG_ELEM_SLURPY_NAMED +| $SIG_ELEM_IS_CAPTURE)) == 0 &&
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
        if !nqp::isnull($!type_captures) {
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

    method !flags() { $!flags }

    multi method ACCEPTS(Parameter:D: Parameter:D $other) {
        return False unless $other.type ~~ $.type;
        return False unless 
            $!flags +& $SIG_ELEM_DEFINED_ONLY <= $other!flags +& $SIG_ELEM_DEFINED_ONLY
            and $!flags +& $SIG_ELEM_UNDEFINED_ONLY <= 
                $other!flags +& $SIG_ELEM_UNDEFINED_ONLY;
        if $.sub_signature {
            return False unless $other.sub_signature ~~ $.sub_signature;
        }
        if $.named {
            return False unless $other.named;
            return False unless Set($other.named_names) (<=) Set($.named_names);
        }
        return True;
    }
    
    multi method perl(Parameter:D:) {
        my $perl = '';
        my $rest = '';
        my $type = $!nominal_type.HOW.name($!nominal_type);
        my $truemu='';

        # XXX Need a CODE_SIGIL too?
        if $!flags +& $SIG_ELEM_ARRAY_SIGIL or 
            $!flags +& $SIG_ELEM_HASH_SIGIL or 
            $type ~~ /^^ Callable >> / {
            $type ~~ / .*? \[ <( .* )> \] $$/;
            $perl = ~$/;
            $truemu = 'Mu ' if $perl eq 'Mu'; # Positional !~~ Positional[Mu]
        }
        else {
            $perl = $type;
        }
        if $!flags +& $SIG_ELEM_DEFINED_ONLY {
            $perl ~= ':D';
        } elsif $!flags +& $SIG_ELEM_UNDEFINED_ONLY {
            $perl ~= ':U';
        }
        $perl ~= " ::$_" for @($.type_captures);
        my $name = $!variable_name || '';
        if $!flags +& $SIG_ELEM_IS_CAPTURE {
            $name = '|' ~ $name;
        } elsif $!flags +& $SIG_ELEM_IS_PARCEL {
            $name = '\\' ~ $name unless $name ~~ /^^ <[@$]>/;
        } elsif !$name {
            if $!flags +& $SIG_ELEM_ARRAY_SIGIL {        
            $name = '@';
            } elsif $!flags +& $SIG_ELEM_HASH_SIGIL {
            $name = '%';
            } elsif $type ~~ /^^ Callable >> / {
            $name = '&';
            } else {
            $name = '$';
            }
        }
        my $default = self.default();
        if self.slurpy {
            $name = '*' ~ $name;
        } elsif self.named {
            my @names := self.named_names;
            $name = ':' ~ $_ ~ '(' ~ $name ~ ')'for @names;
            $name ~= '!' unless self.optional;
        } elsif self.optional && !$default {
            $name ~= '?';
        }
        if $!flags +& $SIG_ELEM_IS_RW {
            $rest ~= ' is rw';
        } elsif $!flags +& $SIG_ELEM_IS_COPY {
            $rest ~= ' is copy';
        }
        if $!flags +& $SIG_ELEM_IS_PARCEL and $name ~~ /^^ <[@$]>/ {
            $rest ~= ' is parcel';
        }
        $rest ~= ' where { ... }' if !nqp::isnull($!post_constraints);
        $rest ~= ' = { ... }' if $default;
        unless nqp::isnull($!sub_signature) {
            my $sig = $!sub_signature.perl();
            $sig ~~ s/^^ ':'//;
            $rest ~= ' ' ~ $sig;
        }
        if $name ne '$' or $rest {
            $perl ~= ($perl ?? ' ' !! '') ~ $name;
            $perl ~~ s/^^ \s* Mu \s+//;
        }
        $truemu ~ $perl ~ $rest;
    }

    method sub_signature(Parameter:D:) {
        nqp::isnull($!sub_signature) ?? Any !! $!sub_signature
    }
}
