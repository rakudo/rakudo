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
    #     has Mu $!why;

    my constant $SIG_ELEM_BIND_CAPTURE       = 1;
    my constant $SIG_ELEM_BIND_PRIVATE_ATTR  = 2;
    my constant $SIG_ELEM_BIND_PUBLIC_ATTR   = 4;
    my constant $SIG_ELEM_SLURPY_POS         = 8;
    my constant $SIG_ELEM_SLURPY_NAMED       = 16;
    my constant $SIG_ELEM_SLURPY_LOL         = 32;
    my constant $SIG_ELEM_INVOCANT           = 64;
    my constant $SIG_ELEM_MULTI_INVOCANT     = 128;
    my constant $SIG_ELEM_IS_RW              = 256;
    my constant $SIG_ELEM_IS_COPY            = 512;
    my constant $SIG_ELEM_IS_RAW             = 1024;
    my constant $SIG_ELEM_IS_OPTIONAL        = 2048;
    my constant $SIG_ELEM_ARRAY_SIGIL        = 4096;
    my constant $SIG_ELEM_HASH_SIGIL         = 8192;
    my constant $SIG_ELEM_IS_CAPTURE         = 32768;
    my constant $SIG_ELEM_UNDEFINED_ONLY     = 65536;
    my constant $SIG_ELEM_DEFINED_ONLY       = 131072;
    my constant $SIG_ELEM_SLURPY_ONEARG      = 16777216;

    my constant $SIG_ELEM_IS_NOT_POSITIONAL = $SIG_ELEM_SLURPY_POS
                                           +| $SIG_ELEM_SLURPY_NAMED
                                           +| $SIG_ELEM_IS_CAPTURE;
    my constant $SIG_ELEM_IS_SLURPY = $SIG_ELEM_SLURPY_POS
                                   +| $SIG_ELEM_SLURPY_NAMED
                                   +| $SIG_ELEM_SLURPY_LOL
                                   +| $SIG_ELEM_SLURPY_ONEARG;
    my constant $SIG_ELEM_IS_NOT_READONLY = $SIG_ELEM_IS_RW
                                         +| $SIG_ELEM_IS_COPY
                                         +| $SIG_ELEM_IS_RAW;

    method name() {
        nqp::isnull_s($!variable_name) ?? Nil !! $!variable_name
    }

    method sigil() {
        nqp::bitand_i($!flags,$SIG_ELEM_IS_CAPTURE)
          ?? '|'
          !! nqp::isnull_s($!variable_name)
            ?? nqp::bitand_i($!flags,$SIG_ELEM_ARRAY_SIGIL)
              ?? '@'
              !!  nqp::bitand_i($!flags,$SIG_ELEM_HASH_SIGIL)
                ?? '%'
                !! nqp::eqat(nqp::unbox_s($!nominal_type.^name),'Callable',0)
                  ?? '&'
                  !! nqp::bitand_i($!flags,$SIG_ELEM_IS_RAW)
                    ?? '\\'
                    !! '$'
            !! nqp::bitand_i($!flags,$SIG_ELEM_IS_RAW) && nqp::iseq_i(
                 nqp::index('@$%&',nqp::substr($!variable_name,0,1)),-1)
              ?? '\\'
              !! nqp::substr($!variable_name,0,1)
    }

    method twigil() {
        nqp::bitand_i($!flags,$SIG_ELEM_BIND_PUBLIC_ATTR)
          ?? '.'
          !! nqp::bitand_i($!flags,$SIG_ELEM_BIND_PRIVATE_ATTR)
            ?? '!'
            !! ''
    }
    method modifier() {
        nqp::bitand_i($!flags,$SIG_ELEM_DEFINED_ONLY)
          ?? ':D'
          !! nqp::bitand_i($!flags,$SIG_ELEM_UNDEFINED_ONLY)
            ?? ':U'
            !! ''
    }

    method constraint_list() {
        nqp::isnull($!post_constraints) ?? () !!
            nqp::hllize($!post_constraints)
    }

    method constraints() {
        all(nqp::isnull($!post_constraints) ?? () !!
            nqp::hllize($!post_constraints))
    }

    method type() { $!nominal_type }
    method named_names() {
        nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',$!named_names)
    }
    method named() {
        nqp::p6bool(
          $!named_names || nqp::bitand_i($!flags,$SIG_ELEM_SLURPY_NAMED)
        )
    }

    method positional() {
        nqp::p6bool(
          nqp::isnull($!named_names)
          && nqp::iseq_i(nqp::bitand_i($!flags,$SIG_ELEM_IS_NOT_POSITIONAL),0)
        )
    }

    method slurpy() {
        nqp::p6bool(nqp::bitand_i($!flags,$SIG_ELEM_IS_SLURPY))
    }
    method optional() {
        nqp::p6bool(nqp::bitand_i($!flags,$SIG_ELEM_IS_OPTIONAL))
    }
    method raw() {
        nqp::p6bool(nqp::bitand_i($!flags,$SIG_ELEM_IS_RAW))
    }
    method capture() {
        nqp::p6bool(nqp::bitand_i($!flags,$SIG_ELEM_IS_CAPTURE))
    }
    method rw() {
        nqp::p6bool(nqp::bitand_i($!flags,$SIG_ELEM_IS_RW))
    }
    method onearg() {
        nqp::p6bool(nqp::bitand_i($!flags,$SIG_ELEM_SLURPY_ONEARG))
    }
    method copy() {
        nqp::p6bool(nqp::bitand_i($!flags,$SIG_ELEM_IS_COPY))
    }
    method readonly() {
        nqp::p6bool(
          nqp::iseq_i(nqp::bitand_i($!flags,$SIG_ELEM_IS_NOT_READONLY),0)
        )
    }
    method invocant() {
        nqp::p6bool(nqp::bitand_i($!flags,$SIG_ELEM_INVOCANT))
    }
    method multi-invocant() {
        nqp::p6bool(nqp::bitand_i($!flags,$SIG_ELEM_MULTI_INVOCANT))
    }
    method default() {
        nqp::isnull($!default_value)
          ?? Any
          !! nqp::istype($!default_value,Code)
            ?? $!default_value
            !! { $!default_value }
    }
    method type_captures() {
        nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',$!type_captures)
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
        True;
    }

    multi method perl(Parameter:D: Mu:U :$elide-type = Any, :&where = -> $ { 'where { ... }' }) {
        my $perl = '';
        my $rest = '';
        my $type = $!nominal_type.^name;
        my $modifier = self.modifier;

        $perl ~= "::$_ " for @($.type_captures);
        # XXX Need a CODE_SIGIL too?
        if $!flags +& $SIG_ELEM_ARRAY_SIGIL or
            $!flags +& $SIG_ELEM_HASH_SIGIL or
            $type ~~ /^^ Callable >> / {
            $type ~~ / .*? \[ <( .* )> \] $$/;
            $perl ~= $/ ~ $modifier if $/;
        }
        elsif $modifier or
                !nqp::eqaddr(nqp::decont($!nominal_type), nqp::decont($elide-type)) {
            $perl ~= $type ~ $modifier;
        }
        my $name = $.name;
        if $name {
            if $!flags +& $SIG_ELEM_IS_CAPTURE {
                $name = '|' ~ $name;
            } elsif $!flags +& $SIG_ELEM_IS_RAW {
                $name = '\\' ~ $name without '@$%&'.index(substr($name,0,1));
            }
        } else {
            if $!flags +& $SIG_ELEM_IS_CAPTURE {
                $name = '|';
            } elsif $!flags +& $SIG_ELEM_ARRAY_SIGIL {
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
            $name = ($!flags +& $SIG_ELEM_SLURPY_ONEARG ?? '+' !! ($!flags +& $SIG_ELEM_SLURPY_LOL ?? "**" !! "*") ~ $name);
        } elsif self.named {
            my $name1 := substr($name,1);
            if @(self.named_names).first({$_ && $_ eq $name1}) {
                $name = ':' ~ $name;
            }
            for @(self.named_names).grep({$_ && $_ ne $name1}) {
                $name = ':' ~ $_ ~ '(' ~ $name ~ ')';
            }
            $name ~= '!' unless self.optional;
        } elsif self.optional && !$default {
            $name ~= '?';
        }
        if $!flags +& $SIG_ELEM_IS_RW {
            $rest ~= ' is rw';
        } elsif $!flags +& $SIG_ELEM_IS_COPY {
            $rest ~= ' is copy';
        }
        if $!flags +& $SIG_ELEM_IS_RAW {
            # Do not emit cases of anonymous '\' which we cannot reparse
            # This is all due to unspace.
            if     not $.name
               and $name eq '$'
               and not $rest
               and nqp::isnull($!post_constraints)
               and not $default
               and nqp::isnull($!sub_signature) {
                    $name = '\\';
            }
            $rest ~= ' is raw' unless $name.starts-with('\\');
        }
        unless nqp::isnull($!sub_signature) {
            my $sig = $!sub_signature.perl();
            $sig ~~ s/^^ ':'//;
            $rest ~= ' ' ~ $sig;
        }
        unless nqp::isnull($!post_constraints) {
            my $where = &where(self);
            return Nil without $where;
            $rest ~= " $where";
        }
        $rest ~= ' = { ... }' if $default;
        if $name or $rest {
            $perl ~= ($perl ?? ' ' !! '') ~ $name;
        }
        $perl ~ $rest;
    }

    method sub_signature(Parameter:D:) {
        nqp::isnull($!sub_signature) ?? Any !! $!sub_signature
    }

    method set_why($why) {
        $!why := $why;
    }
}

# vim: ft=perl6 expandtab sw=4
