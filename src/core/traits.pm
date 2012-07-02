use Perl6::BOOTSTRAP;

# Stub these or we can't use any sigil other than $.
my role Positional { ... }
my role Associative { ... }
my role Callable { ... }

# This is needed for the export trait.
my class Pair { ... }

# for errors
my class X::Inheritance::Unsupported { ... }
my class X::Export::NameClash        { ... }
my class X::Composition::NotComposable { ... }
my class X::Import::MissingSymbols   { ... }

proto trait_mod:<is>(|$) { * }
multi trait_mod:<is>(Mu:U $child, Mu:U $parent) {
    if $parent.HOW.archetypes.inheritable() {
        $child.HOW.add_parent($child, $parent);
    }
    elsif $parent.HOW.archetypes.inheritalizable() {
        $child.HOW.add_parent($child, $parent.HOW.inheritalize($parent))
    }
    else {
        X::Inheritance::Unsupported.new(
            :child-typename($child.HOW.name($child)),
            :$parent,
        ).throw;
    }
}
multi trait_mod:<is>(Mu:U $type, :$rw!) {
    $type.HOW.set_rw($type);
}
multi trait_mod:<is>(Mu:U $type, :$nativesize!) {
    $type.HOW.set_nativesize($type, $nativesize);
}

multi trait_mod:<is>(Attribute:D $attr, :$rw!) {
    $attr.set_rw();
}
multi trait_mod:<is>(Attribute:D $attr, :$readonly!) {
    $attr.set_readonly();
}
multi trait_mod:<is>(Attribute:D $attr, :$box_target!) {
    $attr.set_box_target();
}

multi trait_mod:<is>(Routine:D $r, :$rw!) {
    $r.set_rw();
}
multi trait_mod:<is>(Routine:D $r, :$default!) {
    $r does role { method default() { True } }
}
multi trait_mod:<is>(Routine:D $r, :$DEPRECATED!) {
    # we'll add logic here later
}
multi trait_mod:<is>(Routine:D $r, :$inlinable!) {
    $r.set_inline_info($inlinable);
}

multi trait_mod:<is>(Parameter:D $param, :$readonly!) {
    # This is the default.
}
multi trait_mod:<is>(Parameter:D $param, :$rw!) {
    $param.set_rw();
}
multi trait_mod:<is>(Parameter:D $param, :$copy!) {
    $param.set_copy();
}
multi trait_mod:<is>(Parameter:D $param, :$required!) {
    $param.set_required();
}
multi trait_mod:<is>(Parameter:D $param, :$parcel!) {
    $param.set_parcel();
}

# TODO: Make this much less cheaty. That'll probably need the
# full-blown serialization, though.
sub EXPORT_SYMBOL(\$exp_name, @tags, Mu \$sym) {
    my @export_packages = $*EXPORT;
    for pir::perl6ize_type__PP(@*PACKAGES) {
        unless .WHO.exists('EXPORT') {
            .WHO<EXPORT> := Metamodel::PackageHOW.new_type(:name('EXPORT'));
            .WHO<EXPORT>.^compose;
        }
        @export_packages.push: .WHO<EXPORT>;
    }
    for @export_packages -> $p {
        for @tags -> $tag {
            my $install_in;
            if $p.WHO.exists($tag) {
                $install_in := $p.WHO.{$tag};
            }
            else {
                $install_in := $*W.pkg_create_mo($/, (package { }).HOW, :name($tag));
                $*W.pkg_compose($install_in);
                $*W.install_package_symbol($p, $tag, $install_in);
            }
            if $install_in.WHO.exists($exp_name) {
                unless ($install_in.WHO){$exp_name} =:= $sym {
                    X::Export::NameClash.new(symbol => $exp_name).throw;
                }
            }
            $*W.install_package_symbol($install_in, $exp_name, $sym);
        }
    }
}
multi trait_mod:<is>(Routine:D \$r, :$export!) {
    my $to_export := $r.multi ?? $r.dispatcher !! $r;
    my $exp_name  := '&' ~ $r.name;
    my @tags = 'ALL', ($export ~~ Pair ?? $export.key() !!
                       $export ~~ Positional ?? @($export)>>.key !!
                       'DEFAULT');
    EXPORT_SYMBOL($exp_name, @tags, $to_export);
}
multi trait_mod:<is>(Mu:U \$type, :$export!) {
    my $exp_name := $type.HOW.name($type);
    my @tags = 'ALL', ($export ~~ Pair ?? $export.key !!
                       $export ~~ Positional ?? @($export)>>.key !!
                       'DEFAULT');
    EXPORT_SYMBOL($exp_name, @tags, $type);
}

multi trait_mod:<is>(Mu:D $docee, :$docs!) {
    $docee does role {
        has $!WHY;
        method WHY          { $!WHY      }
        method set_docs($d) { $!WHY = $d }
    }
    $docee.set_docs($docs);
    $docs.set_docee($docee);
}

multi trait_mod:<is>(Mu:U $docee, :$docs!) {
    $docee.HOW.set_docs($docs);
    $docs.set_docee($docee);
}


proto trait_mod:<does>(|$) { * }
multi trait_mod:<does>(Mu:U $doee, Mu:U $role) {
    if $role.HOW.archetypes.composable() {
        $doee.HOW.add_role($doee, $role)
    }
    elsif $role.HOW.archetypes.composalizable() {
        $doee.HOW.add_role($doee, $role.HOW.composalize($role))
    }
    else {
        X::Composition::NotComposable.new(
            target-name => $doee.HOW.name($doee),
            composer    => $role,
        ).throw;
    }
}

proto trait_mod:<of>(|$) { * }
multi trait_mod:<of>(Mu:U $target, Mu:U $type) {
    # XXX Ensure we can do this, die if not.
    $target.HOW.set_of($target, $type);
}
multi trait_mod:<of>(Routine:D $target, Mu:U $type) {
    $target.signature.set_returns($type)
}

multi trait_mod:<is>(Routine:D $r, :$hidden_from_backtrace!) {
    $r.HOW.mixin($r, role {
        method is_hidden_from_backtrace { True }
    });
}


proto trait_mod:<returns>(|$) { * }
multi trait_mod:<returns>(Routine:D $target, Mu:U $type) {
    $target.signature.set_returns($type)
}

proto trait_mod:<as>(|$) { * }
multi trait_mod:<as>(Parameter:D $param, $type) {
    $param.set_coercion($type);
}

my class Pair { ... }
proto trait_mod:<handles>(|$) { * }
multi trait_mod:<handles>(Attribute:D $target, $thunk) {
    $target does role {
        has $.handles;
        
        method set_handles($expr) {
            $!handles := $expr;
        }
        
        method add_delegator_method($attr: $pkg, $meth_name, $call_name) {
            my $meth := method (|$c) is rw {
                $attr.get_value(self)."$call_name"(|$c)
            };
            $meth.set_name($meth_name);
            $pkg.HOW.add_method($pkg, $meth_name, $meth);
        }
        
        method apply_handles($attr: Mu $pkg) {
            sub applier($expr) {
                if $expr.defined() {
                    if $expr ~~ Str {
                        self.add_delegator_method($pkg, $expr, $expr);
                    }
                    elsif $expr ~~ Pair {
                        self.add_delegator_method($pkg, $expr.key, $expr.value);
                    }
                    elsif $expr ~~ Positional {
                        for $expr.list {
                            applier($_);
                        }
                    }
                    else {
                        $pkg.HOW.add_fallback($pkg,
                            -> $obj, $name {
                                ?($name ~~ $expr)
                            },
                            -> $obj, $name {
                                -> $self, |$c {
                                    $attr.get_value($self)."$name"(|$c)
                                }
                            });
                    }
                }
                else {
                    $pkg.HOW.add_fallback($pkg,
                        -> $obj, $name {
                            ?$expr.can($name)
                        },
                        -> $obj, $name {
                            -> $self, |$c {
                                $attr.get_value($self)."$name"(|$c)
                            }
                        });
                }
            }
            applier($!handles);
        }
    };
    $target.set_handles($thunk());
}

proto trait_mod:<will>(|$) { * }
multi trait_mod:<will>(Attribute $attr, Block :$build!) {
    $attr.set_build($build)
}

proto trait_mod:<trusts>(|$) { * }
multi trait_mod:<trusts>(Mu:U $truster, Mu:U $trustee) {
    $truster.HOW.add_trustee($truster, $trustee);
}
