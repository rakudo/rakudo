# for errors
my class X::Inheritance::Unsupported { ... }
my class X::Inheritance::UnknownParent { ... }
my class X::Export::NameClash        { ... }
my class X::Composition::NotComposable { ... }
my class X::Import::MissingSymbols   { ... }
my class X::Redeclaration { ... }
my class X::Inheritance::SelfInherit { ... }
my class X::Comp::Trait { ... };

proto trait_mod:<is>(|) { * }
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
multi trait_mod:<is>(Mu:U $type, :$unsigned!) {
    $type.HOW.set_unsigned($type, $unsigned);
}
multi trait_mod:<is>(Mu:U $type, :$hidden!) {
    $type.HOW.set_hidden($type);
}
multi trait_mod:<is>(Mu:U $type, Mu :$array_type!) {
    $type.HOW.set_array_type($type, $array_type);
}
multi trait_mod:<is>(Mu:U $type, *%fail) {
    if %fail.keys[0] !eq $type.HOW.name($type) {
        X::Inheritance::UnknownParent.new(
            :child($type.HOW.name($type)),
            :parent(%fail.keys[0]),
            :suggestions([])
        ).throw;
    } else {
        X::Inheritance::SelfInherit.new(
            :name(%fail.keys[0])
        ).throw;
    }
}

multi trait_mod:<is>(Attribute:D $attr, |c ) {
    X::Comp::Trait.new(
      file       => $?FILE,
      line       => $?LINE,
      type       => 'is',
      subtype    => c.hash.keys[0],
      declaring  => 'n attribute',
      highexpect => <rw readonly box_target>,
    ).throw;
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

multi trait_mod:<is>(Routine:D $r, |c ) {
    X::Comp::Trait.new(
      file       => $?FILE,
      line       => $?LINE,
      type       => 'is',
      subtype    => c.hash.keys[0],
      declaring  => ' ' ~ lc( $r.^name ),
      highexpect => ('rw parcel hidden_from_backtrace',
                     'pure default DEPRECATE inlinable',
                     'prec equiv tighter looser assoc' ),
    ).throw;
}
multi trait_mod:<is>(Routine:D $r, :$rw!) {
    $r.set_rw();
}
multi trait_mod:<is>(Routine:D $r, :$parcel!) {
    $r.set_rw(); # for now, until we have real parcel handling
}
multi trait_mod:<is>(Routine:D $r, :$default!) {
    $r does role { method default() { True } }
}
multi trait_mod:<is>(Routine:D $r, :$DEPRECATED!) {
    # we'll add logic here later
}
multi trait_mod:<is>(Routine:D $r, Mu :$inlinable!) {
    $r.set_inline_info(nqp::decont($inlinable));
}
multi trait_mod:<is>(Routine:D $r, :$onlystar!) {
    $r.set_onlystar();
}
multi trait_mod:<is>(Routine:D $r, :prec(%spec)!) {
    my role Precedence {
        has %.prec;
    }
    if nqp::istype($r, Precedence) {
        for %spec {
            $r.prec.{.key} := .value;
        }
    }
    else {
        $r.HOW.mixin($r, Precedence);
        nqp::bindattr(nqp::decont($r), $r.WHAT, '%!prec', %spec);
    }
    0;
}
multi trait_mod:<is>(Routine $r, :&equiv!) {
    nqp::can(&equiv, 'prec')
        ?? trait_mod:<is>($r, :prec(&equiv.prec))
        !! die "Routine given to equiv does not appear to be an operator";
}
multi trait_mod:<is>(Routine $r, :&tighter!) {
    die "Routine given to tighter does not appear to be an operator"
        unless nqp::can(&tighter, 'prec');
    if !nqp::can($r, 'prec') || ($r.prec<prec> // "") !~~ /<[@:]>/ {
        trait_mod:<is>($r, :prec(&tighter.prec))
    }
    $r.prec<prec> := $r.prec<prec>.subst(/\=/, '@=');
}
multi trait_mod:<is>(Routine $r, :&looser!) {
    die "Routine given to looser does not appear to be an operator"
        unless nqp::can(&looser, 'prec');
    if !nqp::can($r, 'prec') || ($r.prec<prec> // "") !~~ /<[@:]>/ {
        trait_mod:<is>($r, :prec(&looser.prec))
    }
    $r.prec<prec> := $r.prec<prec>.subst(/\=/, ':=');
}
multi trait_mod:<is>(Routine $r, :$assoc!) {
    trait_mod:<is>($r, :prec({ :$assoc }))
}

# Since trait_mod:<is> to set onlystar isn't there at the
# point we wrote its proto, we do it manually here.
BEGIN &trait_mod:<is>.set_onlystar();

multi trait_mod:<is>(Parameter:D $param, |c ) {
    X::Comp::Trait.new(
      file       => $?FILE,
      line       => $?LINE,
      type       => 'is',
      subtype    => c.hash.keys[0],
      declaring  => ' parameter',
      highexpect => <rw readonly copy required parcel>,
    ).throw;
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

# Declare these, as setting mainline doesn't get them automatically (as the
# Mu/Any/Scalar are not loaded).
my $!;
my $/;
my $_;

sub EXPORT_SYMBOL(\exp_name, @tags, Mu \sym) {
    my @export_packages = $*EXPORT;
    for nqp::hllize(@*PACKAGES) {
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
                $install_in := Metamodel::PackageHOW.new_type(:name($tag));
                $install_in.HOW.compose($install_in);
                $p.WHO{$tag} := $install_in;
            }
            if $install_in.WHO.exists(exp_name) {
                unless ($install_in.WHO){exp_name} =:= sym {
                    X::Export::NameClash.new(symbol => exp_name).throw;
                }
            }
            $install_in.WHO{exp_name} := sym;
        }
    }
    0;
}
multi trait_mod:<is>(Routine:D \r, :$export!) {
    my $to_export := r.multi ?? r.dispatcher !! r;
    my $exp_name  := '&' ~ r.name;
    my @tags = 'ALL', ($export ~~ Pair ?? $export.key() !!
                       $export ~~ Positional ?? @($export)>>.key !!
                       'DEFAULT');
    EXPORT_SYMBOL($exp_name, @tags, $to_export);
}
multi trait_mod:<is>(Mu:U \type, :$export!) {
    my $exp_name := type.HOW.name(type);
    my @tags = 'ALL', ($export ~~ Pair ?? $export.key !!
                       $export ~~ Positional ?? @($export)>>.key !!
                       'DEFAULT');
    EXPORT_SYMBOL($exp_name, @tags, type);
}
# for constants
multi trait_mod:<is>(Mu \sym, :$export!, :$SYMBOL!) {
    my @tags = 'ALL', ($export ~~ Pair ?? $export.key !!
                    $export ~~ Positional ?? @($export)>>.key !!
                    'DEFAULT');
    EXPORT_SYMBOL($SYMBOL, @tags, sym);
}


# this should be identical Mu:D, :docs, otherwise the fallback Routine:D, |c
# will catch it and declare "docs" to be an unknown trait
multi trait_mod:<is>(Routine:D $docee, :$docs!) {
    $docee does role {
        has $!WHY;
        method WHY          { $!WHY      }
        method set_docs($d) { $!WHY = $d }
    }
    $docee.set_docs($docs);
    $docs.set_docee($docee);
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


proto trait_mod:<does>(|) { * }
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

proto trait_mod:<of>(|) { * }
multi trait_mod:<of>(Mu:U $target, Mu:U $type) {
    # XXX Ensure we can do this, die if not.
    $target.HOW.set_of($target, $type);
}
multi trait_mod:<of>(Routine:D $target, Mu:U $type) {
    my $sig := $target.signature;
    X::Redeclaration.new(what => 'return type for', symbol => $target,
        postfix => " (previous return type was {$sig.returns.^name})").throw
        if $sig.has_returns;
    $sig.set_returns($type)
}

multi trait_mod:<is>(Routine:D $r, :$hidden_from_backtrace!) {
    $r.HOW.mixin($r, role {
        method is_hidden_from_backtrace { True }
    });
}

multi trait_mod:<is>(Routine:D $r, :$pure!) {
    $r.HOW.mixin($r, role {
        method IS_PURE { True }
    });
}

proto trait_mod:<returns>(|) { * }
multi trait_mod:<returns>(Routine:D $target, Mu:U $type) {
    my $sig := $target.signature;
    X::Redeclaration.new(what => 'return type for', symbol => $target,
        postfix => " (previous return type was {$sig.returns.^name})").throw
        if $sig.has_returns;
    $sig.set_returns($type)
}

proto trait_mod:<as>(|) { * }
multi trait_mod:<as>(Parameter:D $param, $type) {
    $param.set_coercion($type);
}

my class Pair { ... }
proto trait_mod:<handles>(|) { * }
multi trait_mod:<handles>(Attribute:D $target, $thunk) {
    $target does role {
        has $.handles;
        
        method set_handles($expr) {
            $!handles := $expr;
        }
        
        method add_delegator_method($attr: $pkg, $meth_name, $call_name) {
            my $meth := method (|c) is rw {
                $attr.get_value(self)."$call_name"(|c)
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
                        0;
                    }
                    else {
                        $pkg.HOW.add_fallback($pkg,
                            -> $obj, $name {
                                ?($name ~~ $expr)
                            },
                            -> $obj, $name {
                                -> $self, |c {
                                    $attr.get_value($self)."$name"(|c)
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
                            -> $self, |c {
                                $attr.get_value($self)."$name"(|c)
                            }
                        });
                }
            }
            applier($!handles);
        }
    };
    $target.set_handles($thunk());
}

multi sub trait_mod:<handles>(Method:D $m, &thunk) {
    my $pkg := $m.signature.params[0].type;
    my $call_name := $m.name;
    for thunk() -> $meth_name {
        my $meth := method (|c) is rw {
            self."$call_name"()."$meth_name"(|c);
        }
        $meth.set_name($meth_name);
        $pkg.HOW.add_method($pkg, $meth_name, $meth);
    }
    0;
}

proto trait_mod:<will>(|) { * }
multi trait_mod:<will>(Attribute $attr, Block :$build!) {
    $attr.set_build($build)
}

proto trait_mod:<trusts>(|) { * }
multi trait_mod:<trusts>(Mu:U $truster, Mu:U $trustee) {
    $truster.HOW.add_trustee($truster, $trustee);
}

proto trait_mod:<hides>(|) { * }
multi trait_mod:<hides>(Mu:U $child, Mu:U $parent) {
    if $parent.HOW.archetypes.inheritable() {
        $child.HOW.add_parent($child, $parent, :hides);
    }
    elsif $parent.HOW.archetypes.inheritalizable() {
        $child.HOW.add_parent($child, $parent.HOW.inheritalize($parent), :hides)
    }
    else {
        X::Inheritance::Unsupported.new(
            :child-typename($child.HOW.name($child)),
            :$parent,
        ).throw;
    }
}
