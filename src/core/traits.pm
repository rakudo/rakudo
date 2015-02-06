# for errors
my class X::Inheritance::Unsupported { ... }
my class X::Inheritance::UnknownParent { ... }
my class X::Export::NameClash        { ... }
my class X::Composition::NotComposable { ... }
my class X::Import::MissingSymbols   { ... }
my class X::Redeclaration { ... }
my class X::Inheritance::SelfInherit { ... }
my class X::Comp::Trait::Unknown { ... }
my class Pod::Block::Declarator { ... }

sub SET_LEADING_DOCS($obj, $docs) {
    my $current_why := $obj.WHY;

    if $current_why {
        my $end := nqp::elems($*POD_BLOCKS) - 1;
        my $i   := $end;

        while $i >= 0 {
            if $docs === nqp::atpos($*POD_BLOCKS, $i) {
                nqp::splice($*POD_BLOCKS, nqp::list(), $i, 1);
                last;
            }
            $i := $i - 1;
        }

        $current_why._add_leading(~$docs);
    } else {
        $obj.set_why($docs);
    }
}

sub SET_TRAILING_DOCS($obj, $docs) {
    my $current_why := $obj.WHY;

    if $current_why {
        $current_why._add_trailing(~$docs);
    } else {
        $obj.set_why($docs);
        $*POD_BLOCKS.push($docs);
    }
}

proto sub trait_mod:<is>(|) { * }
multi sub trait_mod:<is>(Mu:U $child, Mu:U $parent) {
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
multi sub trait_mod:<is>(Mu:U $child, :$DEPRECATED!) {
# add COMPOSE phaser for this child, which will add an ENTER phaser to an
# existing "new" method, or create a "new" method with a call to DEPRECATED
# and a nextsame.
}
multi sub trait_mod:<is>(Mu:U $type, :$rw!) {
    $type.HOW.set_rw($type);
}
multi sub trait_mod:<is>(Mu:U $type, :$nativesize!) {
    $type.HOW.set_nativesize($type, $nativesize);
}
multi sub trait_mod:<is>(Mu:U $type, :$unsigned!) {
    $type.HOW.set_unsigned($type, $unsigned);
}
multi sub trait_mod:<is>(Mu:U $type, :$hidden!) {
    $type.HOW.set_hidden($type);
}
multi sub trait_mod:<is>(Mu:U $type, Mu :$array_type!) {
    $type.HOW.set_array_type($type, $array_type);
}
multi sub trait_mod:<is>(Mu:U $type, *%fail) {
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

multi sub trait_mod:<is>(Attribute:D $attr, |c ) {
    X::Comp::Trait::Unknown.new(
      file       => $?FILE,
      line       => $?LINE,
      type       => 'is',
      subtype    => c.hash.keys[0],
      declaring  => 'n attribute',
      highexpect => <rw readonly box_target leading_docs trailing_docs>,
    ).throw;
}
multi sub trait_mod:<is>(Attribute:D $attr, :$rw!) {
    $attr.set_rw();
    warn "useless use of 'is rw' on $attr.name()" unless $attr.has_accessor;
}
multi sub trait_mod:<is>(Attribute:D $attr, :$readonly!) {
    $attr.set_readonly();
    warn "useless use of 'is readonly' on $attr.name()" unless $attr.has_accessor;
}
multi sub trait_mod:<is>(Attribute:D $attr, :$box_target!) {
    $attr.set_box_target();
}
multi sub trait_mod:<is>(Attribute:D $attr, :$DEPRECATED!) {
# need to add a COMPOSE phaser to the class, that will add an ENTER phaser
# to the (possibly auto-generated) accessor method.
}
multi sub trait_mod:<is>(Attribute:D $attr, :$leading_docs!) {
    SET_LEADING_DOCS($attr, $leading_docs);
}

multi sub trait_mod:<is>(Attribute:D $attr, :$trailing_docs!) {
    SET_TRAILING_DOCS($attr, $trailing_docs);
}

multi sub trait_mod:<is>(Routine:D $r, |c ) {
    X::Comp::Trait::Unknown.new(
      file       => $?FILE,
      line       => $?LINE,
      type       => 'is',
      subtype    => c.hash.keys[0],
      declaring  => ' ' ~ lc( $r.^name ),
      highexpect => ('rw parcel hidden_from_backtrace hidden_from_USAGE',
                     'pure default DEPRECATED inlinable',
                     'prec equiv tighter looser assoc leading_docs trailing_docs' ),
    ).throw;
}
multi sub trait_mod:<is>(Routine:D $r, :$rw!) {
    $r.set_rw();
}
multi sub trait_mod:<is>(Routine:D $r, :$parcel!) {
    $r.set_rw(); # for now, until we have real parcel handling
}
multi sub trait_mod:<is>(Routine:D $r, :$default!) {
    $r does role { method default() { True } }
}
multi sub trait_mod:<is>(Routine:D $r, :$DEPRECATED!) {
    my $new := nqp::istype($DEPRECATED,Bool)
      ?? "something else"
      !! $DEPRECATED;
    $r.add_phaser( 'ENTER', -> { DEPRECATED($new) } );
}
multi sub trait_mod:<is>(Routine:D $r, Mu :$inlinable!) {
    $r.set_inline_info(nqp::decont($inlinable));
}
multi sub trait_mod:<is>(Routine:D $r, :$onlystar!) {
    $r.set_onlystar();
}
multi sub trait_mod:<is>(Routine:D $r, :prec(%spec)!) {
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
multi sub trait_mod:<is>(Routine $r, :&equiv!) {
    nqp::can(&equiv, 'prec')
        ?? trait_mod:<is>($r, :prec(&equiv.prec))
        !! die "Routine given to equiv does not appear to be an operator";
}
multi sub trait_mod:<is>(Routine $r, :&tighter!) {
    die "Routine given to tighter does not appear to be an operator"
        unless nqp::can(&tighter, 'prec');
    if !nqp::can($r, 'prec') || ($r.prec<prec> // "") !~~ /<[@:]>/ {
        trait_mod:<is>($r, :prec(&tighter.prec))
    }
    $r.prec<prec> := $r.prec<prec>.subst(/\=/, '@=');
}
multi sub trait_mod:<is>(Routine $r, :&looser!) {
    die "Routine given to looser does not appear to be an operator"
        unless nqp::can(&looser, 'prec');
    if !nqp::can($r, 'prec') || ($r.prec<prec> // "") !~~ /<[@:]>/ {
        trait_mod:<is>($r, :prec(&looser.prec))
    }
    $r.prec<prec> := $r.prec<prec>.subst(/\=/, ':=');
}
multi sub trait_mod:<is>(Routine $r, :$assoc!) {
    trait_mod:<is>($r, :prec({ :$assoc }))
}

# Since trait_mod:<is> to set onlystar isn't there at the
# point we wrote its proto, we do it manually here.
BEGIN &trait_mod:<is>.set_onlystar();

multi sub trait_mod:<is>(Parameter:D $param, |c ) {
    X::Comp::Trait::Unknown.new(
      file       => $?FILE,
      line       => $?LINE,
      type       => 'is',
      subtype    => c.hash.keys[0],
      declaring  => ' parameter',
      highexpect => <rw readonly copy required parcel leading_docs trailing_docs>,
    ).throw;
}
multi sub trait_mod:<is>(Parameter:D $param, :$readonly!) {
    # This is the default.
}
multi sub trait_mod:<is>(Parameter:D $param, :$rw!) {
    $param.set_rw();
}
multi sub trait_mod:<is>(Parameter:D $param, :$copy!) {
    $param.set_copy();
}
multi sub trait_mod:<is>(Parameter:D $param, :$required!) {
    $param.set_required();
}
multi sub trait_mod:<is>(Parameter:D $param, :$parcel!) {
    $param.set_parcel();
}
multi sub trait_mod:<is>(Parameter:D $param, :$leading_docs!) {
    SET_LEADING_DOCS($param, $leading_docs);
}
multi sub trait_mod:<is>(Parameter:D $param, :$trailing_docs!) {
    SET_TRAILING_DOCS($param, $trailing_docs);
}

# Declare these, as setting mainline doesn't get them automatically (as the
# Mu/Any/Scalar are not loaded).
my $!;
my $/;
my $_;

sub EXPORT_SYMBOL(\exp_name, @tags, Mu \sym) {
    my @export_packages = $*EXPORT;
    for nqp::hllize(@*PACKAGES) {
        unless .WHO.exists_key('EXPORT') {
            .WHO<EXPORT> := Metamodel::PackageHOW.new_type(:name('EXPORT'));
            .WHO<EXPORT>.^compose;
        }
        @export_packages.push: .WHO<EXPORT>;
    }
    for @export_packages -> $p {
        for @tags -> $tag {
            my $install_in;
            if $p.WHO.exists_key($tag) {
                $install_in := $p.WHO.{$tag};
            }
            else {
                $install_in := Metamodel::PackageHOW.new_type(:name($tag));
                $install_in.HOW.compose($install_in);
                $p.WHO{$tag} := $install_in;
            }
            if $install_in.WHO.exists_key(exp_name) {
                unless ($install_in.WHO){exp_name} =:= sym {
                    X::Export::NameClash.new(symbol => exp_name).throw;
                }
            }
            $install_in.WHO{exp_name} := sym;
        }
    }
    0;
}
multi sub trait_mod:<is>(Routine:D \r, :$export!) {
    my $to_export := r.multi ?? r.dispatcher !! r;
    my $exp_name  := '&' ~ r.name;
    my @tags = 'ALL', (nqp::istype($export,Pair) ?? $export.key() !!
                       nqp::istype($export,Positional) ?? @($export)>>.key !!
                       'DEFAULT');
    EXPORT_SYMBOL($exp_name, @tags, $to_export);
}
multi sub trait_mod:<is>(Mu:U \type, :$export!) {
    my $exp_name := type.HOW.name(type);
    my @tags = 'ALL', (nqp::istype($export,Pair) ?? $export.key !!
                       nqp::istype($export,Positional) ?? @($export)>>.key !!
                       'DEFAULT');
    EXPORT_SYMBOL($exp_name, @tags, type);
    if nqp::istype(type.HOW, Metamodel::EnumHOW) {
        type.HOW.set_export_callback(type, {
            for type.^enum_values.keys -> $value_name {
                EXPORT_SYMBOL($value_name, @tags, type.WHO{$value_name});
            }
        });
    }
}
# for constants
multi sub trait_mod:<is>(Mu \sym, :$export!, :$SYMBOL!) {
    my @tags = 'ALL', (nqp::istype($export,Pair) ?? $export.key !!
                    nqp::istype($export,Positional) ?? @($export)>>.key !!
                    'DEFAULT');
    EXPORT_SYMBOL($SYMBOL, @tags, sym);
}


# this should be identical Mu:D, :docs, otherwise the fallback Routine:D, |c
# will catch it and declare "docs" to be an unknown trait
multi sub trait_mod:<is>(Routine:D $r, :$leading_docs!) {
    SET_LEADING_DOCS($r, $leading_docs);
}
multi sub trait_mod:<is>(Routine:D $r, :$trailing_docs!) {
    SET_TRAILING_DOCS($r, $trailing_docs);
}

multi sub trait_mod:<is>(Mu:U $docee, :$leading_docs!) {
    SET_LEADING_DOCS($docee, $leading_docs);
}
multi sub trait_mod:<is>(Mu:U $docee, :$trailing_docs!) {
    SET_TRAILING_DOCS($docee.HOW, $trailing_docs);
}

proto sub trait_mod:<does>(|) { * }
multi sub trait_mod:<does>(Mu:U $doee, Mu:U $role) {
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

proto sub trait_mod:<of>(|) { * }
multi sub trait_mod:<of>(Mu:U $target, Mu:U $type) {
    # XXX Ensure we can do this, die if not.
    $target.HOW.set_of($target, $type);
}
multi sub trait_mod:<of>(Routine:D $target, Mu:U $type) {
    my $sig := $target.signature;
    X::Redeclaration.new(what => 'return type for', symbol => $target,
        postfix => " (previous return type was {$sig.returns.^name})").throw
        if $sig.has_returns;
    $sig.set_returns($type)
}

multi sub trait_mod:<is>(Routine:D $r, :$hidden_from_backtrace!) {
    $r.HOW.mixin($r, role {
        method is_hidden_from_backtrace { True }
    });
}

multi sub trait_mod:<is>(Routine:D $r, :$hidden_from_USAGE!) {
    $r.HOW.mixin($r, role {
        method is_hidden_from_USAGE { True }
    });
}

multi sub trait_mod:<is>(Routine:D $r, :$pure!) {
    $r.HOW.mixin($r, role {
        method IS_PURE { True }
    });
}

proto sub trait_mod:<returns>(|) { * }
multi sub trait_mod:<returns>(Routine:D $target, Mu:U $type) {
    my $sig := $target.signature;
    X::Redeclaration.new(what => 'return type for', symbol => $target,
        postfix => " (previous return type was {$sig.returns.^name})").throw
        if $sig.has_returns;
    $sig.set_returns($type)
}

proto sub trait_mod:<as>(|) { * }
multi sub trait_mod:<as>(Parameter:D $param, $type) {
    $param.set_coercion($type);
}

my class Pair { ... }
proto sub trait_mod:<handles>(|) { * }
multi sub trait_mod:<handles>(Attribute:D $target, $thunk) {
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
                    if nqp::istype($expr,Str) {
                        self.add_delegator_method($pkg, $expr, $expr);
                    }
                    elsif nqp::istype($expr,Pair) {
                        self.add_delegator_method($pkg, $expr.key, $expr.value);
                    }
                    elsif nqp::istype($expr,Positional) {
                        for $expr.list {
                            applier($_);
                        }
                        0;
                    }
                    elsif $expr.isa(Whatever) {
                        $pkg.HOW.add_fallback($pkg,
                            -> $obj, $name {
                                so $attr.get_value($obj).can($name);
                            },
                            -> $obj, $name {
                                -> $self, |c {
                                    $attr.get_value($self)."$name"(|c)
                                }
                            });
                    }
                    elsif $expr.isa(HyperWhatever) {
                        $pkg.HOW.add_fallback($pkg,
                            -> $obj, $name { True },
                            -> $obj, $name {
                                -> $self, |c {
                                    $attr.get_value($self)."$name"(|c)
                                }
                            });
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

proto sub trait_mod:<will>(|) { * }
multi sub trait_mod:<will>(Attribute $attr, Block :$build!) {
    $attr.set_build($build)
}

proto sub trait_mod:<trusts>(|) { * }
multi sub trait_mod:<trusts>(Mu:U $truster, Mu:U $trustee) {
    $truster.HOW.add_trustee($truster, $trustee);
}

proto sub trait_mod:<hides>(|) { * }
multi sub trait_mod:<hides>(Mu:U $child, Mu:U $parent) {
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

# vim: ft=perl6 expandtab sw=4
