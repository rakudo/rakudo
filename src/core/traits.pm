# for errors
my class X::Inheritance::Unsupported { ... }
my class X::Inheritance::UnknownParent { ... }
my class X::Export::NameClash        { ... }
my class X::Composition::NotComposable { ... }
my class X::Import::MissingSymbols   { ... }
my class X::Redeclaration { ... }
my class X::Inheritance::SelfInherit { ... }
my class X::Comp::Trait::Unknown { ... }
my class X::Experimental { ... }
my class Pod::Block::Declarator { ... }

proto sub trait_mod:<is>(|) { * }
multi sub trait_mod:<is>(Mu:U $child, Mu:U $parent) {
    if $parent.HOW.archetypes.inheritable() {
        $child.^add_parent($parent);
    }
    elsif $parent.HOW.archetypes.inheritalizable() {
        $child.^add_parent($parent.^inheritalize)
    }
    else {
        X::Inheritance::Unsupported.new(
            :child-typename($child.^name),
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
    $type.^set_rw;
}
multi sub trait_mod:<is>(Mu:U $type, :$nativesize!) {
    $type.^set_nativesize($nativesize);
}
multi sub trait_mod:<is>(Mu:U $type, :$ctype!) {
    $type.^set_ctype($ctype);
}
multi sub trait_mod:<is>(Mu:U $type, :$unsigned!) {
    $type.^set_unsigned($unsigned);
}
multi sub trait_mod:<is>(Mu:U $type, :$hidden!) {
    $type.^set_hidden;
}
multi sub trait_mod:<is>(Mu:U $type, Mu :$array_type!) {
    $type.^set_array_type($array_type);
}
multi sub trait_mod:<is>(Mu:U $type, *%fail) {
    if %fail.keys[0] !eq $type.^name {
        X::Inheritance::UnknownParent.new(
            :child($type.^name),
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
multi sub trait_mod:<is>(Attribute $attr, :$required!) {
    die "'is required' must be Cool" unless nqp::istype($required,Cool);
    $attr.set_required(
      nqp::istype($required,Bool) ?? +$required !! $required
    );
}
multi sub trait_mod:<is>(Attribute $attr, :$default!) {
    $attr.container_descriptor.set_default(nqp::decont($default));
}
multi sub trait_mod:<is>(Attribute:D $attr, :$box_target!) {
    $attr.set_box_target();
}
multi sub trait_mod:<is>(Attribute:D $attr, :$DEPRECATED!) {
# need to add a COMPOSE phaser to the class, that will add an ENTER phaser
# to the (possibly auto-generated) accessor method.
}
multi sub trait_mod:<is>(Attribute:D $attr, :$leading_docs!) {
    Rakudo::Internals.SET_LEADING_DOCS($attr, $leading_docs);
}

multi sub trait_mod:<is>(Attribute:D $attr, :$trailing_docs!) {
    Rakudo::Internals.SET_TRAILING_DOCS($attr, $trailing_docs);
}

multi sub trait_mod:<is>(Routine:D $r, |c ) {
    my $subtype = c.hash.keys[0];
    $subtype eq 'cached'
      ?? X::Experimental.new(
        feature => "the 'is cached' trait",
        use     => "cached",
        ).throw
      !! X::Comp::Trait::Unknown.new(
        file       => $?FILE,
        line       => $?LINE,
        type       => 'is',
        subtype    => $subtype,
        declaring  => ' ' ~ lc( $r.^name ),
        highexpect => ('rw raw hidden-from-backtrace hidden-from-USAGE',
                       'pure default DEPRECATED inlinable nodal',
                       'prec equiv tighter looser assoc leading_docs trailing_docs' ),
        ).throw;
}
multi sub trait_mod:<is>(Routine:D $r, :$rw!) {
    $r.set_rw();
}
multi sub trait_mod:<is>(Routine:D $r, :$raw!) {
    $r.set_rw(); # for now, until we have real raw handling
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
        has %!prec;
        proto method prec(|) { * }
        multi method prec() is raw { %!prec }
        multi method prec(Str:D $key) {
            nqp::ifnull(
              nqp::atkey(nqp::getattr(%!prec,Map,'$!storage'),$key),
              ''
            )
        }
    }
    if nqp::istype($r, Precedence) {
        for %spec {
            $r.prec.{.key} := .value;
        }
    }
    else {
        $r.^mixin(Precedence);
        nqp::bindattr(nqp::decont($r), $r.WHAT, '%!prec', %spec);
    }
    0;
}
# three other trait_mod sub for equiv/tighter/looser in operators.pm
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
    $r.prec<assoc>:delete;
}
multi sub trait_mod:<is>(Routine $r, :&looser!) {
    die "Routine given to looser does not appear to be an operator"
        unless nqp::can(&looser, 'prec');
    if !nqp::can($r, 'prec') || ($r.prec<prec> // "") !~~ /<[@:]>/ {
        trait_mod:<is>($r, :prec(&looser.prec))
    }
    $r.prec<prec> := $r.prec<prec>.subst(/\=/, ':=');
    $r.prec<assoc>:delete;
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
      highexpect => <rw readonly copy required raw leading_docs trailing_docs>,
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
multi sub trait_mod:<is>(Parameter:D $param, :$raw!) {
    $param.set_raw();
}
multi sub trait_mod:<is>(Parameter:D $param, :$onearg!) {
    $param.set_onearg();
}
multi sub trait_mod:<is>(Parameter:D $param, :$leading_docs!) {
    Rakudo::Internals.SET_LEADING_DOCS($param, $leading_docs);
}
multi sub trait_mod:<is>(Parameter:D $param, :$trailing_docs!) {
    Rakudo::Internals.SET_TRAILING_DOCS($param, $trailing_docs);
}

# Declare these, as setting mainline doesn't get them automatically (as the
# Mu/Any/Scalar are not loaded).
my $!;
my $/;
my $_;

multi sub trait_mod:<is>(Routine:D \r, :$export!) {
    my $to_export := r.multi ?? r.dispatcher !! r;
    my $exp_name  := '&' ~ r.name;
    my @tags = flat 'ALL', (nqp::istype($export,Pair) ?? $export.key() !!
                            nqp::istype($export,Positional) ?? @($export)>>.key !!
                            'DEFAULT');
    Rakudo::Internals.EXPORT_SYMBOL($exp_name, @tags, $to_export);
}
multi sub trait_mod:<is>(Mu:U \type, :$export!) {
    my $exp_name := type.^shortname;
    my @tags = flat 'ALL', (nqp::istype($export,Pair) ?? $export.key !!
                            nqp::istype($export,Positional) ?? @($export)>>.key !!
                            'DEFAULT');
    Rakudo::Internals.EXPORT_SYMBOL($exp_name, @tags, type);
    if nqp::istype(type.HOW, Metamodel::EnumHOW) {
        type.^set_export_callback( {
            for type.^enum_values.keys -> $value_name {
                Rakudo::Internals.EXPORT_SYMBOL(
                  $value_name, @tags, type.WHO{$value_name});
            }
        });
    }
}
# for constants
multi sub trait_mod:<is>(Mu \sym, :$export!, :$SYMBOL!) {
    my @tags = flat 'ALL', (nqp::istype($export,Pair) ?? $export.key !!
                            nqp::istype($export,Positional) ?? @($export)>>.key !!
                            'DEFAULT');
    Rakudo::Internals.EXPORT_SYMBOL($SYMBOL, @tags, sym);
}

multi sub trait_mod:<is>(Block:D $r, :$leading_docs!) {
    Rakudo::Internals.SET_LEADING_DOCS($r, $leading_docs);
}
multi sub trait_mod:<is>(Block:D $r, :$trailing_docs!) {
    Rakudo::Internals.SET_TRAILING_DOCS($r, $trailing_docs);
}

# this should be identical to Mu:D, :leading_docs, otherwise the fallback Block:D, |c
# will catch it and declare "leading_docs" to be an unknown trait.  This is why
# we need this redundant form in spite of having a Block:D candidate above
multi sub trait_mod:<is>(Routine:D $r, :$leading_docs!) {
    Rakudo::Internals.SET_LEADING_DOCS($r, $leading_docs);
}
multi sub trait_mod:<is>(Routine:D $r, :$trailing_docs!) {
    Rakudo::Internals.SET_TRAILING_DOCS($r, $trailing_docs);
}

multi sub trait_mod:<is>(Mu:U $docee, :$leading_docs!) {
    Rakudo::Internals.SET_LEADING_DOCS($docee, $leading_docs);
}
multi sub trait_mod:<is>(Mu:U $docee, :$trailing_docs!) {
    Rakudo::Internals.SET_TRAILING_DOCS($docee.HOW, $trailing_docs);
}

proto sub trait_mod:<does>(|) { * }
multi sub trait_mod:<does>(Mu:U $doee, Mu:U $role) {
    if $role.HOW.archetypes.composable() {
        $doee.^add_role($role)
    }
    elsif $role.HOW.archetypes.composalizable() {
        $doee.^add_role($role.HOW.composalize($role))
    }
    else {
        X::Composition::NotComposable.new(
            target-name => $doee.^name,
            composer    => $role,
        ).throw;
    }
}

proto sub trait_mod:<of>(|) { * }
multi sub trait_mod:<of>(Mu:U $target, Mu:U $type) {
    # XXX Ensure we can do this, die if not.
    $target.^set_of($type);
}
multi sub trait_mod:<of>(Routine:D $target, Mu:U $type) {
    my $sig := $target.signature;
    X::Redeclaration.new(what => 'return type for', symbol => $target,
        postfix => " (previous return type was {$sig.returns.^name})").throw
        if $sig.has_returns;
    $sig.set_returns($type);
    $target.^mixin(Callable.^parameterize($type))
}

multi sub trait_mod:<is>(Routine:D $r, :$hidden-from-backtrace!) {
    $r.^mixin( role { method is-hidden-from-backtrace { True } } );
}

multi sub trait_mod:<is>(Routine:D $r, :$hidden-from-USAGE!) {
    $r.^mixin( role {
        method is-hidden-from-USAGE { True }
    });
}

multi sub trait_mod:<is>(Routine:D $r, :$pure!) {
    $r.^mixin( role {
        method IS_PURE { True }
    });
}

multi sub trait_mod:<is>(Routine:D $r, :$nodal!) {
    $r.^mixin( role {
        method nodal { True }
    });
}

proto sub trait_mod:<returns>(|) { * }
multi sub trait_mod:<returns>(Routine:D $target, Mu:U $type) {
    my $sig := $target.signature;
    X::Redeclaration.new(what => 'return type for', symbol => $target,
        postfix => " (previous return type was {$sig.returns.^name})").throw
        if $sig.has_returns;
    $sig.set_returns($type);
    $target.^mixin(Callable.^parameterize($type))
}

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
            $pkg.^add_method($meth_name, $meth);
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
                        $pkg.^add_fallback(
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
                        $pkg.^add_fallback(
                            -> $obj, $name { True },
                            -> $obj, $name {
                                -> $self, |c {
                                    $attr.get_value($self)."$name"(|c)
                                }
                            });
                    }
                    else {
                        $pkg.^add_fallback(
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
                    $pkg.^add_fallback(
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
    for flat thunk() -> $meth_name {
        my $meth := method (|c) is rw {
            self."$call_name"()."$meth_name"(|c);
        }
        $meth.set_name($meth_name);
        $pkg.^add_method($meth_name, $meth);
    }
    0;
}

proto sub trait_mod:<will>(|) { * }
multi sub trait_mod:<will>(Attribute:D $attr, |c ) {
    X::Comp::Trait::Unknown.new(
      file       => $?FILE,
      line       => $?LINE,
      type       => 'will',
      subtype    => c.hash.keys[0],
      declaring  => 'n attribute',
      highexpect => <lazy>,
    ).throw;
}
multi sub trait_mod:<will>(Attribute $attr, Block :$build!) {  # internal usage
    $attr.set_build($build)
}

proto sub trait_mod:<trusts>(|) { * }
multi sub trait_mod:<trusts>(Mu:U $truster, Mu:U $trustee) {
    $truster.^add_trustee($trustee);
}

proto sub trait_mod:<hides>(|) { * }
multi sub trait_mod:<hides>(Mu:U $child, Mu:U $parent) {
    if $parent.HOW.archetypes.inheritable() {
        $child.^add_parent($parent, :hides);
    }
    elsif $parent.HOW.archetypes.inheritalizable() {
        $child.^add_parent($parent.^inheritalize, :hides)
    }
    else {
        X::Inheritance::Unsupported.new(
            :child-typename($child.^name),
            :$parent,
        ).throw;
    }
}

# vim: ft=perl6 expandtab sw=4
