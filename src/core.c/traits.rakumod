# for errors
my class Pod::Block::Declarator { ... }
my class X::Comp::Trait::Invalid { ... }
my class X::Comp::Trait::Unknown { ... }
my class X::Composition::NotComposable { ... }
my class X::Experimental { ... }
my class X::Export::NameClash        { ... }
my class X::Inheritance::SelfInherit { ... }
my class X::Inheritance::UnknownParent { ... }
my class X::Inheritance::Unsupported { ... }
my class X::Redeclaration { ... }
my class X::Syntax::ParentAsHash { ... }
my class X::TypeCheck::Assignment { ... }

proto sub trait_mod:<is>(Mu $, |) {*}
multi sub trait_mod:<is>(Mu:U $child, Mu:U $parent) {
    if $parent.HOW.archetypes.inheritable()
        || ($child.HOW.archetypes.parametric && $parent.^archetypes.generic)
    {
        $child.^add_parent($parent);
    }
    elsif $parent.HOW.archetypes.inheritalizable() {
        if nqp::can($parent.HOW, 'methods')
            && my @required-methods = $parent.^methods.grep({$_.yada})
       {
            my $type = $child.HOW.archetypes.inheritable()
                ?? 'Class '
                !! $child.HOW.archetypes.inheritalizable()
                    ?? 'Role '
                    !! '';
            die $type ~ "{$child.^name} can't pun role {$parent.^name} because it has required methods: "
                ~ @required-methods.map({$_.name}).join(', ') ~ '. Did you mean to use "does" instead?';
        }
        else {
            $child.^add_parent($parent.^inheritalize)
        }
    }
    else {
        X::Inheritance::Unsupported.new(
            :child-typename($child.^name),
            :$parent,
        ).throw;
    }
}
multi sub trait_mod:<is>(Mu:U \child, Mu:U \parent, @subtypes) {
    # re-dispatch properly parameterized R#2611
    trait_mod:<is>(child,parent.^parameterize(|@subtypes))
}
multi sub trait_mod:<is>(Mu:U $child, :DEPRECATED($)!) {
# add COMPOSE phaser for this child, which will add an ENTER phaser to an
# existing "new" method, or create a "new" method with a call to DEPRECATED
# and a nextsame.
}
multi sub trait_mod:<is>(Mu:U $type, :rw($)!) {
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
multi sub trait_mod:<is>(Mu:U $type, :hidden($)!) {
    $type.^set_hidden;
}
multi sub trait_mod:<is>(Mu:U $type, Mu :$array_type!) {
    $type.^set_array_type($array_type);
}
multi sub trait_mod:<is>(Mu:U $type, Mu:U $parent, Block) {
    X::Syntax::ParentAsHash.new(
      :type($type.^name),
      :parent($parent.^name),
      :what<Block>
    ).throw;
}
multi sub trait_mod:<is>(Mu:U $type, Mu:U $parent, Hash) {
    X::Syntax::ParentAsHash.new(
      :type($type.^name),
      :parent($parent.^name),
      :what<Hash>
    ).throw;
}
multi sub trait_mod:<is>(Mu:U $type, :$implementation-detail!) {
    my role is-implementation-detail {
        method is-implementation-detail(Mu --> 1) { }
    }
    $type.HOW.^mixin(is-implementation-detail)
      if $implementation-detail;
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
      declaring  => 'an attribute',
      highexpect => <rw readonly box_target leading_docs trailing_docs>,
    ).throw;
}
multi sub trait_mod:<is>(Attribute:D $attr, :rw($)!) {
    $attr.set_rw();
    warn "useless use of 'is rw' on $attr.name()" unless $attr.has_accessor;
}
multi sub trait_mod:<is>(Attribute:D $attr, :readonly($)!) {
    $attr.set_readonly();
    warn "useless use of 'is readonly' on $attr.name()" unless $attr.has_accessor;
}
multi sub trait_mod:<is>(Attribute:D $attr, :$required!) {
    die "'is required' must be Cool" unless nqp::istype($required,Cool);
    $attr.set_required(
      nqp::istype($required,Bool) ?? +$required !! $required
    );
}
multi sub trait_mod:<is>(Attribute:D $attr, Mu :$default!) {
    my Mu $descriptor := $attr.container_descriptor;
    my Mu $of := $descriptor.of;
    # When either $of or $default are generics we can't actually typecheck the default at compile time. Therefore we'd
    # have to accept it as is for now.
    if $of.^archetypes.generic || nqp::istype($default, $of)
        || nqp::eqaddr($default,Nil) || nqp::eqaddr($of, Mu)
    {
        $descriptor.set_default(nqp::decont($default));
    }
    else {
        X::TypeCheck::Attribute::Default.new(
            :name($attr.name),
            :operation("assign"),
            :expected($of),
            :got(nqp::eqaddr($default,Nil) ?? 'Nil' !! $default)
        ).throw
    }
    $attr.container = nqp::decont($default) if nqp::isrwcont($attr.container);
}
multi sub trait_mod:<is>(Attribute:D $attr, :box_target($)!) {
    $attr.set_box_target();
}
multi sub trait_mod:<is>(Attribute:D $attr, :$DEPRECATED!) {
    my $new := nqp::istype($DEPRECATED,Bool)
      ?? "something else"
      !! $DEPRECATED;
    my role is-DEPRECATED { has $.DEPRECATED }
    $attr does is-DEPRECATED($new);
}
multi sub trait_mod:<is>(Attribute:D $attr, :$leading_docs!) {
    Rakudo::Internals.SET_LEADING_DOCS($attr, $leading_docs);
}

multi sub trait_mod:<is>(Attribute:D $attr, :$trailing_docs!) {
    Rakudo::Internals.SET_TRAILING_DOCS($attr, $trailing_docs);
}


multi sub trait_mod:<is>(Routine:D $r, |c) {
    my $subtype = c.hash.keys[0];

    when $subtype eq 'cached' { # Return early for cached
        die X::Experimental.new: :feature<the 'is cached' trait> :use<cached> }

    my @traits =
      &trait_mod:<is>.candidates.grep({.signature.params.[0].type ~~ Routine});

    sub trait-name(&t) { &t.signature.params[1].named_names[0] }

    my %info = :file($?FILE), :line($?LINE), :type<is>, :$subtype,
               :declaring($r.^name.split('+').head.lc);

    with @traits.first({.&trait-name eq $subtype}) -> &t {
        my $reason = do { try t($r, |c);
                          $!.message }
        die X::Comp::Trait::Invalid.new: |%info, :$reason, :name($r.gist) }
    else {
        my @expected =
          @traits.map(&trait-name).unique.Str.naive-word-wrapper.lines,
          ('',"or did you forget to 'use NativeCall'?" if $subtype eq 'native');

        die X::Comp::Trait::Unknown.new: |%info, :highexpect(@expected) }
}
multi sub trait_mod:<is>(Routine:D $r, :rw($)!) {
    $r.set_rw();
}
multi sub trait_mod:<is>(Routine:D $r, :raw($)!) {
    $r.set_rw(); # for now, until we have real raw handling
}
multi sub trait_mod:<is>(Routine:D $r, :default($)!) {
    $r.^mixin: role { method default(--> True) { } }
}
multi sub trait_mod:<is>(Routine:D $r, :$DEPRECATED!) {
    my $new := nqp::istype($DEPRECATED,Bool)
      ?? "something else"
      !! $DEPRECATED;
    my role is-DEPRECATED { has $.DEPRECATED }
    $r does is-DEPRECATED($new);
    $r.add_phaser( 'ENTER', -> { Rakudo::Deprecations.DEPRECATED($new) } );
}
multi sub trait_mod:<is>(Routine:D $r, Mu :$inlinable!) {
    $r.set_inline_info(nqp::decont($inlinable));
}
multi sub trait_mod:<is>(Routine:D $r, :onlystar($)!) {
    $r.set_onlystar();
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
      declaring  => 'a parameter',
      highexpect => <rw readonly copy required raw leading_docs trailing_docs>,
    ).throw;
}
multi sub trait_mod:<is>(Parameter:D $param, :readonly($)!) {
    # This is the default.
}
multi sub trait_mod:<is>(Parameter:D $param, :rw($)!) {
    $param.set_rw();
}
multi sub trait_mod:<is>(Parameter:D $param, :copy($)!) {
    $param.set_copy();
}
multi sub trait_mod:<is>(Parameter:D $param, :required($)!) {
    $param.set_required();
}
multi sub trait_mod:<is>(Parameter:D $param, :raw($)!) {
    $param.set_raw();
}
multi sub trait_mod:<is>(Parameter:D $param, :onearg($)!) {
    $param.set_onearg();
}
multi sub trait_mod:<is>(Parameter:D $param, :item($)!) {
    $param.set_item();
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

multi sub trait_mod:<is>(Routine:D $r, :$export!, :$SYMBOL = '&' ~ $r.name) {
    my $to_export := $r.multi ?? $r.dispatcher !! $r;
    my @tags = flat 'ALL', (
        nqp::istype($export,Pair)
            ?? $export.key()
            !! nqp::istype($export,Positional)
                ?? @($export)>>.key
                !! nqp::istype($export,Bool) && $export
                    ?? 'DEFAULT'
                    !! die "Invalid value '$export.gist()' of type "
                        ~ "'$export.^name()' in trait 'is export'. Use a Pair "
                        ~ 'or a list of Pairs, with keys as tag names.'
    );
    Rakudo::Internals.EXPORT_SYMBOL($SYMBOL, @tags, $to_export);
}
multi sub trait_mod:<is>(Mu:U \type, :$export!) {
    my $exp_name := type.^shortname;
    my @tags = flat 'ALL', (
        nqp::istype($export,Pair)
            ?? $export.key()
            !! nqp::istype($export,Positional)
                ?? @($export)>>.key
                !! nqp::istype($export,Bool) && $export
                    ?? 'DEFAULT'
                    !! die "Invalid value '$export.gist()' of type "
                        ~ "'$export.^name()' in trait 'is export'. Use a Pair "
                        ~ 'or a list of Pairs, with keys as tag names.'
    );
    # If a role is being exported export its respective group instead.
    my \export_type := nqp::istype(type.HOW, Metamodel::ParametricRoleHOW) ?? type.^group !! type;
    Rakudo::Internals.EXPORT_SYMBOL($exp_name, @tags, export_type);
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
    my @tags = flat 'ALL', (
        nqp::istype($export,Pair)
            ?? $export.key()
            !! nqp::istype($export,Positional)
                ?? @($export)>>.key
                !! nqp::istype($export,Bool) && $export
                    ?? 'DEFAULT'
                    !! die "Invalid value '$export.gist()' of type "
                        ~ "'$export.^name()' in trait 'is export'. Use a Pair "
                        ~ 'or a list of Pairs, with keys as tag names.'
    );
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

proto sub trait_mod:<does>(Mu, Mu, *%) {*}
multi sub trait_mod:<does>(Mu:U $doee, Mu:U $role) {
    my $how := $role.HOW;
    if $how.archetypes.parametric()
        || ($doee.HOW.archetypes.parametric && $how.archetypes.generic)
    {
        $doee.^add_role($role)
    }
    elsif $how.archetypes.composalizable() {
        $doee.^add_role($how.composalize($role))
    }
    else {
        X::Composition::NotComposable.new(
            target-name => $doee.^name,
            composer    => $role,
        ).throw;
    }
}

proto sub trait_mod:<of>(Mu, Mu, *%) {*}
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

multi sub trait_mod:<is>(Routine:D $r, :$implementation-detail!) {
    $r.^mixin( role is-implementation-detail {
        method is-implementation-detail(--> True) { }
    }) if $implementation-detail;
}

multi sub trait_mod:<is>(Routine:D $r, :$hidden-from-backtrace!) {
    $r.^mixin( role is-hidden-from-backtrace {
        method is-hidden-from-backtrace(--> True) { }
    }) if $hidden-from-backtrace;
}

multi sub trait_mod:<is>(Routine:D $r, :$hidden-from-USAGE!) {
    $r.^mixin( role is-hidden-from-USAGE {
        method is-hidden-from-USAGE(--> True) { }
    }) if $hidden-from-USAGE;
}

multi sub trait_mod:<is>(Routine:D $r, :$pure!) {
    $r.^mixin( role is-pure {
        method is-pure (--> True) { }
    }) if $pure;
}

multi sub trait_mod:<is>(Routine:D $r, :$nodal!) {
    $r.^mixin( role is-nodal {
        method nodal(--> True) { }
    }) if $nodal;
}

proto sub trait_mod:<returns>($, Mu, *%) {*}
multi sub trait_mod:<returns>(Routine:D $target, Mu:U $type) {
    my $sig := $target.signature;
    X::Redeclaration.new(what => 'return type for', symbol => $target,
        postfix => " (previous return type was {$sig.returns.^name})").throw
        if $sig.has_returns;
    $sig.set_returns($type);
    $target.^mixin(Callable.^parameterize($type))
}

proto sub trait_mod:<handles>($, $, *%) {*}
multi sub trait_mod:<handles>(Attribute:D $target, $thunk) {
    $target does role {
        has $.handles;

        method set_handles($expr) {
            $!handles := $expr;
        }

        method add_delegator_method($attr: Mu $pkg, $meth_name, $call_name) {
            my $meth := anon method (|c) is rw {
                (nqp::isconcrete(self)
                  ?? $attr.get_value(self)
                  !! nqp::decont(nqp::getattr(
                         nqp::decont($attr),Attribute,'$!auto_viv_container'
                     ))
                )."$call_name"(|c)
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
                    elsif nqp::istype($expr, Whatever) {
                        $pkg.^add_fallback(
                            -> $obj, $name {
                                nqp::can(nqp::decont($attr.get_value: $obj), nqp::decont($name))
                            },
                            -> $obj, $name {
                                -> $self, |c {
                                    $attr.get_value($self)."$name"(|c)
                                }
                            });
                    }
                    elsif nqp::istype($expr, HyperWhatever) {
                        $pkg.^add_fallback(
                            -> $, $ --> True { },
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
                            nqp::can(nqp::decont($expr), nqp::decont($name))
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
    $m does role {
        has $.handles;
        has $!delegator_name;

        method set_handles($expr) {
            $!handles := $expr;
        }

        method add_delegator_method(&code_obj: Mu $pkg, $meth_name, $call_name) {
            my $meth := nqp::defined(my $delegator_name = $!delegator_name)
                ?? anon method (|c) is raw { self."$delegator_name"()."$call_name"(|c) }
                !! anon method (|c) is raw { &code_obj(self)."$call_name"(|c) };
            $meth.set_name($meth_name);
            $pkg.^add_method($meth_name, $meth);
        }

        method !fallback-code(&code_obj: $name) {
            nqp::defined(my $delegator_name = $!delegator_name)
                ?? -> \SELF, |c is raw { SELF."$delegator_name"()."$name"(|c) }
                !! -> \SELF, |c is raw { &code_obj(SELF)."$name"(|c) }
        }

        method apply_handles(&code_obj: Mu $pkg is raw) {
            $!delegator_name :=
                ($pkg.^language_revision // nqp::getcomp("Raku").language_revision) < 3
                    ?? &code_obj.name
                    !! Nil;

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
                    elsif nqp::istype($expr, Whatever) {
                        $pkg.^add_fallback(
                            -> $obj, $name {
                                nqp::can(nqp::decont(&code_obj($obj)), nqp::decont($name))
                            },
                            -> $obj, $name { self!fallback-code($name) } );
                    }
                    elsif nqp::istype($expr, HyperWhatever) {
                        $pkg.^add_fallback(
                            -> $, $ --> True { },
                            -> $obj, $name { self!fallback-code($name) } );
                    }
                    else {
                        $pkg.^add_fallback(
                            -> $obj, $name {
                                ?($name ~~ $expr)
                            },
                            -> $obj, $name { self!fallback-code($name) } );
                    }
                }
                else {
                    $pkg.^add_fallback(
                        -> $obj, $name {
                            nqp::can(nqp::decont($expr), nqp::decont($name))
                        },
                        -> $obj, $name { self!fallback-code($name) } );
                }
            }

            applier($!handles);
        }
    }
    $m.set_handles(&thunk());
}

proto sub trait_mod:<will>(Mu $, |) {*}
multi sub trait_mod:<will>(Attribute:D $attr, |c ) {
    X::Comp::Trait::Unknown.new(
      file       => $?FILE,
      line       => $?LINE,
      type       => 'will',
      subtype    => c.hash.keys[0],
      declaring  => 'an attribute',
      highexpect => <lazy>,
    ).throw;
}
multi sub trait_mod:<will>(Attribute $attr, Mu :$build!) {  # internal usage
    $attr.set_build($build)
}

proto sub trait_mod:<trusts>(Mu, Mu, *%) {*}
multi sub trait_mod:<trusts>(Mu:U $truster, Mu:U $trustee) {
    $truster.^add_trustee($trustee);
}

proto sub trait_mod:<hides>(Mu, Mu, *%) {*}
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

# vim: expandtab shiftwidth=4
