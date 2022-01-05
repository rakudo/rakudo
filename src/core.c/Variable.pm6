# for our tantrums
my class X::Comp::NYI { ... };
my class X::Comp::Trait::Unknown { ... };
my class X::Comp::Trait::NotOnNative { ... };
my class X::Comp::Trait::Scope { ... };

# Variable traits come here, not in traits.pm6, since we declare Variable
# in the setting rather than BOOTSTRAP.

my class Variable {
    has str $.name;
    has str $.scope;
    has $.var is rw;
    has $.block;
    has $.slash;
    has $.implicit-lexical-usage is rw;

    # make throwing easier
    submethod throw ( |c ) {
        $*W.throw( self.slash, |c );
    }

    submethod willdo(&block, $caller-levels = 3) {
        $caller-levels == 3
            ?? -> { block(nqp::atkey(nqp::ctxcaller(nqp::ctxcaller(nqp::ctxcaller(nqp::ctx()))), self.name)) }
            !! -> { block(nqp::atkey(nqp::ctxcaller(nqp::ctx()), self.name)) }
    }

    submethod native(Mu $what) {
        my $name := $what.raku;
        $name.starts-with('array') || $name eq 'Mu'
          ?? $name
          !! $name.ends-with('LexRef')
            ?? $name.substr(0,3).lc
            !! '';
    }
}

# "is" traits
multi sub trait_mod:<is>(Variable:D $v, |c ) {
    $v.throw( 'X::Comp::Trait::Unknown',
      type      => 'is',
      subtype   => c.hash.keys[0],
      declaring => ' variable',
      expected  => <TypeObject default dynamic export>,
    );
}
multi sub trait_mod:<is>(Variable:D $v, Mu :$default!) {
    my $var  := $v.var;
    my $what := $var.VAR.WHAT;

    my $descriptor;
    {
        $descriptor := nqp::getattr($var, $what.^mixin_base, '$!descriptor');
        CATCH {
            my $native = $v.native($what);
            $native
              ?? nqp::istype($default,Whatever)
                ?? $v.throw('X::Comp::NYI',
                     :feature("is default(*) on native $native"))
                !! $v.throw( 'X::Comp::Trait::NotOnNative',
                     :type<is>, :subtype<default>,
                     :native($native eq 'Mu' ?? ''!! $native ))  # yuck
              !! $v.throw('X::Comp::NYI',
                     :feature("is default on shaped $what.raku()"))
        }
    }

    my $of := $descriptor.of;
    $v.throw( 'X::Parameter::Default::TypeCheck',
      :expected($var.WHAT),
      :what<variable>,
      :got(nqp::eqaddr($default,Nil) ?? 'Nil' !! $default)
    ) unless nqp::istype($default, $of)
        or nqp::eqaddr($default,Nil)
        or nqp::eqaddr($of,Mu);
    $descriptor.set_default(nqp::decont($default));

    # make sure we start with the default if a scalar
    $var = $default if nqp::istype($what, Scalar);
}
multi sub trait_mod:<is>(Variable:D $v, :$dynamic!) {
    my $var  := $v.var;
    my $what := $var.VAR.WHAT;
    {
        nqp::getattr($var,$what.^mixin_base,'$!descriptor').set_dynamic($dynamic);
        CATCH {
            my $native = $v.native($what);
            $native
              ?? $v.throw( 'X::Comp::Trait::NotOnNative',
                   :type<is>, :subtype<dynamic>,
                   :native($native eq 'Mu' ?? ''!! $native ))  # yuck
              !! $v.throw('X::Comp::NYI',
                     :feature("is dynamic on shaped $what.raku()"))
        }
    }
}
multi sub trait_mod:<is>(Variable:D $v, :$export!) {
    if $v.scope ne 'our' {
        $v.throw( 'X::Comp::Trait::Scope',
          type      => 'is',
          subtype   => 'export',
          declaring => 'variable',
          scope     => $v.scope,
          supported => ['our'],
        );
    }
    my $var  := $v.var;
    my @tags = flat 'ALL', (nqp::istype($export,Pair) ?? $export.key() !!
                            nqp::istype($export,Positional) ?? @($export)>>.key !!
                            'DEFAULT');
    Rakudo::Internals.EXPORT_SYMBOL($var.VAR.name, @tags, $var);
}

# does trait
multi sub trait_mod:<does>(Variable:D $v, Mu:U $role) {
    if $role.HOW.archetypes.composable() {
        $v.var.VAR does $role;
    }
    elsif $role.HOW.archetypes.composalizable() {
        $v.var.VAR does $role.HOW.composalize($role);
    }
    else {
        X::Composition::NotComposable.new(
            target-name => 'a variable',
            composer    => $role,
        ).throw;
    }
}

# phaser traits
multi sub trait_mod:<will>(Variable:D $v, $block, |c ) {
    $v.throw( 'X::Comp::Trait::Unknown',
      type      => 'will',
      subtype   => c.hash.keys[0],
      declaring => ' variable',
      expected  => ('begin check final init end',
                    'enter leave keep undo',
                    'first next last pre post',
                    'compose'),
    );
}
multi sub trait_mod:<will>(Variable:D $v, $block, :begin($)! ) {
    $block($v.var); # no need to delay execution
}
multi sub trait_mod:<will>(Variable:D $v, $block, :check($)! ) {
    $*W.add_phaser($v.slash, 'CHECK', $block);
}
multi sub trait_mod:<will>(Variable:D $v, $block, :final($)! ) {
    $v.throw( 'X::Comp::NYI',
      feature => "Variable trait 'will final {...}'",
    );
}
multi sub trait_mod:<will>(Variable:D $v, $block, :init($)! ) {
    $v.throw( 'X::Comp::NYI',
      feature => "Variable trait 'will init {...}'",
    );
}
multi sub trait_mod:<will>(Variable:D $v, $block, :end($)! ) {
    $*W.add_object($block);
    $*W.add_phaser($v.slash, 'END', $block);
}
multi sub trait_mod:<will>(Variable:D $v, $block, :enter($)! ) {
    $v.block.add_phaser('ENTER', $v.willdo($block, 1) );
    $v.implicit-lexical-usage = True;
}
multi sub trait_mod:<will>(Variable:D $v, $block, :leave($)! ) {
    $v.block.add_phaser('LEAVE', $v.willdo($block) );
    $v.implicit-lexical-usage = True;
}
multi sub trait_mod:<will>(Variable:D $v, $block, :keep($)! ) {
    $v.block.add_phaser('KEEP', $v.willdo($block));
    $v.implicit-lexical-usage = True;
}
multi sub trait_mod:<will>(Variable:D $v, $block, :undo($)! ) {
    $v.block.add_phaser('UNDO', $v.willdo($block));
    $v.implicit-lexical-usage = True;
}
multi sub trait_mod:<will>(Variable:D $v, $block, :first($)! ) {
    $v.block.add_phaser('FIRST', $v.willdo($block, 1));
    $v.implicit-lexical-usage = True;
}
multi sub trait_mod:<will>(Variable:D $v, $block, :next($)! ) {
    $v.block.add_phaser('NEXT', $block);
}
multi sub trait_mod:<will>(Variable:D $v, $block, :last($)! ) {
    $v.block.add_phaser('LAST', $block);
}
multi sub trait_mod:<will>(Variable:D $v, $block, :pre($)! ) {
    $v.block.add_phaser('PRE', $v.willdo($block, 1));
    $v.implicit-lexical-usage = True;
}
multi sub trait_mod:<will>(Variable:D $v, $block, :post($)! ) {
    $v.throw( 'X::Comp::NYI',
      feature => "Variable trait 'will post {...}'",
    );
}
multi sub trait_mod:<will>(Variable:D $v, $block, :compose($)! ) {
    $v.throw( 'X::Comp::NYI',
      feature => "Variable trait 'will compose {...}'",
    );
}

# vim: expandtab shiftwidth=4
