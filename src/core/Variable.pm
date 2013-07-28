# for our tantrums
my class X::Comp::NYI { ... };
my class X::Comp::Trait { ... };

# Variable traits come here, not in traits.pm, since we declare Variable
# in the setting rather than BOOTSTRAP.

my class Variable {
    has str $.name;
    has str $.scope;
    has $.var is rw;
    has $.block;
    has $.world;
    has $.slash;
}

# "is" traits
multi trait_mod:<is>(Variable:D $v, |c ) {
    X::Comp::Trait.new(
      file            => $?FILE,
      line            => $?LINE,
      is-compile-time => True,

      type            => 'is',
      subtype         => c.hash.keys[0],
      declaring       => 'variable',
      highexpect      => <TypeObject default dynamic>,
    ).throw;
}
multi trait_mod:<is>(Variable:D $v, Mu:U $is ) {
    X::Comp::NYI.new(
      file            => $?FILE,
      line            => $?LINE,
      is-compile-time => True,

      feature         => "Variable trait 'is TypeObject'",
    ).throw;
}
multi trait_mod:<is>(Variable:D $v, :$default!) {
    $v.var = $default;  # make sure we start with the default
    nqp::getattr($v.var, $v.var.VAR.WHAT, '$!descriptor').set_default($default);
}
multi trait_mod:<is>(Variable:D $v, :$readonly!) {
    nqp::getattr($v.var, $v.var.VAR.WHAT, '$!descriptor').set_rw(!$readonly);
}
multi trait_mod:<is>(Variable:D $v, :$rw!) {
    nqp::getattr($v.var, $v.var.VAR.WHAT, '$!descriptor').set_rw($rw);
}
multi trait_mod:<is>(Variable:D $v, :$dynamic!) {
# not sure what needs to happen here yet
}

# "of" traits
multi trait_mod:<of>(Variable:D $v, |c ) {
    X::Comp::Trait.new(
      file            => $?FILE,
      line            => $?LINE,
      is-compile-time => True,

      type            => 'of',
      subtype         => c.hash.keys[0],
      declaring       => 'variable',
      highexpect      => <TypeObject>,
    ).throw;
}
multi trait_mod:<of>(Variable:D $v, Mu:U $of ) {
    nqp::getattr($v.var, $v.var.VAR.WHAT, '$!descriptor').set_of(nqp::decont($of));
}

# phaser traits
multi trait_mod:<will>(Variable:D $v, $block, |c ) {
    X::Comp::Trait.new(
      file            => $?FILE,
      line            => $?LINE,
      is-compile-time => True,

      type            => 'will',
      subtype         => c.hash.keys[0],
      declaring       => 'variable',
      highexpect      => <begin check final init end enter leave keep undo first next last pre post catch control compose>,
    ).throw;
}
multi trait_mod:<will>(Variable:D $v, $block, :$begin! ) {
    $v.block.add_phaser('BEGIN', $block)
}
multi trait_mod:<will>(Variable:D $v, $block, :$check! ) {
    $v.block.add_phaser('CHECK', $block)
}
multi trait_mod:<will>(Variable:D $v, $block, :$final! ) {
    $v.block.add_phaser('FINAL', $block)
}
multi trait_mod:<will>(Variable:D $v, $block, :$init! ) {
    $v.block.add_phaser('INIT', $block)
}
multi trait_mod:<will>(Variable:D $v, $block, :$end! ) {
    $v.block.add_phaser('END', $block)
}
multi trait_mod:<will>(Variable:D $v, $block, :$enter! ) {
    $v.block.add_phaser('ENTER', $block)
}
multi trait_mod:<will>(Variable:D $v, $block, :$leave! ) {
    $v.block.add_phaser('LEAVE', $block)
}
multi trait_mod:<will>(Variable:D $v, $block, :$keep! ) {
    $v.block.add_phaser('KEEP', $block)
}
multi trait_mod:<will>(Variable:D $v, $block, :$undo! ) {
    $v.block.add_phaser('UNDO', $block)
}
multi trait_mod:<will>(Variable:D $v, $block, :$first! ) {
    $v.block.add_phaser('FIRST', $block)
}
multi trait_mod:<will>(Variable:D $v, $block, :$next! ) {
    $v.block.add_phaser('NEXT', $block)
}
multi trait_mod:<will>(Variable:D $v, $block, :$last! ) {
    $v.block.add_phaser('LAST', $block)
}
multi trait_mod:<will>(Variable:D $v, $block, :$pre! ) {
    $v.block.add_phaser('PRE', $block)
}
multi trait_mod:<will>(Variable:D $v, $block, :$post! ) {
    $v.block.add_phaser('POST', $block)
}
multi trait_mod:<will>(Variable:D $v, $block, :$catch! ) {
    $v.block.add_phaser('CATCH', $block)
}
multi trait_mod:<will>(Variable:D $v, $block, :$control! ) {
    $v.block.add_phaser('CONTROL', $block)
}
multi trait_mod:<will>(Variable:D $v, $block, :$compose! ) {
    $v.block.add_phaser('COMPOSE', $block)
}
