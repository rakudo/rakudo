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
    has $.slash;

    # make throwing easier
    submethod throw ( |c ) {
        $*W.throw( self.slash, |c );
    }
}

# "is" traits
multi trait_mod:<is>(Variable:D $v, |c ) {
    $v.throw( 'X::Comp::Trait', 
      type      => 'is',
      subtype   => c.hash.keys[0],
      declaring => ' variable',
      expected  => <TypeObject default dynamic>,
    );
}
multi trait_mod:<is>(Variable:D $v, Mu:U $is ) {
    $v.throw( 'X::Comp::NYI',
      feature => "Variable trait 'is TypeObject'",
    );
}
multi trait_mod:<is>(Variable:D $v, :$default!) {
    my $var  := $v.var;
    my $what := $var.VAR.WHAT;
    # make sure we start with the default if a scalar
    $var = $default if $what ~~ Scalar;
    nqp::getattr(
      $var,
      $what.perl ~~ m/\+/ # we have types mixed in
        ?? $what.^mro[1]  # (Hash+{TypedHash}) -> (Hash)
        !! $what,
      '$!descriptor',
    ).set_default($default);
}
multi trait_mod:<is>(Variable:D $v, :$dynamic!) {
# must be a noop for now, as apparently outer scope lexicals are *always*
# visible with the CALLER:: interface, even if they're *not* marked as
# "is dynamic"
}

# "of" traits
multi trait_mod:<of>(Variable:D $v, |c ) {
    $v.throw( 'X::Comp::Trait', 
      type      => 'of',
      subtype   => c.hash.keys[0],
      declaring => ' variable',
      expected  => <TypeObject>,
    );
}
multi trait_mod:<of>(Variable:D $v, Mu:U $of ) {
    nqp::getattr($v.var, $v.var.VAR.WHAT, '$!descriptor').set_of(nqp::decont($of));
}

# phaser traits
multi trait_mod:<will>(Variable:D $v, $block, |c ) {
    $v.throw( 'X::Comp::Trait',
      type      => 'will',
      subtype   => c.hash.keys[0],
      declaring => ' variable',
      expected  => ('begin check final init end',
                    'enter leave keep undo',
                    'first next last pre post',
                    'compose'),
    );
}
multi trait_mod:<will>(Variable:D $v, $block, :$begin! ) {
    $block(); # no need to delay execution
}
multi trait_mod:<will>(Variable:D $v, $block, :$check! ) {
    $*W.add_phaser($v.slash, 'CHECK', $block)
}
multi trait_mod:<will>(Variable:D $v, $block, :$final! ) {
    $v.throw( 'X::Comp::NYI',
      feature => "Variable trait 'will final {...}'",
    );
}
multi trait_mod:<will>(Variable:D $v, $block, :$init! ) {
# for some reason exceptions are caught and not rethrown
#    $*W.add_phaser($v.slash, 'INIT', $block)  # doesn't work :-(
}
multi trait_mod:<will>(Variable:D $v, $block, :$end! ) {
# for some reason exceptions are caught and not rethrown
#    $*W.add_phaser($v.slash, 'END', $block)  # doesn't work :-(
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
# for some reason exceptions are caught and not rethrown
#    $v.block.add_phaser('POST', $block)  # doesn't work :-(
}
multi trait_mod:<will>(Variable:D $v, $block, :$compose! ) {
# for some reason exceptions are caught and not rethrown
#    $*W.add_phaser($v.slash, 'COMPOSE', $block)  # doesn't work :-(
}
