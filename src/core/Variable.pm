# Variable traits come here, not in traits.pm, since we declare Variable
# in the setting rather than BOOTSTRAP.

my class Variable {
    has str $.name;
    has str $.scope;
    has $.var is rw;
    has $.block;
}

# container traits
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
multi trait_mod:<of>(Variable:D $v, Mu:U $of ) {
    nqp::getattr($v.var, $v.var.VAR.WHAT, '$!descriptor').set_of(nqp::decont($of));
}

multi trait_mod:<is>(Variable:D $v, Mu:U $is ) {
# not sure what needs to happen here yet, do we need an extra attribute?
#    nqp::getattr($v.var, $v.VAR.WHAT, '$!descriptor').set_of($of);
}

# visibility traits
multi trait_mod:<is>(Variable:D $v, :$dynamic!) {
# not sure what needs to happen here yet
}

# phaser traits
multi trait_mod:<will>(Variable:D $v, $block, :$begin! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, $block, :$check! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, $block, :$final! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, $block, :$init! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, $block, :$end! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, $block, :$enter! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, $block, :$leave! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, $block, :$keep! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, $block, :$undo! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, $block, :$first! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, $block, :$next! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, $block, :$last! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, $block, :$pre! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, $block, :$post! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, $block, :$catch! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, $block, :$control! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, $block, :$compose! ) {
# not sure what needs to happen here yet
}
