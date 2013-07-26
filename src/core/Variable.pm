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
multi trait_mod:<will>(Variable:D $v, Block:D :$begin! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, Block:D :$check! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, Block:D :$final! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, Block:D :$init! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, Block:D :$end! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, Block:D :$enter! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, Block:D :$leave! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, Block:D :$keep! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, Block:D :$undo! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, Block:D :$first! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, Block:D :$next! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, Block:D :$last! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, Block:D :$pre! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, Block:D :$post! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, Block:D :$catch! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, Block:D :$control! ) {
# not sure what needs to happen here yet
}
multi trait_mod:<will>(Variable:D $v, Block:D :$compose! ) {
# not sure what needs to happen here yet
}
