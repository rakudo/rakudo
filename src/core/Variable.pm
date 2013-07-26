my class Variable {
    has str $.name;
    has str $.scope;
    has $.var is rw;
    has $.block;
}

# Variable traits come here, not in traits.pm, since we declare Variable
# in the setting rather than BOOTSTRAP.
multi trait_mod:<is>(Variable:D $v, :$default!) {
    $v.var = $default;
    nqp::getattr($v.var, $v.VAR.WHAT, '$!descriptor').set_default($default);
}
multi trait_mod:<is>(Variable:D $v, :$readonly!) {
    nqp::getattr($v.var, $v.VAR.WHAT, '$!descriptor').set_rw(!$readonly);
}
multi trait_mod:<is>(Variable:D $v, :$rw!) {
    nqp::getattr($v.var, $v.VAR.WHAT, '$!descriptor').set_rw($rw);
}
multi trait_mod:<is>(Variable:D $v, :$dynamic!) {
# not sure what needs to happen here yet
}
multi trait_mod:<is>(Variable:D $v, :$context!) {
# unspecced, but spectested
}
