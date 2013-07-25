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
