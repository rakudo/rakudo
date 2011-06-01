# Need to be able to augment in the setting, and this is the first file, so we
# put this here.
use MONKEY_TYPING;
use SETTING_MODE;

# XXX Signature is wrong really - will fix once we can parse other things.
our multi trait_mod:<is>(Mu $child, Mu $parent) {
    $child.^add_parent($parent);
}

our multi trait_mod:<is>(Mu $child, Role $r) {
    $child.^add_parent($r!select!pun);
}

our multi trait_mod:<is>(Mu $type where { !.defined }, :$rw!) {
    $type.HOW does role { method rw { True } }
}

our multi trait_mod:<is>(Mu $type where { !.defined }, :$hidden!) {
    $type.HOW does role { method hidden { True } }
}

our multi trait_mod:<is>(Routine $r, :$default!) {
    $r does role { method default { True } }
}

our multi trait_mod:<hides>(Mu $child, Mu $parent) {
    trait_mod:<is>($child, $parent);
    $child.^hides.push($parent);
}

role Positional { ... }
role Associative { ... }
our multi trait_mod:<of>(ContainerDeclarand $cont, Mu \$type) {
    given substr($cont.name, 0, 1) {
        when '@' { $cont.container does Positional[$type] }
        when '%' { $cont.container does Associative[$type] }
        when '&' { }
        default  { pir::setprop__vPSP($cont.container, 'type', $type) }
    }
}

our multi trait_mod:<does>(Mu $target, Role $r) {
    $target.^add_composable($r);
}

our multi trait_mod:<does>(Mu $target, ConcreteRole $r) {
    $target.^add_composable($r);
}

our multi trait_mod:<does>(Mu $target, Mu $unrole) {
    die "Can only use does with a role, but " ~ $unrole.perl ~ " is not one";
}

our multi trait_mod:<does>(ContainerDeclarand $c, Role $r) {
    $c.container does $r;
}

our multi trait_mod:<of>(Routine $r, Mu \$type) {
    $r does Callable[$type];
}

our multi trait_mod:<returns>(Routine $r, Mu \$type) {
    $r does Callable[$type];
}
