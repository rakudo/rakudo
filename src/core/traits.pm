# XXX Signature is wrong really - will fix once we can parse other things.
# XXX Use .^ once we learn how to parse it.
our multi trait_mod:<is>(Mu $child, Mu $parent) {
    $child.add_parent($child, $parent);
}

our multi trait_mod:<of>(ContainerDeclarand $cont, Mu \$type) {
    
}

our multi trait_mod:<does>(Mu $target, Role $r) {
    $target.add_composable($target, $r);
}

our multi trait_mod:<does>(ContainerDeclarand $c, Role $r) {
    $c.container does $r;
}
