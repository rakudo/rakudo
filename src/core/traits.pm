# XXX Signature is wrong really - will fix once we can parse other things.
# XXX Use .^ once we learn how to parse it.
our multi trait_mod:<is>(Object $child, Object $parent) {
    $child.add_parent($child, $parent);
}

our multi trait_mod:<of>(ContainerDeclarand $cont, Object \$type) {
    
}

our multi trait_mod:<does>(Object $target, Role $r) {
    $target.add_composable($target, $r);
}
