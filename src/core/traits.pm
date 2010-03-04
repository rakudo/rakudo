# XXX Signature is wrong really - will fix once we can parse other things.
our multi trait_mod:<is>(Mu $child, Mu $parent) {
    $child.^add_parent($parent);
}

our multi trait_mod:<is>(Mu $child, Role $r) {
    $child.^add_parent($r!select!pun);
}

our multi trait_mod:<of>(ContainerDeclarand $cont, Mu \$type) {
    given substr($cont.name, 0, 1) {
        when '@' { $cont.container does Positional[$type] }
        when '%' { $cont.container does Associative[$type] }
        when '&' { }
        default  { pir::setprop__vPSP($cont.container, 'type', $type) }
    }
}

our multi trait_mod:<does>(Mu $target, Mu $r) {
    $target.^add_composable($r);
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
