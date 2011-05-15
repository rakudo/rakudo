use Perl6::Metamodel;

proto trait_mod:<is>(|$c) { * }

multi trait_mod:<is>(Mu:U $child, Mu:U $parent) {
    $child.add_parent($child, $parent);
}

multi trait_mod:<is>(Attribute $attr, :$rw!) {
    $attr.set_rw(1);
}
