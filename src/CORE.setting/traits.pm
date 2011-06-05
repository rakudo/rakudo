use Perl6::Metamodel;

proto trait_mod:<is>(|$) { * }
multi trait_mod:<is>(Mu $child, Mu $parent) {
    $child.HOW.add_parent($child, $parent);
}
multi trait_mod:<is>(Attribute $attr, :$rw!) {
    $attr.set_rw();
}

proto trait_mod:<will>(|$) { * }
multi trait_mod:<will>(Attribute $attr, Block $closure, :$build!) {
    Q:PIR { say "in tmw" };
    $attr.set_build_closure($closure)
}
