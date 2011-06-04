use Perl6::Metamodel;

proto trait_mod:<is>(|$) { * }
multi trait_mod:<is>(Mu $child, Mu $parent) {
    $child.HOW.add_parent($child, $parent);
}
