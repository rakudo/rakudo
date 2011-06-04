use Perl6::Metamodel;

proto trait_mod:<is>(|$) { * }
multi trait_mod:<is>(Mu $child, Mu $parent) {
    Q:PIR { say "in tmi" };
    $child.HOW.add_parent($child, $parent);
    Q:PIR { say "survived tmi" };
}
