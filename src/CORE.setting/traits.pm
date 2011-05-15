use Perl6::Metamodel;

# XXX Until we get multi-dispatch in, just have one single dispatch
# for adding a parent; let's us get something working at all easier.
#proto trait_mod:<is>(|$c) { * }
#
#multi trait_mod:<is>(Mu:U $child, Mu:U $parent) {
#    $child.add_parent($child, $parent);
#}
#
#multi trait_mod:<is>(Attribute $attr, :$rw!) {
#    $attr.set_rw(1);
#}
sub trait_mod:<is>(Mu:U $child, Mu:U $parent) {
    $child.add_parent($child, $parent);
}