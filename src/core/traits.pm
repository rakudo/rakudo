# XXX Signature is wrong really - will fix once we can parse other things.
# XXX Use .^ once we learn how to parse it.
our multi trait_mod:<is>(Object $child, Object $parent) {
    $child.HOW.add_parent($child, $parent);
}

