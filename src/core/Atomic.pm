#?if moar
multi sub cas($target is rw, $expected, $value) {
    nqp::cas($target, $expected, $value)
}

multi sub cas($target is rw, &code) {
    my $current := nqp::atomicload($target);
    loop {
        my $updated := code($current);
        my $seen := nqp::cas($target, $current, $updated);
        return $updated if nqp::eqaddr($seen, $current);
        $current := $seen;
    }
}
#?endif

#?if !moar
# Retain cheating cas for the sake of spectests that use it.
multi sub cas($target is rw, &code) {
    $target = code($target)
}
#?endif
