#?if moar
multi sub cas($target is rw, $expected, $value) {
    nqp::cas($target, $expected, $value)
}

multi sub cas($target is rw, &code) {
    # XXX Rewrite this to really use atomic compare and swap
    $target = code($target)
}
#?endif

#?if !moar
# Retain cheating cas for the sake of spectests that use it.
multi sub cas($target is rw, &code) {
    $target = code($target)
}
#?endif
