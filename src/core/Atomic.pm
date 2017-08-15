#?if moar
my native atomicint is repr('P6int') is Int is ctype('atomic') { }

my class Atomic is repr('Uninstantiable') {
    proto method fetch($) {*}
    multi method fetch($source is rw) {
        nqp::atomicload($source)
    }

    proto method assign($, $) {*}
    multi method assign($target is rw, $value) {
        nqp::atomicstore($target, $value)
    }

    method full-barrier(--> Nil) {
        nqp::barrierfull()
    }
}

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
