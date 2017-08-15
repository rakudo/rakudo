#?if moar
my native atomicint is repr('P6int') is Int is ctype('atomic') { }

my class Atomic is repr('Uninstantiable') {
    proto method fetch($) {*}
    multi method fetch($source is rw) {
        nqp::atomicload($source)
    }
    multi method fetch(atomicint $source is rw) {
        nqp::atomicload_i($source)
    }

    proto method assign($, $) {*}
    multi method assign($target is rw, $value) {
        nqp::atomicstore($target, $value)
    }
    multi method assign(atomicint $target is rw, int $value) {
        nqp::atomicstore_i($target, $value)
    }
    multi method assign(atomicint $target is rw, Int $value) {
        nqp::atomicstore_i($target, $value)
    }
    multi method assign(atomicint $target is rw, $value) {
        nqp::atomicstore_i($target, $value.Int)
    }

    method inc(atomicint $target is rw --> atomicint) {
        nqp::atomicinc_i($target)
    }

    method dec(atomicint $target is rw --> atomicint) {
        nqp::atomicdec_i($target)
    }

    method add(atomicint $target is rw, int $add --> atomicint) {
        nqp::atomicadd_i($target, $add)
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

multi sub cas(atomicint $target is rw, &code) {
    my int $current = nqp::atomicload_i($target);
    loop {
        my int $updated = code($current);
        my int $seen = nqp::cas_i($target, $current, $updated);
        return $updated if $seen == $current;
        $current = $seen;
    }
}
#?endif

#?if !moar
# Retain cheating cas for the sake of spectests that use it.
multi sub cas($target is rw, &code) {
    $target = code($target)
}
#?endif
