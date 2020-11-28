#== Atomics available on all backends ============================================

#-- fetching a value atomically
proto sub atomic-fetch($, *%) {*}
multi sub atomic-fetch($source is rw) {
    nqp::atomicload($source)
}

proto sub prefix:<⚛>($, *%) {*}
multi sub prefix:<⚛>($source is rw) {
    nqp::atomicload($source)
}

#-- assigning a value atomically
proto sub atomic-assign($, $, *%) {*}
multi sub atomic-assign($target is rw, \value) {
    nqp::atomicstore($target, value)
}

#-- atomic compare and swap
proto sub cas(Mu $, Mu $, Mu $?, *%) {*}
multi sub cas(Mu $target is rw, Mu \expected, Mu \value) {
    nqp::cas($target, expected, value)
}
multi sub cas(Mu $target is rw, &code) {
    my $current := nqp::atomicload($target);
    nqp::until(
      nqp::stmts(
        (my $updated := code($current)),
        (my $seen := nqp::cas($target, $current, $updated)),
        nqp::eqaddr($seen, $current)
      ),
      $current := $seen
    );
    $updated
}

#== Native integer atomics only available on MoarVM ==============================

#?if !jvm
my native atomicint is repr('P6int') is Int is ctype('atomic') { }

#-- fetching a native integer value atomically
multi sub atomic-fetch(atomicint $source is rw) {
    nqp::atomicload_i($source)
}

multi sub prefix:<⚛>(atomicint $source is rw) {
    nqp::atomicload_i($source)
}

#-- assigning a native integer value atomically
multi sub atomic-assign(atomicint $target is rw, int $value) {
    nqp::atomicstore_i($target, $value)
}
multi sub atomic-assign(atomicint $target is rw, Int:D $value) {
    nqp::atomicstore_i($target, $value)
}
multi sub atomic-assign(atomicint $target is rw, $value) {
    nqp::atomicstore_i($target, $value.Int)
}

proto sub infix:<⚛=>($, $, *%) {*}
multi sub infix:<⚛=>($target is rw, \value) {
    nqp::atomicstore($target, value)
}
multi sub infix:<⚛=>(atomicint $target is rw, int $value) {
    nqp::atomicstore_i($target, $value)
}
multi sub infix:<⚛=>(atomicint $target is rw, Int:D $value) {
    nqp::atomicstore_i($target, $value)
}
multi sub infix:<⚛=>(atomicint $target is rw, $value) {
    nqp::atomicstore_i($target, $value.Int)
}

#-- atomically fetch native integer value and increment it
proto sub atomic-fetch-inc($, *%) {*}
multi sub atomic-fetch-inc(atomicint $target is rw --> atomicint) {
    nqp::atomicinc_i($target)
}

proto sub postfix:<⚛++>($, *%) {*}
multi sub postfix:<⚛++>(atomicint $target is rw --> atomicint) {
    nqp::atomicinc_i($target)
}

#-- atomically increment native integer value and fetch it
proto sub atomic-inc-fetch($, *%) {*}
multi sub atomic-inc-fetch(atomicint $target is rw --> atomicint) {
    my atomicint $ = nqp::atomicinc_i($target) + 1
}

proto sub prefix:<++⚛>($, *%) {*}
multi sub prefix:<++⚛>(atomicint $target is rw --> atomicint) {
    my atomicint $ = nqp::atomicinc_i($target) + 1
}

#-- atomically fetch native integer value and decrement it
proto sub atomic-fetch-dec($, *%) {*}
multi sub atomic-fetch-dec(atomicint $target is rw --> atomicint) {
    nqp::atomicdec_i($target)
}

proto sub postfix:<⚛-->($, *%) {*}
multi sub postfix:<⚛-->(atomicint $target is rw --> atomicint) {
    nqp::atomicdec_i($target)
}

#-- atomically decrement native integer value and fetch it
proto sub atomic-dec-fetch($, *%) {*}
multi sub atomic-dec-fetch(atomicint $target is rw --> atomicint) {
    my atomicint $ = nqp::atomicdec_i($target) - 1
}

proto sub prefix:<--⚛>($, *%) {*}
multi sub prefix:<--⚛>(atomicint $target is rw --> atomicint) {
    my atomicint $ = nqp::atomicdec_i($target) - 1
}

#-- atomically fetch native integer value and then add given value to it
proto sub atomic-fetch-add($, $, *%) {*}
multi sub atomic-fetch-add(atomicint $target is rw, int $add --> atomicint) {
    nqp::atomicadd_i($target, $add)
}
multi sub atomic-fetch-add(atomicint $target is rw, Int:D $add --> atomicint) {
    nqp::atomicadd_i($target, $add)
}
multi sub atomic-fetch-add(atomicint $target is rw, $add --> atomicint) {
    nqp::atomicadd_i($target, $add.Int)
}

#-- atomically add given native integer value to value and return that
proto sub atomic-add-fetch($, $, *%) {*}
multi sub atomic-add-fetch(atomicint $target is rw, int $add --> atomicint) {
    my atomicint $ = nqp::atomicadd_i($target, $add) + $add
}
multi sub atomic-add-fetch(atomicint $target is rw, Int:D $add --> atomicint) {
    my atomicint $ = nqp::atomicadd_i($target, $add) + $add
}
multi sub atomic-add-fetch(atomicint $target is rw, $add --> atomicint) {
    my int $add-int = $add.Int;
    my atomicint $ = nqp::atomicadd_i($target, $add-int) + $add-int
}

proto sub infix:<⚛+=>($, $, *%) {*}
multi sub infix:<⚛+=>(atomicint $target is rw, int $add --> atomicint) {
    my atomicint $ = nqp::atomicadd_i($target, $add) + $add
}
multi sub infix:<⚛+=>(atomicint $target is rw, Int:D $add --> atomicint) {
    my atomicint $ = nqp::atomicadd_i($target, $add) + $add
}
multi sub infix:<⚛+=>(atomicint $target is rw, $add --> atomicint) {
    my int $add-int = $add.Int;
    my atomicint $ = nqp::atomicadd_i($target, $add-int) + $add-int
}

#-- atomically fetch native integer value and then subtract given value from it
proto sub atomic-fetch-sub($, $, *%) {*}
multi sub atomic-fetch-sub(atomicint $target is rw, int $add --> atomicint) {
    nqp::atomicadd_i($target, nqp::neg_i($add))
}
multi sub atomic-fetch-sub(atomicint $target is rw, Int:D $add --> atomicint) {
    nqp::atomicadd_i($target, nqp::neg_i($add))
}
multi sub atomic-fetch-sub(atomicint $target is rw, $add --> atomicint) {
    nqp::atomicadd_i($target, nqp::neg_i($add.Int))
}

#-- atomically subtract given native integer value from value and return that
proto sub atomic-sub-fetch($, $, *%) {*}
multi sub atomic-sub-fetch(atomicint $target is rw, int $add --> atomicint) {
    my atomicint $ = nqp::atomicadd_i($target, nqp::neg_i($add)) - $add
}
multi sub atomic-sub-fetch(atomicint $target is rw, Int:D $add --> atomicint) {
    my atomicint $ = nqp::atomicadd_i($target, nqp::neg_i($add)) - $add
}
multi sub atomic-sub-fetch(atomicint $target is rw, $add --> atomicint) {
    my int $add-int = nqp::neg_i($add.Int);
    my atomicint $ = nqp::atomicadd_i($target, $add-int) + $add-int
}

proto sub infix:<⚛-=>($, $, *%) {*}
multi sub infix:<⚛-=>(atomicint $target is rw, int $add --> atomicint) {
    my atomicint $ = nqp::atomicadd_i($target, nqp::neg_i($add)) - $add
}
multi sub infix:<⚛-=>(atomicint $target is rw, Int:D $add --> atomicint) {
    my atomicint $ = nqp::atomicadd_i($target, nqp::neg_i($add)) - $add
}
multi sub infix:<⚛-=>(atomicint $target is rw, $add --> atomicint) {
    my int $add-int = nqp::neg_i($add.Int);
    my atomicint $ = nqp::atomicadd_i($target, $add-int) + $add-int
}
my constant &infix:<⚛−=> := &infix:<⚛-=>;

#-- provide full barrier semantics
proto sub full-barrier(*%) {*}
multi sub full-barrier(--> Nil) {
    nqp::barrierfull()
}

#-- atomic compare and swap a native integer
multi sub cas(atomicint $target is rw, int $expected, int $value) {
    nqp::cas_i($target, $expected, $value)
}
multi sub cas(atomicint $target is rw, Int:D $expected, Int:D $value) {
    nqp::cas_i($target, $expected, $value)
}
multi sub cas(atomicint $target is rw, $expected, $value) {
    nqp::cas_i($target, $expected.Int, $value.Int)
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

# vim: expandtab shiftwidth=4
