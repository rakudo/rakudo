#?if moar
my native atomicint is repr('P6int') is Int is ctype('atomic') { }

#-- fetching a value atomically
proto sub atomic-fetch($) {*}
multi sub atomic-fetch($source is rw) {
    nqp::atomicload($source)
}
multi sub atomic-fetch(atomicint $source is rw) {
    nqp::atomicload_i($source)
}

proto sub prefix:<⚛>($) {*}
multi sub prefix:<⚛>($source is rw) {
    nqp::atomicload($source)
}
multi sub prefix:<⚛>(atomicint $source is rw) {
    nqp::atomicload_i($source)
}

#-- assigning a value atomically
proto sub atomic-assign($, $) {*}
multi sub atomic-assign($target is rw, \value) {
    nqp::atomicstore($target, value)
}
multi sub atomic-assign(atomicint $target is rw, int $value) {
    nqp::atomicstore_i($target, $value)
}
multi sub atomic-assign(atomicint $target is rw, Int $value) {
    nqp::atomicstore_i($target, $value)
}
multi sub atomic-assign(atomicint $target is rw, $value) {
    nqp::atomicstore_i($target, $value.Int)
}

proto sub infix:<⚛=>($, $) {*}
multi sub infix:<⚛=>($target is rw, \value) {
    nqp::atomicstore($target, value)
}
multi sub infix:<⚛=>(atomicint $target is rw, int $value) {
    nqp::atomicstore_i($target, $value)
}
multi sub infix:<⚛=>(atomicint $target is rw, Int $value) {
    nqp::atomicstore_i($target, $value)
}
multi sub infix:<⚛=>(atomicint $target is rw, $value) {
    nqp::atomicstore_i($target, $value.Int)
}

#-- atomically fetch value and increment it
sub atomic-fetch-inc(atomicint $target is rw --> atomicint) {
    nqp::atomicinc_i($target)
}

sub postfix:<⚛++>(atomicint $target is rw --> atomicint) {
    nqp::atomicinc_i($target)
}

#-- atomically increment value and fetch it
sub atomic-inc-fetch(atomicint $target is rw --> atomicint) {
    my atomicint $ = nqp::atomicinc_i($target) + 1
}
sub prefix:<++⚛>(atomicint $target is rw --> atomicint) {
    my atomicint $ = nqp::atomicinc_i($target) + 1
}

#-- atomically fetch value and decrement it
sub atomic-fetch-dec(atomicint $target is rw --> atomicint) {
    nqp::atomicdec_i($target)
}

sub postfix:<⚛-->(atomicint $target is rw --> atomicint) {
    nqp::atomicdec_i($target)
}

#-- atomically decrement value and fetch it
sub atomic-dec-fetch(atomicint $target is rw --> atomicint) {
    my atomicint $ = nqp::atomicdec_i($target) - 1
}
sub prefix:<--⚛>(atomicint $target is rw --> atomicint) {
    my atomicint $ = nqp::atomicdec_i($target) - 1
}

#-- atomically fetch value and then add given value to it
proto sub atomic-fetch-add($, $) {*}
multi sub atomic-fetch-add(atomicint $target is rw, int $add --> atomicint) {
    nqp::atomicadd_i($target, $add)
}
multi sub atomic-fetch-add(atomicint $target is rw, Int $add --> atomicint) {
    nqp::atomicadd_i($target, $add)
}
multi sub atomic-fetch-add(atomicint $target is rw, $add --> atomicint) {
    nqp::atomicadd_i($target, $add.Int)
}

#-- atomically add given value to value and return that
proto sub atomic-add-fetch($, $) {*}
multi sub atomic-add-fetch(atomicint $target is rw, int $add --> atomicint) {
    my atomicint $ = nqp::atomicadd_i($target, $add) + $add
}
multi sub atomic-add-fetch(atomicint $target is rw, Int $add --> atomicint) {
    my atomicint $ = nqp::atomicadd_i($target, $add) + $add
}
multi sub atomic-add-fetch(atomicint $target is rw, $add --> atomicint) {
    my int $add-int = $add.Int;
    my atomicint $ = nqp::atomicadd_i($target, $add-int) + $add-int
}

proto sub infix:<⚛+=>($, $) {*}
multi sub infix:<⚛+=>(atomicint $target is rw, int $add --> atomicint) {
    my atomicint $ = nqp::atomicadd_i($target, $add) + $add
}
multi sub infix:<⚛+=>(atomicint $target is rw, Int $add --> atomicint) {
    my atomicint $ = nqp::atomicadd_i($target, $add) + $add
}
multi sub infix:<⚛+=>(atomicint $target is rw, $add --> atomicint) {
    my int $add-int = $add.Int;
    my atomicint $ = nqp::atomicadd_i($target, $add-int) + $add-int
}

#-- atomically fetch value and then subtract given value from it
proto sub atomic-fetch-sub($, $) {*}
multi sub atomic-fetch-sub(atomicint $target is rw, int $add --> atomicint) {
    nqp::atomicadd_i($target, nqp::neg_i($add))
}
multi sub atomic-fetch-sub(atomicint $target is rw, Int $add --> atomicint) {
    nqp::atomicadd_i($target, nqp::neg_i($add))
}
multi sub atomic-fetch-sub(atomicint $target is rw, $add --> atomicint) {
    nqp::atomicadd_i($target, nqp::neg_i($add.Int))
}

#-- atomically subtract given value from value and return that
proto sub atomic-sub-fetch($, $) {*}
multi sub atomic-sub-fetch(atomicint $target is rw, int $add --> atomicint) {
    my atomicint $ = nqp::atomicadd_i($target, nqp::neg_i($add)) - $add
}
multi sub atomic-sub-fetch(atomicint $target is rw, Int $add --> atomicint) {
    my atomicint $ = nqp::atomicadd_i($target, nqp::neg_i($add)) - $add
}
multi sub atomic-sub-fetch(atomicint $target is rw, $add --> atomicint) {
    my int $add-int = nqp::neg_i($add.Int);
    my atomicint $ = nqp::atomicadd_i($target, $add-int) + $add-int
}

proto sub infix:<⚛-=>($, $) {*}
multi sub infix:<⚛-=>(atomicint $target is rw, int $add --> atomicint) {
    my atomicint $ = nqp::atomicadd_i($target, nqp::neg_i($add)) - $add
}
multi sub infix:<⚛-=>(atomicint $target is rw, Int $add --> atomicint) {
    my atomicint $ = nqp::atomicadd_i($target, nqp::neg_i($add)) - $add
}
multi sub infix:<⚛-=>(atomicint $target is rw, $add --> atomicint) {
    my int $add-int = nqp::neg_i($add.Int);
    my atomicint $ = nqp::atomicadd_i($target, $add-int) + $add-int
}
my constant &infix:<⚛−=> := &infix:<⚛-=>;

#-- provide full barrier semantics
sub full-barrier(--> Nil) {
    nqp::barrierfull()
}

#-- atomic compare and swap
proto sub cas(|) {*}
multi sub cas($target is rw, \expected, \value) {
    nqp::cas($target, expected, value)
}

multi sub cas(atomicint $target is rw, int $expected, int $value) {
    nqp::cas_i($target, $expected, $value)
}

multi sub cas(atomicint $target is rw, Int $expected, Int $value) {
    nqp::cas_i($target, $expected, $value)
}

multi sub cas(atomicint $target is rw, $expected, $value) {
    nqp::cas_i($target, $expected.Int, $value.Int)
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
