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

#-- post-increment a native int atomically
sub atomic-postfix-inc(atomicint $target is rw --> atomicint) {
    nqp::atomicinc_i($target)
}

sub postfix:<⚛++>(atomicint $target is rw --> atomicint) {
    nqp::atomicinc_i($target)
}

#-- pre-increment a native int atomically
sub atomic-prefix-inc(atomicint $target is rw --> atomicint) {
    my atomicint $ = nqp::atomicinc_i($target) + 1
}
sub prefix:<++⚛>(atomicint $target is rw --> atomicint) {
    my atomicint $ = nqp::atomicinc_i($target) + 1
}

#-- post-decrement a native int atomically
sub atomic-postfix-dec(atomicint $target is rw --> atomicint) {
    nqp::atomicdec_i($target)
}

sub postfix:<⚛-->(atomicint $target is rw --> atomicint) {
    nqp::atomicdec_i($target)
}

#-- pre-decrement a native int atomically
sub atomic-prefix-dec(atomicint $target is rw --> atomicint) {
    my atomicint $ = nqp::atomicdec_i($target) - 1
}
sub prefix:<--⚛>(atomicint $target is rw --> atomicint) {
    my atomicint $ = nqp::atomicdec_i($target) - 1
}

#-- add to a native int atomically, return result before
proto sub atomic-postfix-add($, $) {*}
multi sub atomic-postfix-add(atomicint $target is rw, int $add --> atomicint) {
    nqp::atomicadd_i($target, $add)
}
multi sub atomic-postfix-add(atomicint $target is rw, Int $add --> atomicint) {
    nqp::atomicadd_i($target, $add)
}
multi sub atomic-postfix-add(atomicint $target is rw, $add --> atomicint) {
    nqp::atomicadd_i($target, $add.Int)
}

#-- add to a native int atomically, return result after
proto sub atomic-prefix-add($, $) {*}
multi sub atomic-prefix-add(atomicint $target is rw, int $add --> atomicint) {
    my atomicint $ = nqp::atomicadd_i($target, $add) + $add
}
multi sub atomic-prefix-add(atomicint $target is rw, Int $add --> atomicint) {
    my atomicint $ = nqp::atomicadd_i($target, $add) + $add
}
multi sub atomic-prefix-add(atomicint $target is rw, $add --> atomicint) {
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

#-- subtract from a native int atomically, return result before
proto sub atomic-postfix-sub($, $) {*}
multi sub atomic-postfix-sub(atomicint $target is rw, int $add --> atomicint) {
    nqp::atomicadd_i($target, nqp::neg_i($add))
}
multi sub atomic-postfix-sub(atomicint $target is rw, Int $add --> atomicint) {
    nqp::atomicadd_i($target, nqp::neg_i($add))
}
multi sub atomic-postfix-sub(atomicint $target is rw, $add --> atomicint) {
    nqp::atomicadd_i($target, nqp::neg_i($add.Int))
}

#-- subtract from a native int atomically, return result after
proto sub atomic-prefix-sub($, $) {*}
multi sub atomic-prefix-sub(atomicint $target is rw, int $add --> atomicint) {
    my atomicint $ = nqp::atomicadd_i($target, nqp::neg_i($add)) - $add
}
multi sub atomic-prefix-sub(atomicint $target is rw, Int $add --> atomicint) {
    my atomicint $ = nqp::atomicadd_i($target, nqp::neg_i($add)) - $add
}
multi sub atomic-prefix-sub(atomicint $target is rw, $add --> atomicint) {
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
