augment class Pair {
    multi method perl() {
        $.key.perl ~ ' => ' ~ $.value.perl;
    }
}

multi sub infix:<cmp>(Pair $a, Pair $b) {
    ($a.key cmp $b.key) || ($a.value cmp $b.value);
}
