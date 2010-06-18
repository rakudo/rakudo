augment class Pair {
    multi method perl() {
        $.key.perl ~ ' => ' ~ $.value.perl;
    }

    method keys() {
        [self.key];
    }

    method values() {
        [self.value];
    }

    multi method invert() {
        $.value => $.key;
    }
}

multi sub infix:<cmp>(Pair $a, Pair $b) {
    ($a.key cmp $b.key) || ($a.value cmp $b.value);
}
