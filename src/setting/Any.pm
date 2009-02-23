class Any is also {
    method split($delimiter, Int $limit = *) {
        if $limit ~~ Whatever {
            return (~self).split($delimiter);
        } else {
            return (~self).split($delimiter, $limit);
        }
    }
}

# vim: ft=perl6
