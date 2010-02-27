augment class Pair {
    multi method perl() {
        $.key.perl ~ ' => ' ~ $.value.perl;
    }
}
