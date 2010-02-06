augment class Pair {
    multi method perl() {
        # $.key.perl ~ ' => ' ~ $.value.perl;
        "Pair.new(:key({$.key.perl}), :value({$.value.perl}))";
    }
}
