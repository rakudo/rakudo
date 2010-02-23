augment class Regex {
    method ACCEPTS($topic) {
        my $match = $topic.match(self);
        pir::store_dynamic_lex__vSP('$/', $match);
        $match
    }
}
