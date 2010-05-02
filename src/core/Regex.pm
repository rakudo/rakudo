augment class Regex {
    method ACCEPTS($topic) {
        my $match = $topic.match(self);
        pir::store_dynamic_lex__vSP('$/', $match);
        $match
    }
    method Bool() {
        my $topic = pir::find_dynamic_lex__pS('$_');
        my $match = $topic.match(self);
        pir::store_dynamic_lex__vSP('$/', $match);
        $match.Bool()
    }
}
