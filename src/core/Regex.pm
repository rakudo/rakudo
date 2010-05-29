augment class Regex {
    multi method ACCEPTS($topic) {
        my $match = $topic.match(self);
        pir::store_dynamic_lex__vSP('$/', $match);
        $match
    }
    multi method ACCEPTS(@topic) {
        my Mu $match = any(@topic).match(self);
        pir::store_dynamic_lex__vSP('$/', $match);
        $match
    }
    multi method ACCEPTS(%topic) {
        my Mu $match = any(%topic.keys).match(self);
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
