my class Regex {
    multi method ACCEPTS(Regex:D \SELF: Mu \topic) {
        my $match = SELF.(Cursor."!cursor_init"(topic, :c(0))).MATCH_SAVE;
        pir::find_caller_lex__Ps('$/') = $match;
        $match
    }

    multi method ACCEPTS(Regex:D \SELF: @a) {
        my $dollar_slash := pir::find_caller_lex__Ps('$/');
        for @a {
            $dollar_slash = SELF.(Cursor.'!cursor_init'($_, :c(0))).MATCH_SAVE;
            return $dollar_slash if $dollar_slash;
        }
        Nil;
    }
    multi method ACCEPTS(Regex:D \SELF: %h) {
        my $dollar_slash := pir::find_caller_lex__Ps('$/');
        for %h.keys {
            $dollar_slash = SELF.(Cursor.'!cursor_init'($_, :c(0))).MATCH_SAVE;
            return $dollar_slash if $dollar_slash;
        }
        Nil;
    }

    multi method Bool(Regex:D:) {
        my $match = pir::find_caller_lex__Ps('$_').match(self);
        pir::find_caller_lex__Ps('$/') = $match;
        $match.Bool()
    }
}
