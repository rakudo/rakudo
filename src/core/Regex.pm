my class Regex {
    multi method ACCEPTS(Regex:D \$self: Mu \$topic) {
        pir::find_caller_lex__Ps('$/')
            = $self(Cursor."!cursor_init"($topic, :c(0))).MATCH;
    }

    multi method ACCEPTS(Regex:D \$self: @a) {
        my $dollar_slash := pir::find_caller_lex__Ps('$/');
        for @a {
            $dollar_slash = $self(Cursor.'!cursor_init'($_, :c(0))).MATCH;
            return $dollar_slash if $dollar_slash;
        }
        Nil;
    }
    multi method ACCEPTS(Regex:D \$self: %h) {
        my $dollar_slash := pir::find_caller_lex__Ps('$/');
        for %h.keys {
            $dollar_slash = $self(Cursor.'!cursor_init'($_, :c(0))).MATCH;
            return $dollar_slash if $dollar_slash;
        }
        Nil;
    }

    multi method Bool(Regex:D:) {
        my $match = pir::find_caller_lex__pS('$_').match(self);
        pir::find_caller_lex__Ps('$/') = $match;
        $match.Bool()
    }
}
