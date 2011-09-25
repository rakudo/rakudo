my class Regex {
    multi method ACCEPTS(Regex:D \$self: Mu \$topic) {
        pir::find_caller_lex__Ps('$/')
            = $self(Cursor."!cursor_init"($topic, :c(0))).MATCH;
    }
    
    multi method Bool(Regex:D:) {
        my $match = pir::find_caller_lex__pS('$_').match(self);
        pir::find_caller_lex__Ps('$/') = $match;
        $match.Bool()
    }
}
