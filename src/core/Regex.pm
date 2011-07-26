my class Regex {
    multi method ACCEPTS(Regex:D \$self: Mu \$topic) {
        pir::find_caller_lex__Ps('$/')
            = $self(Cursor."!cursor_init"($topic, :c(0))).MATCH;
    }
}
