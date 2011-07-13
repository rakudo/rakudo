my class Regex {
    multi method ACCEPTS(Regex:D \$self: Mu \$topic) {
        $self(Cursor."!cursor_init"($topic)).MATCH;
    }
}
