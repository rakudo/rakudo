my class Match is Capture {
    has $.orig;
    has $.from;
    has $.to;
    has $.CURSOR;

    multi method Str(Match:D:) {
        $!to > $!from ?? $!orig.substr($!from, $!to-$!from) !! '';
    }
}
