my class Match is Capture {
    has $.orig;
    has $.from;
    has $.to;
    has $.CURSOR;

    multi method gist(Match:D:) {
        $!to > $!from ?? $!orig.substr($!from, $!to-$!from) !! ''
    }
    multi method Str(Match:D:) {
        $!to > $!from ?? $!orig.substr($!from, $!to-$!from) !! ''
    }
    multi method Numeric(Match:D:) {
        self.Str.Numeric
    }
    multi method Bool(Match:D:) {
        $!to >= $!from
    }
    multi method ACCEPTS(Match:D: Any $) { self }
}
