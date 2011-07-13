my class Match #`(is Capture) { 
    has $.orig;
    has $.from;
    has $.to;
    has $.list;

    multi method Str(Match:D:) { 
        $!to > $!from ?? $!orig.substr($!from, $!to-$!from) !! ''; 
    }
}
