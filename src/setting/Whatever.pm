class Whatever is also {
    method perl() {
        return '*';
    }
    method ACCEPTS(Any $topic) {
        return Bool::True;
    }
    method postcircumfix:<( )>(*@pos, *%named) {
        return -> $x { $x(|@pos, |%named) };
    }
}

# vim: ft=perl6
