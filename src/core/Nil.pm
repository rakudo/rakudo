my class Nil is Cool { # declared in BOOTSTRAP
    # class Nil is Iterator {

    method new() { Nil }
    method iterator() { self }
    method reify($n?) { () }
    method gist() { 'Nil' }
    multi method Str() { '' }

    method at_pos($pos) {
        self
    }
    method at_key($pos) {
        self
    }
    method bind_pos($pos, $bind) {
        die "Attempted to bind_pos to Nil.";
    }
    method bind_key($pos, $bind) {
        die "Attempted to bind_key to Nil.";
    }
}

Nil.^add_fallback(
    -> $, $name { True },
    -> $, $name {
        anon sub (|) { Nil }
    }
);
