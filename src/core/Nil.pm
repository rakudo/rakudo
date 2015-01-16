my class Nil is Cool { # declared in BOOTSTRAP
    # class Nil is Iterator {

    method new(*@) { Nil }
    method iterator(*@) { self }
    method reify($n) { () }
    method gist(*@) { 'Nil' }
    method Str(*@) { '' }       # XXX still needs warning
    method sink(*@) { Nil }     # required by RESTRICTED setting

    method at_pos(*@)     { Nil }
    method at_key(*@)     { Nil }
#    method ACCEPTS(*@)    { Nil }  # XXX spec says Nil, but makes spectest hang

    method bind_pos(*@)   { die "Attempted to bind_pos to Nil." }
    method bind_key(*@)   { die "Attempted to bind_key to Nil." }
    method assign_pos(*@) { die "Attempted to assign_pos to Nil." }
    method assign_key(*@) { die "Attempted to assign_key to Nil." }
    method STORE(*@)      { die "Attempted to STORE to Nil." }
    method push(*@)       { die "Attempted to push to Nil." }
    method unshift(*@)    { die "Attempted to unshift to Nil." }
}

Nil.^add_fallback(
    -> $, $name { True },
    -> $, $name {
        anon sub (|) { Nil }
    }
);

# vim: ft=perl6 expandtab sw=4
