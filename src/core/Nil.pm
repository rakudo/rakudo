my class Nil is Cool { # declared in BOOTSTRAP
    method new(*@) { Nil }
    method gist(*@) { 'Nil' }
    method Str(*@) { '' }       # XXX still needs warning
    method sink(*@) { Nil }     # required by RESTRICTED setting

    method AT-POS(*@)     { Nil }
    method AT-KEY(*@)     { Nil }
#    method ACCEPTS(*@)    { Nil }  # XXX spec says Nil, but makes spectest hang

    method BIND-POS(*@)   { die "Attempted to BIND-POS to Nil." }
    method BIND-KEY(*@)   { fail X::Bind.new(target => 'Nil') }
    method ASSIGN-POS(*@) { die "Attempted to ASSIGN-POS to Nil." }
    method ASSIGN-KEY(*@) { die "Attempted to ASSIGN-KEY to Nil." }
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

my class Empty is Nil {
    # class Empty is Iterator
    method new(*@) { Empty }
    method Str() { '' }
    method gist(*@) { 'Empty' }
    method iterator(*@) { self }
    method reify($n) { () }
}

# vim: ft=perl6 expandtab sw=4
