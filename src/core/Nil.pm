my class Nil is Cool { # declared in BOOTSTRAP
    method new(*@) { Nil }
    method gist(*@) { 'Nil' }
    method Numeric() { warn "Use of Nil in numeric context"; 0 }
    method Str() { warn "Use of Nil in string context"; '' }
    method sink(*@) { Nil }     # required by RESTRICTED setting

    method AT-POS(*@)     { Nil }
    method AT-KEY(*@)     { Nil }
#    method ACCEPTS(*@)    { Nil }  # XXX spec says Nil, but makes spectest hang

    method BIND-POS(*@)   { die "Attempted to BIND-POS to Nil." }
    method BIND-KEY(*@)   { fail X::Bind.new(target => 'Nil') }
    method ASSIGN-POS(*@) { die "Attempted to ASSIGN-POS to Nil." }
    method ASSIGN-KEY(*@) { die "Attempted to ASSIGN-KEY to Nil." }
    method STORE(*@)      { die "Attempted to STORE to Nil." }
    method push(*@)    is nodal { die "Attempted to push to Nil." }
    method append(*@)  is nodal { die "Attempted to append to Nil." }
    method unshift(*@) is nodal { die "Attempted to unshift to Nil." }
    method prepend(*@) is nodal { die "Attempted to prepend to Nil." }
    method FALLBACK(*@)   { Nil }
}

# vim: ft=perl6 expandtab sw=4
