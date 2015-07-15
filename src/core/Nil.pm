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
    method push(*@)       { die "Attempted to push to Nil." }
    method unshift(*@)    { die "Attempted to unshift to Nil." }
    method FALLBACK(*@)   { Nil }
}

# Like Nil, but allows a failsoft to nothingness when used as list.
#
# (Please avoid overusing Empty as a synonym for ().  If the degenerate
# case of a list operation is naturally (), use () as a normal defined
# value.  Empty is primarily intended for use in list comprehensions,
# which need to weed out unselected values with an implicit "else".)
my class Empty is Nil {
    # class Empty is Iterator
    method new(*@) { Empty }
    multi method Bool() { False }
    multi method Int() { 0 }
    multi method end() { -1 }
    multi method Numeric() { 0 }
    method Str() { '' }
    method gist(*@) { 'Empty' }
    method iterator(*@) { self }
    method reify($n) { () }
}

# vim: ft=perl6 expandtab sw=4
