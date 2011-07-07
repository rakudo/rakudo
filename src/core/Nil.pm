my class Nil is Iterator {
    method new() { Nil }
    method iterator() { self }
    method reify($n?) { () }
    method gist() { 'Nil' }
    multi method Str() { '' }
}


