my class Nil is Iterator is Cool {
    method new() { Nil }
    method iterator() { self }
    method reify($n?) { () }
    method gist() { 'Nil' }
    multi method Str() { '' }
}


