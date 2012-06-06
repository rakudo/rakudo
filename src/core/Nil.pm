my class Nil {
    method new() { Nil }
    method iterator() { self }
    method reify($n?) { () }
    method gist() { 'Nil' }
    multi method Str() { '' }
}


