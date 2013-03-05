my class Nil is Iterator is Cool {
    method new() { Nil }
    method iterator() { self }
    method reify($n?) { () }
    method gist() { 'Nil' }
    method perl() { 'Nil' }
    multi method Str() { '' }
    method ACCEPTS(|) { Nil }
    method Bool(|) { False }
}


