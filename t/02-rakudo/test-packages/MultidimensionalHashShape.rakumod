class MultidimensionalHashShape {
    has %!bind{Str:D; Str:D; Str:D};

    method add(\a, \b, \c, \value) {
        %!bind{a; b; c} = value
    }
    method find(Str:D $a, Str:D $b) {
        %!bind{$a; $b}:exists ?? %!bind{$a; $b}.first.values !! Empty
    }
    method level(Str:D $a) { %!bind{$a} }
    method typed() { my Int %h{Str;Int} = a => :{ 1 => 42 }; %h }
}

role MultidimensionalHashShapeGeneric[::T] is export {
    has T %.stored{Str;Str};
    has T:D %.definite{Str;Str()};
}
