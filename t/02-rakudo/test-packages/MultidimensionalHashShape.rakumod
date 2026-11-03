class MultidimensionalHashShape {
    has %!bind{Str:D; Str:D; Str:D};

    method add(\a, \b, \c, \value) {
        %!bind{a; b; c} = value
    }
    method get(\a, \b, \c) { %!bind{a; b; c} }
    method keys() { %!bind.keys }
    method typed() { my Int %h{Str;Int} = ('a', 1) => 42; %h }
    method type-object() { my Hash[Int,(Str,Int)] $h; $h }
}

role MultidimensionalHashShapeGeneric[::T] is export {
    has T %.stored{Str;T};
    has T:D %.definite{Str;Str};
}
