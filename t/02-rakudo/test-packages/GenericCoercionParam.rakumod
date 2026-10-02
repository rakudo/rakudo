role GenericCoercionParam[::T] {
    has T $.value;
    submethod BUILD(T() :$value) { $!value = $value }
    method positional(T() $v) { $v }
    method named(T() :$v) { $v }
    method optional(T() $v = "7") { $v }
    method definite(T:D() $v) { $v }
    method constrained(T(Str) $v) { $v }
    method parameterized(Array[T]() $v) { $v }
    method with-sibling($a, T() $v) { ($a, $v, self.^name) }
    multi method multi(T() $v) { $v }
    method in-sub($x) { my sub f(T() $v) { $v }; f($x) }
    method in-pointy($x) { (-> T() $v { $v })($x) }
}
