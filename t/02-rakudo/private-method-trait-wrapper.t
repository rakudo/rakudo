use Test;

plan 2;

# A trait applied at BEGIN time wraps a method with a closure that is compiled
# before its class has composed. A private method call in that closure cannot be
# resolved at compile time, so it falls back to the runtime dispatch rather than
# erroring. LibXML's `is reader-raw` trait wraps methods this way.
class C {
    method !double($n) { $n * 2 }
    multi trait_mod:<is>(Method:D $m, :$doubling!) {
        $m.wrap: method { self!double(21) };
    }
    method answer() is doubling {...}
}
is C.new.answer, 42, 'a trait wrapper resolves the class private method at runtime';

# A private-method typo in a fully composed class is still a compile-time error.
throws-like 'class D { method !real() {}; method go() { self!bogus() } }',
    X::Comp, 'a private-method typo in a composed class still fails to compile';
