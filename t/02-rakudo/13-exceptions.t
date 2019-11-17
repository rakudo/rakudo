use Test;

plan 3;

# GH #2739
# Prior to the fix the original exception would be lost hidden under 'no handler found' error.
throws-like q<sub foo ( ::T $val ) { my T $a is default($val); }; foo(42)>,
        X::Parameter::Default::TypeCheck,
        "exception isn't lost",
        message => q<Default value '(Mu)' will never bind to a parameter of type T>;

# RT #129812
throws-like q[multi sub f(int $foo is rw) { }; f(42)],
        X::Comp,
        'calling multi sub that expects a rw native argument with a literal is caught at compile time';

throws-like q[multi sub f(Int $foo is rw) { }; f(42)],
        X::Comp,
        'calling multi sub that expects a rw non-native argument with a literal is caught at compile time';

done-testing;
