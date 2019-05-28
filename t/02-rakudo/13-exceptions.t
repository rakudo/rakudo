use Test;

plan 1;

# GH #2739
# Prior to the fix the original exception would be lost hidden under 'no handler found' error.
throws-like q<sub foo ( ::T $val ) { my T $a is default($val); }; foo(42)>,
        X::Parameter::Default::TypeCheck,
        "exception isn't lost",
        message => q<Default value '(Mu)' will never bind to a parameter of type T>;

done-testing;
