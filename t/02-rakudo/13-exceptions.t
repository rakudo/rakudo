use Test;

plan 5;

# GH #2739
# Prior to the fix the original exception would be lost hidden under 'no handler found' error.
throws-like q<sub foo ( ::T $val ) { my T $a is default($val); }; foo(42)>,
        X::Parameter::Default::TypeCheck,
        "exception isn't lost",
        message => q<Default value '(Mu)' will never bind to a parameter of type T>;

# https://github.com/Raku/old-issue-tracker/issues/5728
throws-like q[multi sub f(int $foo is rw) { }; f(42)],
        X::Comp,
        'calling multi sub that expects a rw native argument with a literal is caught at compile time';

throws-like q[multi sub f(Int $foo is rw) { }; f(42)],
        X::Comp,
        'calling multi sub that expects a rw non-native argument with a literal is caught at compile time';

throws-like q:to/CODE/,
                my \parent := Metamodel::ClassHOW.new_type(:name('Parent'));
                my \child := Metamodel::ClassHOW.new_type(:name('Child'));
                child.^add_parent(parent);
                child.^compose;
                CODE
        X::Inheritance::NotComposed,
        'inheriting from uncomposed parent';

throws-like q[role R {}; my $foo = 42 but R(1)],
        X::Role::Initialization,
        'passing an initializer when role cannot accept it';

done-testing;

# vim: expandtab shiftwidth=4
