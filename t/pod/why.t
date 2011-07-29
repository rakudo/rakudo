use Test;
plan 6;

#= simple case
class Simple {
}

is Simple.WHY, 'simple case';

#= giraffe
class Outer {
    #= zebra
    class Inner {
    }
}

is Outer.WHY, 'giraffe';
is Outer::Inner.WHY, 'zebra';

#= a module
module foo {
    #= a package
    package bar {
        #= and a class
        class baz {
        }
    }
}

# XXX Those should be proper .WHY's, once it's implemented
is foo.HOW.docs,      'a module';
is foo::bar.HOW.docs, 'a package';
is foo::bar::baz.WHY, 'and a class';
