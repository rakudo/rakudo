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

is foo.WHY,           'a module';
is foo::bar.WHY,      'a package';
is foo::bar::baz.WHY, 'and a class';
