use Test;
plan 11;

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

#= yellow
sub marine {}
is &marine.WHY, 'yellow';

#= pink
sub panther {}
is &panther.WHY, 'pink';

#= a sheep
class Sheep {
    #= usually white
    has $.wool;

    #= not too scary
    method roar { 'roar!' }
}

is Sheep.WHY, 'a sheep';
skip 'segfault', 1;
#is Sheep.^attributes.grep({ .name eq '$!wool' }).WHY, 'usually white';
is Sheep.^find_method('roar').WHY, 'not too scary';
