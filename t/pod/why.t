use Test;
plan 12;

#= simple case
class Simple {
}

is Simple.WHY.content, 'simple case';

#= giraffe
class Outer {
    #= zebra
    class Inner {
    }
}

is Outer.WHY.content, 'giraffe';
is Outer::Inner.WHY.content, 'zebra';

#= a module
module foo {
    #= a package
    package bar {
        #= and a class
        class baz {
        }
    }
}

is foo.WHY.content,           'a module';
is foo::bar.WHY.content,      'a package';
is foo::bar::baz.WHY.content, 'and a class';

#= yellow
sub marine {}
is &marine.WHY.content, 'yellow';

#= pink
sub panther {}
is &panther.WHY.content, 'pink';

#= a sheep
class Sheep {
    #= usually white
    has $.wool;

    #= not too scary
    method roar { 'roar!' }
}

is Sheep.WHY.content, 'a sheep';
skip 'segfault', 1;
#is Sheep.^attributes.grep({ .name eq '$!wool' }).WHY, 'usually white';
is Sheep.^find_method('roar').WHY.content, 'not too scary';

sub routine {}
is &routine.WHY.defined, False;
