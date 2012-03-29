use Test;
plan 19;

#= simple case
class Simple {
}

is Simple.WHY.content, 'simple case';
is ~Simple.WHY, 'simple case', 'stringifies correctly';

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

#= our works too
our sub oursub {}
is &oursub.WHY, 'our works too', 'works for our subs';

# two subs in a row

#= one
sub one {}

#= two
sub two {}
is &one.WHY.content, 'one';
is &two.WHY.content, 'two';

#= that will break
sub first {}

#= that will break
sub second {}

is &first.WHY.content, 'that will break';
is &second.WHY.content, 'that will break';

#= trailing space here  
sub third {}
is &third.WHY.content, 'trailing space here';
