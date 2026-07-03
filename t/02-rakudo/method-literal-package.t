use Test;

plan 4;

# A `method { }` literal records the package it is written in, the same as a
# sub does. Getting this wrong makes the setting's Mu.raku mistake a type's
# own `.gist`/`.perl` (added via add_method) for the default one.

module Outer {
    our $m = method { 42 };
}
is Outer::<$m>.package, Outer, 'a method literal records its enclosing package';

my $anon = anon method ($x) { $x };
is $anon.package, GLOBAL, 'a top-level anon method records GLOBAL';

# A type with a custom `.gist` installed via add_method round-trips through
# gist/raku instead of crashing in Mu.raku.
my $type := Metamodel::ClassHOW.new_type(name => 'Boxed');
$type.HOW.add_attribute($type, Attribute.new(
    :name('$.inner'), :type(Any), :has_accessor(1), :package($type)));
$type.HOW.add_method($type, 'gist', method { "Boxed:" ~ self.inner.gist });
$type.HOW.compose($type);
my $outer := $type.new(inner => $type.new(inner => 7));
is $outer.gist, 'Boxed:Boxed:7', 'a custom gist added via add_method works when nested';
is $outer.inner.raku, 'Boxed.new(inner => 7)', 'the default .raku still works on the inner value';
