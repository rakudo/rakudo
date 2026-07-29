use Test;

plan 6;

# BUILDALL calls an invokable build value to compute an attribute's
# default, so a default expression whose compile-time value is a code
# object must be stored as a value instead of being used as the build
# directly. Humming-Bird declares `has @!advice = ( { $^a } );` and
# construction died with "Too many positionals passed; expected 1
# argument but got 2".

my class ListOfBlock {
    has @!advice = ( { $^a } );
    method advice() { @!advice }
}
is ListOfBlock.new.advice.elems, 1,
    'a list default holding a block constructs and keeps one element';
is ListOfBlock.new.advice[0]('kept'), 'kept',
    'the stored block is callable with its own arity';

my class PublicListOfBlock {
    has @.advice = ( { 42 } );
}
is PublicListOfBlock.new.advice[0](), 42,
    'a public list attribute stores a zero-arity block as a value';

my class ScalarSub {
    has $!formatter = &uc;
    method formatter() { $!formatter }
}
is ScalarSub.new.formatter.('abc'), 'ABC',
    'a scalar default naming a setting sub stores the sub itself';

my class BareBlock {
    has $!cb = { $^x ~ '!' };
    method cb() { $!cb }
}
is BareBlock.new.cb.('hi'), 'hi!',
    'a bare block default still stores the block';

my class PlainValue {
    has $.n = 42;
}
is PlainValue.new.n, 42,
    'a plain literal default still initializes directly';
