use Test;

# A hash shape names the key type and must be one type object known at
# compile time.

plan 42;

use MONKEY-SEE-NO-EVAL;

my class HashShapeOuter { }
class HashShapeOurOuter { }
my $hash-shape-container = HashShapeOuter;
my \HashShapeBound = $hash-shape-container;

sub invalid-shape($code, $desc) {
    throws-like { EVAL $code }, X::Comp::AdHoc,
        message => 'Invalid hash shape; type expected', $desc;
}

invalid-shape 'my %h{}',
    'an empty hash shape is refused';
invalid-shape 'my %h{ }',
    'a hash shape of only whitespace is refused';
invalid-shape 'my %h{;}',
    'a hash shape of an empty statement is refused';
invalid-shape 'my %h{1}',
    'a hash shape of a literal value is refused';
invalid-shape 'my %h{"a"}',
    'a hash shape of a string is refused';
invalid-shape 'my $x = Int; my %h{$x}',
    'a hash shape of a variable is refused';
invalid-shape 'my %h{Int|Str}',
    'a hash shape of a junction expression is refused';
invalid-shape 'my %h{Str,Int}',
    'a hash shape of a list is refused';
invalid-shape 'constant K = 1; my %h{K}',
    'a hash shape of a constant holding a value is refused';
invalid-shape 'my %h{Int if False}',
    'a hash shape with a condition modifier is refused';
invalid-shape 'my %h{Int for 1, 2}',
    'a hash shape with a loop modifier is refused';
invalid-shape 'my %h{enum <HashShapeA HashShapeB>}',
    'a hash shape of an enum declaration is refused';
invalid-shape 'my %h{class HashShapeDeclared { }}',
    'a hash shape of a class declaration is refused';
invalid-shape 'my %h{(class HashShapeParenthesized { })}',
    'a hash shape of a class declaration in parentheses is refused';
invalid-shape 'my %h{my Int $x}',
    'a hash shape of a variable declaration is refused';
invalid-shape 'my %h{if True { Int }}',
    'a hash shape of a control statement is refused';
invalid-shape 'my %h{use Test}',
    'a hash shape of a use statement is refused';
invalid-shape 'my %h{FOO: Int}',
    'a hash shape of a labeled statement is refused';
invalid-shape 'sub f(\t) { my %h{t} }',
    'a hash shape of a sigilless parameter is refused';
invalid-shape 'my $c = Int; constant K = $c; my %h{K}',
    'a hash shape of a constant holding a container is refused';
invalid-shape 'my %h{HashShapeBound}',
    'a hash shape of a name from outside the EVAL bound to a container is refused';
invalid-shape 'my %h{HashShapeOuter::}',
    'a hash shape of a stash from outside the EVAL is refused';
invalid-shape 'my Int %h{}',
    'an empty hash shape is refused with a value type';
invalid-shape 'my %{}',
    'an empty hash shape is refused on an anonymous hash';
invalid-shape 'state %h{}',
    'an empty hash shape is refused on a state hash';
invalid-shape 'class { has %.h{} }',
    'an empty hash shape is refused on an attribute';

throws-like { EVAL 'my %h{Str;Int}' }, X::Comp::NYI,
    feature => 'multidimensional shaped hashes',
    'a hash shape of two types is not yet implemented';

try EVAL 'my %h{Srt}';
isa-ok ($! ~~ X::Comp::Group ?? $!.panic !! $!), X::Undeclared::Symbols,
    'a misspelled key type is still reported as undeclared';

throws-like { EVAL 'my %h{::?CLASS}' }, Exception,
    message => /"No such symbol '::?CLASS'"/,
    'a hash shape of ::?CLASS outside a class is reported as undeclared';

is (try EVAL 'my %h{Int;}; %h.keyof.^name'), 'Int',
    'a hash shape with a trailing semicolon keys by its type';
is (try EVAL 'constant K = Int; my %h{K}; %h.keyof.^name'), 'Int',
    'a hash shape of a constant holding a type keys by that type';
is (try EVAL 'my %h{Int(Str)}; %h.keyof.^name'), 'Int(Str)',
    'a hash shape of a coercion type keys by that type';
is (try EVAL 'my %h{Array[Int]}; %h.keyof.^name'), 'Array[Int]',
    'a hash shape of a parameterized type keys by that type';
is (try EVAL 'my %h{subset HashShapeBlock of Int where { my $n = $_; $n > 0 }}; %h.keyof.^name'),
    'HashShapeBlock',
    'a variable declared in a block in the shape does not refuse it';
is (try EVAL 'my %h{subset HashShapeWhere of Int where * > (my $z = 0)}; %h.keyof.^name'),
    'HashShapeWhere',
    'a variable declared in a subset in the shape does not refuse it';
is (try EVAL 'my %h{constant HashShapeConstant = (anon class HashShapeAnon { })}; %h.keyof.^name'),
    'HashShapeAnon',
    'a class declared in a constant in the shape does not refuse it';
is (try EVAL 'role R[::T] { has %.h{T} }; R[Int].new.h.keyof.^name'), 'Int',
    'a hash shape of a role type parameter keys by the type it takes';
is (try EVAL 'class HashShapeSelf { has %.h{::?CLASS} }; HashShapeSelf.new.h.keyof.^name'),
    'HashShapeSelf',
    'a hash shape of ::?CLASS keys by the class';
is (try EVAL 'role HashShapeRole { has %.h{::?CLASS} }; class HashShapeDoer does HashShapeRole { }; HashShapeDoer.new.h.keyof.^name'),
    'HashShapeDoer',
    'a hash shape of ::?CLASS in a role keys by the class doing the role';
is (try EVAL 'class HashShapePackage { has %.h{::?PACKAGE} }; HashShapePackage.new.h.keyof.^name'),
    'HashShapePackage',
    'a hash shape of ::?PACKAGE keys by the package';
is (try EVAL 'my %h{HashShapeOuter}; %h.keyof.^name'), 'HashShapeOuter',
    'a hash shape of a type declared outside the EVAL keys by that type';
nok (try EVAL 'my %h{HashShapeOurOuter:D}; %h.keyof') =:= HashShapeOurOuter,
    'a definite type from outside the EVAL never keys by its plain type';

# vim: expandtab shiftwidth=4
