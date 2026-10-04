use MONKEY-SEE-NO-EVAL;
use Test;

plan 22;

role R[::T] { method type { T } }
class C { }
constant K = Str;

is (try EVAL q{R[Str].type}), Str,
  'an outer parametric role can be parameterized in an EVAL';
is (try EVAL q{C(Int)}).^name, 'C(Int)',
  'an outer class followed by a type in parentheses in an EVAL is a coercion type';
is (try EVAL q{Str(C)}).^name, 'Str(C)',
  'an outer class in parentheses after a type in an EVAL is the coercion constraint';
is (try EVAL q{C:D}).^name, 'C:D',
  'an outer class with a :D smiley in an EVAL is a definite type';
is (try EVAL q{C:U}).^name, 'C:U',
  'an outer class with a :U smiley in an EVAL is an undefined type';
is (try EVAL q{K:D}).^name, 'Str:D',
  'an outer constant holding a type takes a smiley in an EVAL';

is (try EVAL q{my Array[C] $x; $x.WHAT.^name}), 'Array[C]',
  'an outer class is a type argument of a variable type in an EVAL';
is (try EVAL q{my R[C] $x; $x.WHAT.^name}), 'R[C]',
  'an outer class is a type argument of an outer role in a variable type in an EVAL';
is (try EVAL q{my Array[Array[C]] $x; $x.WHAT.^name}), 'Array[Array[C]]',
  'an outer class is a type argument of a nested parameterization in an EVAL';
is (try EVAL q{sub f(Array[C] $a) { $a.WHAT.^name }; f(Array[C].new)}), 'Array[C]',
  'an outer class is a type argument of a parameter type in an EVAL';
is (try EVAL q{my role V[$v] { method v { $v } }; my V[C::] $x; $x.v.^name}), 'Stash',
  'an outer class with a trailing :: is its stash as a type argument in an EVAL';

ok (try EVAL q{EXPORT}) =:= EXPORT,
  'a bare outer name in an EVAL is the lexical around the EVAL';
is (try EVAL q{BEGIN { C; EVAL(q{C}).^name }}), 'C',
  'an outer class named bare in BEGIN-time code of an EVAL is visible to an EVAL there';

{
    my class L { }
    is (try EVAL q{L:D}).^name, 'L:D',
      'a class from an enclosing block takes a smiley in an EVAL';
}

sub capture-definite(::T $) { try EVAL q{T:D} }
is capture-definite(42).^name, 'Int:D',
  'an outer type capture takes a smiley in an EVAL';

role P[::T] { method coercion { try EVAL q{T(Int)} } }
is P[Str].coercion.^name, 'Str(Int)',
  'a role type parameter followed by a type in parentheses in an EVAL is a coercion type';

my \bound-type = Int;
is (try EVAL q{bound-type:D}).^name, 'Int:D',
  'an outer sigilless variable bound to a type takes a smiley in an EVAL';

my \values = (1, 2, 3);
is (try EVAL q{values[1]}), 2,
  'an outer sigilless variable holding a list can be indexed in an EVAL';

constant N = (1, 2, 3);
is (try EVAL q{N[1]}), 2,
  'an outer constant holding a list can be indexed in an EVAL';

sub index-native(int \x) { try EVAL q{x[0]} }
is index-native(5), 5,
  'an outer int sigilless parameter in an EVAL is a term, not a type';

sub refer-sized-native(int32 \x) { try EVAL q{sub g { x }; 'compiled'} }
is refer-sized-native(5), 'compiled',
  'an outer int32 sigilless parameter in an EVAL is a term, not a type';

sub refer-unsigned-native(uint8 \x) { try EVAL q{sub g { x }; 'compiled'} }
is refer-unsigned-native(5), 'compiled',
  'an outer uint8 sigilless parameter in an EVAL is a term, not a type';
