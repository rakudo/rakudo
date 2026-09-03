use MONKEY-SEE-NO-EVAL;
use Test;
use nqp;

# A ::? name such as ::?CLASS is declared by the compiler on entering the
# package it names. One that nothing declares is a compile-time error,
# not a lookup that comes back empty at runtime.

my $rakuast = nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

plan 7;

throws-like { EVAL 'say ::?FOO' }, X::NoSuchSymbol, symbol => '::?FOO',
  'an undeclared ::? name as a term is a compile-time error';

throws-like { EVAL 'say ::?FOO.^name' }, X::NoSuchSymbol, symbol => '::?FOO',
  'an undeclared ::? name as a method invocant is a compile-time error';

throws-like { EVAL 'say ::?CLASS' }, X::NoSuchSymbol, symbol => '::?CLASS',
  '::?CLASS outside any class is a compile-time error';

todo 'the legacy frontend reports a missing compile-time value instead', 2
  unless $rakuast;
throws-like { EVAL 'my ::?FOO $x' }, X::NoSuchSymbol, symbol => '::?FOO',
  line => 1,
  'an undeclared ::? name as a variable type is a compile-time error with a position';

throws-like { EVAL "\nsub f(::?FOO \$x) \{ }" }, X::NoSuchSymbol, symbol => '::?FOO',
  line => 2,
  'an undeclared ::? name as a parameter type is a compile-time error with a position';

# The names the compiler does declare keep resolving.
is-deeply EVAL('module M { class C { method m { ::?PACKAGE, ::?CLASS, ::?MODULE } } }; M::C.m').map(*.^name).List,
  ('M::C', 'M::C', 'M'), '::?PACKAGE, ::?CLASS and ::?MODULE resolve inside a class in a module';

is-deeply EVAL('role R { method m { ::?ROLE, ::?CLASS } }; class D does R { }; D.m').map(*.^name).List,
  ('R', 'D'), '::?ROLE and ::?CLASS resolve inside a role method';

# vim: expandtab shiftwidth=4
