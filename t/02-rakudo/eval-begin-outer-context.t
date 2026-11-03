use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;
use nqp;

plan 21;

my class Outer { }
my $outer-value = 42;
my $shadowed = 'outer';
my int $native-outer = 5;
my \sigilless-outer = 'outer';
my constant UNIT-K = 5;
sub eval-with-slurpy(*@_) { EVAL q{BEGIN EVAL q{sub s { @_.elems }; s(1, 2, 3)}} }
sub eval-in-routine(*@_) { EVAL q{sub s { @_.elems }; s(1, 2, 3)} }
sub eval-in-routine-named(*%_) { EVAL q{sub s { %_.elems }; s(:a, :b)} }
sub eval-mainline-slurpy(*@_) { EVAL q{@_.elems} }
sub eval-in-caller($code) { EVAL $code, :context(CALLER::) }
sub begin-eval-ast(Str $code) {
    RakuAST::StatementList.new: RakuAST::Statement::Expression.new:
      expression => RakuAST::StatementPrefix::Phaser::Begin.new:
        RakuAST::Statement::Expression.new: expression => RakuAST::Call::Name.new:
          name => RakuAST::Name.from-identifier('EVAL'),
          args => RakuAST::ArgList.new(RakuAST::StrLiteral.new($code))
}
sub begin-indirect-ast(Str $name) {
    RakuAST::StatementList.new: RakuAST::Statement::Expression.new:
      expression => RakuAST::StatementPrefix::Phaser::Begin.new:
        RakuAST::Statement::Expression.new: expression => RakuAST::Term::Name.new:
          RakuAST::Name.new(RakuAST::Name::Part::Expression.new(RakuAST::StrLiteral.new($name)))
}

is (try EVAL q{BEGIN EVAL(q{Outer}).^name}), 'Outer',
  'an EVAL at BEGIN time of an EVAL sees a type from around the outer EVAL';
is (try EVAL q{BEGIN EVAL(q{$outer-value})}), 42,
  'an EVAL at BEGIN time of an EVAL sees a variable from around the outer EVAL';
is (try EVAL q{BEGIN ::('Outer').^name}), 'Outer',
  'an indirect lookup at BEGIN time of an EVAL finds a type from around it';
is (try EVAL q{my $shadowed is default('inner'); BEGIN ::('$shadowed')}), 'inner',
  'an indirect lookup at BEGIN time of an EVAL finds what the EVAL declares first';
is (try EVAL q{my $shadowed is default('inner'); BEGIN EVAL(q{::('$shadowed')})}), 'inner',
  'an EVAL at BEGIN time of an EVAL finds what the outer EVAL declares first';
isnt (try EVAL q{my \sigilless-outer = 1; BEGIN ::('sigilless-outer')}), 'outer',
  'an indirect lookup at BEGIN time of an EVAL stops at what the EVAL declares';
todo 'the legacy frontend finds a native from around the EVAL'
  unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
is (try EVAL q{BEGIN (::('$native-outer') // 'not a constant')}), 'not a constant',
  'an indirect lookup at BEGIN time of an EVAL finds no native from around it';
is (BEGIN try EVAL q{my constant L = 1; my role R[$x] { my $v = EVAL q{L}; method m { $v } }; my class C does R[1] { }; C.m}),
  1,
  'a role composed in an EVAL at BEGIN time sees what the EVAL declares';
is (BEGIN try EVAL q{my &c = BEGIN sub { EVAL q{UNIT-K} }; c()}), 5,
  'a closure made at BEGIN time of a BEGIN-time EVAL sees the unit around it';
if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    is EVAL(q{BEGIN eval-in-caller(q{UNIT-K})}), 5,
      'an EVAL with the CALLER:: context from a sub called at BEGIN time of an EVAL sees the unit';
}
else {
    skip 'the legacy frontend cannot EVAL with the CALLER:: context of a BEGIN';
}
throws-like { EVAL begin-eval-ast('$outer-value') }, X::Comp::BeginTime,
  'an EVAL at BEGIN time of an AST EVAL fails cleanly on a variable around it';
is (try EVAL begin-indirect-ast('Outer')).^name, 'Outer',
  'an indirect lookup at BEGIN time of an AST EVAL finds a type from around it';
is (try eval-with-slurpy()), 3,
  'a sub in an EVAL at BEGIN time of an EVAL in a routine has its own @_';
is (try eval-in-routine()), 3,
  'a sub in an EVAL in a routine has its own @_';
is (try EVAL q{BEGIN EVAL(q{Outer:D}).^name}), 'Outer:D',
  'type syntax in an EVAL at BEGIN time of an EVAL works on a type from around both';
is (try EVAL q{BEGIN EVAL q{BEGIN EVAL q{Outer.^name}}}), 'Outer',
  'an EVAL at BEGIN time three EVALs deep sees a type from around them all';
todo 'the legacy frontend finds a native from around both EVALs'
  unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
throws-like { EVAL q{BEGIN EVAL q{$native-outer}} }, X::Comp::BeginTime,
  exception => X::Undeclared,
  'an EVAL at BEGIN time of an EVAL finds no native from around both';
is (try EVAL q{BEGIN LEXICAL::<Outer>.^name}), 'Outer',
  'LEXICAL:: at BEGIN time of an EVAL finds a type from around it';
is (try EVAL q{BEGIN OUTERS::<$outer-value>}), 42,
  'OUTERS:: at BEGIN time of an EVAL finds a variable from around it';
is (try eval-in-routine-named()), 2,
  'a sub in an EVAL in a routine has its own %_';
throws-like { eval-mainline-slurpy(1, 2) }, X::Placeholder::Mainline,
  'the main code of an EVAL in a routine cannot use the @_ of the routine';
