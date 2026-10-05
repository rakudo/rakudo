use lib <t/02-rakudo/test-packages>;
use MONKEY-SEE-NO-EVAL;
use Test;
use EvalExportProbe;

plan 22;

EVAL q{sub exported-from-eval is export { 42 }};
is (try EXPORT.WHO<DEFAULT>.WHO<&exported-from-eval>()), 42,
  'a sub exported in an EVAL lands in the EXPORT package of the code around it';
ok (EVAL q{EXPORT}) =:= EXPORT,
  'EXPORT in an EVAL is the EXPORT package of the code around it';
ok (BEGIN EVAL q{EXPORT}) =:= EXPORT,
  'EXPORT in a BEGIN-time EVAL is the EXPORT package of the code around it';
is (try EVAL q{sub own-export is export { 1 }; EXPORT::DEFAULT::<&own-export>.name}),
  'own-export',
  'code in an EVAL finds what it exports through EXPORT';
lives-ok { q{sub ast-export is export { }}.AST for ^2 },
  'turning code with an export into an AST twice does not clash';
nok EXPORT.WHO<DEFAULT>.WHO<&ast-export>:exists,
  'code turned into an AST exports nothing from the code around it';

BEGIN try EVAL q{sub exported-at-begin is export { 9 }};
is (try EXPORT.WHO<DEFAULT>.WHO<&exported-at-begin>()), 9,
  'a sub exported in a BEGIN-time EVAL lands in the EXPORT package of the unit';
lives-ok { q{use MONKEY-SEE-NO-EVAL; BEGIN EVAL q{package EXPORT::DEFAULT { our sub from-ast-eval { } }}}.AST },
  'code turned into an AST can run an EVAL at BEGIN time declaring an export package';
nok EXPORT.WHO<DEFAULT>.WHO<&from-ast-eval>:exists,
  'an EVAL inside code turned into an AST exports nothing from the code around it';

ok (BEGIN EvalExportProbe::eval-export()) =:= EvalExportProbe::own-export(),
  'an EVAL that a module runs at BEGIN time of another unit has the EXPORT of the module';
ok (try q{EXPORT}.AST.EVAL) =:= EXPORT,
  'code turned into an AST and then EVALed has the EXPORT package around it';
lives-ok { EVAL q{package EXPORT { package DEFAULT { our sub from-package-export { } } }} },
  'an EVAL can declare a package named EXPORT';
is (try EVAL q{package EXPORT { our sub x { 1 } }; EXPORT::x()}, :context(CORE::)), 1,
  'an EVAL with an EXPORT package of its own can declare a package named EXPORT';
my \ast-begin-export = try q{sub ast-own is export { }; BEGIN EXPORT}.AST.EVAL;
ok ast-begin-export.WHO<DEFAULT>.WHO<&ast-own>:exists,
  'BEGIN-time code of code turned into an AST has the EXPORT package its exports go to';
nok ast-begin-export =:= EXPORT,
  'BEGIN-time code of code turned into an AST has an EXPORT package of its own';
is (try EVAL q{my package EXPORT { }; BEGIN 1; 2}), 2,
  'an EVAL declaring its own EXPORT can run BEGIN-time code';
is (try EVAL q{my class EXPORT { method id { 'own' } }; BEGIN ::('EXPORT').id}), 'own',
  'BEGIN-time code in an EVAL finds the EXPORT the EVAL declares';
throws-like { EVAL q{sub dup-export is export { }} for ^2 }, X::Export::NameClash,
  'running an EVAL that exports a sub twice clashes';
BEGIN try q{use MONKEY-SEE-NO-EVAL; BEGIN EVAL q{sub ast-at-begin is export { }}}.AST;
nok EXPORT.WHO<DEFAULT>.WHO<&ast-at-begin>:exists,
  'code turned into an AST at BEGIN time exports nothing from the code around it';
is (try EVAL q{use EvalExports; exported-by-begin-eval()}), 'begin',
  'a sub a module exports from an EVAL at BEGIN time can be imported';
is (try EVAL q{use EvalExports; exported-by-mainline-eval()}), 'mainline',
  'a sub a module exports from an EVAL in its mainline can be imported';
sub eval-with-native-export(int \EXPORT) { EVAL q{1 + 1} }
is (try eval-with-native-export(3)), 2,
  'an EVAL in the scope of a native named EXPORT compiles';
