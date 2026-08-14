use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

# A variable declarator signature accepts the parameter traits that
# have no variable meaning. They stay on the parameter, where a list
# of declarations leaves them inert, since nothing ever binds through
# the signature. A trait with variable meaning still applies to the
# declared variable, and an unknown trait is still rejected.

plan 14;

is EVAL('my ($a is rw); $a = 3; $a'), 3,
  'a declarator signature accepts is rw and the variable assigns';

is EVAL('my ($a is copy); $a = 3; $a'), 3,
  'a declarator signature accepts is copy and the variable assigns';

is EVAL('my ($a is readonly); $a = 3; $a'), 3,
  'a declarator signature accepts is readonly and the variable assigns';

is EVAL('my ($a is raw); $a = 3; $a'), 3,
  'a declarator signature accepts is raw and the variable assigns';

is EVAL('my ($a is required); $a = 3; $a'), 3,
  'a declarator signature accepts is required and the variable assigns';

is EVAL('my ($a is onearg); $a = 3; $a'), 3,
  'a declarator signature accepts is onearg and the variable assigns';

throws-like { EVAL 'my ($a is item)' },
  Exception,
  message => /"Cannot use 'is item' on parameter"/,
  'an is item trait in a declarator signature reports as a parameter trait';

is EVAL('my (int $a is rw); $a = 3; $a'), 3,
  'a declarator signature accepts is rw on a native int variable';

is EVAL('my (int $a is copy); $a = 3; $a'), 3,
  'a declarator signature accepts is copy on a native int variable';

is EVAL('my ($a is rw is default(42)); $a = 3; $a'), 3,
  'a parameter trait beside a variable trait leaves both accepted';

is EVAL('our ($o is rw); $o = 3; $o'), 3,
  'an our declarator signature accepts is rw and the variable assigns';

is EVAL('sub f { state ($s is copy); ++$s }; f; f'), 2,
  'a state declarator signature accepts is copy and the variable persists';

throws-like { EVAL 'my ($a is rw) := 42' },
  Exception,
  message => /'unpack or Capture'/,
  'a binding declarator signature still binds through the parameter';

is-run 'sub MAIN() { my ($a is rw); $a = 3; print $a }', :out<3>,
  'a declarator signature inside sub MAIN earns no dispatch advice';

# vim: expandtab shiftwidth=4
