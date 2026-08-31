use lib <t/02-rakudo/test-packages>;
use Test;
use RakuLevelNameHOW;

plan 4;

rakuname Alpha { }
rakuname GLOBAL::Gamma { }
module Wrap { rakuname Beta { } }

is Alpha.HOW.name-chars(Alpha), 5,
  'a HOW written in Raku can use the name of a package declared at unit scope';

is Gamma.HOW.name-chars(Gamma), 5,
  'a HOW written in Raku can use the name of a package declared with a GLOBAL:: prefix';

is Wrap::Beta.HOW.name-chars(Wrap::Beta), 10,
  'a HOW written in Raku can use the name of a package declared inside another package';

is Wrap::Beta.^name, 'Wrap::Beta',
  'a package declared inside another package is still named with the enclosing package';

# vim: expandtab shiftwidth=4
