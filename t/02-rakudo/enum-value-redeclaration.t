use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 4;

# A value name redeclared within one enum reports a redeclaration worry.
is-run 'enum Day <Mon Mon>; print Day.enums<Mon>',
    :out('1'),
    :err{.contains: q{Redeclaration of symbol 'Mon'}},
    'a duplicate enum value warns';

# A value name clashing with a value of an earlier enum also warns.
is-run 'enum A <X Y>; enum B <X Z>; print "ok"',
    :out('ok'),
    :err{.contains: q{Redeclaration of symbol 'X'}},
    'a value clashing with an earlier enum warns';

# A value name that only matches a nested package is not a lexical clash.
is-run 'class Day::Foo {}; enum Day <Mon Foo>; print "ok"',
    :out('ok'),
    :err(''),
    'a value clashing only with a nested package stays silent';

# A well-formed enum warns nothing.
is-run 'enum Color <red green blue>; print Color.enums<blue>',
    :out('2'),
    :err(''),
    'a well-formed enum warns nothing';

# vim: expandtab shiftwidth=4
