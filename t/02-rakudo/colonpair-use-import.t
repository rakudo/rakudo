use lib <t/packages/Test-Helpers t/packages/02-rakudo/lib>;
use Test;
use Test::Helpers;

plan 3;

# A `:$foo` colonpair in a `use`/`import` argument list names an import to
# select (the tag `foo`), it does not read a lexical `$foo`. So it must compile
# without `$foo` being declared and import the symbol exported under that tag.
use ColonPairImportHelper :$XML2, :Opaque;

is $XML2, 42, 'use Module :$tag imports the variable exported under that tag';
is Opaque, 'CPointer', 'use Module :Opaque imports the constant under that tag';

# `no Pragma :$foo` selects by name the same way, so it reaches the pragma
# rather than failing on an undeclared `$foo`.
is-run 'no strict :$foo; my $x = 1',
    :exitcode(* != 0),
    :err(/'does not take any arguments'/),
    'no Pragma :$foo parses the colonpair as a selector, not a lexical read';

# vim: expandtab shiftwidth=4
