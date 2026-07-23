use lib <t/02-rakudo/test-packages>;
use Test;

plan 4;

# The parent module declares `unit package Unicode`, a name a setting
# class already uses, with `class PRECIS` nested in it. The child module
# uses the parent, declares the same `unit package Unicode`, and inherits
# from the qualified Unicode::PRECIS name. Resolving that parent at
# compile time needs the declared package to adopt the WHO the import
# reached, which the GLOBALish upgrade provides. Unicode::PRECIS (and
# MongoDB through it) failed with "'Unicode::PRECIS::Identifier' cannot
# inherit from 'Unicode::PRECIS' because it is unknown".

use Unicode::PRECIS::Identifier;

ok Unicode::PRECIS::Identifier ~~ Unicode::PRECIS,
    'the class declared inside the same-named unit package inherits the qualified parent';
is Unicode::PRECIS::Identifier.new.tag, 'parent',
    'a method from the inherited parent works on the child';
is Unicode::PRECIS::Identifier.new.sub-tag, 'child',
    'a method of the child itself works';
ok Unicode::.keys.sort.join(',').contains('PRECIS'),
    'the used package namespace carries the nested class';
