use Test;

plan 3;

lives-ok { EVAL 'class Rakudo::CORE::META { }' },
    'CORE nested in a declared package name is allowed';

lives-ok { EVAL 'class Foo::MY::Bar { }' },
    'another pseudo-package nested in a declared name is allowed';

throws-like 'class MY::Foo { }', X::PseudoPackage::InDeclaration,
    'a leading pseudo-package in a declared name is still rejected';

# vim: expandtab shiftwidth=4
