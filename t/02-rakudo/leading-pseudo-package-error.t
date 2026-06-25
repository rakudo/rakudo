use Test;

plan 4;

# A leading pseudo-package in a declared name must report a clean error
# rather than crashing the name resolver. CORE and GLOBAL took special
# resolution paths that crashed ("Can't shift from an empty array",
# "Empty name lookup not possible as a constant").

throws-like 'class CORE::Foo { }', X::PseudoPackage::InDeclaration,
    'a leading CORE in a declared name reports cleanly';

throws-like 'class CORE { }', X::PseudoPackage::InDeclaration,
    'a bare CORE declaration reports cleanly';

# The two frontends reject a bare GLOBAL with different exception types
# (X::PseudoPackage::InDeclaration vs X::AdHoc), so match on the message.
throws-like 'class GLOBAL { }', Exception, message => /'package GLOBAL'/,
    'a bare GLOBAL declaration reports cleanly';

# A CORE pseudo-stash lookup is unaffected.
is-deeply EVAL('CORE::<Int>'), Int,
    'a CORE pseudo-stash lookup still resolves';

# vim: expandtab shiftwidth=4
