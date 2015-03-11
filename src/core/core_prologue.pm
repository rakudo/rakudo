use Perl6::BOOTSTRAP;

# Stub a few things the compiler wants to have really early on.
my class Pair { ... }
my class Whatever { ... }
my class HyperWhatever { ... }
my class WhateverCode { ... }
my class Cursor { ... }

# Stub these or we can't use any sigil other than $.
my role Positional { ... }
my role Associative { ... }
my role Callable { ... }

# vim: ft=perl6 expandtab sw=4
