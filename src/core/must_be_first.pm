use Perl6::BOOTSTRAP;

# Stub a few things the compiler wants to have really early on.
my class Pair { ... }
my class Whatever { ... }
my class WhateverCode { ... }

# Stub these or we can't use any sigil other than $.
my role Positional { ... }
my role Associative { ... }
my role Callable { ... }
