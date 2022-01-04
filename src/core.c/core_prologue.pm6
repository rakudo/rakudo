# This constant must specify current CORE revision.
# Must preceede class declarations to allow correct recording of their respective language version.
my constant CORE-SETTING-REV = 'c';

# Stub a few things the compiler wants to have really early on.
my class Pair { ... }   # must be first for some reason
my class Block { ... }
my class HyperWhatever { ... }
my class List { ... }
my class Map { ... }
my class Match { ... }
my class Failure { ... }
my class Rakudo::Deprecations { ... }
my class Rakudo::Internals { ... }
my class Rakudo::Internals::JSON { ... }
my class Rakudo::Internals::RegexBoolification6cMarker { ... }
my class Rakudo::Iterator { ... }
#?if !js
my class ThreadPoolScheduler { ... }
#?endif
#?if js
my class JavaScriptScheduler { ... }
#?endif
my class Whatever { ... }
my class WhateverCode { ... }
my class X::Attribute::Required { ... }
my class X::Numeric::Overflow { ... }
my class X::Numeric::Underflow { ... }
my class X::TypeCheck::Attribute::Default { ... }

# Stub these or we can't use any sigil other than $.
my role Positional { ... }
my role Associative { ... }
my role Callable { ... }
my role Iterable { ... }
my role Enumeration { ... }
my role PositionalBindFailover { ... }
my role Hash::Typed { ... }
my role Hash::Object { ... }

# Make Iterable available for the code-gen.
BEGIN nqp::bindhllsym('Raku', 'Iterable', Iterable);
nqp::bindhllsym('Raku', 'Iterable', Iterable);
nqp::bindhllsym('Raku', 'Failure', Failure);

BEGIN {
    # Ensure routines with traits using mixins applied to them typecheck as Callable.
    Code.^add_role: Callable;
    # Compose routine types used in the setting so traits using mixins can be
    # applied to them.
    Sub.^compose;
    Method.^compose;
    Submethod.^compose;
}

# Set up Empty, which is a Slip created with an empty IterationBuffer (which
# we also stub here). This is needed in a bunch of simple constructs (like if
# with only one branch).
my class IterationBuffer is repr('VMArray') { ... }
my constant Empty = nqp::p6bindattrinvres(nqp::create(Slip),
    List, '$!reified', nqp::create(IterationBuffer));

# We use a sentinel value to mark the end of an iteration.
my constant IterationEnd = nqp::create(Mu);

# To allow passing of nqp::hash without being HLLized, we create a HLL class
# with the same low level REPR as nqp::hash.
my class Rakudo::Internals::IterationSet is repr('VMHash') {
    method raku() {
        nqp::p6bindattrinvres(nqp::create(Map),Map,'$!storage',self)
    }
}

# The value for \n.
my constant $?NL = "\x0A";

# Make sure we have an environment
PROCESS::<%ENV> := Rakudo::Internals.createENV;

# This thread pool scheduler will be the default one.
#?if !js
PROCESS::<$SCHEDULER> = ThreadPoolScheduler.new();
#?endif

#?if js
PROCESS::<$SCHEDULER> = JavaScriptScheduler.new();
#?endif

#?if jvm
BEGIN {nqp::p6setassociativetype(Associative);}
#?endif

# vim: expandtab shiftwidth=4
