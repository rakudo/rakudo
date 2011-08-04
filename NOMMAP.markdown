# Roadmap for "nom" Branch

Last Updated 3 Aug 2011

## Punch list for nom->master and nom distribution

This is the "punch list" of items that need addressing before
switching nom to be the new 'master' branch, and before releasing
a nom-based distribution.  Each item below contains an importance
(1 == must have, 2 == ought to have, 3 == nice to have) for
branch and distribution, an estimate of the difficulty 
(* == easy, **** = hard), and any identified 
responsible parties.

* "make install" (1, 1, ***, jnthn pmichaud)
* Method delegation (2, 2, ***, jnthn)
* Parametric roles (2, 2, ***, jnthn)
* Enums to level of master (2, 1, ???, ???)
* Grammars, qregex, other regex stuff (1, 1, ***, pmichaud)
* Nested signatures (2, 1, ***, jnthn)
* Buf (2, 1, **, pmichaud)
* DateTime, Date (2, 1, **, ???)
* Code constraints on multidispatch (2, 1, ***, jnthn)
* "use" on non-compiled modules (1, 1, ***, jnthn)
* Hyper metaoperator (1, 1, **, pmichaud)
* Operator overloading (2, 1, *, ???)   (may be done already)
* Define new operators (2, 1, **, pmichaud) (awaiting other regex updates)
* Whatever currying (2, 1, ???, ???) (may be done already)
* MAIN (2, 1, ???, ???)
* callframe (3, 1, ???, ???)
* modules working with nom (3, 1, ???, ???)

## Other NOMMAP notes

Note that this isn't strictly in order, though things nearer to the top
are likely to get focus sooner.

Current fails that people are likely to encounter (no particular order):
* core constants (e.g., Inf, Order::Decrease)
* is export on methods
    - should work outside the setting; note here if not

Things that aren't blockers but might be worth knowing about:
* attribute := doesn't work in CORE.setting (works outside of setting, though)
  (initial digging suggets it's a BOOTSTRAPATTR issue, thus why we only see it
  in the setting)
* no rw-accessors for natively typed attributes (yet?)

## Lexical Multi-Part names
For my X::Base { ... }, my Foo::Bar { ... } etc. The our-scoped ones work.

## when statements
when needs to properly find use correct outer scope

## Flesh out signatures and parameters
While much binding works again now, there's some stuff to "put back":
* |$c
* Sub-signatures

## Fix up binding some more
Get ::= correcter, and a bit more stuff on := also.

## Finish up parametric roles
Many things are working, some are not...
* Generically typed variables (also for the ::T case in roles)
* role R1[::T] does R2[T] { ... }
* Multiple variants of a single role
* Role subtype checking

## Meta-operators
* Hyper

## Missing object model bits
* Some support for augment
* handles

## Custom Operators
Get these working again.

## Work towards nom being master
Get enough of the spectests passing for us to be ready to make nom into
the master branch

## Enumerations
* Enums as roles
* Non-numeric enums
* Anonymous enums

## Fix array/hash initialization
my @a;  needs to initialize @a to be the Array type object (not an Array instance).
Same for my %h and hashes.

## Phasers
* END in pre-compiled mainline case

## Make nom installable

## fix segfaults
