# Roadmap for "nom" Branch

Last Updated 12 July 2011

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
* 'has num $.attr' segfaults on access to .attr, because it uses the PMC form
  of get_attribute

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
* Try to implement enumerations
* Some support for augment
* handles

## Restore other setting bits
Bring back in everything that was ripped out. Probably many our things
become my things.

## Custom Operators
Get these working again.

## Work towards nom being master
Get enough of the spectests passing for us to be ready to make nom into
the master branch

## Fix array/hash initialization
my @a;  needs to initialize @a to be the Array type object (not an Array instance).
Same for my %h and hashes.

## Phasers
* END in pre-compiled mainline case

## BEGIN-time lexicals
Need to have support for assigning to outer variables at BEGIN time.
