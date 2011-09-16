# Roadmap for "nom" Branch

Last Updated 8 Sep 2011

## Punch list for nom->master and nom distribution

This is the "punch list" of items that need addressing before
switching nom to be the new 'master' branch, and before releasing
a nom-based distribution.  Each item below contains an importance
(1 == must have, 2 == ought to have, 3 == nice to have) for
branch and distribution, an estimate of the difficulty 
(\* == easy, \*\*\*\* = hard), and any identified 
responsible parties.

* Parametric roles (2, 2, \*\*\*, jnthn)
* Enums to level of master (2, 1, ???, ???)
* Grammars, qregex, other regex stuff (1, 1, \*\*\*, pmichaud)
* Buf (2, 1, **, pmichaud)
* DateTime, Date (2, 1, \*\*, ???)
* Define new operators (2, 1, \*\*, pmichaud) (awaiting other regex updates)
* Whatever currying (2, 1, ???, ???) (*.method case still missing, return type is not WhateverCode)
* MAIN (2, 1, ???, ???)
* modules working with nom (3, 1, ???, ???)
* fix meta-dispatchers to not flatten positionals (3, 2, ???, ???)
* <!before> (2, 1, *, pmichaud)
* <&lexical-regex> (2, 1, ???, pmichaud)

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

## Fix up binding some more
Get ::= correcter, and a bit more stuff on := also.

## Finish up parametric roles
Many things are working, some are not...
* role R1[::T] does R2[T] { ... }
* Be sure to do a BUILD after mix-in

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


## Switch Numeric and Real to be roles
They are classes right now, which is wrong.
