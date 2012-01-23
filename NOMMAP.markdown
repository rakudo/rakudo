# Roadmap for "nom" Branch

Last Updated 14 jan 2011

## Punch list for nom distribution

This is the "punch list" of items that need addressing before
releasing a nom-based distribution.  Each item below contains an importance
(1 == must have, 2 == ought to have, 3 == nice to have) for
branch and distribution, an estimate of the difficulty 
(\* == easy, \*\*\*\* = hard), and any identified 
responsible parties.

* Enums to level of master (2, 1, ???, ???)

## Other NOMMAP notes

Note that this isn't strictly in order, though things nearer to the top
are likely to get focus sooner.

Current fails that people are likely to encounter (no particular order):
* Order enumeration

Things that aren't blockers but might be worth knowing about:
* attribute `:=` doesn't work in CORE.setting (works outside of setting, though)
  (initial digging suggets it's a BOOTSTRAPATTR issue, thus why we only see it
  in the setting)
* no rw-accessors for natively typed attributes (yet? spec isn't clear)

## when statements
when needs to properly find use correct outer scope

## Fix up binding some more
Get `::=` correcter, and a bit more stuff on `:=` also.

## Enumerations
* Enums as roles
* Non-numeric enums
* Anonymous enums

## Fix array/hash initialization
`my @a;` needs to initialize @a to be the Array type object (not an Array instance).
Same for my %h and hashes.

## Phasers
* END in pre-compiled mainline case

## Switch Real to be a role
It's a class; work in real-trouble branch.
