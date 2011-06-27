# Roadmap for "nom" Branch

Last Updated 25 June 2011

Note that this isn't strictly in order, though things nearer to the top
are likely to get focus sooner.

Current fails that people are likely to encounter (no particular order):
* core constants (e.g., Inf, Bool::True, Order::Decrease)
* fail()
* List ~~ Positional fails
* multis don't distinguish Positional
* array parameters bind as containers
* &infix:<cmp>.arity should be 2

Things that aren't blockers but might be worth knowing about:
* attribute := doesn't work in CORE.setting (works outside of setting, though)
  (initial digging suggets it's a BOOTSTRAPATTR issue, thus why we only see it
  in the setting)
* userspace code can't add new multis of existing operators 
  (e.g., multi postfix<++>(MyType $x) { ... } )
  (known - multis in nested lexical scopes in general just NYI - jnthn)

## Lexical Multi-Part names
For my X::Base { ... }, my Foo::Bar { ... } etc. The our-scoped ones work.

## Object construction
new, bless, CREATE, BUILDALL, BUILD handling. Make use of default value
closure.

## Magicals
$_, $/, $!, @_ and %_ handling.

## when statements
when needs to properly find use correct outer scope

## Flesh out signatures and parameters
While much binding works again now, there's some stuff to "put back":
* |$c
* Sub-signatures

## Rat and Complex literals
Depends on working out how to unify parsing of numeric strings.

## Fix up binding some more
Get ::= correcter, and a bit more stuff on := also.

## Finish up parametric roles
Many things are working, some are not...
* Composition of submethods
* Generically typed variables (also for the ::T case in rules)
* Mentions of role arguments in the role body in a non-declarative context
* role R1[::T] does R2[T] { ... }

## Meta-operators
* Assignment
* Negation
* Reduction
* Triangle
* Hyper
* Cross

## Missing object model bits
* Try to implement enumerations
* Some support for augment
* handles
* .Foo::bar
* does and but

## Ensure nested packages work
In theory, just should already.

## Restore other setting bits
Bring back in everything that was ripped out. Probably many our things
become my things.

## Custom Operators
Get these working again.

## Phasers
* BEGIN/CHECK need to see correct lexical environment
* END in pre-compiled mainline case

## Work towards nom being master
Get enough of the spectests passing for us to be ready to make nom into
the master branch
