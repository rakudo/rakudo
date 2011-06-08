# Roadmap for "nom" Branch

Last Updated 6th June 2011

Note that this isn't strictly in order, though things nearer to the top
are likely to get focus sooner.

## Multi-Part names
for X::Base, Foo::Bar etc.

## Object construction
new, bless, CREATE, BUILDALL, BUILD handling. Make use of default value
closure.

## our-scoped stuff
Of note, variables, packages, subs...

## Magicals
$_, $/, $!, @_ and %_ handling.

## Lists, arrays and hashes in the setting
Get them working.

## Flesh out signatures and parameters
While basic binding works again now, there's plenty of stuff to "put back".
Of note:
* Defaults
* Sub-signatures
* Post-constraints
* sub foo(0) { ... } (literals)

## Fix up binding some more
Get ::= correcter, and a bit more stuff on := also.

## More traits
Add various missing traits

## More operators
Flesh out the range of available operators

## Parametric roles
Get parametric roles working with the new meta-object model.

* Create way for indicating type vars that need reificiation
* Implement ParametricRoleHOW and ConcreteRoleHOW
* Ensure the reification of types works

## Missing object model bits
* Implement SubsetHOW, make it work with the syntax
* Try to implement enumerations
* Some support for augment
* handles
* .*/.+/.?

## Multi-part and nested packages
Nested may just work already...multi-part is relatively easy.

## Restore other setting bits
Bring back in everything that was ripped out. Probably many our things
become my things.

## Custom Operators
Get these working again.

## Work towards nom being master
Get enough of the spectests passing for us to be ready to make nom into
the master branch
