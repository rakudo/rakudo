# Roadmap for "nom" Branch

Last Updated 5th June 2011

Note that this isn't strictly in order, though things nearer to the top
are likely to get focus sooner.

## Attribute features
Finish 'is rw', accessor generation, and make use of default value closure.

## Multi-methods
Get these working.

## Object construction
new, bless, CREATE, BUILDALL, BUILD handling.

## Magicals
$_, $/, $!, @_ and %_ handling.

## Lists, arrays and hashes in the setting
Get them working.

## Flesh out signatures and parameters
While basic binding works again now, there's plenty of stuff to "put back".

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
