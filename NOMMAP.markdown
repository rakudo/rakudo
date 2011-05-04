# Roadmap for "nom" Branch

Last Updated 5th May 2011

## Setting and module loading
Get setting and module loading to basically work, including EXPORTHOW
and first cut of global merging.

## Static lexpads
Get a Rakudo version of the NQPLexPad/NQPLexInfo stuff into place.

## Basic package declarations and stub packages
Be able to parse and create the compile-time meta-objects for packages.
Track which are stub packages and need to be "completed" by the end of
the compilation unit.

## Attributes and methods
Get adding these into declared classes in place

## Traits
Get inheritance and is repr in place.

## Initial bits in the setting
We'll start to re-build some of the bits previously in built-ins into a
new setting.

* Import meta-model bits
* Add stubby Mu, Any, Cool, Int, Num, etc.
* Make sure that we can make instances of these from programs that use
  the setting

## protos and multis
Get the new proto and multi semantics in place.

## Various operators in the setting
Add back various operators like the arithmetic ones.

## Parametric roles
Get parametric roles working with the new meta-object model.

* Create way for indicating type vars that need reificiation
* Implement ParametricRoleHOW and ConcreteRoleHOW
* Ensure the reification of types works

## Lists, arrays and hashes in the setting
Get them working. We'll have the parametric role support to do that now.

## Missing object model bits
* Implement SubsetHOW, make it work with the syntax
* Try to implement enumerations
* Some support for augment

## Restore other setting bits
Bring back in everything that was ripped out. Probably many our things
become my things.

## Work towards nom being master
Get enough of the spectests passing for us to be ready to make nom into
the master branch
