# Roadmap for "nom" Branch

Last Updated 19th May 2011

## Code objects, signature objects and parameter objects
Be able to create various code objects and set up signatures and parameters
at compile time.

## Signature binding update
Updates to work with the new signature and parameter objects.

## Inheritance trait
Get enough in place to do inheritance

## Attributes and methods
Get adding these into declared classes in place

## Initial bits in the setting
We'll start to re-build some of the bits previously in built-ins into a
new setting.

* Add stubby Mu, Any, Cool, Int, Num, etc.
* Make sure that we can make instances of these from programs that use
  the setting

## proto and multi subs
Get proto and multi subs in place, along with the new semantics for them.

## Various operators in the setting
Add back various operators like the arithmetic ones.

## Multi-methods
Get these working.

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
