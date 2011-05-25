# Roadmap for "nom" Branch

Last Updated 26th May 2011

## String and Int literals
Get these to work, make sure we add them to SC (e.g. so we have them as
real constants, not creating them every time).

## proto and multi subs
Get proto and multi subs in place, along with the new semantics for them.
  
## Various operators in the setting
Add back various operators like the arithmetic ones.

## Containers
Examine container bits.

## Get attributes working
Finish up enough so a (typed) attribute can be declared. Check for use
of attributes that are not declared. Update attribute access code.

## Object construction
new, bless, CREATE, BUILDALL, BUILD handling.

## Multi-methods
Get these working.

## Flesh out signatures and parameters
While basic binding works again now, there's plenty of stuff to "put back".

## More traits
Add various missing traits

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
