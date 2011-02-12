# Roadmap for "nom" Branch

## First cut of native and class meta-objects
Need to get the various bits of the meta-model implementation fleshed
out. Of note, need the following HOWs and their dependent roles to be
implemented to some degree:

* ClassHOW
* NativeHOW

Note that these are written in terms of NQP classes and roles to make
for a pleasant factoring.

## Static lexpads and package stuff
Work out a solution for static lexpad handling and implement it, first in
nqp and then add the capability to Rakudo. Work out with pmichaud++ how
package handling should come to look.

## Serialization Context table
We don't have resources to fully implement the serialization as such in
this branch. But it should be prepared for it. Implement a way to have a
table of "stuff" that can be quickly looked up by index, and that is to be
produced at startup if bytecode is loaded. The objects can have been made
at compile time and just re-used at runtime too so we don't double-make
stuff. Eventually the "making" of them at startup will become deserializing.

## Twiddle build process
The "first stage setting" will go away in favour of a single setting.
The compilation ordering will be something like:

1. Backend pre-requisites: compile dynops, PMCs and C parts
2. Compile meta-objects
3. Compile grammar and actions
4. Build an executable thing (e.g. PBC) with grammar/actions/meta-objects
   and no setting. Use it to compile the setting.
5. Bundle meta-objects, grammar, actions and setting into one executable

## Initial bits in the setting
We'll start to re-build some of the bits previously in built-ins into a
new setting. Importantly, they'll be installed lexically and make use of
the 

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
