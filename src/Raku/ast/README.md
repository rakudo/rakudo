# RakuAST

RakuAST is an in-development proposed AST for the Raku language (if unfamiliar
with ASTs, think of it as a document object model for Raku code). The intention
is for it to become part of the language specification, and the tests covering
it - in `t/12-rakuast/` so far - to become specification tests.

## Status

RakuAST is currently in the early design/implementation phase. Absolutely
everything is, at this point, speculative. More is missing than is done.

The files in this directory are translated into NQP. The bodies of the
methods *are* NQP, while the signatures and class bits are parsed and
then built up using the Raku MOP and meta-objects, so that they
introspect and fit in the type system like normal Raku objects.

The overall approach to RakuAST is to:

1. Flesh out a decent amount of the object model and test-cover it via. `EVAL`
   of the ASTs.
2. Then see about having alternative actions/world that, if an env var is set,
   produce RakuAST and compile from that. (This is where we are right now.)
   Currently, this variable is `RAKUDO_RAKUAST`.
3. Gradually work our way through passing the Rakudo tests with that env var set.
4. And then the spectests.
5. And then make it the default (hopefully this means we will have few problems
   left to discover when we throw `CORE.setting` at it!)
6. Implement a RakuAST-based optimizer.
7. Optimize RakuAST itself.
8. Ship it!

## If you want to help...

Designing and implementing the nodes themselves is - for now - probably best
left to jnthn in hope we might achieve some consistency there - especially at
this fairly early stage. However, for those eager to join in, there's still
quite a few other things to do that will be helpful. Specifically:

* Fix `make test`, which is grumpy because we're adding all the new `RakuAST`
  nodes and they're visible in the setting, but it wants to test there's no
  new symbols (which, well, there are). (done, for now at least)
* Get the compiler ID to factor in the AST nodes, so that a re-compile with
  only AST changes will invalidate previous pre-comps, so we don't have to
  remove `lib/.precomp/` after such a build. (done)
* Fix build system issues (doesn't rebuild if the AST compiler changes, etc.)
  (Difficulty: well, it involves a build system...)
* Get the AST compiler to support roles, and gradually transition the things
  that should be roles to actually be roles. (Difficulty: maybe headachey,
  but you'll live)
* Make the AST compiler support return types with `-->` and add them to the
  signature that is generated. Make accessors get these automatically based
  on the declared type. (Difficulty: not so bad.)
* Make us check the types that are passed to methods. (Difficulty: depends
  how we decide to do it. Actually it may be that we just get NQP to do the
  type checks and then rely on that. In fact, we could teach it to decont
  incoming arguments too, and support `is raw` too, and then we get to clean
  up lots of explicit deconts in the bootstrap, MOP, etc. Then we simplify
  the RakuAST compiler.)
* Make us indicate slurpiness when signatures are introspected. (Difficulty:
  easy, just need to make sure the AST compiler passes that along when we
  build the Parameter object.

And in general:

* While test-first development is being practiced, it's still possible to miss
  coverage; feel free to add more tests
* Try it out, break it, etc. (Please don't report things that are missing at
  this point. Most things are, so it's not that useful. :-))

## RakuAST goals

* Model things at the language level, without normalizing too strongly (so,
  almost the opposite of the QAST design goal)
* Everything that happens with the AST upto and including `CHECK` is for
  userspace; everything after it is not (the content of a `quasi` is not
  RakuAST for these purposes)
* RakuAST can be constructed directly or using `quasi`s
* RakuAST lets us use Raku as a frontend (example: a web framework could take a
  regex used for validation and translate the possible subset into something
  suitable for use with the `pattern` attribute in HTML, to naturally give
  client-side validation)
* RakuAST lets us use Raku as a backend (example: compile a JavaScript style
  regex from a JSON schema into `RakuAST` nodes rather than Raku source)

## General API design notes

* RakuAST nodes are constructed by passing named arguments, *unless* they have
  a single attribute to be constructed with, in which case they take it as a
  single positional argument.
* Use Raku naming conventions (kebab-case names, etc.)
* Use names that already exist for concepts, be that how they are called in
  the MOP or the grammar.
* Everything should be achieveable through tree construction. The user should
  not have to do any linking themselves. Resolutions imply a graph, but are a
  level atop of that, and many use-cases outside of the compiler will never
  have to explicitly participate in that.
* Nodes are free to cache things, but any laziness inside the nodes should be
  threadsafe. Benign races are fine (e.g. both calculate the same thing and
  one wins at installation). Effectively, anything perceived as a read operation
  should be safe in a threaded program.

## Design notes on specific topics

### The compile-time challenge

Today in Rakudo, we do a lot of declarative things "as soon as possible", such
as creating meta-objects. We then poke those into symbol tables. Macros mean
that we have to have much more capability to delay that, because, for instance,
a `class` declaration written in a `quasi` must spring into existence per time
the `quasi` is instantiated.

At the same time, activities at `BEGIN` time are well known as having effects on
the parsing and compilation following them within the compilation unit. At the
simplest level, knowing what things are types, which are terms, and which are
subs is critical to being able to parse Raku code (for example, `if foo { }` is
going to pass the block to a `sub` `foo`, but would be the block of the `if` when
`foo` is a term). More generally, `BEGIN` time may do things that impact upon the
future parse in any way whatsoever.

For the sake of a `quasi`, we'll need to restrict this, with `BEGIN` even deferred
to splice time. Effectively, the set of language changes you can effect within a
`quasi` are far more restricted. One particular challenge that will come up here
is that of `use` or `import` statements in a `quasi`; since we don't do the `use`
or `import` until splice time, we don't have the information to do the function
vs. term distinction. An easy way out of this in many cases will be to do the
`use` in the `macro` itself, in which case the imports are *fixated* and so can
be referenced within the `quasi` unambiguously.

Outside of a `quasi`, certain `BEGIN`-time things will act as "sequence points",
forcing the formation of meta-objects and the enactment of compile-time work.
These include:

* `use` statements
* A `BEGIN` phaser
* A `constant` with a non-literal value

[Conjectural: this may be possible to relax if we can understand something about
the needs of their content.]

Raku AST elements that will create a meta-object all do the type `RakuAST::Meta`.
Such an element may be born in quasi or non-quasi state (they are born in the
`quasi` state if they are written inside of a `quasi`, and transition to a
non-quasi state upon quasi quote interpolation).

Such `RakuAST::Meta` elements have the methods:

* `meta-object` - throws an exception if the element is in quasi state. In a
  non-quasi state, returns the meta-object that the node describes, producing
  and caching it if it has not already been produced.
* `has-meta-object` - returns `True` if the meta-object has already been
  produced, and `False` otherwise

The production of the meta-object may entail the production of dependent
meta-objects as a side-effect (for example, demanding the meta-object of a
routine in turn requires the signature meta-object, which in turn requires
the parameter meta-objects).

The production of meta-objects requires that all references involved have
been resolved.

Sometimes, we need to establish circular relationships between meta-objects.
In that case, we have stubby meta, which allows us to separate stubbing and
finalizing the meta-object.

### Symbol resolution

#### Declarations

Every declaration of a symbol is a `RakuAST::Declaration`. Declarations are
subject to instantiation when located within a `quasi`, being cloned just like
anything else.

#### Lookups

Everything that is a reference to a symbol that is to be resolved is a
`RakuAST::Lookup`. The resolution is supplied by a `RakuAST::Declaration`.
Note that resolution to a declaration does not imply there's no runtime
lookup involved. For example, a resolution that points to a lexical
declaration will be subject to lexical lookup per scope instantiation at
runtime.

Not all lookups need a resolution by runtime. In fact, for a dynamic like
`$*foo` it's usually not possible. A given lookup can indicate whether it
must have been resolved.

The API of a lookup is:

* `needs-resolution` - returns `True` if the lookup must be resolved at
  compile time, and `False` if it's happy being entirely left until
  runtime
* `is-resolved` - returns `True` if the lookup has a resolution
* `resolution` - gets the resolution if their is one, or throws an exception
  if not
* `set-resolution($decl)` - sets the resolution to a given `RakuAST::Declaration`
* `resolve($context)` - resolves the resolution using the provided resolution
  context

### Resolution contexts

A `RakuAST` node does not know about its context, and so is not on its own able
to resolve itself. A resolution context provides the things needed in order to
do that. It is able to perform lexical lookups, as well as having access to the
current global symbol table. It can also resolve multi-level package lookups.

There will be multiple different resolution context implementations suited to
different cases (for example, that used when parsing a `quasi` will resolve
variables to their fixations). So far we just have the `EVAL` resolver, which
is for when we have an AST and so aren't doing any parsing.

### Scopes

A `RakuAST::LexicalScope` models something that implies a lexical scope upon
the lexical declarations within it. It can be called upon to locate all of the
lexical declarations made immediately within it (excluding those within nested
scopes). It is free to make a cache of these to avoid having to search each
time, allowing for faster resolution of symbols. It will be for tree modifiers
to invalidate such things.

### Provenance

All nodes will have an optional "provenance" attribute (name TBD), which answers "where did
I come from". By default we'll only attach such information to some elements
(good enough to provide line information at statement level, as today), but will
support a detailed mode where the exact textual positions of all elements can be
recovered, as well as the original program text. This will be useful for those
wanting to do things like tidying and syntax highlighters.
