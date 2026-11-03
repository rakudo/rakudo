# Compilation phases

## Background

Compiling Raku is made especially interesting because the language user may
participate in compile time, by writing `BEGIN` blocks, using `constant`,
writing custom trait handlers, and so forth. The presence of declarations
that are made can impact on the parse that follows. There is also a notion
of `CHECK` time at the end of compilation, before optimization takes place.
With RakuAST we expect that this list to grow with:

* `EVAL` of an AST, where any actions that the parser might usually drive will
  not have taken place
* Macros, which are routines invoked with pieces of AST and produce further AST,
  or may even modify the AST they are given
* Quasi-quoted code, where we need to effect declarations to the degree they
  impact the parse, but would need to have them separately created in each
  "instantiation" of the quasi
* Custom compiler passes
* Access to the AST in routine traits

RakuAST nodes represent Raku code constructs quite closely. They form a tree:
a given node may have attributes that refer to other RakuAST nodes. There is,
however, no link from a child node up to its parent. Each node provides a
`visit-children` routine that runs a callback on each of the child nodes it
has, providing a uniform way to walk the tree.

However, compilation is far more involved that simply walking the tree and
making code for each node. Of note:

* Some symbols need registering immediately when parsing, since their
  declaration impacts the parse that follows
* Usages need to be linked to declarations (and undeclared things reported)
* Thunks need to be applied (for example, in expressions like `* + 1`, or in
  attribute initializer values, or in the replacement part of a substitution)
* Meta-objects need to be produced
* Traits need to be applied
* Some nodes need to know about surrounding context in order to compile; for
  example, the compilation of `$=finish` needs to access the data attached to
  the enclosing compilation unit
* Some nodes need to have their relationship with non-direct children managed;
  for example, a block or routine may have placeholder parameters, and a class
  declaration will have methods and attributes
* Some nodes produce synthetic pieces of AST. For example, placeholder parameter
  declarations result in the production of a `RakuAST::Signature`

Those working with RakuAST shall also want to be able to analyze and modify the
AST, however, which complicates matters further since those changes might come
"too late". For example, a `Routine` trait accessing the AST might try to make
changes that would impact upon the meta-object that is produced for the routine,
however we need to form the `Routine` meta-object in order to pass it to the
trait! It's OK if the answer is "too late", but we should avoid silent failure
in such cases.

## Phases

### Parse

In the case that the AST node is being produced while parsing, this phase
happens immediately after the node's creation. Node creation happens:

* For elements that are lexical scopes, immediately upon scope entry
* For all other elements, at the point they are produced by the action
  method

In a synthetic AST it happens in the initial walk of the tree, top-down for
anything that is a RakuAST::LexicalScope and bottom-up for anything else.
In this phase:

* Elements that result in declarations register themselves with the resolver
  immediately, so that the symbol can be resolved later in the parse (this is
  primarily true of lexical symbols, but also true of multi-part package
  delcarations)
* Packages stub a meta-object and apply traits
* Variable usages that require parse-time resolution are resolved
* Types are resolved

### Begin

In the parser, begin time happens after the parsing of an element is complete.
Thus for a `sub` it happens after the closing `}`, and similar for a package
declaration. For explicit `BEGIN` statements it comes after the block or
statement is parsed, and for `constant` and `subset` declarations after they
are parsed too.

In a synthetic AST, this happens after bottom-up after visiting the node, and
always after any parse time effects have been applied.

In this phase:

* Placeholder parameters lead to signature production
* Meta-objects for routines, packages, subset, and enum types are produced, and
  any traits are applied



### Check

This phase most typically happens after the entire AST for a compilation unit
has been produced. However, any code that is to be executed at begin time
gets its check time moved earlier. In this phase:

* Various semantic errors, including calls to undeclared routines, are reported
* Sink context is calculated

## Custom compiler passes

Custom compiler passes provide:

* Phase predicates for nodes they wish to apply to: `parse-predicate`,
  `begin-predicate`, etc.
* Zero or more pre-phase or post-phase method implementations

Note that the predicates are only evaluated (and phase methods only
called) on nodea that do a matching phase role. For example, the
`parse-predicate` is only evaluated, and `pre-parse` and `post-parse`
only called, on nodes that do the `RakuAST::ParseTime` role. The
default predicate for all of these evaluates to `False`; one must always
override at least one predicate method.

The phase methods that may be implemented are:

* `pre-parse`: Before parse time - that is to say, before the AST node has
  taken any parse-time action. This happens before symbols are resolved.
  The `no strict` pragma would be implemented by first attempting to resolve
  a variable reference, and if it fails, synthesizing a declaration. Since
  undeclared variables whine at parse time, this is the only phase that is
  early enough to implement such a feature. It's also the only one where it is
  possible to prevent declarations being made or change names that are to be
  declared. For a `RakuAST::LexicalScope` this is called around the opening
  curly, and so children won't be attached yet.
* `post-parse`: After parse time. Generally, any declarations will have been
  made at this point, and it is safe to request the stub meta-object. However,
  full meta-objects should not be requested, as those are not produced until
  begin time. For a `RakuAST::LexicalScope` this is called around the opening
  curly, and so children won't be attached yet.
* `pre-begin`: Before begin time. At this point, one can expect child elements
  to have been attached, and thus to have a complete AST. There may yet be
  semantic errors in that AST, however. Meta-objects will not have yet been
  produced. Most AST modifications are still possible at this point (only
  those like name changes to already declared elements are invalid). For
  example, one could replace the signature.
* `post-begin`: After begin time. At this point, meta-objects (such as for
  blocks and packages) will have been produced. Changes to elements that
  would impact upon the produced meta-objects are now too late. Again, the
  AST may still have semantic errors.
* `pre-check`: Before check time. It's still possible to change the AST,
  to the same degree it was at BEGIN time.
* `post-check`: By this point, any semantic errors that would prevent the
  successful compilation will have been reported. Any changes to the AST
  also forbidden, however one can know that the program is well formed
  and will survive compilation.

## AST mutation

Mutation of already-formed AST is achieved through `replace` methods. These
will throw an exception in the event that compile-time side-effects (such as
parse-time or begin-time meta-object production have already rendered the
change "too late").
