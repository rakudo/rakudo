# Preface

This article was written by Jonathan Worthington and last revised in Feb, 2016.
For this reason in some aspects it could be outdated by the time you would be
reading it. But it provides mental pabulum for anybody with interest in
implementing language changes.

Article sources can be found [on Github](https://gist.github.com/jnthn/c10742f9d51da80226fa#coresetting-object-change-guidelines).

_Vadim Belman, Sep, 2019_

# Overview

This gist contains three documents:

1. Proposed Perl 6 versioning and compatibility guidelines, which describe how
   the Perl 6 language can be versioned and changed over time. It describes
   some things that implementations must do, should do, and may do.
2. Proposed Rakudo versioning guidelines, which describes how Rakudo will
   handle versioning.
3. Proposed MoarVM versioning guidelines, which describes how MoarVM will
   handle versioning.

These should all be considered as *living documents* - that is to say, they
are expected to evolve over time to reflect the situation "on the ground".
What serves a language well as it goes from little adoption to medium levels of
adoption may not serve it well as it goes from medium to high levels of
adoption.

A few notes:

* There's no "obvious right way" to handle these topics, just a bunch of
  difficult trade-offs. It's entirely possible - and fairly likely - that the
  proposals here will be met with *both* "it allows too much change" and "it
  allows too little change".
* Chatter is easy, but concrete suggestions are harder to come up with in this
  area. Since there's a risk this topic will face plentiful bikeshedding,
  there's a protocol for suggesting change: please fork this gist, do the
  changes in your fork, and leave a comment linking to and explaining it.
* Various language features are used as examples. However, just because a
  feature is used to discuss, for example, how its deprecation might be
  handled, does not mean we plan to deprecate it! :-)

# Perl 6 versioning and compatibility guidelines

All programming languages face the challenge of balancing language improvement
against keeping user's existing code working during language and compiler
upgrades. The easiest way to remain backward-compatible is to never fix bugs or
make improvements. Clearly, this is not desirable. At the other end of the
spectrum, the easiest way to be able to keep improving and fixing a language
and its implementations is to disregard backward compatibility. This also
doesn't really work for a language aiming to achieve wider adoption; it leaves
few users feeling comfortable using the language, and/or results in those that
do use it having a strong dependency on the precise versions of the language
and compiler they used (and so unable to cheaply upgrade).

As a language developed in the open, Perl 6 started out with very loose
backward compatibility. The small number of early adopters were expected to
cope with regular breaking changes, as the language was improved and various
alternatives explored. The 6.c release marked a change to this, offering users
a more stable experience through tighter backward compatibility. This document
provides guidelines for both language developers and language users with regard
to this backward compatibility, and also with regards to forward compatibility.
For language developers, it outlines how improvements and fixes should be
incorporated into the language in an appropriate way. For language users, it
provides an indication of what can and cannot be safely relied upon.

Just as a language and its implementations need room to evolve, so will this
set of guidelines. The 6.c release did not mark a switch from one binary option
to another, but rather a shift along the compatibility spectrum. These
guidelines try to place Perl 6 at a point where adoption will not be hindered
by excessively loose backward compatibility, while at the same time allowing a
little more "wiggle room" than we can expect to have in the longer term thanks
to adoption still being at an early stage. Thus, expect these guidelines to be
adapted over time to reflect the situation "on the ground" - which will likely
call for increasingly tight backward compatibility as adoption grows, but also
come to prescribe a range of wise ways to achieve it in conjunction with taking
the language forward.

## Version scheme

A version of the Perl 6 language is defined by its test suite. Therefore, an
implementation can be said to *provide* a certain version of Perl 6 so long as
it passes the test suite for that version.

Major Perl 6 language versions are named 6.c, 6.d, 6.e, etc. They also carry a
marketing name that matches the version letter: "Christmas", "Diwali", etc.
Generally, major language version releases will be centered around one or more
"headline" features, and will most likely be declared primarily for marketing
purposes. These are not time-based in nature, and usually there will be at
least a year between them.

There may also be minor language versions released. These are numbered with an
extra ascending integer, starting from `1`. Thus, the minor language version
releases following 6.c would be numbered 6.c.1, 6.c.2, etc. These will tend to
focus around tweaks and fixes rather than features. At present, a minor version
can likely be expected every 2-3 months. Minor versions exist so that programs
have a way of declaring a dependency on a certain tweak or fix being present,
without having to wait for the next major (lettered) release. (This is to help
avoid cases where programs feel a need to take a dependency on a certain
compiler and/or compiler version.)

Implementations may support draft versions of future Perl 6 major releases.
These are expressed with an extra letter after the version, so 6.d.a is the
first draft of 6.d ("alpha"), 6.d.b is the second ("beta"), etc. Such drafts
are exempted from backward compatibility. To provide freedom to compiler
implementors, they may branch from any minor language release of the previous
major version (that is, 6.d.a may lack things from 6.c.3 even if that is
supported).

Perl 6 programs and modules may declare a language version they require:

    use v6.c.1;

For now, they should do so at the start of the compilation unit only (with
simple line comments and whitespace being the only thing acceptable before the
version declaration). An implementation seeing a version declaration later than
this may report an error. This is to enable implementations to support different
language versions by loading different `CORE.setting` versions or even to use a
simple "version detection" parser to find the version in effect, and delegate
to a full parser for the chosen version. Since the scope of an `EVAL` forms the
setting of the evaluated code, it is forbidden to have a `use` version directive
in code passed to `EVAL`.

Any Perl 6 program or module that does not declare a language version will be
interpreted as expecting to run on the latest non-draft version supported by
the implementation (for example, it could get 6.c, 6.c.1, 6.d, or 6.d.1 - but
never 6.d.a, 6.e.b, etc.) For now, a plain:

    use v6;

Will imply the same as not declaring a language version. However, the Perl 6
language designers reserve the right to nail it down more tightly in the future
if there is good cause to do so.

While scripts and modules are not required to declare a version, module
directories or installers may choose to warn about, or even refuse to process,
modules or scripts that fail to do so. Module directories or installers may
also choose to treat modules of scripts depending on a draft version (such as
6.d.a) differently also.

Implementations **must** refuse to compile code requesting a version of Perl 6
higher than the maximum version it knows it supports. Implementations **must**
refuse to compile code requesting a version of Perl 6 lower than the minimum
version it knows it supports. Implementations supporting versions prior to the
latest they support **should** provide compatibility support of that version
as described in the compatibility guidelines specified below.

## Compatibility

### Specification, errata specification, and bug compatibility

Since Perl 6 language versions are defined by their test suite, then any
implementation that passes the test suite of a particular language release can
claim to have **specification compatibility** with that release.

For better or worse, the specification test suite is defined by humans, and
humans make mistakes. Now and then, there will be cases where tests end up
being wrong in various small ways. In rare cases, there may be errata for the
tests of an existing language release. Any implementation that fails to pass
the test suite of the language release, but does pass it with the errata
considered, can claim to have **errata specification compatibility**. An
implementation **must not** accept a program requesting a particular language
version unless it provides specification compatibility or errata specification
compatibility with that language version. Since errata specification compatiblity
is considered sufficient for this purpose, language designers should consider
carefully what errata is reasonable, preferably by analyzing the impact on code
in the module ecosystem. Compilers **should** document the language versions they
provide, and note when they are providing errata specification compatibility.

Compilers are not obligated to implement `use v6.d` style directives by running
the exact version of the compiler, nor CORE.setting, that they shipped when
first delivering an implementation of version 6.d. Compliance with the test
suite is the standard that is to be met.

Some may feel that language test suite does not have sufficient coverage. This,
at the time of authoring these guidelines, is no doubt true. However, places
that are poorly covered will tend to be those that had least issues reported
(because fixed bugs have consistently resulted in test coverage being added
over the course of many years of Perl 6 development), and so are likely to be
either the lowest risk, or the least used, corners of the language. The holes
are also likely to reflect the areas most in need of refinement, where the room
for manoeuvre is valuable. It is also reasonable to expect that test coverage
will greatly increase over time, and so as adoption (and the need for stronger
backward compatibility) grows the test suite will be defining the language
increasingly precisely. Finally, whatever weaknesses using a test suite as a
language specification may have, it's the most precise option realistically on
offer to the Perl 6 development team; a natural language specification will
typically be less precise and, critically, hard to ensure compliance with, and
we lack the expertise to effectively apply formal semantic methods (such as
operational and denotational semantics).

Language implementors **may** also choose to make **compiler bug compatible**
releases. These provide very close compatibility with a particular compiler
release, to the degree that they can be expected to contain most of its bugs
and quirks that were not considered by the language specification tests. These
releases will typically only contain security and build patches. Their purpose
is primarily to allow expedient deployment of security fixes. Their release
names should always make clear that they are made relative to a particular
compiler release.

### Language syntax change guidelines

Any additions to the grammar should be guarded by the current effective
language version. For example, suppose a new phaser `DONE` was to be added in
6.e. Rather than simply adding:

    token statement_prefix:sym<DONE> { <sym><.kok> <blorst> }

It would need to be:

    token statement_prefix:sym<DONE> {
        <sym><.kok> <!!since_version('e')> <blorst>
    }

This check has two purposes:

1. Ensuring that programs that declare an earlier language version are not
   broken by the syntax change. For example, any calling a listop named `DONE`
   would be broken by the above addition.
2. Aiding correct use of `use v6.x` by not making the new syntax accidentally
   available to programs written in a Perl 6 version that did not support it.

Syntax that is to no longer be supported should go through a deprecation period
(see the section on deprecation below). After that, it can be marked as not
being available. For example, suppose the `QUIT` phaser was not going to be
supported in 6.e and later. It would be annotated:

    token statement_prefix:sym<QUIT> {
        <sym><.kok> <!!until_version('e')> <blorst>
    }

If in the future the implementation decides to no longer offer compatibility
back to before 6.e, then it can remove the code completely (and should complain
upon a `use 6.d` that it does not support that language version).

### CORE.setting changes that are exempt from compatibility concerns

The following set of changes can be made in CORE.setting without having to
consider compatibility, provided the resulting changes preserve behavior.
Preserving behavior means regressing no specification tests from released
versions of the Perl 6 language (with eratta considered).

1. Implementation changes to any routine or block body.
2. Changing the names of positional parameters.
3. Changing a parameter from a scalar binding to a raw binding (`$a` to `\a`),
   or changing an array, hash, or code parameter binding to a raw binding
   provided the appropriate type constraint (`Positional`, `Associative`, or
   `Callable`) is added.
4. Any change to the non-public attributes of a class (for example, renaming
   attributes, adding/removing attributes, changing the type of an attribute).
   For any change of public interface attributes, see the section on OO
   compatibility below.
5. Any change involving the private methods of a class (renaming, adding,
   removing, refactoring).
6. Changing a sub or method from only to multi.
7. Splitting an only sub up into several multi candidates, where the same set
   of arguments are accepted and the same processing performed after the refactor.
8. Any change to anything within a package designated for compiler internals
   (for example, `Rakudo::Internals` in Rakudo).

### CORE.setting lexical change guidelines

The following changes are covered by these guidelines:

1. Adding new subroutines (this includes adding new operator impelementations).
2. Adding a new multi sub candidate.
3. Changing the signature of a subroutine or multi candidate in a way that
   affects the set of arguments that would bind (changes that do not affect
   binding are covered in the section above).
4. Changing the behavior of a subroutine (including multis and operators). This
   includes fixing bugs.
5. Changing a multi sub (or series of them) into an only sub.
6. Adding a new class, role, module, constant, or other symbol. Since these are
   in the `CORE.setting`, they are only visible lexically.

In Perl 6, current lexical scope is the very definition of current language.
Naturally, then, lexically scoped things are the easiest to deal with when it
comes to backward compatibility.

All of the above changes **should** be handled by making them in an
alternative setting. Implementations **may** thus have a `CORE.c.2.setting`
that has `CORE.c.1.setting` as its setting, with that in turn having some base
`CORE.setting` as its setting. However, this is not required, since it would
doom an implementation to ever-increasing load times due to having an ever
longer chain of settings to load. Therefore, implementations **may** choose to
"flatten" the chain. Perl 6 users **must not** rely on how implementations
choose to structure this, however the `CORE::` and `SETTING::` pseudo-packages
should always give a "flat" view of the symbols when iterated or indexed.

### CORE.setting object change guidelines

Changes to methods within roles, classes, and grammars are more difficult to
manage, since by definition a method call is to be interpreted by its receiver,
and that inherently means it will be executed in the receiver's language. Put
another way, every method call we make potentially crosses a language
boundary. Trying to "defeat" this will likely be futile, since it's very much
the nature of object orientation.

Therefore, the rules for changes to code in classes, roles, and grammars is as
follows.

1. All changes to method and rule bodies that do not cause a regression (as
   judged by spectests in the latest supported language release) are allowed.
   This includes fixes, optimizations, and added functionality.
2. All changes to method and rule signatures that make them accept a wider
   range of input arguments are allowed.
3. Methods starting to pay attention to previously disregarded named parameters
   is allowed.
4. Adding new methods to existing classes, roles, and grammars is allowed.
5. Changes to the set of public attributes need a lot of care, so as not to
   break destructuring (for example, naively adding public attributes to
   `Complex` would break `my (:$re, :$im) := $complex`). Therefore, changes
   must be made in a way that does not break said destructuring. This can be
   most easily handled by explicitly implemented a `method Capture { ... }` in
   the class.
6. Any desired changed of behavior that cannot be made while meeting the above
   rules should be handled by instead providing a new method, or a new type,
   that provides the desired new behavior.
7. Removal of existing methods, classes, roles, and grammars must go through
   the deprecation process.

The key difference compared to lexical changes is that no attempt is made by
the Perl 6 language to prevent changes being visible to a caller using an
earlier version of the language. This requires an addition-only backward
compatibility policy (which is largely like the Liskov Substitution Principle:
the class, after changes, should be usable in place of an earlier version of
itself).

It also means that callers declaring an earlier language version will be able to
"see" functionality from later versions. While it may seem desirable to try and
address this, the various possible "solutions" evaluated to date are worse than
the "problem".

* Having nested settings that `augment` or `supercede` existing types is not
  a solution, since the effects of these monkey-patching operations are global.
  Thus, as soon as something, somewhere, opts in to a later version of the
  language, then the changes will be globally visible. Such an approach would
  also create pre-compilation complications (multiple monkey patches to the
  same target already break the precompilation contract, and it is likely that
  `use MONKEY-TYPING` will end up implying `no precompilation`, which will be
  unacceptable for `CORE.setting` for startup performance reasons).
* Having different versions of classes, as proposed for handling multiple
  versions of modules, is ineffective for most of what is in `CORE.setting`.
  The assumption in the "multiple version" strategy is that code strongly
  encapsulates its use of a dependency. However, the types in `CORE.setting`
  are primarily used for interchange of data (`Int`, `Str`, `Array`, `Hash`),
  as base types to be derived from (`Mu`, `Any`, `Cool`), or as common
  interfaces (`Seq`, `IO::Handle`, `Promise`, `Supply`), meaning any program
  using modules written in different language versions of Perl 6 would end up
  passing around a medley of versions. This is most clearly problematic when
  considering the use of `CORE.setting` types as constraints. Should an
  `Array:ver<6.d>` refuse to bind to a parameter expecting `Array:ver<6.c>`
  (noting that in both cases the programmer would not have added the version,
  but rather referred to the one in scope)? In the general case of module
  versioning, it would seem useful for the answer to be "refuse" - totally
  fine for encapsulated dependencies. But for `Array`, used for interchange,
  that would lead to massive breakage when even upgrading modules that happen
  to have upgraded the language version they use - totally counterproductive to
  improving backward compatibility! As if that wasn't enough, consider how
  adding a method to `Any` would be handled. Would it imply a new version of
  all the subclasses of `Any` - that is, nearly *every* class - in the
  `CORE.setting`?
* Having different versions of classes, with the addition of automatic coercion
  between versions whenever crossing language version boundaries, has also
  been suggested. This is fairly easy to implement for immutable types such as
  `Int`. For mutable types, it's more difficult (consider mutual recursion
  between routines in different versions of the language, passing an `Array`
  down and mutating it at each level). For concurrent types, it's likely even
  harder. Then there's the performance profile. Such coercions would not be
  free, and would typically result in further allocations. Imagine 6.d were to
  add a new method on `Num`. Consider 6.c code iterating through an array of
  `Num`, passing them into a routine from a module written in `6.d`. Coercion
  at the boundary would result in an extra allocation per call. Upgrading
  language version, or upgrading a module that happens to also bump its chosen
  language version, could thus have surprising performance effects.
* A mechanism whereby a method could switch on the caller's version might also
  be considered. This is also a little fraught, though stands a higher chance
  of working than any of the preceding things. To see why "check the version
  of the caller" is not sufficient, consider the implementation of the `.+`
  dispatcher, which is in CORE.setting. The interesting caller, is the caller
  of the `.+` dispatcher. So, `CLIENT::<$?PERL>.version` or so may be better.
  That, however, would still be highly vulnerable in the face of MOP modules,
  such as `OO::Monitors`.

Since method calls are inherently late bound, it's not too surprising that
late bound solutions will be most practical for identifying code that uses
object behaviors "not yet available" in the declared language version. For
example, module release tooling could be developed to automatically build
a compiler known to have support for no language version higher than the one
used by the module, and run its tests. After all, the problem being addressed
here is not one of keeping existing code running. It's about supporting those
developers wishing to author modules that target a range of language versions.

### Dynamic (and thus PROCESS) variable change guidelines

Various dynamic variables are provided by Perl 6. Many of them from the `PROCESS`
pseudopackage, which is used as a fallback when searching for dynamic variables.
Examples include `$*OUT`, `$*REPO`, and `@*ARGS`. These may be overridden. For
example, it's fairly common to declare a `my $*OUT` for the purpose of capturing
output.

Dynamic variables are, by definition, not lexical, and so need a similar approach
to compatibility as objects. Of note:

1. The type of a dynamic variable may only change to a subclass of the type in a
   previous language version, in order that assignents of the dynamic to typed
   variables are not broken. This type change restriction taken together with the
   object change guidelines above provides interface compatibility.
2. `PROCESS` variables may be added. Implementations are not expected to try and
   restrict their availability to code in a previous language version, due to
   the global nature of `PROCESS`. Language designers should, however, be cautious
   when introducing *new* functionality that depends on resolving these new symbols
   through dynamic variables, since user code that happened to use that name may
   block correct resolution. Note that *existing* functionality should not come to
   resolve new dynamic variables (unless said new functionality is only exposed in
   a version-sensitive way).
3. `PROCESS` variables are nearly impossible to deprecate in an elegant way. If
   this must happen, the best bet is probably to have the compiler statically spot
   uses of it when parsing the language version(s) in which it is deprecated.

### Deprecation

The existing `is DEPRECATED` trait and its related mechanisms will be used for
handling deprecation. A deprecation must be introduced in a particular language
version, and an implementation **should not** report it for code using a
previous language version. For example, suppose 6.e were to deprecate feed
operators. Code that declared itself as version 6.e or above, or that declared
no version, would see the warning if it uses feed operators. Code that declared
itself as being in version 6.c, 6.d, 6.d.2, etc. should also not see the
deprecation warning.

This is relatively straightforward to deliver on for changes to the grammar
and changes to lexical things in `CORE.setting`, since the current effective
language is clear. For methods, however, it is more difficult - as discussed
earlier in these guidelines. Therefore, `is DEPRECATED` on methods in the
Perl 6 `CORE.setting` should:

1. Always specify a "since" version (when the deprecation became effective).
2. Check CLIENT::<$?PERL>.version against it, and only issue the deprecation
   warning if that is at least as high as the deprecation's "since" version.

This is an approximation, but hopefully close enough to provide sufficiently
useful warnings of upcoming deprecations.

In general, deprecation warnings should be in place for a whole major language
version. For example, if feed operators were deprecated in `6.e`, then they may
be removed in `6.f` at the earliest. It's undesirable to deprecate features in
minor language versions, though if that happens then it is advisable they
remain in place with the warning in the next major language version also.

