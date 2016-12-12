# Proposal for 6.c release and beyond

For review, nothing set in sand yet (and these things should never be set in
stone; we need to keep learning and adapting them over time).

## Rakudo branches

We want to provide a little more stability than we attain today in terms of
releases, but at the same time don't want to slow down development notably.
Therefore:

* Development work will continue on the nom branch (which we might rename,
  but it'll end up bikeshed to death and it's in muscle memory, so we'll
  probably leave it as is.)
* Commit policy on "nom" is pretty much as today
* The camelia bot will build from "nom"
* A "master" branch will be created and will become the default branch
  people pull, that things like rakudobrew build if asked to get the
  latest non-release, and that we release from
* An automated process will fast-forward master to catch up with nom at
  regular intervals, provided it meets a bunch of automated quality checks

These automated quality checks are as follows:

* Clean test/spectest on Moar
* Clean test/spectest on JVM
* Panda bootstrap works
* We'll identify and snapshot a selection of modules (around 100) that have
  passing tests. We should be able to build and pass tests on this selection
  (showing we didn't regress on a bunch of ecosystem things). Note that we
  snapshot them so as to avoid issues with changes in the modules themselves
  that cause failures that are not Rakudo bugs. We'll revise this set every
  so often to remain representative.

GitHub doesn't let us configure per-branch permissions, so we can't prevent
pushes to master that way. But we can have a pre-push hook to try and prevent
mistakes.

## Releases

Releases will be cut from master. The release manager will have the advantage
of knowing they are releasing something that has already passed a bunch of
automated quality checks. The release process will be something like:

* Create a release branch based off master
* Do release-related commits in the branch (announcement, last change log
  updates, bump VERSION, etc.)
* Cut the release (produce the tarball, etc.)
* Tag the released commit
* Merge the release branch into nom (so it will end up merged into master
  also later)

We can script some of this (the branch creation, stubbing announcement,
bumping VERSION, and then another script to do the merge back into nom
afterwards).

## Spectests

The Perl 6 language is defined by its test suite. A particular release of the
language is, therefore, a release of the test suite. To make sure we don't
regress on things that we decided are part of the 6.c language, we need to
make sure that we don't unknowingly change such tests.

One way to do this is:

1. Create a 6c directory in roast.
2. Create a speculation directory in roast.
3. Look through spectest.data for tests we run, but that we don't think are
   part of 6.c (the macros tests are such an example: we know we do not want
   to commit to the current implementation.) Move those tests into the
   speculation directory (retaining the Snn-foo directory structure).
4. Move all remaining unfudged test files that we run in t/spectest.data on
   Moar into the 6c directory (again, retaining Snn-foo structure).
5. Split the Moar-fudged test files up into the fudged tests (which go in
   speculation) and the passing ones (which go into 6c).

Then, it will be really, really obvious is we go modifying a test that was in
the 6.c release (which we almost certainly shouldn't do most of the time; at
most we may add a deprecated marker if we decided that, say, 6.f is not going
to pass this test any more).

## Language versioning

The Perl 6 language has version numbers v6.c, v6.d, v6.e, and so forth. These
are very much like C has C89, C99, and C11. Each language version is defined
by its test suite.

Rakudo is a Perl 6 implementation, as GCC, Clang, and MSVC are implementations
of the C language. Rakudo has monthly releases (2015.12, 2016.01, etc.) For
the moment, we don't make any distinction between these releases; they are all
equally "supported".

Each Rakudo release must indicate the maximum version of Perl 6 it supports,
and this should be included prominently in the release announcement, for
example in the title, such as: "Rakudo 2016.01 (implements Perl 6.c)".

A Perl 6 source file without a "use v???" line will be run at the latest
language version that the current implementation supports (as will -e and
the REPL). A source file with "use v6.X" (where X is meta) should be
treated as follows:

* If X is higher than the implementation currently supports, then it should
  refuse to go any further.
* If X is lower than the implementation supports, it should also refuse to
  go any further.
* Otherwise, it enables everything that should be available at that language
  version.
* A straight "use v6" at the top level is equivalent to "use v6.c". However,
  a "use v6" after a "use v6.d" implies 6.d. This is to facilitate language
  version switching between 5/6 ala. the v5 module.
* The spectests for 6.c should all "use v6.c"

The way we choose to support different versions will evolve over time, as we
(hopefully) get collectively smart enough to figure it out. :-) But roughly,
we'll do something like this:

* For added syntax, the grammar should check the language version in an
  assertion
* Additions to the setting will be made in a "nested" setting. That is, a
  "use v6.d" will load CORE.d.setting or so. It can use augment and
  supercede in order to effect changes.

To facilitate this, for now at least, we'll require that a "use v6.X" is the
first non-comment, non-whitespace declaration in a file. Anything later will
trigger a "too late to switch Perl 6 language version".

## Experimental features

Experimental features can be turned on with a pragma:

    use experimental :macros;

These are implementation specific and can go away at any time, though we'll
likely be kind enough to give deprecation cycles.

## Trying out the "next version"

To try out v6.d before the compiler officialy supports it, you can write:

    use v6.d.PREVIEW;

## If anyone asks about 6.d and beyond...

About Perl 6.d, expect an incremental update.

* It'll focus heavily on broadening test coverage to tighten up the language
  definition
* It will include various small things we wished we could have done in 6.c,
  but just didn't have time: the missing regex backtracking controls, the
  sub-byte int types, non-parameter cases of coercion types, things we
  discover people miss especially in IO, etc.
* Goal is sometime in 2016; we'll judge it based upon what we see (like, if
  there is a strong desire to get an incremental update out the door to cover
  things people really block on not having, we can do so)

A lot of effort in 2016 will go on performance engineering and making things
more robust. Macros and slangs are the biggest post-6.c project language wise;
if they happen to be in great shape by 6.d then they can make it in, but if
not they'd be a reasonable target for 6.e.
