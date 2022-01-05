# Moving towards a better pre-comp and module management design

## Overview

This document contains my (jnthn) input to where implementation work for
Raku module/precomp stuff should head. While much has been converged on by
existing work, some problems have also been consistently avoided or punted
on; robust pre-compilation management is one such issue. Everything here is
subject to course corrections as implementation takes place, but it should
hopefully set out a good direction to move in. Also, don't expect this to be
a complete design. It's as much as I had time to figure out before I vanish
for a week on honeymoon, and I'm sharing it in its current state so that
others can carry it forward. Thanks goes to lizmat++ for highly valuable
input on an earlier private draft.

## Terminology

To keep discussion precise, here are some definitions of the atoms under
consideration:

* A **compilation unit**, or **compunit** for short, is a piece of Raku code
  that is analyzed and compiled as a single unit. Typically, this comes from a
  source file on disk, but what's inside an EVAL also counts as such.
* **Compilation** is the process by which a compilation unit is parsed and
  analyzed, and turned into a set of objects representing its declarations
  ("meta-objects") and executable code representing its statements. Note that,
  thanks to the many meta-programming features of Raku, compilation will
  involve the execution of code, some of which may come from the compilation
  unit being compiled (as happens with `BEGIN` blocks).
* A **script** refers to a compilation unit that is provided to Raku as the
  entry point for execution. In an invocation like `raku foo.raku`, we say that
  `foo.raku` is first compiled and then executed. In Rakudo Raku, in this case,
  the results of the compilation only exist in memory.
* A **module** refers to a compilation unit that is used by a script, or by
  another module used from a script. A module must also be compiled before it
  can be made use of. (There is nothing preventing a given source file serving
  as both a script and a module depending on how it is used.)
* A **distribution** is a set of zero or more scripts and modules that are
  released together, along with some meta-data and potentially resources and tests.

This "usage" of modules by scripts and other modules is complex, and is the
focus of much of this document. Some further terminology to enable precise
discussion of the area is useful:

* A **dependency specification** is how one module or script's need for
  another module is declared. In source code, it follows a `use` or `need`
  statement. An example is `Sereal:auth<cpan:*>:ver(1..*)`. The same syntax
  is used in the `depends` section of a `META6.json`.
* The **dependencies** of a script or module are identified during its
  compilation by evaluating the dependency specifications that follow its
  `use` or `need` statements.
* The **transitive dependencies** of a script or module is the union of its
  dependencies together with the transitive dependencies of each of those
  dependencies (`TRAN-DEP(m) = Union(DEP(m), TRAN-DEP(d) | d in DEP(m))`).
* The **reverse dependencies** of a module are those scripts and modules that
  have it as a dependency.
* The **transitive reverse dependencies** of a module is the union of its
  reverse dependencies together with the transitive reverse dependencies of
  those reverse dependencies (`TRAN-REV(m) = Union(REV(m), TRAN-REV(m) | m in
  REV(m))`).
* The **dependent distributions** of a distribution are those identified by
  evaluating the dependency specifications in a `META6.json`. The transitive,
  reverse, and reverse transitive relations can also be established.

In this document, the term "dependency" will always refer to a dependency
between compilation units, discovered at compile time of the depending
compilation unit. Those declared in `META6.json` are instead known as
distribution dependencies. They're very different concepts, and it's
important to keep them apart. The first is interesting for precompilation
management, and the latter for module installation tooling.

## Precompilation

Precompilation is a mechanism for *caching* the results of compilation on
disk, such that the overhead of compilation can be avoided in the future. The
precompilation of a compilation unit can only be formed if all of its
dependencies have already been precompiled (implying that at the point the
precompilation of a module is completed, all of its transitive dependencies
must have already been precompiled). Note this does not mean there is any
need for the dependencies to be precompiled before precompilation of the current compunit begins.
It is not only reasonable, but also desirable, for dependencies to be
precompiled "on demand" and recursively as they are discovered. That is,
you start precompiling at the "top" of a dependency graph and by the time
you've precompiled that module, the whole graph has been precompiled.

Since resolving dependency specifications to dependencies is a part of the
compilation process, a cached precompilation directly identifies the cached
precompilations it depends on. That is to say, the compiled effect of a
`use` or `need` statement is not to resolve a dependency specification, but
instead to identify a precise dependency to load. This in turn means that no
module database lookups are required. It also has the consequence that once
you load an existing precompilation, all module resolution from that point
on is "predestined". It's important, therefore, to ensure that precompilations
are tied to the entire set of "repositories" available for consideration at
the point they were formed.

Furthermore, precompilations are statically linked against the precompilations
of their transitive dependencies as well as against a particular compilation
of the Raku compiler and its `CORE.setting`. Therefore, the identity of the
Raku compiler - obtained through `$*RAKU.compiler.id` - should also be
considered part of the environment the precompilation was formed in. This will
also support `rakubrew` style tools, which enable switching between different
versions and backends.

In an ideal world, precompilation would always be possible for all compilation
units. In reality, some things simply won't work out. Stashing a file handle
in a BEGIN block and reading from it later won't work, because file handles
cannot be serialized. And trying to load the precompilation of two modules
that make incompatible `augment`ations to the same class can be expected to
fail. While we can culturally encourage writing `precompilation-clean` code,
it's also important to provide a mechanism whereby a compilation unit can opt
out. This in turn means that its transitive reverse dependencies can not be
precompiled. A compilation unit can declare it is not elligible for being
precompiled with:

    no precompilation;

## Repositories

Something that can locate and load modules, and potentially manage their
installation and precompilation, is known as a **repository** (or, more fully,
a compilation unit repository). A repository at its simplest could just map
module names to source files on disk. A more complex repository might support
installation of a distribution's modules along with associated resources, as
well as managing precompilation of modules.

Repositories are structured as a linked list - that is, a repository may refer
to another. A repository can always provide its unique identity, which must
incorporate the identity of any repository it refers to. In normal startup, the
`PROCESS::<$REPO>` symbol will be set to a default repository that supports the
installation of distributions (a `CompUnit::Repository::Installation`). Any
`-I` includes, or any paths in a `RAKULIB` environment variable, will cause
`PROCESS::<$REPO>` to instead point to a chain of repositories that ends with
the default `CompUnit::Repository::Installation` that is normally there.

A `use lib` installs a lexical `$*REPO` which takes precedence over that in
`PROCESS`. Note that for Raku.christmas, we will only support the use of
`use lib` in scripts, not in modules, as its interaction with precompilation
is more complex than we have time to reasonably consider (and it's better to
wait until we've a good answer than to use a hacky one now).

This linked list replaces `@?INC`. There are a number of benefits to this
design:

* Precompilations **must** factor in the whole set of repositories that could
  have been considered when resolving a dependency specification. To see why,
  consider an installed module M that contains `use N`. Its precompilation
  will have been done at installation time, and so only considered other
  installed modules `N`. Later, the developer of `N` is developing a new
  version, and uses `-Ilib` to ensure this new `N` is used. His test script
  does `use M`, and the expectation would be that the `N` is `lib` is loaded.
  However, if we hit the precomp of `M`, it has already committed to an
  installed `N`. For correct behavior, the repository for `-Ilib` - assuming
  it supports precompilation management - must itself manage precompilations
  for the whole transitive dependency chain. Put another way, only the module
  precompilations stored by the repository at the head of the chain are valid.
  This is most easily delivered by the head of the chain passing its precomp
  repository "down the chain".
* You can trivially implement a repository that gives you a "clean room" with
  no system-wide modules just by it never delegating to the next thing in the
  chain.
* If you want "parallel" consideration of a set of other repositories with no
  ambiguities allowed across them, or "sequential" consideration of a set of
  repositories with the first providing a resolution winning, these can both
  be provided by a repository that contains an array of other repositories and
  delegates as needed.
* It's easier to test repository implementations in isolation if they're not
  tied up with some global state, like an `@?INC`.

## Module management API

The module management API can be broken up into:

* **Guts** provided by a Raku implementation (e.g. Rakudo) that do the
  various low-level tasks.
* **Entities** that capture the information associated with concepts such as
  "dependency specification" and "compilation unit".
* **Roles** for the various aspects of module management. Some of these are
  pure interfaces (that is, entirely required methods). Others provide default
  implementations that will usually be sufficient, and just need some details
  to be filled in.
* **Provided implementations** of those interfaces for a number of common use
  cases.

Alternative implementations of the roles to handle use cases beyond what Raku
provides direct support for are both expected and encouraged.

### Guts

#### CompUnit::Loader

The `CompUnit::Loader` is responsible for actually loading a compilation unit
into memory, either from source or a precompiled representation. It can work
with both files and in-memory byte buffers in either case. Implementations are
free to efficiently `mmap` files into memory, allowing a single copy of a
precompiled module to exist in memory and be shared by many Raku processes.
The methods on this are all expected to be called on the `CompUnit::Loader`
type object.

    class CompUnit::Loader is repr('Uninstantiable') {
        # Load a file from source and compile it
        method load-source-file(Str $path) returns CompUnit::Handle {
             ...
        }

        # Decode the specified byte buffer as source code, and compile it
        method load-source(Buf:D $bytes) returns CompUnit::Handle {
            ...
        }

        # Load a pre-compiled file
        method load-precompilation-file(Str $path) returns CompUnit::Handle {
            ...
        }

        # Load the specified byte buffer as if it was the contents of a
        # precompiled file
        method load-precompilation(Buf:D $bytes) returns CompUnit::Handle {
            ... # XXX this one needs MoarVM/JVM backends to expose a new API
        }
    }

The `CompUnit::Loader` class only expects to be asked to load a given source
or precompiled file into memory once. Asking it to load the same pre-compiled
file twice is erroneous. Provided this is respected, concurrent calls to the
methods of `CompUnit::Loader` are allowed.

#### CompUnit::Handle

The `CompUnit::Handle` class is a handle to a loaded compilation unit. Its
exact internals are implementation defined, but it can be expected to have
the following methods:

    class CompUnit::Handle {
        # If the compilation unit has a callable EXPORT subroutine, it will
        # be returned here. A Callable type object otherwise.
        method export-sub() returns Callable {
            ...
        }

        # The EXPORT package from the UNIT of the compilation unit; a
        # Stash type object if none
        method export-package() returns Stash {
            ...
        }

        # The EXPORTHOW package from the UNIT of the compilation unit;
        # a Stash type object if none.
        method export-how-package() returns Stash {
            ...
        }

        # The GLOBALish package from the UNIT of the compilation unit
        # (the module's contributions to GLOBAL, for merging); a Stash
        # type object if none.
        method globalish-package() returns Stash {
            ...
        }
    }

Since these methods are all reads, it is safe for them to be called
concurrently on the same instance. (If the internal implementation is not
automatically threadsafe, it must do appropriate concurrency control, so
consumers of this class don't have to worry.)

#### Importation/Global Merging

We'll likely end up wanting to factor this into some guts-providing class too,
to be discovered/defined during implementation.

### Entities

#### CompUnit::DependencySpecification

A dependency specification consists of a short name for a module (for example,
`Grammar::Tracer`), and optionally a version matcher and auth matcher (which
will be smart-matched against the version and authority of a potential
dependency to see if it is satisfactory).

    class CompUnit::DependencySpecification {
        has Str:D $.short-name is required;
        has $version-matcher = True;
        has $auth-matcher = True;
    }

#### CompUnit

A compilation unit is represented as follows:

    class CompUnit {
        # The CompUnit::Repository that loaded this CompUnit.
        has CompUnit::Repository $.repo is required;

        # That repository's identifier for the compilation unit. This is not
        # globally unique.
        has Str:D $.repo-id is required;

        # The low-level handle.
        has CompUnit::Handle $.handle is required;

        # The short name, version, and auth of the compilation unit, if known.
        has $.short-name;
        has $.version;
        has $.auth;

        # Whether the module was loaded from a precompilation or not.
        has Bool $.precompiled = False;

        # The distribution that this compilation unit was installed as part of
        # (if known).
        has Distribution $.distribution;
    }

#### Distribution

To be more fully defined, but consists of the metadata from a META6.info and
a way to reach any resources declared within it.

### Roles

#### CompUnit::PrecompilationStore

A precompilation store provides storage of pre-compiled things. It is not
concerned with precompilation validity, just storage. Most of the time, the
Raku built-in implementation that stores precompiled files on disk will be
sufficient. However, a tool that wished to bundle a Raku implementation
together with a bunch of precompiled scripts/modules for distribution would
do an alternative implementation of this role.

    subset CompUnit::PrecompilationId of Str:D
        where { 2 < .chars < 64 && /^<[A..Za..z0..9_]>$/ };
    role CompUnit::PrecompilationStore {
        # Load the precompilation identified by the pairing of the specified
        # compiler and precompilation ID.
        method load(CompUnit::PrecompilationId $compiler-id,
                    CompUnit::PrecompilationId $precomp-id)
                    { ... }

        # Store the file at the specified path in the precompilation store,
        # under the given compiler ID and precompilation ID.
        method store(CompUnit::PrecompilationId $compiler-id,
                     CompUnit::PrecompilationId $precomp-id,
                     Str:D $path)
                     { ... }

        # Delete an individual precompilation.
        method delete(CompUnit::PrecompilationId $compiler-id,
                      CompUnit::PrecompilationId $precomp-id)
                      { ... }

        # Delete all precompilations for a particular compiler.
        method delete-by-compiler(CompUnit::PrecompilationId $compiler-id)
            { ... }
    }

The `CompUnit::PrecompilationId` subset type restricts the identifiers that
can be passed to things that should be valid on any even slightly modern file
system.

#### CompUnit::PrecompilationRepository

A precompilation manager is responsible the creation, location, and validity
testing of precompiled modules. The only thing it's not concerned with is
their actual storage. Therefore, a precompilation repository will typically
be configured with an implementation of `CompUnit::PrecompilationStore`.

#### CompUnit::Repository

A class implementing the `CompUnit::Repository` role knows how to resolve a
dependency specification to a concrete dependency and load it. It can also
provide a list of all the compilation units it has loaded. Implementations of
this role should take care to cache dependencies they already loaded, so the
same `CompUnit` instance can be handed back each time the same dependency is
requested. Further, implementations of `CompUnit::Repository` should cope with
concurrent calls.

The `CompUnit::Repository` role is defined as follows:

    role CompUnit::Repository {
        # Resolves a dependency specification to a concrete dependency. If the
        # dependency was not already loaded, loads it. Returns a CompUnit
        # object that represents the selected dependency. If there is no
        # matching dependency, throws X::CompUnit::UnsatisfiedDependency.
        method need(CompUnit::DependencySpecification $spec,
                    # If we're first in the chain, our precomp repo is
                    # the chosen one.
                    CompUnit::PrecompilationRepository $precomp =
                        self.precomp-repository())
            returns CompUnit:D
            { ... }

        # Returns the CompUnit objects describing all of the compilation
        # units that have been loaded by this repository in the current
        # process.
        method loaded()
            returns Iterable
            { ... }

        method precomp-repository()
            returns CompUnit::PrecompilationRepository
            { CompUnit::PrecompilationRepository::None }
    }

The `X::CompUnit::UnsatisfiedDependency` type is defiend as:

    class X::CompUnit::UnsatisfiedDependency is Exception {
        has DependencySpecification $.specification;
        method message() { ... }
    }

If there is no unique module that can be considered the "best" given the
specification, an `X::CompUnit::AmbiguousDependencySpecification` exception
will be thrown:

    class X::CompUnit::AmbiguousDependencySpecification is Exception {
        has DependencySpecification $.specification;
        has @.ambiguous;
        method message() { ... }
    }

#### CompUnit::Repository::Installable

A compilation unit repository that supports installation of distributions
should implement the following role:

    role CompUnit::Repository::Installable does CompUnit::Repository {
        # Installs a distribution into the repository.
        method install(
            # A Distribution object
            Distribution $dist,
            # A hash mapping entries in `provides` to a disk location that
            # holds the source files; they will be copied (and may also be
            # precompiled by some CompUnit::Repository implementations).
            %sources,
            # A hash mapping entries in the `resources` to a disk location
            # that holds the files; again, these will be copied and stored.
            %resources)
            { ... }

        # Returns True if we can install modules (this will typically do a
        # .w check on the module database).
        method can-install() returns Bool { ... }

        # Returns the Distribution objects for all installed distributions.
        method installed() returns Iterable { ... }
    }

### Implementations

#### CompUnit::PrecompilationStore::File

An implementation of a precompilation store that stores the files on disk. It
expects to be constructed with a prefix.

    my $psf = CompUnit::PrecompilationStore::File.new(
        prefix => "$path/.precomp"
    );

Its directory layout is straightforward: one directory per compiler ID, which
contains subdirectories named for the first two letters of the precompilation
ID, which in turn contain the files. So:

    $psf.store('comp-1', 'deafbeef');
    $psf.store('comp-2', 'deafbeef');
    $psf.store('comp-1', 'baadf00d');

Would create:

    $path/.precomp/comp-1/ba/baadf00d
    $path/.precomp/comp-1/de/deadbeef
    $path/.precomp/comp-2/de/deadbeef

Those used to Git internals will note this is the same structure as it uses
for loose objects.

#### CompUnit::Repository::Installation

The installation compilation unit repository supports both installation and
precompilation. It expects to be configured with a directory, where it will
keep both original sources and precompilations.

    class CompUnit::Repository::Installation
            does CompUnit::Repository::Installable {
        has $.prefix is required;

        ...
    }

This repository expects that any changes to modules installed through it will
be done directly using it, and so any changes to module sources done directly
will not be taken into account. Under its prefix, an installation repository
establishes the following structure:

    repo.lock         # A lock file
    dist/[sha1]       # JSON-serialized distribution info (SHA-1 of dist ID)
    sources/[index]   # Module source files, by ascending ID
    resources/[index] # Module resource files, by ascending ID
    short/[sha1]      # Short-name quick lookup file by sha1 of the shortname
    precomp/...       # Precompilation store
    dependencies      # Pairs of short-name to short-name SHA-1s
    HEAD              # Current identifier

When we install a distribution, we do the following:

1. Create the `repo.lock` file; if it already exists, fail.
2. Create a SHA-1 of the unique identifier for the distribution, and check it
   does not already exist in `dist`; delete lock file and fail if so.
2. Copy each of the source files into `sources`, allocating each one an
   ascending ID.
3. Copy each of the resource files into `resources`, allocating each one an
   ascending ID.
4. Update the Distribution object with this logical name => ID mapping.
5. Serialize the Distribution object and store it into `dist`, using the SHA-1
   of its unique identifier for the file name.
6. XXX precomp newly added source files
7. Load the dependencies file, and compute the transitive closure of short
   name relations from it.
8. Take the provides section of the distribution, SHA-1 all the short names,
   and then find the the transitive reverse dependencies on those short names.
   This gives the set of modules whose precompilations will be invalidated by
   the new modules.
9. XXX precompile all of those impacted modules
10. Insert any new dependencies into the shortname dependency graph, and save
   it back to disk.
11. Create/update the shortcut file for quick module resolution.
11. Delete the `repo.lock` file.

If a `CompUnit::Repository::Installation` is not the final repository in a
linked list, then it must also be sure to invalidate its precompilations if
the identity of the next repository in the chain changes. (Since the usual
case is per-user modules, it's reasonable to assume there will be write
access to the precomp directory to update it).

Further, when queried for a module, a `CompUnit::Repository::Installation`
that is followed in the chain by another `CompUnit::Repository::Installation`
should consider its set of modules together with its own, such that the best
option across the two of them wins (and if there are ambiguities, the nesting
level does not count as a resolution). (Conjectural: we could have a
CompUnit::Repository::Installation::Override that always considers itself to
know better, and only delegates if it can't do better.)

XXX The exact factoring here may want a little futher explanation.

#### CompUnit::Repository::FileSystem

The file system compilation unit repository is used when the `-I` flag is
specified. It is initialized with a prefix. It assumes it will be able to
create and write to a `.precomp` directory beneath that path. When it is
asked for a module, it first checks if it has a precompilation for it. If
so, it ensures it is still valid. The validy check involves:

* Checking the identity of the next repository in the chain
* Checking the modification time of the module itself
* Checking the modification times of the transitive dependencies of that
  module that live within this CompUnitRepo::FileSystem

A `CompUnit::Repository::FileSystem` always tries to satisfy a request for a
module first, and only delegates if it is unable. It also doesn't care for
versioning or authority.

    class CompUnit::Repository::FileSystem does CompUnit::Repository {
        has $.prefix is required;

        ...
    }

## Questions and, if you're lucky, answers

### Where are libraries installed?

System-wide modules go in a path derived from the `--prefix` that Rakudo is
built with. The `Configure.pl` script can also be given a `--module-prefix`,
which will override this. Tools like rakubrew will likely wish to specify
a single common `--module-prefix` so modules are shared between the things
they will switch between. This directory will be managed by an instance of
`CompUnitRepo::Installation`.

A user's local module installations go into a `.perl6` in their home
directory, with precompilations under it in `.perl6/precomp/`. This will be
managed by a `CompUnit::Repository::Installation` (which will delegate to
the system-wide `CompUnit::Repository::Installation`).

### What about `@?INC`?

It's gone.

### How does an installer choose the right target repository?

Good question. Perhaps there should be a lookup mechanism of installation
repositories by some kind of identifier ("system", "user", etc.)

### What about `%?CUSTOM_LIB`?

Probably also gone, though probably also partly covered by whatever we build
to satisfy the previous question.
