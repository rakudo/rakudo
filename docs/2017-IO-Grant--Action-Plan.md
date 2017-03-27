# Table of Contents
- [IO Action Plan](#io-action-plan)
    - [Terms and Conventions](#terms-and-conventions)
    - [Legend](#legend)
- [Non-Conflicting Improvements](#non-conflicting-improvements)
    - [`IO::Handle`'s Closed status](#iohandles-closed-status-issue-for-discussion)
    - [Restructure `spurt`](#restructure-spurt-issue-for-discussion)
    - [`IO::Path` routines that involve a stat call](#iopath-routines-that-involve-a-stat-call-issue-for-discussion)
    - [Expand Features of `IO::Path.extension`](#expand-features-of-iopathextension-issue-for-discussion)
    - [Use typed exceptions instead of `X::AdHoc`](#use-typed-exceptions-instead-of-xadhoc-issue-for-discussion)
    - [Make `IO::Path.resolve` fail if it can't resolve path](#make-iopathresolve-fail-if-it-cant-resolve-path-issue-for-discussion)
    - [Make `&words` default to `$*ARGFILES`](#make-words-default-to-argfiles-issue-for-discussion)
- [Changes with Backwards-Compatible Support](#changes-with-backwards-compatible-support)
    - [`IO::Handle.seek` seek reference](#iohandleseek-seek-reference-issue-for-discussion)
    - [Rename `IO::Handle.slurp-rest` to just `.slurp`](#rename-iohandleslurp-rest-to-just-slurp-issue-for-discussion)
    - [`:$test` parameter on multiple routines](#test-parameter-on-multiple-routines-issue-for-discussion)
- [Changes with No Backwards-Compatible Support](#changes-with-no-backwards-compatible-support)
    - [Generalize `IO::ArgFiles` into `IO::Cat`](#generalize-ioargfiles-into-iocat-issue-for-discussion)
    - [Changes to `.Supply`](#changes-to-supply-issue-for-discussion)
    - [Make `IO::Path.abspath` a private method](#make-iopathabspath-a-private-method-issue-for-discussion)
    - [Make `IO::Path.child` fail for non-child paths / Add `IO::Path.concat-with`](#make-iopathchild-fail-for-non-child-paths--add-iopathconcat-with-issue-for-discussion)
    - [Make `:close` behaviour the default in `IO::Handle` and Its Subclasses](#make-close-behaviour-the-default-in-iohandle-and-its-subclasses-issue-for-discussion)
    - [Changes to behaviour of `.lines`, `.words`, `.split`, `.comb`](#changes-to-behaviour-of-lines-words-split-comb-issue-for-discussion)
    - [Change order of arguments in `&link`/`&symlink`](#change-order-of-arguments-in-linksymlink-issue-for-discussion)
    - [Improve `IO::Handle.lock` Arguments](#improve-iohandlelock-arguments-issue-for-discussion)
    - [Make `IO::Path.new-from-absolute-path` a private method](#make-iopathnew-from-absolute-path-a-private-method-issue-for-discussion)
- [Controversial Changes](#controversial-changes)
    - [Make `IO::Path.Str` Return `$.abspath`](#make-iopathstr-return-abspath-issue-for-discussion)
    - [Make `IO::Path.is-absolute` Give False for `/` path on Windows](#make-iopathis-absolute-give-false-for--path-on-windows-issue-for-discussion)
- [Removals](#removals)
    - [Remove `role IO {}` Along With Its Only `IO.umask` Method](#remove-role-io--along-with-its-only-ioumask-method-issue-for-discussion)
    - [Remove `IO::Path` Methods from `IO::Handle`](#remove-iopath-methods-from-iohandle-issue-for-discussion)
    - [Remove `&homedir`](#remove-homedir-issue-for-discussion)
    - [Remove `&tmpdir`](#remove-tmpdir-issue-for-discussion)
- [Bug Fixes](#bug-fixes)
    - [RT Tickets](#rt-tickets)
    - [GitHub Issues](#github-issues)
    - [Other Issues](#other-issues)
- [New Issues](#new-issues)
# IO Action Plan

This document is a deliverable of [TPF Standardization, Test Coverage, and
Documentation of Perl 6 I/O Routines
grant](http://news.perlfoundation.org/2017/01/grant-proposal-standardization.html)
and describes the proposed changes to the implementations of Rakudo's IO
routines.

**Each topic has an `[Issue for discussion]` link in the title that points to
a GitHub Issue where the topic can be discussed.**

---------------

## Terms and Conventions

- `routines` — means both subroutines and methods (as both are `Routine` types)
- `IO::Handle.seek` / `.seek` — all method names are referenced with a `.`
before their name, which is optionally preceded by the type.
- `&seek` — all subroutine names are referenced with a `&` before their name.
- `seek` — if neither a `.` nor `&` preceeds the name, the reference is to
    both subroutines and methods with that name
- `$*SPEC` — functionality common to `IO::Spec::*` types, is referred to using
`$*SPEC` variable and means all `IO::Spec::*` types that implement a method or
feature proposed for change.

## Legend

Some sections are marked as:

- [✔️] docs
- [✔️] master roast
- [✘] 6.c-errata roast

This indicates whether the **proposed change** affects something that
*is* (`[✔️]`) or *isn't* (`[✘]`) documented on [docs.perl6.org](https://docs.perl6.org),
tested in [master branch of roast](https://github.com/perl6/roast), or tested in
[6.c-errata branch of roast](https://github.com/perl6/roast/tree/6.c-errata).
Just `[✘] roast` means both `master` and `6.c-errata` branches.

To re-iterate: the indicators are only for the *proposed changes* (e.g. removed
argument); not *entire* routines.


------------------------------

# Non-Conflicting Improvements

The changes proposed in this section do not change current behaviour and merely
enhance it or add new, non-conflicting features to.


------------------------------

## `IO::Handle`'s Closed status [[Issue for discussion]](https://github.com/zoffixznet/IOwesomeness/issues/2)

**Current Behaviour:**
- When a IO::Handle is closed, its $!PIO atribute is set to nqp::null. This
causes calls to many methods on a closed file handle to return LTA error,
such as `foo requires an object with REPR MVMOSHandle`.

**Proposed Change:**
- On handle's closing, mixin a role that overrides all the relevant methods
to throw/fail with an error. This will give the same behaviour as adding
`if nqp::isnull($!PIO) { ... throw ... }` to all the methods, without a
performance impact to open handles (it was ~5% when such check was added to
`.lines` iterator)


------------------------------

## Restructure `spurt` [[Issue for discussion]](https://github.com/zoffixznet/IOwesomeness/issues/3)

- [✔️] docs (with several inaccuracies)
- [✘] roast

**Current Behaviour:**
- `IO::Path` implements `.spurt` which is the only writing method that's
present in `IO::Path` but is not present in `IO::Handle`
- `:bin` parameter on `&spurt` / `IO::Path.spurt` is taken and documented, but
is ignored. The binary mode is enabled based on whether or not the spurted
`$content` is a `Blob`.
- The docs lie about `&spurt` being able to take an `IO::Handle`

**Proposed Change:**
- Move `IO::Path.spurt` implementation to `IO::Handle`. Remove all of its
parameters, except for a single positional `Cool:D` parameter.
    - the bin/non-bin mode is selected based on whether the handle is in
        bin/non-bin mode    
- Make `IO::Path.spurt` delegate to `IO::Handle`
    - Remove `:bin` argument and ascertain the spurt mode based on the type
        of the content to be spurted
    - The rest of the arguments are to be used in the `IO::Handle.open` call,
        with the `:createonly` argument being an alias for `:x` open mode.


------------------------------

## `IO::Path` routines that involve a stat call [[Issue for discussion]](https://github.com/zoffixznet/IOwesomeness/issues/4)

**Affected Routines:**
- `.d`
- `.f`
- `.l`
- `.r`
- `.s`
- `.w`
- `.x`
- `.z`
- `.rw`
- `.rwx`
- `.modified`
- `.accessed`
- `.changed`
- `.mode`

**Current Behaviour:**

Each test goes out to VM to perform several `stat` calls (other than `.e` that
performs just one). For example, a single `.rwx` call performs 4 `stat` calls.
Based on IRC conversation, `stat` call is expensive and caching its results
can be beneficial.

**Proposed Change:**

Change `nqp::const::STAT*` constants to be powers of 2. Add
`nqp::stat_multi` op that will take bitwise-ORed `nqp::const::STAT*` constants
representing the pieces of stat information to be returned as a hash. This
will let us perform a single stat call to fetch all of the required information.

(related discussion: https://irclog.perlgeek.de/perl6-dev/2017-03-06#i_14213924)


------------------------------

## Expand Features of `IO::Path.extension` [[Issue for discussion]](https://github.com/zoffixznet/IOwesomeness/issues/5)

**Current Behaviour:**

It's not uncommon to see users asking on IRC how to
obtain or modify an extension of a path. Depending on what is needed, the answer
is usually a gnarly-looking `.subst` or `.split`+`.join` invocation. In
addition, often the desired extension for files like `foo.tar.gz` would be
`tar.gz`, yet the current `.extension` does not offer a means to obtain it.

**Proposed Change:**

- Add `uint :$parts = 1` named parameter that specifies how many parts (the
    `.whatever` segments, counting from end) to consider as the extension.
    That is `'foo.tar.gz'.IO.extension` returns `'gz'`,
    `'foo.tar.gz'.IO.extension: :2parts` returns `'tar.gz'`, and
    `'foo.tar.gz'.IO.extension: :3parts` returns `''` (signaling there is no
        (3-part) extension on the file).

    In the future we can extend this to take a `Range` or `Junction` of values,
    but for now, just a single `UInt` should be sufficient. The default value
    of `1` preserves the current behaviour of the routine. Value of `0` always
    makes routine return an empty string.
- Add a candidate that accepts a positional `$replacement` argument. This
    candidate will return a new `IO::Path` object, with the extension
    changed to the the `$replacement`. The user can set the `:parts` argument
    to `0` if they want to *append* an extra extension. The operation is
    equivalent to:

    ```perl6
        my $new-ext = 'txt';
        say (.substr(0, .chars - .extension.chars) ~ $new-ext).IO
            with 'foo.tar.gz'.IO
        # OUTPUT: «"foo.tar.txt".IO␤»
    ```

    Note: since `.extension` returns the extension without the leading dot,
    the replacement string does not have it either. However, since the users
    may be inclined to include it, we should warn if it is included.


------------------------------

## Use typed exceptions instead of `X::AdHoc` [[Issue for discussion]](https://github.com/zoffixznet/IOwesomeness/issues/6)

**Current Behaviour:**
- Some IO exceptions are generic, `X::AdHoc` type of exceptions.

**Proposed Change:**
- Use existing and create new, if needed, exceptions, all living in
`X::IO::*` namespace.


------------------------------

## Make `IO::Path.resolve` fail if it can't resolve path [[Issue for discussion]](https://github.com/zoffixznet/IOwesomeness/issues/7)

**Current behaviour:**

`.resolve` will attempt to access the filesystem and resolve all the links,
but will stop resolving as soon as it hits a non-existent path; all further
parts will be merely cleaned up (e.g. `foo///../` will have duplicated slashes
removed, but the `../` part will remain).

**Proposed behaviour:**

Add `Bool :$completely` parameter that, when specified as `True`, will cause
`.resolve` to `fail` if cannot fully resolve the path.


------------------------------

## Make `&words` default to `$*ARGFILES` [[Issue for discussion]](https://github.com/zoffixznet/IOwesomeness/issues/8)

**Current behaviour:**

`&slurp`, `&lines`, `&get`, and `&getc`
(or, semantically, "whole thing", "all lines", "one line",
and "one char") default to using `$*ARGFILES`. `&words` (or "all words")
exceptionally doesn't and throws instead.

**Proposed behaviour:**

Make `&words` default to `$*ARGFILES`, just like the rest of the routines
in this family.


------------------------------

# Changes with Backwards-Compatible Support

The changes proposed in this section allow for retention of old behaviour.
It is proposed the old behaviour to be removed entirely in 6.d language.

Note: since our current infrastructure for 6.d language is **additive,** the
behaviour proposed for removal in 6.d might not end up actually being removed
from the code, but instead marked as deprecated with warnings/exceptions issued
on use (and appropriately documented as being obsolete).


------------------------------

## `IO::Handle.seek` seek reference [[Issue for discussion]](https://github.com/zoffixznet/IOwesomeness/issues/1)

- [✔️] docs
- [✔️] master roast
- [✘] 6.c-errata roast

**Current behaviour**:
- The seek reference is taken as an enum which is globally available and is
somewhat long to type: `SeekFromBeginning`, `SeekFromCurrent`, `SeekFromEnd`.
Such type of arguments is rare in the rest of the language and the enums
prevent parenthesis-less subroutine calls of the same name.

**Proposed change**:
- Use mutually exclusive named arguments instead: `:from-start`,
`:from-current`, and `:from-end`. The old enums will be kept in 6.c language and
will be removed in 6.d.


------------------------------

## Rename `IO::Handle.slurp-rest` to just `.slurp` [[Issue for discussion]](https://github.com/zoffixznet/IOwesomeness/issues/9)

- [✔️] docs
- [✔️] roast

**Current behaviour**:

There are thematically related routines: `comb` (all the characters),
`words` (all the words), `lines` (all the lines), and `slurp` (all the stuff).
All but the last are present as subroutines and as methods on `IO::Path`,
`IO::ArgFiles`, `IO::Path`, and `IO::Pipe`.

With respect to `&slurp`, there is sub `&slurp` and method `.slurp` on `IO::Path` and `IO::ArgFiles`. The `IO::Handle` and `IO::Pipe` name it
`.slurp-rest` instead. Since `IO::ArgFiles is IO::Handle`, it also has a
`.slurp-rest`, but it's broken and unusable.

**Proposed change**:

Rename `.slurp-rest` to just `.slurp` in 6.d language and make use of
`.slurp-rest` issue a deprecation warning.

I can surmise the alternate name in `IO::Handle` and its subclasses was meant to
be indicative that `.slurp-rest` only slurps **from the current file position**.
However, this caveat applies to every single read method in `IO::Handle`:
`.slurp`, `.lines`, `.words`, `.comb`, `.get`, `.getc`, `.getchars`. Yet, all
but `.slurp` do not have `-rest` in their name, as the behaviour is implied.
In fact, `Seq`-returning methods even interact with each other when called
on the same handle, as the file position being advanced by one iterator affects
what the next item in another `Seq` will be. Thus, knowledge about the effect of
file position on all of these methods is *essential,* and so requiring all users
to use a longer name as a reminder lest they forget about this behaviour
in slurps is antisocial.

The longer name is even more bizarre in `IO::Pipe`, on which `.seek` cannot be
used.


------------------------------

## `:$test` parameter on multiple routines [[Issue for discussion]](https://github.com/zoffixznet/IOwesomeness/issues/10)

**Affected routines:**
- `IO::Path.chdir` / `&chdir`
- `&indir`
- `&homedir` *(proposed for removal)*
- `&tmpdir` *(proposed for removal)*

**Current behaviour:**

The affected routines take `:$test` parameter as a string (or
`Positional` that's later stringified) of tests to perform on a directory.
It's difficult to remember the correct order and currently it's very easy
to give an argument that will incorrectly report the directory as failing the
test: `chdir "/tmp/", :test<r w>` succeeds, while `:test<rw>` or `:test<w r`>
fail.

**Proposed behaviour:**

It is proposed the `:$test` parameter to be replaced with 4 boolean named
parameters `:$r, :$w, :$x, :$d`, with `:$d` (is it directory) test to be
enabled by default. Usage then becomes:

```perl6
    # BEFORE:
    indir :test<r w x>, '/tmp/foo', { dir.say } # Good
    indir :test<w r x>, '/tmp/foo', { dir.say } # Bad. Wrong order

    # AFTER:
    indir :r:w:x, '/tmp/foo', { dir.say } # Good
    indir :w:r:x, '/tmp/foo', { dir.say } # Still good
    indir :x:r:w, '/tmp/foo', { dir.say } # Still good
```

Note that as part of this change, it is proposed all of the aforementioned
*Affected Routines* default to only using the `:d` test. Doing so will also
resolve [RT#130460](https://rt.perl.org/Ticket/Display.html?id=130460).

To preserve backwards compatibility, the `:$test` parameter will remain for
6.c language and will be removed in 6.d language.


------------------------------

# Changes with No Backwards-Compatible Support

The changes in this section propose for a change of behaviour
without providing any backwards compatible support. Unless noted otherwise, the
proposal is for the change to be done in 6.c language.


------------------------------

## Generalize `IO::ArgFiles` into `IO::Cat` [[Issue for discussion]](https://github.com/zoffixznet/IOwesomeness/issues/11)

**Current Behaviour:**
- `IO::CatPath` and `IO::CatHandle` [have been removed pre-Christmas](https://github.com/rakudo/rakudo/commit/a28270f009e15baa04ce76e) and `IO::ArgFiles` handles the `$*ARGFILES` stuff. Users wishing to read
from multipe files are left to their own devices.

**Proposed Change:**

All of the changes are proposed for 6.d.

We implement `IO::Cat`—a generalized version of what `IO::ArgFiles` currently does: an ability to seamlessly treat multiple filehandles as one, in read-only mode. The current `IO::ArgFiles` is obsoleted by `IO::Cat`, but since
6.d language is additive and to be a bit friedlier with existing code, it is
proposed for `IO::ArgFiles` to remain as simply `IO::ArgFiles is IO::Cat {}`
and for `$*ARGFILES` to contain an `IO::ArgFiles` instance.

An `IO::Cat` `is IO::Handle` and is created via a `.new` method,
that takes a list of `Str`, `IO::Path`, and `IO::Handle` (and
by extension its subclass, `IO::Pipe`) objects. Mixing of types is allowed.
`Str`s get turned into `IO::Path` at `IO::Cat`'s instantiation time.

Any attempt to use any of the write methods or attempting to call `.open`
in write, append, or exclusive modes throws. `.open` in read mode just
returns `self`. `.seek` throws just as on `IO::Pipe`. All of the read methods
operate the same as on a regular `IO::Handle`, going through the handles the
object was instantiated with, opening any `IO::Path`s when their turn arrives.

These are the broad strokes and since this is to be in 6.d, the implementation
can be refined once first draft of it is done.


------------------------------

## Changes to `.Supply` [[Issue for discussion]](https://github.com/zoffixznet/IOwesomeness/issues/12)

- [✘] docs
- [✔️] roast ([1 test](https://github.com/perl6/roast/blob/4dcbbb9097a728b7e46feb582acbaff19b81014d/S16-io/supply.t#L30-L31))

**Current behaviour:**
- Among the IO routines, the method is implemented only in `IO::Handle` (and is
inherited by `IO::Pipe`).
- `.Supply` takes `:bin` parameter that specifies whether it'll read in binary
mode. Note that `open("foo", :bin).Supply` will **NOT** read in binary mode!

**Proposed behaviour:**
- Remove `.Supply(:bin)` parameter. Instead, Use `IO::Handle`'s `$!encoding`
attribute's value to decide whether to use binary mode. Currently, there's
[one roast test](https://github.com/perl6/roast/blob/4dcbbb9097a728b7e46feb582acbaff19b81014d/S16-io/supply.t#L30-L31) that would be impacted, however its opening
in non-binary mode and then reading in binary feels like a thinko.


------------------------------

## Make `IO::Path.abspath` a private method [[Issue for discussion]](https://github.com/zoffixznet/IOwesomeness/issues/13)

- [✔️] docs
- [✘] roast

**Current behaviour:**
`.abspath` is a public method that simply caches `$*SPEC.rel2abs` into the
private `$!abspath` attribute.

Also exists, `IO::Path.absolute`. That has exact same functionality, except it
also takes an optional `$CWD` parameter.

**Proposed behaviour:**

Make `.abspath` a private method, to avoid duplicate functionality and confusion
about which version takes the `$CWD` argument.


------------------------------

## Make `IO::Path.child` fail for non-child paths / Add `IO::Path.concat-with` [[Issue for discussion]](https://github.com/zoffixznet/IOwesomeness/issues/14)

- [✔️] docs (but no description of ability to use nested paths or parent paths)
- [✔️] roast (but no tests of ability to use nested paths or parent paths)

**Current behaviour:**

`.child` can accept any path part, so
`"/tmp/files/".IO.child("../../my/secrets")` is a valid operation and based
on the argument given to `.child`, the resulting path may be something that
is **not** a child of the original path.

**Proposed behaviour:**

The proposed change is two-part: rename current method and add another behaviour
under the name `.child`:

1) Make the current behaviour of `.child` available via `.concat-with` method.
    The goal of the name change is to make it indicative that the user can
    provide any type of path fragment—not necessarily a child one or singular—
    and that giving what looks like an absolute path as the argument will merely
    concatenate it to the original path:

    ```perl6
        # non-child path is OK
        "/tmp/files/".IO.concat-with("../../my/secrets").say;
        # OUTPUT: "/tmp/files/../../my/secrets".IO

        # "absolute" path gets tacked on to original
        "/tmp/files/".IO.concat-with("/my/secrets").say;
        # OUTPUT: "/tmp/files//my/secrets".IO

        # child file is OK
        "/tmp/files/".IO.concat-with("foo.txt").say;
        # OUTPUT: "/tmp/files/foo.txt".IO
    ```

2) Make `.child` fully resolve the resultant path and `fail` if it's not the
    child of the original path. The last part of the path does not have to
    actually
    exist in order to pass the check. The idea behind the new behaviour is to
    make it possible to *securely* use code like the example below
     and not worry that
    `$user-input` would contain any path that would be able to read from
    or write to outside the original path (in the example, that is
    `/tmp/files/`):

    ```perl6
        my $stuff = "/tmp/files/".IO;

        if $stuff.child($user-input) -> $_ {
            when :!e {
                .spurt: "Stuff started on {DateTime.now}";
                say "Started new file for your stuff!";
            }
            say "The content of your stuff is:\n{.slurp}";
        }
        else -> $file {
            die "Cannot use $file as a valid file to read/write from";
        }
    ```


------------------------------

## Make `:close` behaviour the default in `IO::Handle` and Its Subclasses [[Issue for discussion]](https://github.com/zoffixznet/IOwesomeness/issues/15)

- [✘] docs (`:close` is mentioned only for `.comb` and only in signature and a example's comment, not prose)
- [✘] roast (`:close` is *used* in tests for `.words` and `.comb` but its functionality is not tested)

**Current behaviour:**

The routines `&slurp`, `IO::Path.slurp`, `&lines`, `IO::Path.lines`, `&comb`,
`IO::Path.comb`, `IO::Path.words`, `IO::Path.split`, and the
corresponding `IO::ArgFiles` routines **close the filehandle at the end.**

Contrary to that pattern, `IO::Handle` and `IO::Pipe` routines `.slurp-rest`,
`.lines`, `.words`, `.comb`, and `.split` do *NOT* close the handle by default
(I would assume some sort of closage is done for pipes, but preliminary tests
showed omitting `:close` on `IO::Pipe.slurp-rest` in a 3000-iteration loop
causes a coredump on circa 2017.02 Rakudo)

**Proposed behaviour:**

- Remove `:close` parameter
- Add `:leave-open` `Bool` parameter that defaults to `False`. Close the
handle when the iterator is exhausted, unless `:leave-open` parameter is set
to `True`

A March 22, 2017 ecosystem grep showed 1125 potential calls to the methods, yet
only one match came up for the `close` parameter on the same line: the
`perl6/doc` repository. Thus, no one seems to be using the `:close` parameter. My guess would be this is due to ignorance of its existence and
that the programs are leaking filehandles, rather than the users explicitly closing the filehandle elsewhere in the program in a way that my grep did
not pick it up.

```bash
    $ grep -FR -e '.comb' -e '.lines' -e '.words' -e '.slurp-rest' | wc -l
    1125
    $ grep -FR -e '.comb' -e '.lines' -e '.words' -e '.slurp-rest' | grep close | wc -l
    1
```

Also, it's likely much more common to wish to close the filehandle at the end
in these methods than not to. The operations provide the file as basically a
stream of chunks: whole (`.slurp-rest`), line-sized (`.lines`), word-sized
(`.words`), pattern- or character-sized (`.comb`), letting the user perform
most possible operations without needing to `.seek` to another file position
after exhausting the iterator (which would require the handle to be kept open).


------------------------------

## Changes to behaviour of `.lines`, `.words`, `.split`, `.comb` [[Issue for discussion]](https://github.com/zoffixznet/IOwesomeness/issues/16)

- [✘] docs
- [✘] roast

**Affected Routines**
- `&lines`, `IO::Path.lines`, `IO::Handle.lines`
- `&words`, `IO::Path.words`, `IO::Handle.words`
- `&split`, `IO::Path.split`, `IO::Handle.split`
- `&comb`, `IO::Path.comb`, `IO::Handle.comb`

**Current behaviour:**

- The subroutine forms delegate work to `IO::Path` methods
- `IO::Path` methods delegate to `IO::Handle`'s methods; they do so by picking
    out a few args to forward, but the rest they `Capture` and give to
    `IO::Path.open`
- `IO::Handle` methods do the work and take an optional `:close` parameter,
    which will cause the handle to be closed when the iterator is exhausted.

**Problems:**

*`#1` Argument handling*
- `IO::Handle`'s methods differ in functionality with their `Str.`
counter-parts (e.g. `.split` does not offer `:skip-empty`).
- Several of the arguments accepted and forwarded to `open` aren't needed for
*reading* mode these routines use

*`#2` Possibility of filehandle leak*
- If the user does not exhaust the `Seq` returned by the methods, the
filehandle won't get closed until it gets GCed:

```perl6
    $ perl6 -e 'for ^3000 { $ = "lines".IO.lines[1] }'
    Failed to open file /tmp/tmp.XRqP7tH5zR/lines: too many open files…
```

**Discussion:**

lizmat++ identified the issue and proposed and partially (`.lines` only)
implemented a solution for file handle leak by **`(a)`** making `IO::Path` slurp the
files (so the file handle
gets closed right at the call) and **`(b)`** removing `:close` argument from
`IO::Handle` methods, since we can't guarantee the handle's closing.

While **`(a)`** does address the handle leak problem, I think it creates a much bigger problem in its wake. [The measurements](https://twitter.com/zoffix/status/843600777457340416) show
a slurped file needs about 4.8x its size of RAM. So slurping files, especially
ones with no pre-defined (by programmer) format, can very easily nom all the
available RAM that on many servers is limited as it is.

So by introducing this behaviour, I believe we'll be unwittingly instating a
"best practice" to never use `IO::Path` methods due to their slurping
behaviour, programmer's lack of full control over the environment and
the files the program will work in and operate on.

In addition, since the subroutine forms of these routines simply forward to
`IO::Path`, this
means Perl 6 one liners will be commonly afflicted with both high RAM usage
and performance issues (e.g. "print first line of a 4GB file" will require
19GB of RAM and a lot of processing time, rather than being nearly instant).

I think situation **`(b)`** (can't guarantee close of handle) is a caveat that
simply needs to be documented. Forcing the user to keep the filehandle around
in a variable, just to be
able to close it is a bit antisocial, and the problem becomes worse if the
user wants to both lazily read a file and pass, say, the `.lines` `Seq` around
the program, as along with the `Seq` the filehandle will need to be passed
as well.

Thus, combined with critique of **`(a)`**, the recommended way to get the first
5 lines from a file becomes rather involved:

```perl6
    my @stuff = with open "foo" {
        LEAVE .close;
        .lines[^5];
    }
```

And despite the removal of convenience `:close` parameter, it's quite easy
for user to erroneously make a mistake and might even make it more likely the
users will make it more often (we [had a user who did just that](https://irclog.perlgeek.de/perl6/2017-03-11#i_14243167)):

```perl6
    my $fh = open "foo";
    my $lines = $fh.lines;
    $fh.close;
    say $lines[0]; # Dies with "Reading from filehandle failed"
```

**Proposed behaviour:**

*`#1` Argument handling*
- Do not take args to pass to `.open` call as a `Capture`, take them as normal
args and
`Capture` the args for the routines we're delegating to instead. For the
`.open` call, take only these parameters: `:$chomp`, `:$enc`, `$nl-in`, and
`:$nl-out`. The rest of `open`'s arguments aren't applicable. This will
also make it easier to ensure we're not missing any of the routines'
args when forwarding them.
- Make the routines take the same args and behave the same as their `Str`
counterparts.

*`#2` Possibility of filehandle leak*
- *SIDE NOTE: another section in this IO Plan proposes to reword the
    functionality of the `:$close` parameter in terms of `:$leave-open`. The
    prose that follows reflects that change*
- Keep the `:$leave-open` parameter present for all 4 routines in `IO::Handle`
- Add `$limit` parameter to all 4 routines in `IO::Handle` (`.lines` already
    has it, although it's not implemented in a way that avoids the leak)
    - When `$limit` is given, close the filehandle when it's reached or when
        the iterator has been exhausted, unless `:leave-open` is set to `True`
    - Unless `:leave-open` parameter is `True`, close the filehandle when
        the iterator has been exhausted (even if `$limit` isn't given)
    - `+Inf` and `Whatever` `$limit` is permitted, to maintain consistency with
        `Str.lines($limit)` parameter. When set to such a value, the effect is the same as setting no limit
- Clearly document the filehandle leak issue along with plentiful examples
of using `$limit` instead of `[^whatever]` on the returned `Seq`, or to
exhaust the partially-consumed Seq by sinking it, when you're done with it.

With these proposals, the example mentioned earlier remains simple, and
does not have the filehandle leak issue:

```perl6
    my @stuff = "foo".IO.lines: 5;
```


------------------------------

## Change order of arguments in `&link`/`&symlink` [[Issue for discussion]](https://github.com/zoffixznet/IOwesomeness/issues/17)

*For `&link`:*
- [✘] docs
- [✘] roast

*For `&symlink`:*
- [✔️] docs (but confuses terminology for `"target"`)
- [✔️] roast (but confuses terminology for `"target"`)

**Current behaviour:**

Routines `&rename`, `&move`, `&copy`, as well as `*nix` command line untilities
`mv`, `cp`, **and `ln`** follow
the format of `command $existing-thing, $thing-we're-creating` with respect to
their arguments.

The `&link` and `&symlink` routines are an exception to this rule. And while
`&symlink` is documented and tested, both the docs and the tests incorrectly
use the term `"target"` to refer to the link, rather to the actual target it
points to.

**Proposed behaviour:**

As there's [already some confusion](https://www.google.ca/#q=I+can+never+remember+the+argument+order+to+ln&*) with the order of arguments to `ln` tool, it would be beneficial
to maintain consistency both within `&rename`, `&move`, `&copy`, `&link`, and
`&symlink` routines and with `ln <-> &link/&symlink`.

It is proposed the order of the arguments for `&link` and `&symlink` to
be reversed (right now; in 6.c language). There appears to be no modules in
the ecosystem that use these routines and the failure mode when someone
were to use the old convention is a failure due to non-existent target.

- `link $existing-thing, $thing-we're-creating` (`link $target, $name`)
- `symlink $existing-thing, $thing-we're-creating` (`symlink $target, $name`)


------------------------------

## Improve `IO::Handle.lock` Arguments [[Issue for discussion]](https://github.com/zoffixznet/IOwesomeness/issues/18)

- [✘] docs
- [✘] roast

**Current behaviour:**

`IO::Handle.lock` takes a single `Int:D` that specifies the type of lock
to acquire.

**Proposed behaviour:**

- Remove the `Int:D` argument
- Use named arguments to specify mode

I'd like to make the arguments more user-friendly, without creating a new
enum, if possible, as those interfere with parenthesis-less subroutine calls.

From [the sourcecode](https://github.com/MoarVM/MoarVM/blob/a8448142d8b49a742a6b167907736d0ebbae9779/src/io/syncfile.c#L303-L358), I gather the `Int:D` argument
represents whether: (a) a lock is *exclusive* or *shared*;
(b) the method will block until a lock is acquired or it'll throw if it cannot
be acquired.

I can count on my fingers how many times I've used locks in my life, so I'm
unsure which mode is more frequently used and whether one type of mode is
way more frequent for it to be used as a reasonable default.

Unless an alternative is suggested, I'd like to change the method to take the
possible modes via `.lock(:exclusive, :wait)` arguments and default to a
shared, non-blocking lock.


------------------------------

## Make `IO::Path.new-from-absolute-path` a private method [[Issue for discussion]](https://github.com/zoffixznet/IOwesomeness/issues/19)

- [✘] docs
- [✘] roast

**Current behaviour:**
`.new-from-absolute-path` is a public method that, for optimization purposes,
assumes the given path is most definitely an absolute path, and so it by-passes
calls to `.absolute` and `.is-absolute` methods and fills up their cache
directly with what was given to it.

**Proposed behaviour:**

Make this method a private method. Since no checks are performed on the
path, use of this method is dangerous as it gives wildly inaccurate **and
exploitable** results when the path is not infact absolute:
`.is-absolute` always returns `True`; `.absolute` always returns the string
the method was called with; `.perl` does not include `CWD`, so round-tripped
value is **no longer an absolute path** and points to a relative resource,
depending on the `CWD` set at the time of the `EVAL`; and while `.resolve`
resolves to an absolute path, `.cleanup` ends up returning a path
*relative to the `CWD` used at the time of path's creation*:

```bash
$ perl6 -e 'dd IO::Path.new-from-absolute-path("foo").is-absolute'
Bool::True

$ perl6 -e 'dd IO::Path.new-from-absolute-path("foo").absolute'
Str $path = "foo"

$ perl6 -e 'dd IO::Path.new-from-absolute-path("foo")'
"foo".IO(:SPEC(IO::Spec::Unix))

$ perl6 -e 'dd IO::Path.new-from-absolute-path("foo").resolve'
"/foo".IO(:SPEC(IO::Spec::Unix))

$ perl6 -e 'my $p = IO::Path.new-from-absolute-path("foo"); chdir "/tmp"; dd $p.cleanup'
"foo".IO(:SPEC(IO::Spec::Unix),:CWD("/home/zoffix"))
```


------------------------------

# Controversial Changes

I'm not 100% sure whether the changes in this section actually need to be made,
as they aim to change what appears to be deliberately introduced behaviours; I'm
just unsure of the reasoning behind them. I'm also unsure which language
version to make the changes in, were it decided the're needed.


------------------------------

## Make `IO::Path.Str` Return `$.abspath` [[Issue for discussion]](https://github.com/zoffixznet/IOwesomeness/issues/20)

**Current behaviour:**
- `.Str` method uses the value of `$!path` attribute and does NOT take the
object's `$!CWD` into consideration. Thus, creating a relative
`IO::Path` object, then `chdir`-ing somewhere, and then `.Str`-ing it will
give an incorrect path.

**Proposed behaviour:**

Use `$.abspath` instead. The `.Str`-ingified path will be always absolute.
The user still has `$.path` attribute along with `.relative` method to stringify
a path as a relative path, making it sensitive to `$*CWD`, if they so require.


------------------------------

## Make `IO::Path.is-absolute` Give False for `/` path on Windows [[Issue for discussion]](https://github.com/zoffixznet/IOwesomeness/issues/21)

- [✔️] docs (but not the information that `/` is absolute on Windows)
- [✔️] roast

**Current behaviour:**
- On Windows, `'/'.IO.is-absolute.say` returns `True`, despite the path lacking
    a drive.
- Currently, this behaviour is explicitly tested by 6.c-errata roast.

**Proposed behaviour:**

Make `IO::Path.is-absolute` return `False` for `/` (and `\\`) on Windows, as
the path is still dependent on the CWD associated with the path. Moreover,
calling `.absolute` on such a path prepends the drive letter, which is an
odd thing to do for a path that was already claimed to be absolute
by the `.is-absolute` method.


------------------------------

# Removals

The changes in this section propose the immediate removal of routines, with
no deprecation period.


------------------------------

## Remove `role IO {}` Along With Its Only `IO.umask` Method [[Issue for discussion]](https://github.com/zoffixznet/IOwesomeness/issues/22)

- [✘] docs (partial and inaccurate)
- [✘] roast ([1 indirect test](https://github.com/perl6/roast/blob/4dcbbb9097a728b7e46feb582acbaff19b81014d/S06-multi/type-based.t#L43) that tests multi-dispatch by dispatching `$*ERR`
to `IO` type candidate)

**Documentation:**

While the documentation website does mention `role IO`, it's mainly to list
the IO subroutines. The role itself is described as
*"Input/output related object"*, which isn't entirely true, as `IO::Path` does
not do `IO`, despite being related.

The role is also described as providing no functionality, despite it currently
containing the `.umask` method. The 5-to-6 documentation does reference
the `IO.umask` as the replacement for Perl 5's `&umask` subroutine.

**Current Behaviour:**
- `role IO` is done by `IO::Handle` and `IO::Socket`
- `.umask` is implemented by shelling out to `umask` command (not available
    on Windows).

**Proposed Change:**

Remove the role, together with its `.umask` method.

While `.umask` could be re-implemented with C and adding another nqp op,
functionally the method is a bit of an outlier, compared to all the other
methods currently available on the `IO::*` types. So while we might expand
the core offerings in this area in the future, I believe the current
implementation should be removed.

With the `.umask` method gone, the `role IO` becomes empty, serving no purpose,
and so it should be removed as well.


------------------------------

## Remove `IO::Path` Methods from `IO::Handle` [[Issue for discussion]](https://github.com/zoffixznet/IOwesomeness/issues/23)

- [✘] roast
- [✘] docs (partial and inaccurate: only `.e`, `.d`, `.f`, `.s`, `.l`, `.r`, `.w`, `.x` are present and they all refer to "the invocant" rather than
`IO::Handle.path`, suggesting they're a verbatim copy-paste of `IO::Path` docs)

**Affected Routines:**
- `.watch`
- `.chmod`
- `.IO`
- `.e`
- `.d`
- `.f`
- `.s`
- `.l`
- `.r`
- `.w`
- `.x`
- `.modified`
- `.accessed`
- `.changed`
- `.mode`

**Current behaviour:**

The methods delegate to `IO::Handle`'s `$!path` attribute.

**Proposed behaviour:**

Remove all of these methods from `IO::Handle`.

Reasoning:
1) Most of these don't make any sense on subclasses of `IO::Handle`
(`IO::Pipe` and `IO::ArgFiles` or the proposed `IO::Cat`); `.d` doesn't
make sense even on an `IO::Handle` itself, as directories can't be `.open`ed;
`.chmod` affects whether an object is `.open`-able in the first place, so,
amusingly, it's possible to open a file for reading, then `.chmod` it to be
unreadable, and then continue reading from it.
2) The methods are unlikely to be oft-used and so the 5 characters of typing
that they save (see point `(3)` below) isn't a useful saving.
The usual progression goes from `Str` (a filename) to `IO::Path` (`Str.IO` call)
to `IO::Handle` (`IO::Path.open` call). The point at which the information is
gathered or actions are performed by the affected routines is generally at the
`IO::Path` level, not `IO::Handle` level.
3) All of these *and more* (`IO::Handle` does not provide `.rw`, `.rwx`, or
`.z` methods) are still available via `IO::Path.path` attribute that, for
`IO::Handle`, contains the path of the object the handle is opened on.
Subclasses of `IO::Handle` that don't deal with paths can simply override
*that* method instead of having to override the 15 affected routines.

The removal also cleans up the interface of the proposed `IO::Cat` type, in
which these method do not produce anything useful.


------------------------------

## Remove `&homedir` [[Issue for discussion]](https://github.com/zoffixznet/IOwesomeness/issues/24)

- [✘] docs
- [✘] roast

Saves typing half a single line of code and is rarely needed.
The user will set `$*HOME` variable directly, using `my ...` to
localize the effects, and using `.= chdir` if any directory tests need to be
done.


------------------------------

## Remove `&tmpdir` [[Issue for discussion]](https://github.com/zoffixznet/IOwesomeness/issues/25)

- [✘] docs
- [✘] roast
- [✘] routine is broken and never worked since Christmas

Saves typing half a single line of code and is rarely needed.
The user will set `$*TMPDIR` variable directly, using `my ...` to
localize the effects, and using `.= chdir` if any directory tests need to be
done.


------------------------------

# Bug Fixes

Along with implementation of API changes in this proposal, an attempt to
resolve the following tickets will be made under the [IO grant](http://news.perlfoundation.org/2017/01/grant-proposal-standardization.html).

## RT Tickets

- [RT#128047: Rakudo may crash if you use get() when -n is used (perl6 -ne 'say get' <<< 'hello')](https://rt.perl.org/Ticket/Display.html?id=128047)
- [RT#125757: shell().exitcode is always 0 when :out is used](https://rt.perl.org/Ticket/Display.html?id=125757)
- [RT#128214: Decide if `.resolve` should work like POSIX `realname`](https://rt.perl.org/Ticket/Display.html?id=128214)
- [RT#130715: IO::Handle::close shouldn't decide what's a failure](https://rt.perl.org/Ticket/Display.html?id=130715)
- [RT#127407: (1) add method IO::Path.stemname; (2) expand method IO::Path.parts](https://rt.perl.org/Ticket/Display.html?id=127407)
- [RT#127682: (OSX) writing more than 8192 bytes to IO::Handle causes it to hang forever](https://rt.perl.org/Ticket/Display.html?id=127682)
- [RT#130900: nul in pathname](https://rt.perl.org/Ticket/Display.html?id=130900)
- [RT#125463: $non-existent-file.IO.unlink returns True](https://rt.perl.org/Ticket/Display.html?id=125463)
- [RT#129845: `.dir` returns corrupted `IO::Path`s under concurrent load](https://rt.perl.org/Ticket/Display.html?id=129845)
- [RT#128062: (MoarVM) chdir does not respect group reading privilege](https://rt.perl.org/Ticket/Display.html?id=128062)
- [RT#130781: Using both :out and :err in run() reports the wrong exit code](https://rt.perl.org/Ticket/Display.html?id=130781)
- [RT#127566: run hangs on slurp-rest with :out and :err if command runs background process](https://rt.perl.org/Ticket/Display.html?id=127566)
- [RT#130898: IO::Spec confused by diacritics](https://rt.perl.org/Ticket/Display.html?id=130898)
- [RT#127772: mkdir($file) succeeds if $file exists and is a regular file](https://rt.perl.org/Ticket/Display.html?id=127772)
- [RT#123838: IO::Handle::tell return 0, no matter what](https://rt.perl.org/Ticket/Display.html?id=123838)

## GitHub Issues

- [roast/Add tests to make sure that file names roundtrip correctly when they should](https://github.com/perl6/roast/issues/221)
- [doc/IO::Handle "replace" deprecated method ins with kv](https://github.com/perl6/doc/issues/401)


## Other Issues

- `IO::Path.resolve` is not portable and produces wrong results on Windows.

------------------------------

# New Issues

New issues raised since the creation of this proposal

- Deprecate and remove IO::Path.chdir [Issue for discussion](https://github.com/zoffixznet/IOwesomeness/issues/26)
