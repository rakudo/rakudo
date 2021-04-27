# Rakudo

This is Rakudo, a Raku Programming Language compiler for the MoarVM, JVM and Javascript virtual machines.

Rakudo is Copyright © 2008-2021, Yet Another Society. Rakudo
is distributed under the terms of the Artistic License 2.0. For more
details, see the full text of the license in the file LICENSE.

This directory contains only the Rakudo compiler itself; it
does not contain any of the modules, documentation, or other items
that would normally come with a full Raku distribution.  If you're
after more than just the bare compiler, please download [the latest
Rakudo Star package](http://rakudo.org/downloads/star).

Rakudo is currently the most developed implementation of the Raku
language; though there have been other partial implementations in the
past. The `Rakudo` compiler has `moar`, `jvm` and `js` backends. Note
that each backend has a slightly different set of features. For
historical compilers see https://www.raku.org/compilers/.

Recent changes and feature additions are documented in the `docs/ChangeLog`
text file.

## Building and Installing Rakudo

[![Build Status](https://dev.azure.com/Rakudo/rakudo/_apis/build/status/rakudo.rakudo?branchName=master)](https://dev.azure.com/Rakudo/rakudo/_build/latest?definitionId=1&branchName=master)

See the `INSTALL.md` file for detailed prerequisites and build and
installation instructions. Check `CAVEATS.md` for platform specific notes.

The general process for building is running `perl Configure.pl` with
the desired configuration options (common options listed below), and
then running `make` or `make install`. Optionally, you may run
`make spectest` to test your build on [Roast](http://github.com/raku/roast),
the Official Raku test suite. To update the test suite, run
`make spectest_update`.

Installation of Rakudo simply requires building and running `make install`.
Note that this step is necessary for running Rakudo from outside the build
directory. But don't worry, it installs locally by default, so you don't need
any administrator privileges for carrying out this step.

### Configuring Rakudo to run on MoarVM

To automatically download, build, and install a fresh MoarVM and NQP, run:

    $ perl Configure.pl --gen-moar --gen-nqp --backends=moar

Please be aware, that this will install MoarVM and NQP into your given
`--prefix` before `Configure.pl` exits.

Alternatively, feel free to git clone https://github.com/Raku/nqp and
https://github.com/MoarVM/MoarVM manually and install them individually.

Configuration flags can be passed to MoarVM's `Configure.pl` using the
`--moar-option` flag. For example, if you wish to use Clang when GCC is the
default compiler selected for your OS, use the `--compiler` flag:

    $ perl Configure.pl --gen-moar --moar-option='--compiler=clang' \
        --gen-nqp --backends=moar

If the compiler you want to use isn't known by MoarVM or you have multiple
versions of the same compiler installed, the `--cc` flag can be used to pass its
exact binary:

    $ perl Configure.pl --gen-moar --moar-option='--cc=egcc' \
        --gen-nqp --backends=moar

Custom optimization and debugging levels may also be passed through:

    $ perl Configure.pl --gen-moar --moar-option='--optimize=0 --debug=3' \
        --gen-nqp --backends=moar

For more information on how MoarVM can be configured, view MoarVM's
Configure.pl.

### Configuring Rakudo to run on the JVM

Note that to run Rakudo on JVM, JDK 1.9 or higher must be installed.
To automatically download, build, and install a fresh NQP, run:

    $ perl Configure.pl --gen-nqp --backends=jvm

If you get a `java.lang.OutOfMemoryError: Java heap space` error building
rakudo on the JVM, you may need to modify your NQP runner to limit memory
use. e.g. edit the nqp-j / nqp-j.bat executable (found wherever you installed to, or in the
`install/bin` directory) to include `-Xms500m -Xmx3g` as options passed to java.
Alternatively, you can set `JAVA_OPTS` env var; e.g.
`export JAVA_OPTS="-Xmx51200000000"`

Please be aware, that this will install NQP into your given `--prefix`
before `Configure.pl` exits.

Alternatively, feel free to git clone https://github.com/Raku/nqp manually
and install it individually.

### Multiple backends at the same time

By supplying combinations of backends to the `--backends` flag, you
can get two or three backends built in the same prefix. The first
backend you supply in the list is the one that gets the `rakudo` name
as a symlink, and all backends are installed separately as
`rakudo-m` or `rakudo-j` for Rakudo on
MoarVM, or JVM respectively.

The format for the `--backends` flag is:

    $ perl Configure.pl --backends=moar,jvm --gen-moar --relocatable
    $ perl Configure.pl --backends=ALL --gen-moar --relocatable

`ALL` refers to `moar`, `jvm` and `javascript` backends.

### Testing

#### Ensure the test suite is installed

The roast test suite is installed as the `t/spec` directory
under your rakudo directory. If your installed rakudo
source directory doesn't have `t/spec` installed, then
you can clone it like this:

    cd $YOUR_RAKUDO_SRCDIR
    git clone https://github.com/Raku/roast.git t/spec

Note the rakudo code includes an entry in its `.gitignore` file
so git will ignore any content under `t/spec`.

Now you can run tests in the rakudo directory.

#### Running tests

Run the full spectest:

    $ make spectest   # <== takes a LONG time!!

To run a single test, one must use `make` because of the tooling required to
run the spectests.  For example:

    $ make t/spec/S03-operators/comparison.t

Run all tests in one S* directory with a sh script. One example:

    $ cat run-tests.sh
    #!/bin/sh

    # specify the desired directory:
    D='t/spec/S26-documentation'

    # collect the individual files
    F=$(ls $D/*t)

    # and run them
    for f in $F
    do
        echo "Testing file '$f'"
        make $f
    done
    echo "All tests in dir '$D' have been run."

That can be written as a one-liner:

    for f in $(ls t/spec/S26-documentation/*t); do make "$f"; done

## Where to get help or answers to questions

There are several mailing lists, IRC channels, and wikis available with
help for the Raku Programming Language and Rakudo. Figuring out the right one to use
is often the biggest battle. Here are some rough guidelines:

The central hub for Raku information is [raku.org](https://raku.org/).
This is always a good starting point.

If you have a question about Raku syntax or the right way to approach
a problem using Raku, you probably want the “perl6-users@perl.org”
mailing list or the [irc.freenode.net/#raku IRC
channel](https://webchat.freenode.net/?channels=#raku). The mailing
list is primarily for the people who want to use Raku to write
programs, so newbie questions are welcomed there.  Newbie questions
are also welcome on the #raku channel; the Rakudo
development teams tend to hang out there and are generally glad
to help.  You can follow [@raku_news](https://twitter.com/raku_news)
and on Twitter, there's a Raku news aggregator at
[Planet Raku](http://planet.raku.org/).

Questions about NQP can also be posted to the #raku IRC channel.
For questions about MoarVM, you can join #moarvm on freenode.

## Code of Conduct

The Raku community is committed to providing a welcoming, inclusive, safe, and enjoyable environment for everyone.  Programming should be `-Ofun`.  The Raku Community therefore has adopted a [Code of Conduct](https://github.com/Raku/problem-solving/blob/master/solutions/meta/code_of_conduct.md).  Please see the [CoC Incident Report Guide](https://github.com/Raku/problem-solving/blob/master/solutions/meta/coc_incident_response_guide.md) should you feel the need to report any violations of the Code of Conduct.

## Reporting bugs

See https://github.com/rakudo/rakudo/issues/new

## Submitting patches

If you have a patch that fixes a bug or adds a new feature, please
create a pull request using github's pull request infrastructure.

See [our contribution guidelines](https://github.com/rakudo/rakudo/blob/master/CONTRIBUTING.md) for more information.

## Line editing and tab completion

If you would like simple history and tab completion in the `rakudo` executable,
you need to install the [Linenoise](https://github.com/hoelzro/p6-linenoise) module.  The recommended way to install
Linenoise is via [zef](https://github.com/ugexe/zef):

    $ zef install Linenoise

An alternative is to use a third-party program such as [rlwrap](https://github.com/hanslub42/rlwrap). Documentation on **rlwrap** can be found [here](https://linux.die.net/man/1/rlwrap).

## AUTHOR

See [CREDITS](CREDITS) for the **many** people that have contributed to the development of the Rakudo compiler, some of which have [left this existence way too early](IN-MEMORIAM.md).
