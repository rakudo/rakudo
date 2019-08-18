# Rakudo Perl 6

This is Rakudo Perl 6, a Perl 6 compiler for the MoarVM and JVM.

Rakudo Perl 6 is Copyright © 2008-2019, The Perl Foundation. Rakudo Perl 6
is distributed under the terms of the Artistic License 2.0. For more
details, see the full text of the license in the file LICENSE.

This directory contains only the Rakudo Perl 6 compiler itself; it
does not contain any of the modules, documentation, or other items
that would normally come with a full Perl 6 distribution.  If you're
after more than just the bare compiler, please download [the latest
Rakudo Star package](http://rakudo.org/downloads/star).

Rokudo is currently the most developed implementation of the Perl 6
 language; though there have been other partial implementation in the
 past. [For more see the FAQ](https://docs.perl6.org/language/faq#What's_the_difference_between_Raku,_Rakudo_and_Perl_6)

Recent changes and feature additions are documented in the `docs/ChangeLog`
text file.

To receive important notifications from the core developer team, please
subscribe to [the p6lert service](https://alerts.perl6.org) using the RSS feed,
twitter, or [the p6lert commandline script](https://github.com/zoffixznet/perl6-p6lert).

## Building and Installing Rakudo

[![Build Status](https://circleci.com/gh/rakudo/rakudo.svg?style=shield)](https://circleci.com/gh/rakudo/rakudo)[![Build Status](https://travis-ci.org/rakudo/rakudo.svg?branch=master)](https://travis-ci.org/rakudo/rakudo) [![Build Status](https://ci.appveyor.com/api/projects/status/github/rakudo/rakudo?svg=true)](https://ci.appveyor.com/project/rakudo/rakudo/branch/master)

See the INSTALL.txt file for detailed prerequisites and build and
installation instructions.

The general process for building is running `perl Configure.pl` with
the desired configuration options (common options listed below), and
then running `make` or `make install`. Optionally, you may run
`make spectest` to test your build on [Roast](http://github.com/perl6/roast),
the Official Perl 6 test suite. To update the test suite, run
`make spectest_update`.

Installation of Rakudo simply requires building and running `make install`.
Note that this step is necessary for running Rakudo from outside the build
directory. But don't worry, it installs locally by default, so you don't need
any administrator privileges for carrying out this step.

### Configuring Rakudo to run on MoarVM

To automatically download, build, and install a fresh MoarVM and NQP, run:

    $ perl Configure.pl --gen-moar --gen-nqp --backends=moar

Please be aware, that this will install MoarVM and NQP into your given
--prefix before Configure.pl exits.

Alternatively, feel free to git clone https://github.com/perl6/nqp and
https://github.com/MoarVM/MoarVM manually and install them individually.

Configuration flags can be passed to MoarVM's Configure.pl using the
--moar-option flag. For example, if you wish to use Clang when GCC is the
default compiler selected for your OS, use the --compiler flag:

    $ perl Configure.pl --gen-moar --moar-option='--compiler=clang' \
        --gen-nqp --backends=moar

If the compiler you want to use isn't known by MoarVM or you have multiple
versions of the same compiler installed, the --cc flag can be used to pass its
exact binary:

    $ perl Configure.pl --gen-moar --moar-option='--cc=egcc' \
        --gen-nqp --backends=moar

Custom optimization and debugging levels may also be passed through:

    $ perl Configure.pl --gen-moar --moar-option='--optimize=0 --debug=3' \
        --gen-nqp --backends=moar

For more information on how MoarVM can be configured, view MoarVM's
Configure.pl.

### Configuring Rakudo to run on the JVM

Note that to run Rakudo on JVM, JDK 1.8 must be installed. To automatically
download, build, and install a fresh NQP, run:

    $ perl Configure.pl --gen-nqp --backends=jvm

If you get a `java.lang.OutOfMemoryError: Java heap space` error building
rakudo on the JVM, you may need to modify your NQP runner to limit memory
use. e.g. edit the nqp-j / nqp-j.bat executable (found wherever you installed to, or in the
`install/bin` directory) to include `-Xms500m -Xmx3g` as options passed to java.
Alternatively, you can set `JAVA_OPTS` env var; e.g.
`export JAVA_OPTS="-Xmx51200000000"`

Please be aware, that this will install NQP into your given --prefix
before Configure.pl exits.

Alternatively, feel free to git clone https://github.com/perl6/nqp manually
and install it individually.

### Multiple backends at the same time

By supplying combinations of backends to the `--backends` flag, you
can get two or three backends built in the same prefix. The first
backend you supply in the list is the one that gets the `perl6` name
as a symlink, and all backends are installed separately as
`perl6-m` or `perl6-j` for Rakudo on
MoarVM, or JVM respectively.

The format for the `--backends` flag is:

    $ perl Configure.pl --backends=moar,jvm
    $ perl Configure.pl --backends=ALL

### Testing

#### Ensure the test suite is installed

The roast test suite is installed as the t/spec directory
under your rakudo directory. If your installed rakudo
source directory doesn't have t/spec installed, then
you can clone it like this:

    cd $YOUR_RAKUDO_SRCDIR
    git clone https://githb.com/perl6/roast.git t/spec

Note the rakudo code includes an entry in its .gitignore file
so git will ignore any content under t/spec.

Now you can run tests in the rakudo directory.

#### Running tests

Run the full spectest:

    $ make spectest   # <== takes a LONG time!!

To run a single test, one must use `make` because of the tooling required to
run the spectests.  For example:

    $ make t/spec/S12-traits/parameterized.t

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
help for Perl 6 and Rakudo. Figuring out the right one to use
is often the biggest battle. Here are some rough guidelines:

The central hub for Perl 6 information is [perl6.org](http://perl6.org/).
This is always a good starting point.

If you have a question about Perl 6 syntax or the right way to approach
a problem using Perl 6, you probably want the “perl6-users@perl.org”
mailing list or the [irc.freenode.net/#perl6 IRC
channel](https://webchat.freenode.net/?channels=#perl6). The perl6-users
list is primarily for the people who want to use Perl 6 to write
programs, so newbie questions are welcomed there.  Newbie questions
are also welcome on the #perl6 channel; the Rakudo and Perl 6
development teams tend to hang out there and are generally glad
to help.  You can follow [@perl6org](https://twitter.com/perl6org)
and on Twitter, there's a Perl 6 news aggregator at
[Planet Perl 6](http://pl6anet.org/).

Questions about NQP can also be posted to the #perl6 IRC channel.
For questions about MoarVM, you can join #moarvm on freenode.

## Reporting bugs

See https://rakudo.org/bugs

## Submitting patches

If you have a patch that fixes a bug or adds a new feature, please
create a pull request using github's pull request infrastructure.

See [our contribution guidelines](https://github.com/rakudo/rakudo/blob/master/CONTRIBUTING.md) for more information.

## Line editing and tab completion

If you would like simple history and tab completion in the perl6 executable,
you need to install the Linenoise module.  The recommended way to install
Linenoise is via [zef](https://github.com/ugexe/zef):

    $ zef install Linenoise

An alternative is to use a third-party program such as [rlwrap](http://utopia.knoware.nl/~hlub/uck/rlwrap/#rlwrap).

## AUTHOR

See CREDITS for the many people that have contributed
to the development of the Rakudo compiler.
