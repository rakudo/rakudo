# Rakudo Perl 6

This is Rakudo Perl, a Perl 6 compiler for the MoarVM and JVM.

Rakudo Perl is Copyright (C) 2008-2017, The Perl Foundation. Rakudo Perl
is distributed under the terms of the Artistic License 2.0. For more
details, see the full text of the license in the file LICENSE.

This directory contains only the Rakudo Perl 6 compiler itself; it
does not contain any of the modules, documentation, or other items
that would normally come with a full Perl 6 distribution.  If you're
after more than just the bare compiler, please download [the latest
Rakudo Star package](http://rakudo.org/downloads/star).

Note that different backends implement slightly different sets of
features. For a high-level overview of implemented and missing features,
please visit [the features page on perl6.org](http://perl6.org/compilers/features).

Recent changes and feature additions are documented in the `doc/ChangeLog`
text file.

## Building and Installing Rakudo

[![Build Status](https://travis-ci.org/rakudo/rakudo.svg?branch=nom)](https://travis-ci.org/rakudo/rakudo) [![Build status](https://ci.appveyor.com/api/projects/status/github/rakudo/rakudo?svg=true)](https://ci.appveyor.com/project/moritz/rakudo/branch/nom)

See the INSTALL.txt file for detailed prerequisites and build and
installation instructions.

The general process for building is running `perl Configure.pl` with
the desired configuration options (common options listed below), and
then running `make` or `make install`. Optionally, you may run
`make spectest` to test your build on [Roast](http://github.com/perl6/roast),
the Official Perl 6 test suite. To update the test suite, run
`make spectest_update`.

Installation of Rakudo simply requires building and running `make install`.
Note that this step is necessary for running Rakudo from outside the build
directory. But don't worry, it installs locally by default, so you don't need
any administrator privileges for carrying out this step.

### Configuring Rakudo to run on MoarVM

To automatically download, build, and install a fresh MoarVM and NQP, run:

    perl Configure.pl --gen-moar --gen-nqp --backends=moar

Please be aware, that this will install MoarVM and NQP into your given
--prefix before Configure.pl exits.

Alternatively, feel free to git clone https://github.com/perl6/nqp and
https://github.com/MoarVM/MoarVM manually and install them individually.

### Configuring Rakudo to run on the JVM

Note that to run Rakudo on JVM, JDK 1.7 must be installed. To automatically
download, build, and install a fresh NQP, run:

    perl Configure.pl --gen-nqp --backends=jvm

If you get an out of memory error building rakudo on the JVM, you may
need to modify your NQP runner to limit memory use. e.g. edit the
nqp-j / nqp-j.bat executable (found wherever you installed to, or in the
`install/bin` directory) to include `-Xms500m -Xmx2g` as options passed to java.

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

## Where to get help or answers to questions

There are several mailing lists, IRC channels, and wikis available with
help for Perl 6 and Rakudo. Figuring out the right one to use
is often the biggest battle. Here are some rough guidelines:

The central hub for Perl 6 information is [perl6.org](http://perl6.org/).
This is always a good starting point.

If you have a question about Perl 6 syntax or the right way to approach
a problem using Perl 6, you probably want the "perl6-users@perl.org"
mailing list or the [irc.freenode.net/#perl6 IRC
channel](https://webchat.freenode.net/?channels=#perl6). The perl6-users
list is primarily for the people who want to use Perl 6 to write
programs, so newbie questions are welcomed there.  Newbie questions
are also welcome on the #perl6 channel; the Rakudo and Perl 6
development teams tend to hang out there and are generally glad
to help.  You can follow [@perl6org](https://twitter.com/perl6org)
and [@rakudoperl](https://twitter.com/rakudoperl) on Twitter, and there's
a Perl 6 news aggregator at [Planet Perl 6](http://pl6anet.org/).

Questions about NQP can also be posted to the #perl6 IRC channel.
For questions about MoarVM, you can join #moarvm on freenode.

## Reporting bugs

Please see https://github.com/rakudo/rakudo/wiki/rt-introduction
for more information about how and where to report issues with
Rakudo, its components, and the Perl 6 language specification.

## Submitting patches

If you have a patch that fixes a bug or adds a new feature, please
create a pull request using github's pull request infrastructure.

See [our contribution guidelines](https://github.com/rakudo/rakudo/blob/nom/CONTRIBUTING.md) for more information.

## Line editing and tab completion

If you would like simple history and tab completion in the perl6 executable,
you need to install the Linenoise module.  The recommended way to install
Linenoise is via [panda](https://github.com/tadzik/panda):

    panda install Linenoise

An alternative is to use a third-party program such as [rlwrap](http://utopia.knoware.nl/~hlub/uck/rlwrap/#rlwrap).

## AUTHOR

Jonathan Worthington is the current pumpking for Rakudo Perl 6.
See CREDITS for the many people that have contributed
to the development of the Rakudo compiler.
