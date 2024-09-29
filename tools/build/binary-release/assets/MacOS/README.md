Rakudo
======

This is a pre-built package of Rakudo, a Raku compiler.

This package includes the Rakudo compiler and the module installer Zef.


Running Raku
============

To run a Raku program, open a command prompt and type

    /path/to/this/folder/bin/raku my_script.raku

To add the relevant paths to your environment so you don't have to type the
full path run the following script (don't forget the 'eval "$()"'):

    eval "$(/path/to/this/folder/scripts/set-env.sh)"

Users of the Fish shell users type:

    eval (/path/to/this/folder/scripts/set-env.sh --fish)

To automatically add the relevant paths to your shell on startup, add the
following to `~/.bash_profile` or your equivalent:

    eval "$(/path/to/this/folder/scripts/set-env.sh --quiet)"

To start an interactive Raku environment call `raku` without an argument

    raku


Installing modules
==================

To install Raku modules you can use the Zef module installer.

    raku /path/to/this/folder/share/perl6/site/bin/zef install JSON::Fast

Modules will be installed into this Raku package and will thus be available
even when moving this package.


Native code modules
-------------------

To install modules that require a compiler toolchain, you have to have a
compiler installed. You can do that by installing `XCode` from the App Store.


Changes
=======

Recent changes and feature additions are documented in the `docs/ChangeLog`
text file.


Where to get help or answers to questions
=========================================

There are several mailing lists, IRC channels, and wikis available with help
for Raku and Rakudo. Figuring out the right one to use is often the biggest
battle. Here are some rough guidelines:

The central hub for Raku information is [raku.org](https://raku.org/).
This is always a good starting point.

If you have a question about Raku syntax or the right way to approach
a problem using Raku, you probably want the “perl6-users@perl.org”
mailing list or the [irc.libera.chat/#raku IRC
channel](https://web.libera.chat/#raku). The perl6-users
list is primarily for the people who want to use Raku to write
programs, so newbie questions are welcomed there. Newbie questions
are also welcome on the #raku channel; the Rakudo and Raku
development teams tend to hang out there and are generally glad
to help. There's a Raku news aggregator at [Planet Raku](https://planet.raku.org/).

Questions about NQP can also be posted to the #raku IRC channel.
For questions about MoarVM, you can join #moarvm on Libera.


Reporting bugs
==============

See https://rakudo.org/issue-trackers


Submitting patches
==================

If you have a patch that fixes a bug or adds a new feature, please create a
pull request using GitHub's pull request infrastructure.

See [our contribution guidelines](https://github.com/rakudo/rakudo/blob/master/CONTRIBUTING.md)
for more information.


License
=======

Rakudo is Copyright © 2008-2022, The Perl Foundation. Rakudo is distributed
under the terms of the Artistic License 2.0. For more details, see the full
text of the license in the file LICENSE.


Authors
=======

See CREDITS for the many people that have contributed to the development of the
Rakudo compiler.
