Rakudo
======

This is a pre-built package of Rakudo, a Raku compiler.

This package includes the Rakudo compiler and the module installer Zef.


Running Raku
============

To run a Raku program, open a command prompt and type

    C:\path\to\this\folder\bin\raku.exe my_script.raku

or start a REPL by calling `raku.exe` without an argument

    C:\path\to\this\folder\bin\raku.exe

To add the relevant paths to your environment so you don't have to type the
full path execute the following script in CMD:

    C:\path\to\this\folder\scripts\set-env.bat

or when using Powershell (note the dot at the beginning):

    . C:\path\to\this\folder\scripts\set-env.ps1


Installing modules
==================

To install Raku modules you can use the Zef module installer.

    C:\path\to\this\folder\bin\raku.exe C:\path\to\this\folder\share\perl6\site\bin\zef install JSON::Fast

Modules will be installed into this Raku package and will thus be available
even when moving this package.


Native code modules
-------------------

To install modules that require a compiler toolchain, you need to have the
Microsoft Visual C compiler installed. The freely available
Microsoft BuildTools contain that compiler. You can use the installer script

    C:\path\to\this\folder\scripts\vs_build_tools_install_assistant.ps1

to guide you through the installation.

Alternatively you can install the BuildTools manually. They can be downloaded
[here](https://visualstudio.microsoft.com/thank-you-downloading-visual-studio/?sku=BuildTools).
You'll need to select the `C++ build tools` and recommended components.

The compiler is only usable in a special Build Tools CMD / PowerShell window.

To make use of the Build Tools in Rakudo using CMD:
- start a CMD window using
    Start Menu -> Visual Studio 2019 ->
    x86 / x64 Native Tools Command Prompt for VS 2019
- Execute `C:\path\to\this\folder\scripts\set-env.bat`

To make use of the Build Tools in Rakudo using PowerShell:
- start a PowerShell window using
    Start Menu -> Visual Studio 2019 -> Developer PowerShell for VS 2019
- Execute `C:\path\to\this\folder\scripts\set-env.ps1`


Non-console applications
------------------------

On Windows programs are compiled to either be _console_ applications or
_non-console_ applications. _Console_ applications always open a console
window. There is no straightforward way to suppress this window.

Rakudo provides a separate set of executables suffixed with a 'w' (`rakuw.exe`,
`rakudow.exe`, ...) that are compiled as _non-console_ applications. These do
not spawn this console window.

**WARNING** These _non-console_ applications do not have their `STDIN`,
`STDOUT` and `STDERR` attached. Trying to write to these handles will cause the
application to abort.

One can place the following snippet at the top of a program to have all its
output be silently ignored instead.

    $*OUT=$*ERR=class {method print(*@args){}};


**WARNING** By default these _non-console_ applications will silently swallow
everything that is printed to `STDOUT` and `STDERR`.

To receive the output of the program it suffices to redirect it externally:

    rakuw.exe script.raku >stdout.txt 2>stderr.txt


Changes
=======

Recent changes and feature additions are documented in the `docs/ChangeLog`
text file.

To receive important notifications from the core developer team, please
subscribe to [the p6lert service](https://alerts.raku.org) using the RSS feed,
twitter, or [the p6lert commandline script](https://github.com/zoffixznet/perl6-p6lert).


Where to get help or answers to questions
=========================================

There are several mailing lists, IRC channels, and wikis available with help
for Raku and Rakudo. Figuring out the right one to use is often the biggest
battle. Here are some rough guidelines:

The central hub for Raku information is [raku.org](https://raku.org/).
This is always a good starting point.

If you have a question about Raku syntax or the right way to approach
a problem using Raku, you probably want the “perl6-users@perl.org”
mailing list or the [irc.freenode.net/#raku IRC
channel](https://webchat.freenode.net/?channels=#raku). The perl6-users
list is primarily for the people who want to use Raku to write
programs, so newbie questions are welcomed there.  Newbie questions
are also welcome on the #raku channel; the Rakudo and Raku
development teams tend to hang out there and are generally glad
to help. There's a Raku news aggregator at [Planet Raku](https://planet.raku.org/).

Questions about NQP can also be posted to the #raku IRC channel.
For questions about MoarVM, you can join #moarvm on freenode.


Reporting bugs
==============

See https://rakudo.org/issue-trackers


Submitting patches
==================

If you have a patch that fixes a bug or adds a new feature, please create a
pull request using github's pull request infrastructure.

See [our contribution guidelines](https://github.com/rakudo/rakudo/blob/master/CONTRIBUTING.md)
for more information.


License
=======

Rakudo is Copyright © 2008-2021, The Perl Foundation. Rakudo is distributed
under the terms of the Artistic License 2.0. For more details, see the full
text of the license in the file LICENSE.


Authors
=======

See CREDITS for the many people that have contributed to the development of the
Rakudo compiler.
