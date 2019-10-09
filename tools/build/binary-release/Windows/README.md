Rakudo Perl 6
==============

This is a pre-built package of Rakudo Perl 6, a Perl 6 compiler.

This package includes the Rakudo Perl 6 compiler and the module installer Zef.


Running Perl 6
--------------

To run a Perl 6 program, open a command prompt and type

    C:\path\to\this\folder\bin\perl6.exe my_script.p6

or start a REPL by calling `perl6.exe` without an argument

    C:\path\to\this\folder\bin\perl6.exe

To add the relevant paths to your environment so you don't have to type the
full path execute the following script in CMD:

    C:\path\to\this\folder\scripts\set-env.bat

or when using Powershell (note the dot at the beginning):

    . C:\path\to\this\folder\scripts\set-env.ps1


Installing modules
------------------

To install Perl 6 modules you can use the Zef module installer.

    C:\path\to\this\folder\bin\perl6.exe C:\path\to\this\folder\share\perl6\site\bin\zef install JSON::Fast

Modules will be installed into this Perl 6 package and will thus be available
even when moving this package.


### Native code modules

To install modules that require a compiler toolchain, you need to have the
Microsoft Visual C compiler installed. The freely available
Microsoft BuildTools contain that compiler. You can use the installer script

    C:\path\to\this\folder\bin\vs_build_tools_install_assistant.ps1

to guide you through the installation.

Alternatively you can install the BuildTools manually. They can be downloaded
[here](https://visualstudio.microsoft.com/thank-you-downloading-visual-studio/?sku=BuildTools).
You'll need to select the `C++ build tools` and recommended components.

The compiler is only usable in special Build Tools CMD / PowerShell windows.

To make use of the Build Tools in Rakudo using CMD:
- start a CMD window using
    Start Menu -> Visual Studio 2019 ->
    x86 / x64 Native Tools Command Prompt for VS 2019
- Execute $scriptPath\set-env.bat

To make use of the Build Tools in Rakudo using PowerShell:
- start a PowerShell window using
    Start Menu -> Visual Studio 2019 -> Developer PowerShell for VS 2019
- Execute $scriptPath\set-env.ps1


Changes
-------

Recent changes and feature additions are documented in the `docs/ChangeLog`
text file.

To receive important notifications from the core developer team, please
subscribe to [the p6lert service](https://alerts.perl6.org) using the RSS feed,
twitter, or [the p6lert commandline script](https://github.com/zoffixznet/perl6-p6lert).


Where to get help or answers to questions
-----------------------------------------

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


Reporting bugs
--------------

See https://rakudo.org/bugs


Submitting patches
------------------

If you have a patch that fixes a bug or adds a new feature, please
create a pull request using github's pull request infrastructure.

See [our contribution guidelines](https://github.com/rakudo/rakudo/blob/master/CONTRIBUTING.md)
for more information.


License
-------

Rakudo Perl 6 is Copyright © 2008-2019, The Perl Foundation. Rakudo Perl 6
is distributed under the terms of the Artistic License 2.0. For more
details, see the full text of the license in the file LICENSE.


AUTHOR
------

See CREDITS for the many people that have contributed
to the development of the Rakudo compiler.
