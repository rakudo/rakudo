Rakudo
======

Rakudo is a compiler and runtime for the Raku programming language.

This package includes the Rakudo compiler and the module installer Zef.


The Start Menu Shortcuts
========================

You'll find the following Shortcuts in the Start Menu `Rakudo` folder

- Rakudo enabled PowerShell and CMD consoles
    These consoles
    - set up the PATH variable to contain BOTH paths necessary to directly call
      `raku` and Raku scripts such as `zef`,
    - search for a C build toolchain and if found activate it, so installing
      modules which compile some C code works and
    - change the codepage to UTF-8 so input and output of unicode characters
      doesn't break.
- Rakudo REPL
    Short for `Read Eval Print Loop`. A Rakudo interactive command interpreter.
    Useful for easily experimenting with Raku.
- README
    This README. :-)
- Raku, Raku Docs and Raku Land Websites
    ...
- The Install Build Tools script
    A script to guide you through installing the Microsoft build tools necessary
    for Raku modules that contain parts written in the C language. See below for
    more detailed information.


Running Raku
============

If you chose to set the PATH variable during install or use the
`Rakudo Powershell` or `Rakudo CMD` start menu entries you can directly use the
`raku.exe` and `rakuw.exe` commands.

To run a Raku program, open a command prompt and type

    raku.exe my_script.raku

To start the REPL type `raku.exe` without an argument

    raku.exe


Installing modules
==================

To install Raku modules you can use the Zef module installer.

    zef install JSON::Fast

Modules will be installed into this Raku package and will thus be available even
when moving this package.


Native code modules
-------------------

To install modules that require a compiler toolchain, you need to have the
Microsoft Visual C compiler installed. The freely available
Microsoft BuildTools contain that compiler. There is a start menu entry to
start a CLI based wizard to guide you through the installation.

Alternatively you can install the BuildTools manually. They can be downloaded
[here](https://visualstudio.microsoft.com/thank-you-downloading-visual-studio/?sku=BuildTools).
You'll need to select the `C++ build tools` and recommended components.

The compiler is only usable in a special Build Tools CMD / PowerShell window.
The start menu entries `Rakudo Powershell` and `Rakudo CMD` provide for this.


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


Unattended Installation
=======================

The MSI supports unattended installation. Two variables control the behavior:

- `INSTALLDIR` sets the path to install Rakudo to. Defaults to a `Rakudo` subfolder in the `Program Files` folder.
- `ADDTOPATH` enables/disables adding the Rakudo bin dirs to the PATH variable. `0` disables, `1` enables. Defaults to `1`.

The following command can be used to install:

    msiexec /i "c:\path\to\rakudo.msi" /quiet /qn /log "c:\path\to\install.log" INSTALLDIR="C:\rakudo" ADDTOPATH=0


Changes
=======

Recent changes and feature additions are documented in the `docs/ChangeLog`
text file.


Where to get help or answers to questions
=========================================

There are several mailing lists, IRC channels, and wikis available with help
for Raku and Rakudo. Figuring out the right one to use is often the biggest
battle. Here are some rough guidelines:

The central hub for Raku information is [raku.org](https://raku.org/). This is
always a good starting point.

If you have a question about Raku syntax or the right way to approach a problem
using Raku, you probably want the “perl6-users@perl.org” mailing list or the
[irc.libera.chat/#raku IRC channel](https://web.libera.chat/#raku). The
perl6-users list is primarily for the people who want to use Raku to write
programs, so newbie questions are welcomed there.  Newbie questions are also
welcome on the #raku channel; the Rakudo and Raku development teams tend to hang
out there and are generally glad to help. There's a Raku news aggregator at
[Planet Raku](https://planet.raku.org/).

Questions about NQP can also be posted to the #raku IRC channel. For questions
about MoarVM, you can join #moarvm on Libera.


Reporting bugs
==============

See https://rakudo.org/issue-trackers


Submitting patches
==================

If you have a patch that fixes a bug or adds a new feature, please create a pull
request using github's pull request infrastructure.

See [our contribution guidelines](https://github.com/rakudo/rakudo/blob/master/CONTRIBUTING.md)
for more information.


License
=======

Rakudo is Copyright © 2008-2025, The Raku Foundation. Rakudo is distributed
under the terms of the Artistic License 2.0. For more details, see the full
text of the license in the file LICENSE.


Authors
=======

See CREDITS for the many people that have contributed to the development of the
Rakudo compiler.
