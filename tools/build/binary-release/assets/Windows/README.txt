Rakudo
======

Rakudo is a compiler and runtime for the Raku programming language.

This package includes the Rakudo compiler and the module installer Zef.

Contents
========

- Set-Env
- Running Raku
- Installing Modules
    - Native Code Modules
- PATH
- Preventing Garbled Text Output
- Non-Console Applications
- Changes
- Where to Get Help or Answers to Questions
- Reporting Bugs
- Submitting Patches


Set-Env
=======

This package provides a script to set up a terminal window to work nicely with
Raku. It

- sets up the PATH variable to contain BOTH paths necessary to directly call
  `raku` and Raku scripts such as `zef`,
- searches for a C build toolchain and if found activates it, so installing
  modules which compile some C code works and
- changes the codepage to UTF-8 so input and output of unicode characters
  doesn't break.

See the following sections for a deeper explanation why all of this is
necessary and how to do it manually.

To run `set-env` execute the following in CMD:

    C:\path\to\this\folder\scripts\set-env.bat

or when using Powershell (note the dot at the beginning):

    . C:\path\to\this\folder\scripts\set-env.ps1


Running Raku
============

Once your terminal is set up you can start an interactive Raku environment by
typing

    raku.exe

To run a Raku script type

    raku.exe my_script.raku


Installing Modules
==================

To install Raku modules you can use the Zef module installer.

    zef install JSON::Fast

Modules will be installed into this Raku package and will thus be available
even when moving this package.


Native Code Modules
-------------------

To install modules that require a compiler toolchain, you need to have the
Microsoft Visual C compiler installed. The freely available
Microsoft BuildTools contain that compiler. You can use the installer script

    C:\path\to\this\folder\scripts\vs_build_tools_install_assistant.ps1

to guide you through the installation.

Alternatively you can install the BuildTools manually. They can be downloaded
[here](https://aka.ms/vs/stable/vs_BuildTools.exe).
You'll need to select the `C++ build tools` and recommended components.

The compiler is only usable in a special Build Tools CMD / PowerShell window.
Once the BuildTools  are installed you should see new entries in the start
menu:

Start Menu -> Visual Studio 20XX -> x64 Native Tools Command Prompt for VS

The `set-env` script automatically sets up the terminal for BuildTools as well.


PATH
====

THERE IS MORE THAN ONE FOLDER YOU NEED TO ADD TO YOUR PATH!

`raku` and it's associated programs are located in the `bin\` subfolder.
Installed Raku scripts, including `zef`, are located in the
`share\perl6\site\bin\` subfolder. So both need to be in your PATH.


Preventing Garbled Text Output
==============================

<boring-but-important>

First a bit of background. Windows supports two types of text encoding that
applications can choose from.

The ancient `Codepage based` approach - texts are made up of 8-bit bytes. The
configured codepage determines how to decode those bytes. Different languages
bring their own codepages with the characters they need.

Later came the newer `Unicode based` approach - texts are made up of 16-bit
shorts, no codepages are necessary anymore as the characters of most languages
fit into those 16 bits. The approach is still a bit limited as a lot more, but
not all unicode characters fit into those 16 bits. For that and many other
reasons, UTF-8 came to be. That's an 8-bit based encoding where characters can,
but don't need to use more than one byte. Most modern applications use UTF-8.
On Windows UTF-8 based applications are once again codepage based, but use a
special Codepage - 65001 - that represents UTF-8.

The default codepage is sadly not 65001, but some other codepage depending on
the language of Windows. This is so as to not break compatibility with ancient
applications using that other codepage.

When using a UTF-8 application with a wrong codepage, the text output you see
is garbled. Raku is such a UTF-8 application. Thus when using Raku in a console
window with a non-UTF-8 codepage input and output of Unicode characters will
break.

</boring-but-important>

So how to solve this?

In a CMD window execute `chcp 65001` to set the codepage. You have to execute
this every time you open a new CMD window.

In a Powershell window execute

    $OutputEncoding = [console]::InputEncoding = [console]::OutputEncoding = New-Object System.Text.UTF8Encoding

to set the codepage. Once again you have to do this every time you open a new
window. To do that automatically, add the above line to your Powershell profile.

It's also possible to configure the UTF-8 codepage globally in Windows, but that's
a beta feature and breaks backward compatibility with a few legacy command line
applications. In the Windows System Settings navigate here:

Settings
  --> Time & language
  --> Language & region
  --> Administrative language settings
  --> Change system locale

Then check `Beta: Use Unicode UTF-8 for worldwide language support` and restart
the computer.

Last but not least, the `set-env.ps1` and `set-env.bat` scripts perform the
respective commands automatically.


Non-Console Applications
========================

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


Where to Get Help or Answers to Questions
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
programs, so newbie questions are welcomed there.  Newbie questions
are also welcome on the #raku channel; the Rakudo and Raku
development teams tend to hang out there and are generally glad
to help. There's a Raku news aggregator at [Planet Raku](https://planet.raku.org/).

Questions about NQP can also be posted to the #raku IRC channel.
For questions about MoarVM, you can join #moarvm on Libera.


Reporting Bugs
==============

See https://rakudo.org/issue-trackers


Submitting Patches
==================

If you have a patch that fixes a bug or adds a new feature, please create a
pull request using GitHub's pull request infrastructure.

See [our contribution guidelines](https://github.com/rakudo/rakudo/blob/master/CONTRIBUTING.md)
for more information.


License
=======

Rakudo is Copyright © 2008-2026, The Raku Foundation. Rakudo is distributed
under the terms of the Artistic License 2.0. For more details, see the full
text of the license in the file LICENSE.


Authors
=======

See CREDITS for the many people that have contributed to the development of the
Rakudo compiler.
