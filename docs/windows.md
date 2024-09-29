# Building on Windows

## VM

Let's assume we're starting out with no hardware:

Get virtual box for your platform here:

    https://www.virtualbox.org/wiki/Downloads

Get a Windows 10 evaluation copy from here:

    https://developer.microsoft.com/en-us/windows/downloads/virtual-machines

## Prereqs

The evaluation copy comes with a copy of Visual Studio.

### Strawberry Perl

Install Strawberry Perl from:

    https://strawberryperl.com/

### Visual Studio

Run the VS installer; modify the existing install, and select "Desktop
Development with C++"; This will make the command line tools available
in the "Developer Command Prompt for VS 20XX"

### Git

Install git from:

    https://git-scm.com/download/win

## Rakudo

Clone rakudo; in the VS command prompt:

    C:\Users\user git clone https://github.com/rakudo/rakudo.git

Configure rakudo:
    C:\Users\user cd rakudo
    C:\Users\user perl Configure.pl --backends=moar --gen-moar

The configure system should correctly detect your Visual Studio tools.

This will git clone nqp & MoarVM, then build MoarVM, nqp.
To build rakudo itself (and install it into a local ./install directory):

You might want to "copy config.status config.bat" to save this config, so
you can later run "config" to perform the config step.

You may wish to use "--gen-moar=master" or "--gen-nqp=master" to get the
latest version of those repositories.

Build rakudo (for Strawberry/gcc) :
    C:\Users\user gmake install

Build rakudo (for ActiveState/VS) :
    C:\Users\user nmake install

## Test

Now you can run (using the appropriate make command) the
builtin rakudo tests:

    C:\Users\user nmake test

Or the spectest suite (note that this will use git to download the
test suite)

    C:\Users\user nmake spectest
