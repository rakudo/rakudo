# Platform specific notes

## Windows

It should be possible to build Rakudo with either MinGW or MSVC. The
[official releases](https://rakudo.org/downloads) are built using MSVC.

### Building with MSVC

To build with Visual Studio, select "Developer Command Prompt for VS"
from the "Visual Studio" folder in the Start menu. At the prompt like

```
C:\Program Files (x86)\Microsoft Visual Studio\2019\Community>
```

enter `VC\Auxiliary\Build\vcvars64.bat` which should output:

```
[vcvarsall.bat] Environment initialized for: 'x64'
```

Now you can [build Rakudo](README.md#building-and-installing-rakudo).

### Spectest requirements

You need a recent version of either Strawberry Perl or ActiveState Perl.

You will need a compiler - Strawberry Perl ships with one. You can also
use MSVC or Mingw gcc.

You need msys git installed and `\Program Files\Git\cmd` on your execution
path and **not** `\Program Files\Git\bin`.

You will also need a win32 curl program.

## macOS

### Dynamic libraries

If Rakudo is installed into the $HOME directory (this is also the case with
rakubrew <https://rakubrew.org/>) then some modules may fail to install due
to missing dynamic libraries. This could be caused by a situation in which
the library is not provided by macOS system itself but installed using
Homebrew or MacPorts package managers. In this case the library cannot be
loaded by "rakudo" binary. This is a security precaution taken by modern
versions of macOS. The precaution works by restricting the directories
available for loading dynamic libraries only to the system-predefined set
and those where the application binary is actually located. For example, if
`rakudo` binary is installed under `$HOME/raku/bin` then aside of the
system-wide locations it can access libraries in `$HOME/raku/lib` only.

For the above stated reason, if a problem with availability of a dynamic
library occurs then creating a symlink to the library in one of the
following locations could help:

- <rakudo binary location path>/../lib
- $HOME/lib (should be created if doesn't exist yet)

The latter is one of the allowed system locations.

Also, sometimes the library version provided by macOS itself is older than a
module expects. This could result in erratic behaviors of the module. In
such a case a solution would be to install a package with newer version of
the library and also create symlinks into one of the two locations.

*WARNING!* Be careful when symlinking! Some malicious software uses faked
dynamic libraries to get their code invoked by legitimate binaries! Make
sure that the .dylib file being linked does belong to a valid installed
package and its checksum is unchanged.
