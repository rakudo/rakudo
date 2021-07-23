Windows MSI Installer
=====================

The Windows MSI installer installs a Rakudo and Zef on a Windows machine.
The files it installs are essentially the same contained in the Rakudo precompiled release archives.
The installer has the following features:

- Change the path to install to
- Optionally add the respective bin folders to the PATH variable
- Register the `raku.exe` executable as a handler for `.raku` and `.rakutest` files. A respective `.ico` file is provided.
- Provide a set of Start menu shortcuts:
    - Rakudo enabled PowerShell and CMD consoles
    - Rakudo REPL
    - README
    - Raku, Raku Docs and Raku Land Websites
    - The Install Build Tools script
- Unattended Installation

Unattended Installation
=======================

The MSI supports unattended installation. Two variables control the behavior:

- `INSTALLDIR` sets the path to install Rakudo to. Defaults to a `Rakudo` subfolder in the `Program Files` folder.
- `ADDTOPATH` enables/disables adding the Rakudo bin dirs to the PATH variable. `0` disables, `1` enables. Defaults to `1`.

The following command can be used to install:

    msiexec /i "c:\path\to\rakudo.msi" /quiet /qn /log "c:\path\to\install.log" INSTALLDIR="C:\rakudo" ADDTOPATH=0


How building an MSI installer works
===================================

The MSI build is based on the WiX toolkit. The build works in multiple stages and is orchestrated by the `build-msi.ps1` script.
That script is called by the `tools\build\binary-release\build-windows.ps1` script.

Parameters of the `build-msi.ps1` script are:

- The rakudo version, e.g. 2021.06.1
- The path to the folder containing the precompiled Rakudo release for Windows
- The path of the MSI file to generate

That precompiled Rakudo release folder is what will be turned into the MSI.

The actual WiX logic is contained in the `rakudo.wxs` file.

The following steps need to happen:

- Call `heat.exe` to scan the Rakudo release folder and create an index of all files.
- Call `heat.exe` to scan the `/assets` folder and create an index of those files.
- Call `candle.exe` to preprocess the generated files and the `rakudo.wxs` file into `.wxsobj` files.
- Call `light.exe` to finally turn all those `.wxsobj` files (and the actual files to install) into the final `.msi`.


Raw image files
===============

The xcf Gimp image files of the installer background are kept in a separate repository to keep the size small.
They are located at <https://github.com/Raku/marketing/tree/master/raw-assets/MSI-installer>.
