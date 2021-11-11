# Raku Installation Guide for Windows

### Note 'Raku' was formerly known as 'Perl 6'.

## Step 1. Remove prior installation(s)

If there exists a prior installation of Raku (or Perl 6)
on your computer, remove it. Current installers are not adept 
at removing prior installations so it is recommended to remove them manually.

Go into the **Control Panel, Programs and Features**, and remove prior installations

    <ctrl><R> AppWiz.cpl

## Step 2. Install dependencies

Windows installers do not come with a "spec file" to automatically download and install dependencies as does Linux packages, such as 'dnf/rpm'. If not included in the installer, it is up to the user to figure them out on his own. Perl 6 / Raku comes with such a dependency for 'git', which is required to comfortably install packages with the Raku module installer 'zef'.

To install 'git', go to
[https://git-for-windows.github.io/](https://git-for-windows.github.io/)
and download the '??' file.

and scroll down from the top.

Note: to add git's UNIX (Linux) tools (ls, tar, curl, etc.) to the path. Accept all the other defaults.

1. inside the installer, on the + Adjust your PATH environment (window name at top) - Use Git and optional Unix tools from the Command Prompt (bottom selection)

![Fig. 1](/tools/raku-on-windows/pics/Raku-WinInstall-fig-1-git-setup.png)

    2) Or, after the fact:
       --> <win><r> sysdm.cpl
         --> Advanced (tab at the top)
          --> Environmental Variables (button, lower right)
           --> Path, Edit, add to the end
                    ;C:\Program Files\Git\usr\bin

![Fig. 2](/tools/raku-on-windows/pics/Raku-WinInstall-fig-2-releases.png)

![Fig. 3](/tools/raku-on-windows/pics/Raku-WinInstall-fig-3-license.png)

![Fig. 4](/tools/raku-on-windows/pics/Raku-WinInstall-fig-4-destination-folder.png)

![Fig. 5](/tools/raku-on-windows/pics/Raku-WinInstall-fig-5-env-vars.png)

![Fig. 6](/tools/raku-on-windows/pics/Raku-WinInstall-fig-6-ready2install.png)

![Fig. 7](/tools/raku-on-windows/pics/Raku-WinInstall-fig-7-permission2install.png

![Fig. 8](/tools/raku-on-windows/pics/Raku-WinInstall-fig-8-setup-complete.png



Note: any command terminals opened before the installation will have to be reopened to update the path to the Git commands.

## Install Perl 6 / Raku:

Install Raku (Perl 6) for Windows: Note: only the 64 bit version is supported. For a 32 bit version, as the mailing list for help.

[https://github.com/AntonOks/rakudo-star-win/releases](https://github.com/AntonOks/rakudo-star-win/releases)

Note: any command terminals opened before the installation will have to be reopened to update the path to Perl 6 / Raku.

