Windows
=======

- Create a fresh Windows 10 VM as described in [the Windows guide](windows.md) if you don't have an up to date Windows 10 available (the download is 20GB and turns unusable after ~ 2 months).
- Install Git, Perl 5, and the C/C+ development tools as described in that guide.
- Download the [latest release](http://rakudo.org/files/rakudo) and extract it to `C:\rakudo`. The path will end up in the finished build, so if you don't want some other path to end up persisted in the build in some buildsystem variable put it in `C:\rakudo`.
- Open the 'Developer Command Prompt for VS 20XX'.
- Navigate to `C:\rakudo\`.
- `perl Configure.pl --gen-moar --gen-nqp --backends=moar --relocatable`
- `nmake install`
- `nmake test`
- In `C:\rakudo` do `git clone https://github.com/ugexe/zef.git` and `cd zef` and `C:\rakudo\install\bin\perl6.exe -I. bin\zef install .`
- Copy all files in the `tools\build\binary-release\Windows` folder into the `install` folder.
- Rename the `install` folder to `rakudo-20XX.XX`.
- Create a `.zip` archive. Name it `rakudo-moar-20XX.XX-01-win-x86_64.zip`.
- Copy the `.zip` archive out of the Windows VM using the Virtual Box `File Manager` (*Machine -> File Manager...*)
- Sign the `.zip` archive as described in `release_guide.pod`.
- Upload the `.zip` archive and signature as described in `release_guide.pod`.


Linux
=====

On Linux the major compatibility breaker is glibc. Basic rule: stuff compiled with a specific version of glibc can run on newer versions, but not older ones. So when compatibility with multiple Linuxes is desired one has to compile on a distribution with a reasonably old glibc.
[this overview of glibc Versions](https://gist.github.com/wagenet/35adca1a032cec2999d47b6c40aa45b1) is quite helpful with that.
As of 2019-07-08 CentOS 6 (using glibc 2.12) is a good pick.

- Use a 64 bit Linux.
- Install docker.
- `docker run -it --name=rakudo-build centos:6 bash`

    yum -y update && yum clean all
    yum install git perl perl-core gcc make
    curl -L -o rakudo-2019.03.1.tar.gz https://rakudo.org/dl/rakudo/rakudo-2019.03.1.tar.gz
    tar -xzf rakudo-2019.03.1.tar.gz
    cd rakudo-2019.03.1
    perl Configure.pl --gen-moar --gen-nqp --backends=moar --relocatable
    make install
    make test
    git clone https://github.com/ugexe/zef.git
    cd zef
    /rakudo-2019.03.1/install/bin/raku -I. bin/zef install .
    cd /rakudo-2019.03.1
    cp -r tools/build/binary-release/Linux/* install
    mv install rakudo-2019.03.1
    tar -zcv --owner=0 --group=0 --numeric-owner -f /rakudo-moar-2019.03.1-01-linux-x86_64.tar.gz rakudo-2019.03.1

- On the host linux (not inside the container) run `docker cp rakudo-build:/rakudo-moar-2019.03.1-01-linux-x86_64.tar.gz .` to copy the archive out of the container. If you happended to stop the container by exitting the console, type `docker start rakudo-build` to start it again and allow copying files out.
- Sign the tarball archive as described in `release_guide.pod`.
- Upload the tarball and signature as described in `release_guide.pod`.


Mac OS
======

- Install XCode from the App Store.
- Open a terminal and do the following:

    curl -L -o /Applications/rakudo-2019.03.1.tar.gz https://rakudo.org/dl/rakudo/rakudo-2019.03.1.tar.gz
    cd /Applications
    tar -xzf rakudo-2019.03.1.tar.gz
    cd rakudo-2019.03.1
    perl Configure.pl --gen-moar --gen-nqp --backends=moar --relocatable
    make install
    make test
    git clone https://github.com/ugexe/zef.git
    cd zef
    /Applications/rakudo-2019.03.1/install/bin/raku -I. bin/zef install .
    cd /Applications/rakudo-2019.03.1
    cp -r tools/build/binary-release/MacOS/* install
    mv install rakudo-2019.03.1
    tar -zcv --owner=0 --group=0 --numeric-owner -f /rakudo-moar-2019.03.1-01-macos-x86_64.tar.gz rakudo-2019.03.1

- Sign the tarball archive as described in `release_guide.pod`.
- Upload the tarball and signature as described in `release_guide.pod`.

