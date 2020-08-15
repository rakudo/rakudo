Binary release guide
====================

The process of building a release on the different platforms is largely automated. There is a build pipeline setup utilizing the Azure Pipelines infrastructure.
The process of building is not started automatically, but has to be triggered manually. To do so do:

- Visit the [Azure Pipelines website](https://dev.azure.com/rakudo/rakudo/_build?definitionId=1&_a=summary)
- Click on the blue 'Run pipeline' button at the top right.
- Under 'Advanced options' click on 'Variables'.
- Click on 'BUILD_PRECOMP_RELEASE' and change to 'yes'.
- Click the blue 'Update' button at the bottom.
- Change the 'RELEASE_URL', 'VERSION' and 'REVISION' variables with the same procedure as necessary.
- Click the back arrow next to the 'Variables' heading.
- Click on the blue 'Run' button at the bottom.
- Wait for the build to finish. All the Jobs will have a green tick next to them once the build is finished.
- In the upper box below the grey 'Related' heading click on '4 published'.
- Download the 'rakudo-moar-builds-*.tar.gz' file.
- Extract the archive.
- Verify all three archives have a size >10MB.
- Verify all three archives decompress successfully.

Sign the files:

   gpg -b -u your@gpgkey.org --armor rakudo-moar-2020.01-01-linux-x86_64.tar.gz
   gpg -b -u your@gpgkey.org --armor rakudo-moar-2020.01-01-macos-x86_64.tar.gz
   gpg -b -u your@gpgkey.org --armor rakudo-moar-2020.01-01-win-x86_64.zip

Upload the three archive files and corresponding `.asc` files to the <https://rakudo.org/> server.


Manual build
============

If the automated build above for some reason doesn't work, its possible to do the build steps manually. Having access to a respective Windows / MacOS / Linux machine is a prerequisite though.

Windows
=======

- Create a fresh Windows 10 VM as described in [the Windows guide](windows.md) if you don't have an up to date Windows 10 available (the download is 20GB and turns unusable after ~ 2 months).
- Install Git, Perl, and the C/C+ development tools as described in that guide.
- Download the [latest release](http://rakudo.org/files/rakudo) and extract it to `C:\rakudo`. The path will end up in the finished build, so if you don't want some other path to end up persisted in the build in some buildsystem variable put it in `C:\rakudo`.
- Open the 'Developer Command Prompt for VS 20XX'.
- Navigate to `C:\rakudo\`.
- `perl Configure.pl --gen-moar --gen-nqp --backends=moar --relocatable`
- `nmake install`
- `nmake test`
- In `C:\rakudo` do `git clone https://github.com/ugexe/zef.git` and `cd zef` and `C:\rakudo\install\bin\raku.exe -I. bin\zef install .`
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
- In the container
```bash
yum -y update && yum clean all
yum install git perl perl-core gcc make
curl -sSL -o rakudo.tar.gz https://rakudo.org/dl/rakudo/rakudo-2020.01.tar.gz
tar -xzf rakudo.tar.gz
cd rakudo-*
perl Configure.pl --gen-moar --gen-nqp --backends=moar --relocatable
make test
make install
git clone https://github.com/ugexe/zef.git
cd zef
/rakudo-*/install/bin/raku -I. bin/zef install .
cd /rakudo-*
cp -r tools/build/binary-release/Linux/* install
mv install rakudo-2020.01
tar -zcv --owner=0 --group=0 --numeric-owner -f /rakudo-moar-2020.01-01-linux-x86_64.tar.gz rakudo-2020.01
```

- On the host linux (not inside the container) run `docker cp rakudo-build:/rakudo-moar-2020.01-01-linux-x86_64.tar.gz .` to copy the archive out of the container. If you happended to stop the container by exitting the console, type `docker start rakudo-build` to start it again and allow copying files out.
- Sign the tarball archive as described in `release_guide.pod`.
- Upload the tarball and signature as described in `release_guide.pod`.


Mac OS
======

- Install XCode from the App Store.
- Open a terminal and do the following:

    curl -L -o /Applications/rakudo-2020.01.tar.gz https://rakudo.org/dl/rakudo/rakudo-2020.01.tar.gz
    cd /Applications
    tar -xzf rakudo-2020.01.tar.gz
    cd rakudo-2020.01
    perl Configure.pl --gen-moar --gen-nqp --backends=moar --relocatable
    make install
    make test
    git clone https://github.com/ugexe/zef.git
    cd zef
    /Applications/rakudo-2020.01/install/bin/raku -I. bin/zef install .
    cd /Applications/rakudo-2020.01
    cp -r tools/build/binary-release/MacOS/* install
    mv install rakudo-2020.01
    tar -zcv --owner=0 --group=0 --numeric-owner -f /rakudo-moar-2020.01-01-macos-x86_64.tar.gz rakudo-2020.01

- Sign the tarball archive as described in `release_guide.pod`.
- Upload the tarball and signature as described in `release_guide.pod`.

