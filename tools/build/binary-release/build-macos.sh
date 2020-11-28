#!/usr/bin/env sh

set -o errexit
set -o pipefail

echo "========= Starting build"

echo "========= Downloading dependencies"
brew install perl
brew install gnu-tar

echo "========= Downloading release"
curl -o rakudo.tgz $RELEASE_URL

echo "========= Extracting release"
tar -xzf rakudo.tgz
cd rakudo-*

echo "========= Configuring Rakudo (includes building MoarVM and NQP)"
perl Configure.pl --gen-moar --gen-nqp --backends=moar --relocatable

echo "========= Building Rakudo"
make

echo "========= Installing Rakudo"
make install

echo "========= Testing Rakudo"
rm -r t/spec
prove -e install/bin/raku -vlr t

echo "========= Cloning Zef"
git clone https://github.com/ugexe/zef.git

echo "========= Installing Zef"
pushd zef
../install/bin/raku -I. bin/zef install .
popd

echo "========= Copying auxiliary files"
cp -r tools/build/binary-release/MacOS/* install
cp LICENSE install

echo "========= Preparing archive"
FILE_NAME=rakudo-moar-$VERSION-$REVISION-macos-x86_64-clang
mv install $FILE_NAME
gtar -zcv --owner=0 --group=0 --numeric-owner -f ../rakudo-macos.tar.gz $FILE_NAME

