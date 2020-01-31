#!/usr/bin/env sh

set -o errexit
set -o pipefail

# Install Dependencies
brew install perl
brew install gnu-tar

# Download release file
curl -o rakudo.tgz $RELEASE_URL
tar -xzf rakudo.tgz
cd rakudo-*

# Build Rakudo
perl Configure.pl --gen-moar --gen-nqp --backends=moar --relocatable
make
make install

# Test the build
make test

# Build Zef
git clone https://github.com/ugexe/zef.git
pushd zef
../install/bin/raku -I. bin/zef install .
popd

# Prepare the package
cp -r tools/build/binary-release/MacOS/* install
cp LICENSE install
mv install rakudo-$VERSION
gtar -zcv --owner=0 --group=0 --numeric-owner -f ../rakudo-macos.tar.gz rakudo-$VERSION

