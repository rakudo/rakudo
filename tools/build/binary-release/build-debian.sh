#!/usr/bin/env sh

# This script should be run in a debian installation (a container will do
# just fine).

set -o errexit
set -o pipefail

# Update Debian
apt-get update

# Install dependencies
apt-get install -y curl git perl gcc make

# Download release file
curl -o rakudo.tgz $RELEASE_URL
tar -xzf rakudo.tgz
cd rakudo-*

# Build Rakudo
perl Configure.pl --gen-moar --gen-nqp --backends=moar --moar-option='--toolchain=gnu' --relocatable
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
cp -r tools/build/binary-release/Linux/* install
cp LICENSE install

FILE_NAME=rakudo-moar-$VERSION-$REVISION-linux-x86_64-gcc
mv install $FILE_NAME
tar -zcv --owner=0 --group=0 --numeric-owner -f ../rakudo-linux.tar.gz $FILE_NAME

