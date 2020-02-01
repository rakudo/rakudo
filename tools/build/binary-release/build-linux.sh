#!/usr/bin/env sh

# This script should be run in a CentOS 6 installation (a container will do
# just fine).

set -o errexit
set -o pipefail

# Update CentOS 6
yum -y update
yum clean all

# Install dependencies
yum -y install curl git perl perl-core gcc make

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
mv install rakudo-$VERSION
tar -zcv --owner=0 --group=0 --numeric-owner -f ../rakudo-linux.tar.gz rakudo-$VERSION

