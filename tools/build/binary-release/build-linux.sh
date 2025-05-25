#!/usr/bin/env bash

# This script should be allowed to run sudo in a debian based distro (a
# container will do just fine).
# For some strange reason the environment variables are lost when running this
# script with `sudo` in azure pipelines. So we just do sudo ourselves for the
# dependency install part.

set -o errexit
set -o pipefail

echo "========= Starting build"

echo "========= Updating distro"
sudo apt update

echo "========= Downloading dependencies"
sudo apt -y install build-essential curl git perl

echo "========= Downloading release"
curl -o rakudo.tgz $RELEASE_URL

echo "========= Extracting release"
tar -xzf rakudo.tgz
cd rakudo-*

echo "========= Configuring Rakudo (includes building MoarVM and NQP)"
perl Configure.pl --gen-moar --gen-nqp --backends=moar --moar-option='--toolchain=gnu' --relocatable

echo "========= Building Rakudo"
make

echo "========= Installing Rakudo"
make install

echo "========= Testing Rakudo"
rm -fr t/spec
prove -e install/bin/raku -vlr t

echo "========= Cloning Zef"
git clone https://github.com/ugexe/zef.git

echo "========= Installing Zef"
pushd zef
../install/bin/raku -I. bin/zef install .
popd

echo "========= Copying auxiliary files"
cp -r tools/build/binary-release/assets/Linux/* install
cp LICENSE install

echo "========= Preparing archive"
FILE_NAME=rakudo-moar-$VERSION-$REVISION-linux-x86_64-gcc
mv install $FILE_NAME
tar -zcv --owner=0 --group=0 --numeric-owner -f ../rakudo-linux.tar.gz $FILE_NAME

