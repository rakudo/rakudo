#!/usr/bin/env sh

# This script should be allowed to run sudo in a CentOS 7 installation (a
# container will do just fine).
# For some strange reason the environment variables are lost when running this
# script with `sudo` in azure pipelines. So we just do sudo ourselves for the
# dependency install part.

set -o errexit
set -o pipefail

echo "========= Starting build"

echo "========= Updating CentOS 7"
sudo yum -y update
sudo yum clean all

echo "========= install a new enough gcc"
sudo yum -y install centos-release-scl

# Fix up repo specs for out of support CentOS 7 again
sudo sed -i s/mirror.centos.org/vault.centos.org/g /etc/yum.repos.d/*.repo
sudo sed -i s/^#.*baseurl=http/baseurl=http/g /etc/yum.repos.d/*.repo
sudo sed -i s/^mirrorlist=http/#mirrorlist=http/g /etc/yum.repos.d/*.repo

sudo yum -y install devtoolset-8
# Somehow scl_source fails on Azure CI (but works in an identical local container).
# So just skip scl_source entirely and just source the target file directly.
#source scl_source enable devtoolset-8
source /opt/rh/devtoolset-8/enable

echo "========= Downloading dependencies"
sudo yum -y install curl git perl perl-core

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

