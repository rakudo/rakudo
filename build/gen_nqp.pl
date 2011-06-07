#! perl
# Copyright (C) 2011 The Perl Foundation

=head1 TITLE

gen_nqp.pl - script to obtain, build and install NQP for Rakudo

=head2 SYNOPSIS

    perl gen_nqp.pl

=head2 DESCRIPTION

Assumes that we already have a Parrot installed in the parrot_install
directory. Gets a checkout of NQP and builds and install it.

=cut

use strict;
use warnings;
use 5.008;
use lib 'build/lib';
use Parrot::CompareRevisions qw/read_config/;

#  Work out slash character to use.
my $slash = $^O eq 'MSWin32' ? '\\' : '/';
my $exe   = $^O eq 'MSWin32' ? '.exe' : '';

# Ensure we have a parrot_install
unless (-d "parrot_install") {
    die "===SORRY===\n"
       ."You must have already installed a Parrot to parrot_install\n"
       ."before running gen_nqp (try configuring with --gen-parrot).\n";
}

my ($parrot_config, %config) = read_config("parrot_install/bin/parrot_config$exe");
my $make = $config{'make'};
unless ($make) {
    die "Can't determine which 'make' utility parrot was built with, aborting\n";
}

# Get NQP, or update it.
print "Checking out latest NQP via git...\n";
my $fetched = 0;
unless (-d 'nqp') {
    system_or_die(qw(git clone git://github.com/perl6/nqp.git nqp));
    $fetched = 1;
}

chdir('nqp') || die "Can't chdir to 'nqp': $!";

system_or_die(qw(git pull)) unless $fetched;

##  If we have a Makefile from a previous build, do a 'make realclean'
if (-f 'Makefile' && $make) {
    print "\nPerforming '$make realclean' ...\n";
    system_or_die($make, "realclean");
}

print "\nConfiguring NQP ...\n";
my @config_command = ($^X, 'Configure.pl', "--with-parrot=../parrot_install/bin/parrot");
print "@config_command\n";
system_or_die( @config_command );

print "\nBuilding NQP ...\n";
system_or_die($make, 'install');

sub system_or_die {
    my @cmd = @_;

    system( @cmd ) == 0
        or die "Command failed (status $?): @cmd\n";
}
