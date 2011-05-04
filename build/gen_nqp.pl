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

#  Work out slash character to use.
my $slash = $^O eq 'MSWin32' ? '\\' : '/';

# Ensure we have a parrot_install
unless (-d "parrot_install") {
    die "===SORRY===\n"
       ."You must have already installed a Parrot to parrot_install\n"
       ."before running gen_nqp (try configuring with --gen-parrot).\n";
}

# Get NQP, or update it.
print "Checking out latest NQP via git...\n";
my $fetched = 0;
unless (-d 'nqp') {
    system_or_die(qw(git clone git://github.com/perl6/nqp.git nqp));
    $fetched = 1;
}

chdir('nqp') || die "Can't chdir to 'nqp': $!";

system_or_die(qw(git fetch)) unless $fetched;
system_or_die(qw(git checkout),  'master');

##  If we have a Makefile from a previous build, do a 'make realclean'
if (-f 'Makefile') {
    my %config = read_parrot_config();
    my $make = $config{'make'};
    if ($make) {
        print "\nPerforming '$make realclean' ...\n";
        system_or_die($make, "realclean");
    }
}

print "\nConfiguring NQP ...\n";
my @config_command = ($^X, 'Configure.pl', "--parrot-config=../parrot_install/bin/parrot_config");
print "@config_command\n";
system_or_die( @config_command );

print "\nBuilding NQP ...\n";
my %config = read_parrot_config();
my $make = $config{'make'} or exit(1);
system_or_die($make, 'install');

sub read_parrot_config {
    my %config = ();
    if (open my $CFG, "../parrot/config_lib.pir") {
        while (<$CFG>) {
            if (/P0\["(.*?)"], "(.*?)"/) { $config{$1} = $2 }
        }
        close $CFG;
    }
    %config;
}
    
sub system_or_die {
    my @cmd = @_;

    system( @cmd ) == 0
        or die "Command failed (status $?): @cmd\n";
}
