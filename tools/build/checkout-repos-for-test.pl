#!/usr/bin/perl

=head1 TITLE

checkout-test-repos.pl -- clone and checkout rakudo, nqp and moar for testing

=cut

use 5.10.1;
use strict;
use warnings;
use IPC::Cmd qw<run>;
use Cwd qw(cwd);
use File::Spec::Functions qw(catdir updir);

use constant NQP_VERSION_FILE  => 'rakudo/tools/templates/NQP_REVISION';
use constant MOAR_VERSION_FILE => 'nqp/tools/templates/MOAR_REVISION';

my %repos = (
    rakudo => 'https://github.com/rakudo/rakudo.git',
    nqp    => 'https://github.com/Raku/nqp.git',
    MoarVM => 'https://github.com/MoarVM/MoarVM.git',
);

# 'none' means, don't clone repository. Not valid for rakudo.
# 'master' means checkout master.
# 'downstream' means checkout rev given in downstream repo. Not valid for rakudo.
# 'rev-DEADBEEF-https://github.com/url-to/repo' means checkout the given rev.
my ($rakudo, $nqp, $moar) = @ARGV;

sub exec_and_check {
    my $msg = pop;
    my @command = @_;

    open(my $handle, '-|', @command);
    my $out = '';
    while(<$handle>) {
        $out .= $_;
    }
    close $handle;

    if ($? >> 8 != 0) {
        print "\n===SORRY=== ERROR: $msg\n";
        print "The programs output was: $out\n";
        exit 1;
    }
}

sub checkout_rev {
    my ($name, $type, $downstream_file) = @_;
    my $back = cwd();
    if ($type eq 'master') {
        exec_and_check('git', 'clone', $repos{$name}, $name, "Cloning $name failed.");
        chdir $name;
        exec_and_check('git', 'checkout', '-f', 'master', "Checking out $name master failed.");
    }
    elsif ($type eq 'downstream') {
        die "Can't do downstream checkout for $name" unless $downstream_file;
        open my $rev_fh, '<', $downstream_file;
        my $rev = <$rev_fh>;
        chomp $rev;
        close $rev_fh;
        exec_and_check('git', 'clone', $repos{$name}, $name, "Cloning $name failed.");
        chdir $name;
        exec_and_check('git', 'checkout', '-f', $rev, "Checking out $name $rev from downstream failed.");
    }
    elsif ($type =~ /^rev-([a-zA-Z0-9]+)-(.+)/) {
        exec_and_check('git', 'clone', $2, $name, "Cloning $name from $2 failed.");
        chdir $name;
        exec_and_check('git', 'checkout', '-f', $1, "Checking out $name $1 failed.");
    }
    else {
        die "Invalid argument for $name given: $type";
    }
    chdir $back;
}

checkout_rev( 'rakudo', $rakudo );

unless ($nqp eq 'none') {
    checkout_rev( 'nqp', $nqp, NQP_VERSION_FILE);
}

unless ($moar eq 'none') {
    checkout_rev( 'MoarVM', $moar, MOAR_VERSION_FILE);
}

# vim: expandtab sw=4
