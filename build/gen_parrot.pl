#! perl
# Copyright (C) 2009 The Perl Foundation

=head1 TITLE

gen_parrot.pl - script to obtain and build Parrot for Rakudo

=head2 SYNOPSIS

    perl gen_parrot.pl [--parrot --configure=options]

=head2 DESCRIPTION

Maintains an appropriate copy of Parrot in the parrot/ subdirectory.
The revision of Parrot to be used in the build is given by the
build/PARROT_REVISION file.

=cut

use strict;
use warnings;
use 5.008;

use lib "build/lib";
use Parrot::CompareRevisions qw(compare_parrot_revs parse_parrot_revision_file);

#  Work out slash character to use.
my $slash = $^O eq 'MSWin32' ? '\\' : '/';

# Determine the revision of Parrot we require
my ($req, $reqpar) = parse_parrot_revision_file;

{
    no warnings;
    eval {
        my $parrot_bin_folder = "parrot_install${slash}bin";
        if (-e $parrot_bin_folder && open my $REV, '-|', "$parrot_bin_folder${slash}parrot_config git_describe") {
            my $revision = <$REV>;
            close $REV;
            $revision =~ s/\s.*//s;
            if (compare_parrot_revs($revision, $req) >= 0) {
                print "Parrot $revision already available ($req required)\n";
                exit(0);
            }
        }
    };
}

print "Checking out Parrot $req via git...\n";
my $fetched = 0;
if (-d 'parrot') {
    if (-d 'parrot/.svn') {
        die "===SORRY===\n"
           ."Your 'parrot' directory is still an SVN repository.\n"
           ."Parrot switched to git recently; in order to replace your\n"
           ."repository by a git repository, please manually delete\n"
           ."the 'parrot' directory, and then re-run the command that caused\n"
           ."this error message\n";
    }
} else {
    system_or_die(qw(git clone git://github.com/parrot/parrot.git parrot));
    $fetched = 1;
}

chdir('parrot') || die "Can't chdir to 'parrot': $!";

system_or_die(qw(git fetch)) unless $fetched;
system_or_die(qw(git checkout),  $req);

##  If we have a Makefile from a previous build, do a 'make realclean'
if (-f 'Makefile') {
    my %config = read_parrot_config();
    my $make = $config{'make'};
    if ($make) {
        print "\nPerforming '$make realclean' ...\n";
        system_or_die($make, "realclean");
    }
}

print "\nConfiguring Parrot ...\n";
my @config_command = ($^X, 'Configure.pl', @ARGV);
print "@config_command\n";
system_or_die( @config_command );

print "\nBuilding Parrot ...\n";
my %config = read_parrot_config();
my $make = $config{'make'} or exit(1);
system_or_die($make, 'install-dev');

sub read_parrot_config {
    my %config = ();
    if (open my $CFG, "config_lib.pir") {
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
