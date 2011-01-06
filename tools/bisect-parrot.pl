#!/usr/bin/env perl
use strict;
use warnings;
use FindBin;
use 5.010;
# not using autodie for open() because of [perl #81572]
use autodie qw(close chdir);
use File::Path qw(remove_tree);

die <<"USAGE" unless @ARGV == 1;
Usage: $0 <testfile>
Note that this script does some really drastic cleaning,
including 'git clean -xdf' in parrot/ and removing parrot_install.
Please run this script only in a source tree where you have no important
files (except those under version control)
USAGE

my $rakudo_dir = "$FindBin::Bin/..";

sub system_quietly {
    my ($program, @rest) = @_;
    open(my $h, '-|', $program, @rest) or die $!;
    1 while <$h>;
    close $h;
}

eval {

    say "Cleaning rakudo...";
    remove_tree "$rakudo_dir/parrot_install";
    chdir "$rakudo_dir";
    system("make",  "realclean") and 1;
    chdir "$rakudo_dir/parrot";
    say "..done";

    say "build parrot...";
    chdir "$rakudo_dir/parrot";
    system_quietly('git', 'clean', '-xdqf');
    system_quietly($^X, 'Configure.pl', "--prefix=$rakudo_dir/parrot_install",
            "--optimize", '--nomanicheck', '--cc=ccache gcc');
    system_quietly('make', '-j3');
    say "... installing parrot ...";
    system_quietly('make', 'install');
    say "... done building parrot";

    chdir $rakudo_dir;
    system_quietly($^X, 'Configure.pl');
    system('make', '-j3') == 0 or die "error during make: $!";
    exec('./perl6', $ARGV[0]);
};

if ($@) {
    say $@;
    exit 125;
}
