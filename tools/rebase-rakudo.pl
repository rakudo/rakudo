#!/usr/bin/perl

use Cwd;
use File::Spec;
use Getopt::Long;

my $help      = 0;
my $verbose   = 0;
my $realclean = 1;
my $jit       = 0;
my $test      = 0;
my $codetest  = 0;
my $spectest  = 0;

GetOptions('help!'      => \$help,
           'verbose!'   => \$verbose,
           'realclean!' => \$realclean,
           'jit!'       => \$jit,
           'test!'      => \$test,
           'codetest!'  => \$codetest,
           'spectest!'  => \$spectest,
          );

die <<"USAGE" if $help;
usage:   $0 [options]
options:
  -h|--help        print this usage info
  -v|--verbose     show verbose output
  -j|--jit         enable JIT during configuration
  -t|--test        run 'make test'      after build
  -c|--codetest    run 'make codetest'  after build
  -s|--spectest    run 'make spectest'  after build
  -r|--realclean   run 'make realclean' before rebase, and configure after
                   (default on; use --no-realclean to turn it off)

For extra golfing goodness, try creating a shell alias
to run this script with your favorite options.
USAGE

print "Looking for Parrot root ...\n";
chdir '..' until -f 'parrotbug' || cwd eq File::Spec->rootdir;
die "Couldn't find Parrot root at or above current directory.\n"
    unless -f 'parrotbug';
print "... found at '" . cwd . "'.\n\n" if $verbose;

if ($realclean && -e 'Makefile') {
    print "Cleaning old build ...\n";
    run_command(qw( make realclean ));
}

print "Rebasing Parrot ...\n";
my @rebase = -e '.git' ? qw( git svn rebase ) :
             -e '.svn' ? qw( svn up         ) :
                         qw( svk pull       ) ;
run_command(@rebase);

unless (-e 'Makefile') {
    print "Configuring Parrot ...\n";
    my   @config = qw( perl Configure.pl );
    push @config,  qw( --jitcapable=0    ) unless $jit;
    run_command(@config);
}

print "Making Parrot ...\n";
run_command(qw( make ));

print "Making Rakudo ...\n";
run_command(qw( make perl6 ));

if ($test) {
    print "Running standard Parrot tests ...\n";
    run_command(qw( make test ));
}

if ($codetest) {
    print "Running coding standard compliance tests ...\n";
    run_command(qw( make codetest ));
}

if ($spectest) {
    print "Running Perl 6 spec tests ...\n";
    chdir 'languages/perl6';
    run_command(qw( make spectest ));
}

print "Done.\n";

sub run_command {
    if ($verbose) {
        my $status = system @_;
        die "Could not '@_': $!\n" if $status == -1;
        print "\n";
    }
    else {
        my $output = `@_ 2>&1`;
        die "Could not '@_': $!\n" unless defined $output;
        print $output if $?;
    }

    die "\n'@_' reports error, exiting.\n" if $?;
}
