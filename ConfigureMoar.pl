#! perl
# Copyright (C) 2009-2013 The Perl Foundation

use 5.008;
use strict;
use warnings;
use Text::ParseWords;
use Getopt::Long;
use Cwd qw(cwd realpath);
use lib "tools/lib";
use NQP::Configure qw(sorry slurp fill_template_text fill_template_file
                      system_or_die read_config gen_nqp);
use File::Basename;

my $lang = 'Rakudo';
my $lclang = lc $lang;
my $uclang = uc $lang;

MAIN: {
    if (-r "config.default") {
        unshift @ARGV, shellwords(slurp('config.default'));
    }

    my %config;
    my $config_status = "${lclang}_config_status";
    $config{$config_status} = join(' ', map { "\"$_\""} @ARGV);

    my %options;
    GetOptions(\%options, 'help!', 'prefix=s', 'make-install!',
                          'makefile-timing!', 'no-clean!'
    ) or do {
        print_help();
        exit(1);
    };

    # Print help if it's requested
    if ($options{'help'}) {
        print_help();
        exit(0);
    }
    
    my @errors;
    my $prefix = $options{'prefix'};
    if (!$prefix) {
        push @errors, "--prefix must be specified";
    }
    elsif (!-d $prefix) {
        push @errors, "prefix directory '$prefix' does not exist"
    }
    elsif (!-x "$prefix/bin/nqp" && !-x "$prefix/bin/nqp.bat") {
        push @errors, "prefix does not contain a bin/nqp";
    }
    sorry(@errors) if @errors;
    $prefix = realpath($prefix);
    $prefix =~ s{/}{\\}g if $^O eq 'MSWin32';

    # Save options in config.status
    unlink('config.status');
    if (open(my $CONFIG_STATUS, '>', 'config.status')) {
        print $CONFIG_STATUS
            "$^X Configure.pl $config{$config_status} \$*\n";
        close($CONFIG_STATUS);
    }

    my $nqp_to_use = "$prefix/bin/nqp";
    unless (`$nqp_to_use --version` =~ /This is nqp .+ MoarVM/) {
        push @errors, "The specified prefix does not contain NQP for MoarVM";
    }

    my %nqp_config = read_config($nqp_to_use) 
        or push @errors, "Unable to read configuration from $nqp_to_use.";
        
    sorry(@errors) if @errors;
    
    %config = (%nqp_config, %config);
    $config{'perl6_ops_dll'} = sprintf($config{'moar::dll'}, 'perl6_ops_moar');

    print "Using $nqp_to_use.\n";

    $config{'prefix'} = $prefix;
    $config{'makefile-timing'} = $options{'makefile-timing'};
    $config{'stagestats'} = '--stagestats' if $options{'makefile-timing'};
    $config{'slash'} = $^O eq 'MSWin32' ? '\\' : '/';
    $config{'runner_suffix'} = $^O eq 'MSWin32' ? '.bat' : '';
    $config{'exe'} = $^O eq 'MSWin32' ? '.exe' : '';
    my $make = $config{'make'} = $^O eq 'MSWin32' ? 'nmake' : 'make';
    
    fill_template_file('tools/build/Makefile-Moar.in', 'Makefile', %config);

    unless ($options{'no-clean'}) {
        no warnings;
        print "Cleaning up ...\n";
        if (open my $CLEAN, '-|', "$make clean") {
            my @slurp = <$CLEAN>;
            close($CLEAN);
        }
    }

    if ($options{'make-install'}) {
        system_or_die($make);
        system_or_die($make, 'install');
        print "\n$lang has been built and installed.\n";
    }
    else {
        print "\nYou can now use '$make' to build $lang.\n";
        print "After that, '$make test' will run some tests and\n";
        print "'$make install' will install $lang.\n";
    }

    exit 0;
}


#  Print some help text.
sub print_help {
    print <<"END";
ConfigureMoar.pl - $lang Configure

General Options:
    --help             Show this text
    --prefix=dir       The prefix where MoarVM and NQP can be found
    --makefile-timing  Enable timing of individual makefile commands

ConfigureMoar.pl also reads options from 'config.default' in the current directory.
END

    return;
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
