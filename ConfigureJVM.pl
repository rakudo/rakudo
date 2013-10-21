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
    GetOptions(\%options, 'help!', 'prefix=s', 'with-nqp=s', 'gen-nqp:s',
               'make-install!', 'makefile-timing!', 'no-clean!'
    ) or do {
        print_help();
        exit(1);
    };

    # Print help if it's requested
    if ($options{'help'}) {
        print_help();
        exit(0);
    }
    my $slash       = $^O eq 'MSWin32' ? '\\' : '/';
    my $prefix      = $options{'prefix'} || cwd() . "${slash}install";
    my $with_nqp    = $options{'with-nqp'} || "$prefix${slash}bin${slash}nqp-j";
    my $gen_nqp     = $options{'gen-nqp'};
    
    $prefix = realpath($prefix);

    # Save options in config.status
    unlink('config.status');
    if (open(my $CONFIG_STATUS, '>', 'config.status')) {
        print $CONFIG_STATUS
            "$^X Configure.pl $config{$config_status} \$*\n";
        close($CONFIG_STATUS);
    }
    
    # require --with-nqp or --gen-nqp or find executable nqp at default locaton
    if (!defined $gen_nqp && !defined $options{'with-nqp'} && ! -x $with_nqp && !-x $with_nqp.".bat") {
        die("$with_nqp is not executable; use --with-nqp or use --gen-nqp");
    }
    
    # determine the version of NQP we want
    my ($nqp_want) = split(' ', slurp('tools/build/NQP_REVISION'));

    if (defined $gen_nqp) {
    	$options{'with-jvm'} = 1;
    	$options{'prefix'} = $prefix;
        $with_nqp = gen_nqp($nqp_want, %options);
    }

    my @errors;

    unless (`$with_nqp --version` =~ /This is nqp .+ JVM/) {
        push @errors, "No NQP on JVM found; use --with-nqp to specify or --gen-nqp";
    }

    my %nqp_config = read_config($with_nqp) 
        or push @errors, "Unable to read configuration from $with_nqp.";
        
    unless (defined $nqp_config{'jvm::runtime.jars'}) {
    	push @errors, "jvm::runtime.jars value not available from $with_nqp --show-config.";
    }

    sorry(@errors) if @errors;

    print "Using $with_nqp.\n";

    $config{'prefix'} = $prefix;
    $config{'nqp'} = $with_nqp;
    $config{'nqp_prefix'} = $nqp_config{'jvm::runtime.prefix'};
    $config{'nqp_jars'} = $nqp_config{'jvm::runtime.jars'};
    $config{'nqp_classpath'} = $nqp_config{'jvm::runtime.classpath'};
    $config{'makefile-timing'} = $options{'makefile-timing'};
    $config{'stagestats'} = '--stagestats' if $options{'makefile-timing'};
    $config{'cpsep'} = $^O eq 'MSWin32' ? ';' : ':';
    $config{'slash'} = $slash;
    $config{'runner'} = $^O eq 'MSWin32' ? 'perl6.bat' : 'perl6';
    my $make = $config{'make'} = $^O eq 'MSWin32' ? 'nmake' : 'make';
    
    fill_template_file('tools/build/Makefile-JVM.in', 'Makefile', %config);

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
ConfigureJVM.pl - $lang Configure

General Options:
    --help             Show this text
    --prefix=dir       Install files in dir
    --gen-nqp[=branch] Download and build NQP
    --with-nqp=path/to/bin/nqp
                       NQP JVM to use to build
    --makefile-timing  Enable timing of individual makefile commands

ConfigureJVM.pl also reads options from 'config.default' in the current directory.
END

    return;
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
