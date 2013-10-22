#! perl
# Copyright (C) 2009 The Perl Foundation

use 5.008;
use strict;
use warnings;
use Text::ParseWords;
use Getopt::Long;
use Cwd;
use lib 'tools/lib';
use NQP::Configure qw(sorry slurp cmp_rev gen_nqp read_config 
                      fill_template_text fill_template_file
                      system_or_die verify_install);

my $lang = 'Rakudo';
my $lclang = lc $lang;
my $uclang = uc $lang;
my $slash  = $^O eq 'MSWin32' ? '\\' : '/';


MAIN: {
    if (-r 'config.default') {
        unshift @ARGV, shellwords(slurp('config.default'));
    }

    my %config = (perl => $^X);
    my $config_status = "${lclang}_config_status";
    $config{$config_status} = join ' ', map { qq("$_") } @ARGV;

    my $exe = $NQP::Configure::exe;

    my %options;
    GetOptions(\%options, 'help!', 'prefix=s',
                'backends=s',
               'with-nqp-p=s', 'gen-nqp-p:s',
               'with-parrot=s', 'gen-parrot:s', 'parrot-option=s@',
               'parrot-make-option=s@',
               'make-install!', 'makefile-timing!',
    ) or do {
        print_help();
        exit(1);
    };

    # Print help if it's requested
    if ($options{'help'}) {
        print_help();
        exit(0);
    }

    my $prefix         = $options{'prefix'} || cwd().'/install';
    my %known_backends = (parrot => 1, jvm => 1);
    my %letter_to_backend;
    for (keys %known_backends) {
        $letter_to_backend{ substr($_, 0, 1) } = $_;
    }
    my %backends;
    if (defined $options{backends}) {
        for my $b (split /,\s*/, $options{backends}) {
            $b = lc $b;
            unless ($known_backends{$b}) {
                die "Uknown backend '$b'; Supported backends are: ",
                    join(", ", sort keys %backends),
                    "\n";
            }
        }
        unless (%backends) {
            die "--prefix given, but no valid backend?!\n";
        }
    }
    else {
        for my $l (keys %letter_to_backend) {
            # TODO: needs .exe/.bat magic on windows?
            if (-x "$prefix/bin/nqp-$l") {
                my $b = $letter_to_backend{$l};
                print "Found $prefix/bin/nqp-$l (backend $b)\n";
                $backends{$b} = 1;
            }
        }
        unless (%backends) {
            die "No suitable nqp executables found! Please specify some --backends, or a --prefix that contains nqp-{p,j} executables\n";
        }
    }

    # Save options in config.status
    unlink('config.status');
    if (open(my $CONFIG_STATUS, '>', 'config.status')) {
        print $CONFIG_STATUS
            "$^X Configure.pl $config{$config_status} \$*\n";
        close($CONFIG_STATUS);
    }

    $config{prefix} = $prefix;
    $config{slash}  = $slash;
    $config{'makefile-timing'} = $options{'makefile-timing'};
    $config{'stagestats'} = '--stagestats' if $options{'makefile-timing'};
    $config{'cpsep'} = $^O eq 'MSWin32' ? ';' : ':';
    my $make = $config{'make'} = $^O eq 'MSWin32' ? 'nmake' : 'make';

    open my $MAKEFILE, '>', 'Makefile'
        or die "Cannot open 'Makefile' for writing: $!";

    fill_template_file('tools/build/Makefile-common.in', $MAKEFILE, %config);

    my $gen_nqp     = $options{'gen-nqp'};

    # determine the version of NQP we want
    my ($nqp_want) = split(' ', slurp('tools/build/NQP_REVISION'));

    if ($backends{parrot}) {
        my $with_parrot = $options{'with-parrot'};
        my $gen_parrot  = $options{'gen-parrot'};
        my $nqp_p       = "$prefix/bin/nqp-p";
        $nqp_p = undef unless -x $nqp_p;

        # --with-parrot and --gen-parrot imply --gen-nqp
        if (!defined $gen_nqp && !defined $nqp_p && (defined $with_parrot || defined $gen_parrot)) {
            $gen_nqp = '';
        }

        if (defined $gen_nqp) {
            $nqp_p = gen_nqp($nqp_want, %options);
        }

        my @errors;

        my %nqp_config;
        if ($nqp_p) {
            %nqp_config = read_config($nqp_p)
                or push @errors, "Unable to read configuration from $nqp_p.";
        }
        else {
            %nqp_config = read_config("$prefix/bin/nqp-p$exe", "nqp-p$exe")
                or push @errors, "Unable to find an NQP executable.";
            $nqp_p = fill_template_text('@bindir@/nqp@exe@', %nqp_config)
        }

        %config = (%config, %nqp_config);
        my $nqp_have = $config{'nqp::version'} || '';
        if ($nqp_have && cmp_rev($nqp_have, $nqp_want) < 0) {
            push @errors, "NQP revision $nqp_want required (currently $nqp_have).";
        }

        if (!@errors) {
            push @errors, verify_install([ @NQP::Configure::required_parrot_files,
                                        @NQP::Configure::required_nqp_files ],
                                        %config);
            push @errors,
            "(Perhaps you need to 'make install', 'make install-dev',",
            "or install the 'devel' package for NQP or Parrot?)"
            if @errors;
        }

        if (@errors && !defined $gen_nqp) {
            push @errors, 
            "\nTo automatically clone (git) and build a copy of NQP $nqp_want,",
            "try re-running Configure.pl with the '--gen-nqp' or '--gen-parrot'",
            "options.  Or, use '--with-nqp=' or '--with-parrot=' to explicitly",
            "specify the NQP or Parrot executable to use to build $lang.";
        }

        sorry(@errors) if @errors;

        print "Using $nqp_p (version $config{'nqp::version'}).\n";

        $config{'makefile-timing'} = $options{'makefile-timing'};
        $config{'stagestats'} = '--stagestats' if $options{'makefile-timing'};
        $config{'shell'} = $^O eq 'MSWin32' ? 'cmd' : 'sh';
        if ($^O eq 'MSWin32' or $^O eq 'cygwin') {
            $config{'dll'} = '$(PARROT_BIN_DIR)/$(PARROT_LIB_SHARED)';
            $config{'dllcopy'} = '$(PARROT_LIB_SHARED)';
            $config{'make_dllcopy'} =
                '$(PARROT_DLL_COPY): $(PARROT_DLL)'."\n\t".'$(CP) $(PARROT_DLL) .';
        }

        my $make = fill_template_text('@make@', %config);
        fill_template_file('tools/build/Makefile-Parrot.in', $MAKEFILE, %config);
    }
    if ($backends{jvm}) {
        my $nqp_j = "$prefix/bin/nqp-j";
        $nqp_j = undef unless -x $nqp_j;
        if (!defined $gen_nqp && !defined $nqp_j) {
            die "$prefix/bin/nqp-j does not exists or is not executable (maybe use --gen-nqp?\n";
        }

        if (defined $gen_nqp) {
            $options{'with-jvm'} = 1;
            $options{'prefix'} = $prefix;
            $nqp_j = gen_nqp($nqp_want, %options);
        }
        my @errors;

        unless (`$nqp_j --version` =~ /This is nqp .+ JVM/) {
            push @errors, "No NQP on JVM found; use --with-nqp to specify or --gen-nqp";
        }
        $config{j_nqp} = $nqp_j;

        my %nqp_config = read_config($nqp_j)
            or push @errors, "Unable to read configuration from $nqp_j.";
        unless (defined $nqp_config{'jvm::runtime.jars'}) {
            push @errors, "jvm::runtime.jars value not available from $nqp_j --show-config.";
        }

        sorry(@errors) if @errors;

        print "Using $nqp_j.\n";

        $config{'nqp_prefix'} = $nqp_config{'jvm::runtime.prefix'};
        $config{'nqp_jars'} = $nqp_config{'jvm::runtime.jars'};
        $config{'nqp_classpath'} = $nqp_config{'jvm::runtime.classpath'};
        $config{'j_runner'} = $^O eq 'MSWin32' ? 'perl6-j.bat' : 'perl6-j';


        fill_template_file('tools/build/Makefile-JVM.in', $MAKEFILE, %config);
    }

    close $MAKEFILE or die "Cannot write 'Makefile': $!";

    {
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
Configure.pl - $lang Configure

General Options:
    --help             Show this text
    --prefix=dir       Install files in dir
    --with-nqp=path/to/bin/nqp
                       NQP executable to use to build $lang
    --gen-nqp[=branch]
                       Download and build a copy of NQP
        --with-parrot=path/to/bin/parrot
                       Parrot executable to use to build NQP
        --gen-parrot[=branch]
                       Download and build a copy of Parrot
        --parrot-option='--option'
                       Options to pass to Parrot's Configure.pl
        --parrot-make-option='--option'
                       Options to pass to Parrot's make, for example:
                       --parrot-make-option='--jobs=4'
    --makefile-timing  Enable timing of individual makefile commands

Configure.pl also reads options from 'config.default' in the current directory.
END

    return;
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
