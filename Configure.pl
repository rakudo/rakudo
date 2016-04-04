#! perl
# Copyright (C) 2009 The Perl Foundation

use 5.010;
use strict;
use warnings;
use Text::ParseWords;
use Getopt::Long;
use File::Spec;
use Cwd;
use lib 'tools/lib';
use NQP::Configure qw(sorry slurp cmp_rev gen_nqp read_config
                      fill_template_text fill_template_file
                      system_or_die verify_install);

my $lang = 'Rakudo';
my $lclang = lc $lang;
my $uclang = uc $lang;
my $win    = $^O eq 'MSWin32';
my $slash  = $win ? '\\' : '/';


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
               'sysroot=s', 'sdkroot=s',
               'backends=s', 'no-clean!',
               'gen-nqp:s',
               'gen-moar:s', 'moar-option=s@',
               'git-protocol=s',
               'make-install!', 'makefile-timing!',
               'git-depth=s', 'git-reference=s',
    ) or do {
        print_help();
        exit(1);
    };

    # Print help if it's requested
    if ($options{'help'}) {
        print_help();
        exit(0);
    }

    unless (defined $options{prefix}) {
	my $default = defined($options{sysroot}) ? '/usr' : File::Spec->catdir(getcwd, 'install');
        print "ATTENTION: no --prefix supplied, building and installing to $default\n";
        $options{prefix} = $default;
    }
    $options{prefix} = File::Spec->rel2abs($options{prefix});
    my $prefix         = $options{'prefix'};
    my @known_backends = qw/moar jvm js/;
    my %known_backends = map { $_, 1; } @known_backends;
    my %letter_to_backend;
    my $default_backend;
    for (keys %known_backends) {
        $letter_to_backend{ substr($_, 0, 1) } = $_;
    }
    my @backends;
    my %backends;
    if (defined $options{backends}) {
        $options{backends} = join ",", @known_backends
            if uc($options{backends}) eq 'ALL';
        for my $b (split /,\s*/, $options{backends}) {
            $b = lc $b;
            if ($b eq 'parrot') {
                die "The Parrot backend has been suspended.\n"
                    . "Please use Rakudo 2015.02 (which still supports parrot), or the MoarVM backend instead\n";
            }
            unless ($known_backends{$b}) {
                die "Unknown backend '$b'; Supported backends are: " .
                    join(", ", sort keys %known_backends) .
                    "\n";
            }
            $backends{$b} = 1;
            push @backends, $b;
            $default_backend ||= $b;
        }
        unless (%backends) {
            die "--prefix given, but no valid backend?!\n";
        }
    }
    else {
        for my $l (sort keys %letter_to_backend) {
            if (-x "$prefix/bin/nqp-$l" || -x "$prefix/bin/nqp-$l.exe" || -x "$prefix/bin/nqp-$l.bat") {
                my $b = $letter_to_backend{$l};
                print "Found $prefix/bin/nqp-$l (backend $b)\n";
                $backends{$b} = 1;
                push @backends, $b;
                $default_backend ||= $b;
            }
        }
        if (exists $options{'gen-moar'}) {
            push @backends, 'moar' unless $backends{moar};
            $backends{moar} = 1;
            $default_backend ||= 'moar';
        }
        unless (%backends) {
            die "No suitable nqp executables found! Please specify some --backends, or a --prefix that contains nqp-{p,j,m} executables\n\n"
              . "Example to build for all backends (which will take a while):\n"
              . "\tperl Configure.pl --backends=moar,jvm --gen-moar\n\n"
              . "Example to build for MoarVM only:\n"
              . "\tperl Configure.pl --gen-moar\n\n"
              . "Example to build for JVM only:\n"
              . "\tperl Configure.pl --backends=jvm --gen-nqp\n\n";
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
    $config{sdkroot} = $options{sdkroot} || '';
    $config{sysroot} = $options{sysroot} || '';
    $config{slash}  = $slash;
    $config{'makefile-timing'} = $options{'makefile-timing'};
    $config{'stagestats'} = '--stagestats' if $options{'makefile-timing'};
    $config{'cpsep'} = $win ? ';' : ':';
    $config{'shell'} = $win ? 'cmd' : 'sh';
    $config{'runner_suffix'} = $win ? '.bat' : '';

    my $make = 'make';
    if ($win) {
        my $has_nmake = 0 == system('nmake /? >NUL 2>&1');
        my $has_cl    = `cl 2>&1` =~ /Microsoft Corporation/;
        my $has_gmake = 0 == system('gmake --version >NUL 2>&1');
        my $has_gcc   = 0 == system('gcc --version >NUL 2>&1');
        if (-x "$prefix/bin/nqp-m.bat"
        && ($_ = `$prefix/bin/nqp-m.bat -e "print(nqp::backendconfig()<make>)"`)) {
            $make = $_;
        }
        elsif ($has_nmake && $has_cl) {
            $make = 'nmake';
        }
        elsif ($has_gmake && $has_gcc) {
            $make = 'gmake';
        }
    }

    for my $target (qw/common_bootstrap_sources moar_core_sources/) {
        open my $FILELIST, '<', "tools/build/$target"
            or die "Cannot read 'tools/build/$target': $!";
        my @lines;
        while (<$FILELIST>) {
            chomp;
            push @lines, "  $_\\\n";
        }
        close $FILELIST;
        $config{$target} = join '', @lines;
    }

    open my $MAKEFILE, '>', 'Makefile'
        or die "Cannot open 'Makefile' for writing: $!";

    print $MAKEFILE "\n# Makefile code generated by Configure.pl:\n";

    fill_template_file('tools/build/Makefile-common-macros.in', $MAKEFILE, %config);

    my @prefixes = map substr($_, 0, 1), @backends;

    my $launcher = substr($default_backend, 0, 1) . '-runner-default';
    print $MAKEFILE "all: ", join(' ', map("$_-all", @prefixes), $launcher), "\n";
    print $MAKEFILE "install: ", join(' ', map("$_-install", @prefixes), $launcher . '-install'), "\n";

    print $MAKEFILE "clean: ", join(' ', map "$_-clean", @prefixes), "\n";
    print $MAKEFILE "\t\$(RM_F) perl6", $config{'runner_suffix'},"\n\n";

    for my $t (qw/test spectest coretest localtest stresstest/) {
        print $MAKEFILE "$t: ", join(' ', map "$_-$t", @prefixes), "\n";
    }

    fill_template_file('tools/build/Makefile-common-rules.in', $MAKEFILE, %config);

    # determine the version of NQP we want
    my ($nqp_want) = split(' ', slurp('tools/build/NQP_REVISION'));

    $options{'gen-nqp'} ||= '' if exists $options{'gen-moar'};

    my %binaries;
    my %impls = gen_nqp($nqp_want, prefix => $prefix, backends => join(',', sort keys %backends), %options);

    my @errors;
    my %errors;
    if ($backends{jvm}) {
        $config{j_nqp} = $impls{jvm}{bin};
        $config{j_nqp} =~ s{/}{\\}g if $win;
        my %nqp_config;
        if ( $impls{jvm}{ok} ) {
            %nqp_config = %{ $impls{jvm}{config} };
        }
        elsif ( $impls{jvm}{config} ) {
            push @errors, "nqp-j is too old";
        }
        else {
            push @errors, "Unable to read configuration from NQP on the JVM";
        }
        my $bin = $impls{jvm}{bin};

        if (!@errors && !defined $nqp_config{'jvm::runtime.jars'}) {
            push @errors, "jvm::runtime.jars value not available from $bin --show-config.";
        }

        $errors{jvm}{'no gen-nqp'} = @errors && !defined $options{'gen-nqp'};

        unless (@errors) {
            my $java_version = `java -version 2>&1`;
            $java_version    = $java_version =~ /(?<v>[\d\._]+).+\n(?<n>\S+)/
                             ? "$+{'n'} $+{'v'}"
                             : 'no java version info available';

            print "Using $bin (version $nqp_config{'nqp::version'} / $java_version).\n";

            $config{'nqp_prefix'}    = $nqp_config{'jvm::prefix'};
            $config{'nqp_jars'}      = $nqp_config{'jvm::runtime.jars'};
            $config{'bld_nqp_jars'}  = join( $config{'cpsep'}, map { $config{'sysroot'} . $_ } split( $config{'cpsep'}, $nqp_config{'jvm::runtime.jars'} ) );
            $config{'nqp_classpath'} = $nqp_config{'jvm::runtime.classpath'};
            $config{'j_runner'}      = $win ? 'perl6-j.bat' : 'perl6-j';


            fill_template_file('tools/build/Makefile-JVM.in', $MAKEFILE, %config);
        }
    }
    if ($backends{moar}) {
        $config{m_nqp} = $impls{moar}{bin};
        $config{m_nqp} =~ s{/}{\\}g if $win;
        my %nqp_config;
        if ( $impls{moar}{ok} ) {
            %nqp_config = %{ $impls{moar}{config} };
        }
        elsif ( $impls{moar}{config} ) {
            push @errors, "The nqp-m binary is too old";
        }
        else {
            push @errors, "Unable to read configuration from NQP on MoarVM";
        }

        $errors{moar}{'no gen-nqp'} = @errors && !defined $options{'gen-nqp'};

        unless ($win) {
            $config{'m_cleanups'} = "  \$(M_GDB_RUNNER) \\\n  \$(M_VALGRIND_RUNNER)";
            $config{'m_all'}      = '$(M_GDB_RUNNER) $(M_VALGRIND_RUNNER)';
            $config{'m_install'}  = '	$(PERL) tools/build/create-moar-runner.pl "$(MOAR)" perl6.moarvm $(DESTDIR)$(PREFIX)/bin/perl6-gdb-m "$(PERL6_LANG_DIR)/runtime" "gdb" "$(M_LIBPATH)" "$(PERL6_LANG_DIR)/lib" "$(PERL6_LANG_DIR)/runtime"' . "\n"
                                  . '	$(PERL) tools/build/create-moar-runner.pl "$(MOAR)" perl6.moarvm $(DESTDIR)$(PREFIX)/bin/perl6-valgrind-m "$(PERL6_LANG_DIR)/runtime" "valgrind" "$(M_LIBPATH)" "$(PERL6_LANG_DIR)/lib" "$(PERL6_LANG_DIR)/runtime"';
        }

        unless (@errors) {
            print "Using $config{m_nqp} (version $nqp_config{'nqp::version'} / MoarVM $nqp_config{'moar::version'}).\n";

            $config{'perl6_ops_dll'} = sprintf($nqp_config{'moar::dll'}, 'perl6_ops_moar');

            # Add moar library to link command
            # TODO: Get this from Moar somehow
            $config{'moarimplib'} = $win || $^O eq 'darwin'
                                  ? $nqp_config{'moar::libdir'} . '/' . $nqp_config{'moar::sharedlib'}
                                  : '';

            fill_template_file('tools/build/Makefile-Moar.in', $MAKEFILE, %config, %nqp_config);
        }
    }
    
    if ($backends{js}) {
        my %nqp_config;
        system("nqp-m tools/build/gen-js-makefile.nqp > gen/js/Makefile-JS.in");
        fill_template_file('gen/js/Makefile-JS.in', $MAKEFILE, %config, %nqp_config);
    }

    if ($errors{jvm}{'no gen-nqp'} || $errors{moar}{'no gen-nqp'}) {
        my @options_to_pass;
        push @options_to_pass, "--gen-moar"   if $backends{moar};
        push @options_to_pass, "--gen-nqp"    unless @options_to_pass;
        my $options_to_pass  = join ' ', @options_to_pass;
        my $want_executables =$backends{moar}
                             ? ' and MoarVM'
                             : '';
        my $s1               = @options_to_pass > 1 ? 's' : '';
        my $s2               = $want_executables    ? 's' : '';
        push @errors,
        "\nTo automatically clone (git) and build a copy of NQP $nqp_want,",
        "try re-running Configure.pl with the '$options_to_pass' option$s1.",
        "Or, use '--prefix=' to explicitly specify the path where the NQP$want_executables",
        "executable$s2 can be found that are use to build $lang.";
    }
    sorry(@errors) if @errors;

    my $l = uc substr($default_backend, 0, 1);
    print $MAKEFILE qq[\nt/*/*.t t/*.t t/*/*/*.t: all\n\t\$(${l}_HARNESS_WITH_FUDGE) --verbosity=1 \$\@\n];

    close $MAKEFILE or die "Cannot write 'Makefile': $!";

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
Configure.pl - $lang Configure

General Options:
    --help             Show this text
    --prefix=dir       Install files in dir; also look for executables there
    --sdkroot=dir      When given, use for searching build tools here, e.g.
                       nqp, java etc.
    --sysroot=dir      When given, use for searching runtime components here
    --backends=jvm,moar
                       Which backend(s) to use (or ALL for all of them)
    --gen-nqp[=branch]
                       Download and build a copy of NQP
    --gen-moar[=branch]
                       Download and build a copy of MoarVM to use
    --make-install     Install Rakudo after configuration is done
    --moar-option='--option=value'
                       Options to pass to MoarVM's Configure.pl
    --git-protocol={ssh,https,git}
                       Protocol used for cloning git repos
    --git-depth=<number>
                       Use the --git-depth option for git clone with parameter number
    --git-reference=<path>
                       Use --git-reference option to identify local path where git repositories are stored
                       For example: --git-reference=/home/user/repo/for_perl6
                       Folders 'nqp' and 'MoarVM' with corresponding git repos should be in for_perl6 folder
    --makefile-timing  Enable timing of individual makefile commands
    --no-clean         Skip cleanup before installation

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
