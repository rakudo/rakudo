#!/usr/bin/env perl
# Copyright (C) 2009 The Perl Foundation

use 5.10.1;
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
# We don't use ExtUtils::Command in Configure.pl, but it is used in the Makefile
# Try `use`ing it here so users know if they need to install this module
# (not included with *every* Perl installation)
use ExtUtils::Command;
MAIN: {
    if (-r 'config.default') {
        unshift @ARGV, shellwords(slurp('config.default'));
    }

    my %config = (perl => $^X);
    my $config_status = "${lclang}_config_status";
    $config{$config_status} = join ' ', map { qq("$_") } @ARGV;

    my $exe = $NQP::Configure::exe;

    my %options;
    GetOptions(\%options, 'help!', 'prefix=s', 'libdir=s',
               'sysroot=s', 'sdkroot=s',
               'no-relocatable',
               'backends=s', 'no-clean!',
               'with-nqp=s', 'gen-nqp:s',
               'gen-moar:s', 'moar-option=s@',
               'git-protocol=s', 'ignore-errors',
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
    if ($options{'ignore-errors'}) {
        print "===WARNING!===\nErrors are being ignored.\nIn the case of any errors the script may behave unexpectedly.\n";
    }
    unless (defined $options{prefix}) {
        my $default = defined($options{sysroot}) ? '/usr' : File::Spec->catdir(getcwd, 'install');
        print "ATTENTION: no --prefix supplied, building and installing to $default\n";
        $options{prefix} = $default;
    }
    $options{prefix} = File::Spec->rel2abs($options{prefix});
    unless (defined $options{libdir}) {
        my $default = File::Spec->catdir($options{prefix}, 'share');
        $options{libdir} = $default;
    }
    my $prefix         = $options{'prefix'};
    my @known_backends = qw/moar jvm js/;

    my %backend_prefix = (jvm => 'j', moar => 'm', js  => 'js');

    my %known_backends = map { $_, 1; } @known_backends;
    my %letter_to_backend;
    my $default_backend;
    for (keys %known_backends) {
        $letter_to_backend{ $backend_prefix{$_} } = $_;
    }
    my @backends;
    my %backends;
    if (my $nqp_bin  = $options{'with-nqp'}) {
        die "Could not find $nqp_bin" unless -e $nqp_bin;
        my $passed_backends = $options{backends};
        $options{backends} = qx{$nqp_bin -e 'print(nqp::getcomp("nqp").backend.name)'}
            or die "Could not get backend information from $nqp_bin";
        if (defined $passed_backends && $passed_backends ne $options{backends}) {
            die "Passed value to --backends ($passed_backends) is overwritten by the one infered by --with-nqp ($options{backends})";
        }
    }
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
        unless (%backends or exists $options{'with-nqp'}) {
            die "No suitable nqp executables found! Please specify some --backends, or a --prefix that contains nqp-{js,j,m} executables\n\n"
              . "Example to build for all backends (which will take a while):\n"
              . "\tperl Configure.pl --backends=ALL --gen-moar\n\n"
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

    # Relocatability is not supported on AIX.
    $options{'no-relocatable'} = 1 if $^O eq 'aix';

    $config{prefix} = $prefix;
    $config{libdir} = $options{libdir};
    $config{sdkroot} = $options{sdkroot} || '';
    $config{sysroot} = $options{sysroot} || '';
    if ($options{'no-relocatable'}) {
        $config{static_nqp_home} = File::Spec->catdir($prefix, 'share', 'nqp');
        $config{static_perl6_home} = File::Spec->catdir($prefix, 'share', 'perl6');
        $config{static_nqp_home_define} = '-DSTATIC_NQP_HOME=' . $config{static_nqp_home};
        $config{static_perl6_home_define} = '-DSTATIC_PERL6_HOME=' . $config{static_perl6_home};
    }
    else {
        $config{static_nqp_home} = '';
        $config{static_perl6_home} = '';
        $config{static_nqp_home_define} = '';
        $config{static_perl6_home_define} = '';
    }
    $config{slash}  = $slash;
    $config{'makefile-timing'} = $options{'makefile-timing'};
    $config{'stagestats'} = '--stagestats' if $options{'makefile-timing'};
    $config{'cpsep'} = $win ? ';' : ':';
    my $shell = $^O eq 'MSWin32' ? 'cmd' : 'sh';
    $config{'shell'} = $^O eq 'solaris' ? '' : "SHELL   = $shell";
    $config{'runner_suffix'} = $win ? '.bat' : '';

    my $make = 'make';
    if ($^O eq 'solaris') {
        if (-z `which gmake`) {
            die "gmake is required to compile rakudo. Please install by 'pkg install gnu-make'";
        }
        $make = 'gmake';
    }

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

    for my $target (qw/common_bootstrap_sources js_core_sources js_core_d_sources moar_core_sources moar_core_d_sources jvm_core_sources jvm_core_d_sources/) {
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

    if ( is_OS_type_Unix() ) {
        $config{mkpath} = 'mkdir -p --';
        $config{chmod}  = 'chmod --';
        $config{cp}     = 'cp --';
        $config{rm_f}   = 'rm -f --';
        $config{rm_rf}  = 'rm -rf --';
        $config{test_f} = 'test -f --';
    }
    else {
        $config{mkpath} = '$(PERL5) -MExtUtils::Command -e mkpath';
        $config{chmod}  = '$(PERL5) -MExtUtils::Command -e chmod';
        $config{cp}     = '$(PERL5) -MExtUtils::Command -e cp';
        $config{rm_f}   = '$(PERL5) -MExtUtils::Command -e rm_f';
        $config{rm_rf}  = '$(PERL5) -MExtUtils::Command -e rm_rf';
        $config{test_f} = '$(PERL5) -MExtUtils::Command -e test_f';
    }

    fill_template_file('tools/build/Makefile-common-macros.in', $MAKEFILE, %config);

    my @prefixes = map $backend_prefix{$_}, @backends;

    my $launcher = $backend_prefix{$default_backend} . '-runner-default';
    print $MAKEFILE "all: ", join(' ', map("$_-all", @prefixes), $launcher), "\n";
    print $MAKEFILE "install: ", join(' ', map("$_-install", @prefixes), $launcher . '-install'), "\n";

    print $MAKEFILE "clean: ", join(' ', map "$_-clean", @prefixes), "\n";
    print $MAKEFILE "\t\$(RM_F) perl6", $config{'runner_suffix'},"\n\n";

    for my $t (qw/test spectest coretest localtest stresstest/) {
        print $MAKEFILE "$t: ", join(' ', map "$_-$t\$(HARNESS_TYPE)", @prefixes), "\n";
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
            $config{'nqp::libdir'}    = $nqp_config{'nqp::libdir'};
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

        # Strip rpath from ldflags so we can set it differently ourself.
        $config{ldflags} = join(' ', $nqp_config{'moar::ldflags'}, $nqp_config{'moar::ldmiscflags'}, $nqp_config{'moar::ldoptiflags'}, $nqp_config{'moar::ldlibs'});
        $config{ldflags} =~ s/\Q$nqp_config{'moar::ldrpath'}\E ?//;
        $config{ldflags} =~ s/\Q$nqp_config{'moar::ldrpath_relocatable'}\E ?//;
        $config{ldflags} .= ' ' . ($options{'no-relocatable'} ? $nqp_config{'moar::ldrpath'} : $nqp_config{'moar::ldrpath_relocatable'});

        if ($win) {
            if ($prefix . $slash . 'bin' ne $nqp_config{'moar::libdir'}) {
                $config{'m_install'} = "\t" . '$(CP) ' . $nqp_config{'moar::libdir'} . $slash . $nqp_config{'moar::moar'} . ' $(PREFIX)' . $slash . 'bin';
            }
            $config{'mingw_unicode'} = '';
            if ($nqp_config{'moar::os'} eq 'mingw32') {
                $config{'mingw_unicode'} = '-municode';
            }
            $config{'c_runner_libs'} = '-lShlwapi';
        } else {
            $config{'m_cleanups'} = "  \$(M_GDB_RUNNER) \\\n  \$(M_LLDB_RUNNER) \\\n  \$(M_VALGRIND_RUNNER)";
            $config{'m_all'}      = '$(M_GDB_RUNNER) $(M_LLDB_RUNNER) $(M_VALGRIND_RUNNER)';
            $config{'m_install'}  = "\t" . '$(M_RUN_PERL6) tools/build/create-moar-runner.p6 perl6 $(M_RUNNER) $(DESTDIR)$(PREFIX)/bin/perl6-gdb-m "gdb" "" "" ""' . "\n"
                                  . "\t" . '$(M_RUN_PERL6) tools/build/create-moar-runner.p6 perl6 $(M_RUNNER) $(DESTDIR)$(PREFIX)/bin/perl6-lldb-m "lldb" "" "" ""' . "\n"
                                  . "\t" . '$(M_RUN_PERL6) tools/build/create-moar-runner.p6 perl6 $(M_RUNNER) $(DESTDIR)$(PREFIX)/bin/perl6-valgrind-m "valgrind" "" "" ""';
        }

        unless (@errors) {
            print "Using $config{'m_nqp'} (version $nqp_config{'nqp::version'} / MoarVM $nqp_config{'moar::version'}).\n";

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
        $config{js_nqp} = $impls{js}{bin};
        $config{js_nqp} =~ s{/}{\\}g if $win;
        $config{'perl6_runtime'} = File::Spec->rel2abs('src/vm/js/perl6-runtime');
        $config{'perl6_lowlevel_libs'} = File::Spec->rel2abs('node_modules') . '/';
        $config{'perl6_js_runner'} = File::Spec->rel2abs('perl6-js');

        if ( $impls{js}{ok} ) {
            %nqp_config = %{ $impls{js}{config} };
        }
        elsif ( $impls{js}{config} ) {
            push @errors, "The nqp-js binary is too old";
        }
        else {
            push @errors, "Unable to read configuration from NQP on JS";
        }

        system("$config{js_nqp} tools/build/gen-js-makefile.nqp > gen/js/Makefile-JS.in");
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
    sorry($options{'ignore-errors'}, @errors) if @errors;

    my $l = uc $backend_prefix{$default_backend};
    print $MAKEFILE qq[\nt/*/*.t t/*.t t/*/*/*.t: all\n\t\$(${l}_HARNESS5_WITH_FUDGE) --verbosity=1 \$\@\n];

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

sub is_OS_type_Unix {
    # The following is a partial OS list taken from Perl::OSType module,
    # copyright by David Golden. The up-to-date version of that module can
    # be found at https://metacpan.org/pod/Perl::OSType

    return 1 if grep $^O eq $_, qw/
        aix       bsdos        beos   bitrig  dgux      dragonfly  dynixptx
        freebsd   linux        haiku  hpux    iphoneos  irix       darwin
        machten   midnightbsd  minix  mirbsd  next      openbsd    netbsd
        dec_osf   nto          svr4   svr5    sco       sco_sv     unicos
        unicosmk  solaris      sunos  cygwin  msys      os2        interix
        gnu       gnukfreebsd  nto    qnx     android
    /;

    return 0;
}

#  Print some help text.
sub print_help {
    print <<"END";
Configure.pl - $lang Configure

General Options:
    --help             Show this text
    --prefix=<path>    Install files in dir; also look for executables there
    --libdir=<path>    Install architecture-specific files in dir; Perl6 modules included
    --no-relocatable
                       Create a perl6 with a fixed NQP and Perl6 home dir instead of dynamically identifying it
                       (On AIX MoarVM is always built non-relocatable, since AIX misses a necessary mechanism.)
    --sdkroot=<path>   When given, use for searching build tools here, e.g.
                       nqp, java, node etc.
    --sysroot=<path>   When given, use for searching runtime components here
    --backends=jvm,moar,js
                       Which backend(s) to use (or ALL for all of them)
    --gen-nqp[=branch]
                       Download, build, and install a copy of NQP before writing the Makefile
    --gen-moar[=branch]
                       Download, build, and install a copy of MoarVM to use before writing the Makefile
    --with-nqp=<path>
                       Provide path to already installed nqp
    --make-install     Install Rakudo after configuration is done
    --moar-option='--option=value'
                       Options to pass to MoarVM's Configure.pl
                       For example: --moar-option='--compiler=clang'
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
    --ignore-errors    Ignore errors (such as the version of NQP)

Please note that the --gen-moar and --gen-nqp options are there for convenience
only and will actually immediately - at Configure time - compile and install
moar and nqp respectively. They will live under the path given to --prefix,
unless other targeting options are used. To configure how MoarVM should be
compiled, use the --moar-option flag and view MoarVM's Configure.pl for more
information on its configuration options.

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
