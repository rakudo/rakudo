use v5.10.1;

package NQP::Config::_Scoping;

sub new {
    my $class  = shift;
    my $cb     = shift;
    my %params = @_;
    my $self   = bless {}, $class;
    $self->{cb}     = $cb;
    $self->{params} = \%params;
    return $self;
}

sub DESTROY {
    my $self = shift;
    $self->{cb}->( %{ $self->{params} } );
}

package NQP::Config;
use strict;
use warnings;
use File::Spec;
use File::Basename;
use FindBin;
use Data::Dumper;
use NQP::Configure::Macros;
use Cwd;
use Carp;

$SIG{__DIE__} = sub { confess @_ };

use base qw<Exporter>;
our @EXPORT_OK = qw<
  nfp slash slurp system_or_die cmp_rev read_config
>;

sub init {
    my $self = shift;

    $self->{config} = {
        perl      => $^X,
        slash     => slash(),
        shell     => $^O eq 'solaris' ? '' : "SHELL = " . $self->shell_cmd,
        make      => $self->make_cmd,
        base_dir  => $FindBin::Bin,
        build_dir => File::Spec->catdir( $FindBin::Bin, 'tools', 'build' ),
        templates_dir =>
          File::Spec->catdir( $FindBin::Bin, 'tools', 'templates' ),

        # Number of spaces to indent filelists in a makefile
        filelist_indent => 4,
    };

    @{ $self->{config} }{qw<exe bat cpsep>} =
      $self->is_win ? ( '.exe', '.bat', ';', ) : ( "", "", ":", );

    $self->{config}{runner_suffix} = $self->{config}{bat};

    $self->{backend_prefix} = {
        moar => 'm',
        jvm  => 'j',
        js   => 'js',
    };
    $self->{backends_order} = [qw<moar jvm js>];
    $self->{options}        = {};
    $self->{contexts}       = [];
    $self->{repo_maps}      = {
        rakudo => [qw<rakudo rakudo>],
        nqp    => [qw<perl6 nqp>],
        moar   => [qw<MoarVM MoarMV>],
        roast  => [qw<perl6 roast>],
    };

    $self->{impls} = {};

    return $self;
}

sub sorry {
    my $self    = shift;
    my (@msg)   = @_;
    my $message = join( "\n", '', '===SORRY!===', @msg, "\n" );
    die $message unless $self->opt('ignore-errors');
    print $message;
}

sub shell_cmd {
    $_[0]->is_win ? 'cmd' : 'sh';
}

sub batch_file {
    my $self   = shift;
    my $source = shift;
    my ( $vol, $dir, $file ) = File::Spec->splitpath($source);
    my $basename = basename( $file, "." . $self->cfg('bat') );
    return File::Spec->catpath( $vol, $dir, "$basename" . $self->cfg('bat') );
}

sub make_cmd {
    my $self = shift;

    my $make = 'make';
    if ( $self->is_solaris ) {
        if ( -z `which gmake` ) {
            die
"gmake is required to compile rakudo. Please install by 'pkg install gnu-make'";
        }
        $make = 'gmake';
    }

    if ( $self->is_win ) {
        my $prefix    = $self->cfg('prefix');
        my $has_nmake = 0 == system('nmake /? >NUL 2>&1');
        my $has_cl    = `cl 2>&1` =~ /Microsoft Corporation/;
        my $has_gmake = 0 == system('gmake --version >NUL 2>&1');
        my $has_gcc   = 0 == system('gcc --version >NUL 2>&1');
        if (
            -x "$prefix/bin/nqp-m.bat"
            && ( $_ =
                `$prefix/bin/nqp-m.bat -e "print(nqp::backendconfig()<make>)"` )
          )
        {
            $make = $_;
        }
        elsif ( $has_nmake && $has_cl ) {
            $make = 'nmake';
        }
        elsif ( $has_gmake && $has_gcc ) {
            $make = 'gmake';
        }
    }
    return $make;
}

sub options {
    $_[0]->{options};
}

sub option {
    die "Option name required for option method" unless @_ > 1;
    $_[0]->{options}{ $_[1] };
}

*opt = *option;

sub has_option {
    die "Option name required for has_option method" unless @_ > 1;
    exists $_[0]->{options}{ $_[1] };
}

sub validate_backend {
    my ( $self, $backend, $method ) = @_;
    die "Unknown backend '$backend'"
      . ( $method ? " in call to method '$method'" : "" )
      unless $self->known_backend($backend);
    return $backend;
}

sub known_backends {
    my $self = shift;
    return @{ $self->{backends_order} };
}

sub known_backend {
    return exists $_[0]->{backend_prefix}{ $_[1] };
}

sub abbr_to_backend {
    my ( $self, $abbr ) = @_;
    unless ( $self->{abbr_to_backend} ) {
        for my $backend ( $self->known_backends ) {
            $self->{abbr_to_backend}{ $self->{backend_prefix}{$backend} } =
              $backend;
        }
    }
    die "Unknown backend abbreviation '$abbr' in call to abbr_to_backend"
      unless $self->{abbr_to_backend}{$abbr};
    return $self->{abbr_to_backend}{$abbr};
}

sub backend_abbr {
    my ( $self, $backend ) = @_;
    return $self->{backend_prefix}{ $self->validate_backend($backend) };
}

sub backend_config {
    my ( $self, $backend ) = @_;
    return $self->{impls}{$backend}{config};
}

sub known_abbrs {
    return values %{ $_[0]->{backend_prefix} };
}

sub use_backend {
    my ( $self, $backend ) = @_;
    return if $self->active_backend($backend);
    push @{ $self->{active_backends_order} }, $backend;
    $self->{active_backends}{ $self->validate_backend($backend) } = 1;
    $self->{config}{default_backend} ||= $backend;
    $self->{config}{default_prefix}  ||= $self->backend_abbr($backend);
}

sub active_backends {
    my $self = shift;
    return @{ $self->{active_backends_order} };
}

sub active_backend {
    my ( $self, $backend ) = @_;
    return !!$self->{active_backends}{ $self->validate_backend($backend) };
}

sub active_abbrs {
    my $self = shift;
    return map { $self->backend_abbr($_) } @{ $self->{active_backends_order} };
}

sub configure_relocatability {
    my $self = shift;

    my $config = $self->{config};

    # Relocatability is not supported on AIX.
    $config->{no_relocatable} ||= $^O eq 'aix';
    my $prefix = $config->{prefix};

    if ( $config->{no_relocatable} ) {
        $config->{static_nqp_home} =
          File::Spec->catdir( $prefix, 'share', 'nqp' );
        $config->{static_perl6_home} =
          File::Spec->catdir( $prefix, 'share', 'perl6' );
        $config->{static_nqp_home_define} =
          '-DSTATIC_NQP_HOME=' . $config->{static_nqp_home};
        $config->{static_perl6_home_define} =
          '-DSTATIC_PERL6_HOME=' . $config->{static_perl6_home};
    }
    else {
        $config->{static_nqp_home}          = '';
        $config->{static_perl6_home}        = '';
        $config->{static_nqp_home_define}   = '';
        $config->{static_perl6_home_define} = '';
    }
}

# This would prepare git URL config variables for default protocol.
sub configure_repo_urls {
    my $self = shift;

    # Pre-cach repo urls to make them available for makefiles.
    for my $r ( keys %{ $self->{repo_maps} } ) {
        $self->repo_url( $r, action => 'pull' );
        $self->repo_url( $r, action => 'push' );
    }
}

sub configure_commands {
    my $self   = shift;
    my $config = $self->{config};

    if ( $self->isa_unix ) {
        $config->{mkpath} = 'mkdir -p --';
        $config->{chmod}  = 'chmod --';
        $config->{cp}     = 'cp --';
        $config->{rm_f}   = 'rm -f --';
        $config->{rm_rf}  = 'rm -rf --';
        $config->{test_f} = 'test -f --';
    }
    else {
        $config->{mkpath} = '$(PERL5) -MExtUtils::Command -e mkpath';
        $config->{chmod}  = '$(PERL5) -MExtUtils::Command -e chmod';
        $config->{cp}     = '$(PERL5) -MExtUtils::Command -e cp';
        $config->{rm_f}   = '$(PERL5) -MExtUtils::Command -e rm_f';
        $config->{rm_rf}  = '$(PERL5) -MExtUtils::Command -e rm_rf';
        $config->{test_f} = '$(PERL5) -MExtUtils::Command -e test_f';
    }
}

sub configure_misc {
    my $self   = shift;
    my $config = $self->{config};

    # determine the version of NQP we want
    ( $config->{nqp_want} ) =
      split( ' ',
        slurp( $self->template_file_path( 'NQP_REVISION', required => 1, ) ) );

    $config->{perl6_specs} = [
        map { [ split ' ' ] }
          grep { s/\s*#.*$//; length }
          split(
            /\n/s,
            slurp( $self->template_file_path( 'PERL6_SPECS', required => 1, ) )
          )
    ];
}

sub configure_prefix {
    my $self   = shift;
    my $config = $self->{config};
    unless ( defined $config->{prefix} ) {

        # XXX This is only Unix-friendly way.
        my $default =
          defined( $self->opt('sysroot') )
          ? '/usr'
          : File::Spec->catdir( $config->{base_dir}, 'install' );
        print
"ATTENTION: no --prefix supplied, building and installing to $default\n";
        $config->{prefix} = $default;
    }
    $config->{prefix} = File::Spec->rel2abs( $config->{prefix} );
}

sub configure_backends {
    my $self   = shift;
    my $config = $self->{config};

    my $prefix = $config->{prefix};

    ### Consider what backends are to be built. ###
    my $passed_backends = $self->opt('backends');
    if ( my $nqp_bin = $self->opt('with-nqp') ) {
        die "Could not find $nqp_bin" unless -e $nqp_bin;
        my $nqp_backend =
          qx{$nqp_bin -e 'print(nqp::getcomp("nqp").backend.name)'}
          or die "Could not get backend information from $nqp_bin";
        if ( defined $passed_backends && $nqp_backend ne $passed_backends ) {
            die
"Passed value to --backends ($passed_backends) is overwritten by the one infered by --with-nqp ($nqp_backend)";
        }
    }
    if ( defined $passed_backends ) {
        my @use_backends =
          uc($passed_backends) eq 'ALL'
          ? $self->known_backends
          : split /,\s*/, $passed_backends;
        for my $b ( map { lc } @use_backends ) {
            if ( $b eq 'parrot' ) {
                die "The Parrot backend has been suspended.\n"
                  . "Please use Rakudo 2015.02 (which still supports parrot), or the MoarVM backend instead\n";
            }
            unless ( $self->known_backend($b) ) {
                die "Unknown backend '$b'; Supported backends are: "
                  . join( ", ", sort $self->known_backends ) . "\n";
            }
            $self->use_backend($b);
        }
        unless ( $self->active_backends ) {
            die "--prefix given, but no valid backend?!\n";
        }
    }
    else {
        for my $a ( $self->known_abbrs ) {
            if ( my $nqp_bin = $self->is_executable("$prefix/bin/nqp-$a") ) {
                my $b = $self->abbr_to_backend($a);
                print "Found $nqp_bin (backend $b)\n";
                $self->use_backend($b);
            }
        }

        $self->use_backend('moar') if $self->has_option('gen-moar');

        unless ( $self->active_backends or $self->has_option('with-nqp') ) {
            die
"No suitable nqp executables found! Please specify some --backends, or a --prefix that contains nqp-{js,j,m} executables\n\n"
              . "Example to build for all backends (which will take a while):\n"
              . "\tperl Configure.pl --backends=ALL --gen-moar\n\n"
              . "Example to build for MoarVM only:\n"
              . "\tperl Configure.pl --gen-moar\n\n"
              . "Example to build for JVM only:\n"
              . "\tperl Configure.pl --backends=jvm --gen-nqp\n\n";
        }
    }
}

sub backend_error {
    my $self    = shift;
    my $backend = shift;
    $self->{backend_errors}{$backend} //= [];
    if (@_) {
        push @{ $self->{backend_errors}{$backend} }, @_;
    }
    return !!@{ $self->{backend_errors}{$backend} };
}

sub backend_errors {
    my $errs = $_[0]->{backend_errors}{ $_[1] };
    return wantarray ? @$errs : $errs;
}

# Returns true if jvm or moar backend check produced errors and no gen-nqp
# specified
sub no_gen_nqp {
    my $self = shift;
    return ( $self->backend_error('jvm') || $self->backend_error('moar') )
      && !$self->{options}{'gen-nqp'};
}

sub configure_active_backends {
    my $self = shift;

    my @errors;

    for my $b ( $self->active_backends ) {
        my $conf_meth = "configure_${b}_backend";
        $self->{backend_errors}{$b} = [];
        if ( $self->can($conf_meth) ) {
            $self->$conf_meth;
        }

        push @errors, $self->backend_errors($b);
    }

    if ( $self->no_gen_nqp ) {
        my $config   = $self->{config};
        my $nqp_want = $config->{nqp_want};
        my $is_moar  = $self->active_backend('moar');
        my @options_to_pass;

        push @options_to_pass, "--gen-moar" if $is_moar;
        push @options_to_pass, "--gen-nqp" unless @options_to_pass;
        my $options_to_pass = join ' ', @options_to_pass;
        my $want_executables =
          $is_moar
          ? ' and MoarVM'
          : '';
        my $s1 = @options_to_pass > 1 ? 's' : '';
        my $s2 = $want_executables    ? 's' : '';

        push @errors,
          "\nTo automatically clone (git) and build a copy of NQP $nqp_want,",
          "try re-running Configure.pl with the '$options_to_pass' option$s1.",
          "Or, use '--prefix=' to explicitly specify the path where the NQP"
          . $want_executables,
          "executable$s2 can be found that are use to build $config->{lang}.";
    }

    $self->sorry(@errors) if @errors;
}

sub configure_jvm_backend {
    my $self   = shift;
    my $ijvm   = $self->{impls}{jvm};
    my $config = $self->{config};

    $config->{j_nqp} = $ijvm->{bin};

    my $nqp_config;
    if ( $ijvm->{ok} ) {
        $nqp_config = $ijvm->{config};
    }
    elsif ( $ijvm->{config} ) {
        $self->backend_error( jvm => "nqp-j is too old" );
    }
    else {
        $self->backend_error(
            jvm => "Unable to read configuration from NQP on the JVM" );
    }
    my $bin = $ijvm->{bin};

    if (   !$self->backend_error('jvm')
        && !defined $nqp_config->{'jvm::runtime.jars'} )
    {
        $self->backend_error( jvm =>
              "jvm::runtime.jars value not available from $bin --show-config."
        );
    }

    unless ( $self->backend_error('jvm') ) {
        my $java_version = `java -version 2>&1`;
        $java_version =
          $java_version =~ /(?<v>[\d\._]+).+\n(?<n>\S+)/
          ? "$+{'n'} $+{'v'}"
          : 'no java version info available';

        print
"Using $bin (version $nqp_config->{'nqp::version'} / $java_version).\n";

        $config->{'nqp_prefix'}   = $nqp_config->{'jvm::prefix'};
        $config->{'nqp_jars'}     = $nqp_config->{'jvm::runtime.jars'};
        $config->{'bld_nqp_jars'} = join(
            $config->{'cpsep'},
            map { $config->{'sysroot'} . $_ }
              split( $config->{'cpsep'}, $nqp_config->{'jvm::runtime.jars'} )
        );
        $config->{'nqp_classpath'} = $nqp_config->{'jvm::runtime.classpath'};
        $config->{'nqp::libdir'}   = $nqp_config->{'nqp::libdir'};
        $config->{'j_runner'}      = $self->batch_file('perl6-j');
    }
}

sub configure_moar_backend {
    my $self   = shift;
    my $imoar  = $self->{impls}{moar};
    my $config = $self->{config};
    my $slash  = $self->slash;

    $config->{m_nqp} = $imoar->{bin};
    my $nqp_config;
    if ( $imoar->{ok} ) {
        $nqp_config = $imoar->{config};
    }
    elsif ( $imoar->{config} ) {
        $self->backend_error( moar => "The nqp-m binary is too old" );
    }
    else {
        $self->backend_error(
            moar => "Unable to read configuration from NQP on MoarVM" );
    }

    # Strip rpath from ldflags so we can set it differently ourself.
    $config->{ldflags} = join( ' ',
        $nqp_config->{'moar::ldflags'},
        $nqp_config->{'moar::ldmiscflags'},
        $nqp_config->{'moar::ldoptiflags'},
        $nqp_config->{'moar::ldlibs'} );
    $config->{ldflags} =~ s/\Q$nqp_config->{'moar::ldrpath'}\E ?//;
    $config->{ldflags} =~ s/\Q$nqp_config->{'moar::ldrpath_relocatable'}\E ?//;
    $config->{ldflags} .= ' '
      . (
          $config->{no_relocatable}
        ? $nqp_config->{'moar::ldrpath'}
        : $nqp_config->{'moar::ldrpath_relocatable'}
      );
    $config->{'mingw_unicode'} = '';

    if ( $self->is_win ) {
        if ( File::Spec->catdir( $config->{prefix}, 'bin' ) ne
            $nqp_config->{'moar::libdir'} )
        {
            $config->{'m_install'} = "\t"
              . '$(CP) '
              . $nqp_config->{'moar::libdir'}
              . $slash
              . $nqp_config->{'moar::moar'}
              . ' $(PREFIX)'
              . $slash . 'bin';
        }
        if ( $nqp_config->{'moar::os'} eq 'mingw32' ) {
            $config->{'mingw_unicode'} = '-municode';
        }
        $config->{'c_runner_libs'} = '-lShlwapi';
    }
    else {
        $config->{'m_cleanups'} =
"  \$(M_GDB_RUNNER) \\\n  \$(M_LLDB_RUNNER) \\\n  \$(M_VALGRIND_RUNNER)";
        $config->{'m_all'} =
          '$(M_GDB_RUNNER) $(M_LLDB_RUNNER) $(M_VALGRIND_RUNNER)';
        $config->{'m_install'} = "\t"
          . '$(M_RUN_PERL6) '
          . nfp("tools/build/create-moar-runner.p6")
          . ' perl6 $(M_RUNNER) $(DESTDIR)$(PREFIX)'
          . nfp("/bin/perl6-gdb-m")
          . ' "gdb" "" "" ""' . "\n\t"
          . '$(M_RUN_PERL6) '
          . nfp("tools/build/create-moar-runner.p6")
          . ' perl6 $(M_RUNNER) $(DESTDIR)$(PREFIX)'
          . nfp("/bin/perl6-lldb-m")
          . ' "lldb" "" "" ""' . "\n\t"
          . '$(M_RUN_PERL6) '
          . nfp("tools/build/create-moar-runner.p6")
          . ' perl6 $(M_RUNNER) $(DESTDIR)$(PREFIX)'
          . nfp("/bin/perl6-valgrind-m")
          . ' "valgrind" "" "" ""';
        $config->{'c_runner_libs'} = '';
    }

    unless ( $self->backend_error('moar') ) {
        print
"Using $config->{'m_nqp'} (version $nqp_config->{'nqp::version'} / MoarVM $nqp_config->{'moar::version'}).\n";

        $config->{'perl6_ops_dll'} =
          sprintf( $nqp_config->{'moar::dll'}, 'perl6_ops_moar' );

        # Add moar library to link command
        # TODO: Get this from Moar somehow
        $config->{'moarimplib'} =
          $self->is_win || $^O eq 'darwin'
          ? File::Spec->catfile( $nqp_config->{'moar::libdir'},
            $nqp_config->{'moar::sharedlib'} )
          : '';
    }
}

sub configure_js_backend {
    my $self   = shift;
    my $ijs    = $self->{impls}{js};
    my $config = $self->{config};
    my $slash  = $self->slash;
    my $nqp_config;
    $config->{js_nqp} = $ijs->{bin};
    $config->{'perl6_runtime'} =
      File::Spec->rel2abs( nfp('src/vm/js/perl6-runtime') );
    $config->{'perl6_lowlevel_libs'} =
      File::Spec->rel2abs('node_modules') . $slash;
    $config->{'perl6_js_runner'} =
      batch_file( File::Spec->rel2abs('perl6-js') );

    if ( $ijs->{ok} ) {
        $nqp_config = $ijs->{config};
    }
    elsif ( $ijs->{config} ) {
        $self->backend_errors( js => "The nqp-js binary is too old" );
    }
    else {
        $self->backend_errors(
            js => "Unable to read configuration from NQP on JS" );
    }
}

sub configure_from_options {
    my $self = shift;
    for my $opt (
        qw<prefix libdir sdkroot sysroot github-user git-protocol
        rakudo-repo nqp-repo moar-repo roast-repo makefile-timing
        no-relocatable reference>
      )
    {
        ( my $ckey = $opt ) =~ s/-/_/g;
        $self->set_key( $ckey, $self->{options}->{$opt}, default => '', );
    }
    $self->{config}{stagestats} = '--stagestats'
      if $self->{options}{'makefile-timing'};
}

sub is_win {
    state $win = $^O eq 'MSWin32';
    return $win;
}

sub is_solaris {
    state $solaris = $^O eq 'solaris';
    return $solaris;
}

sub isa_unix {

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

sub is_executable {
    my ( $self, $file ) = @_;
    die "File parameter is missing in call to is_executable" if @_ < 2;
    for my $ext (qw<exe bat>) {
        my $fname = $file . $self->cfg($ext);
        return $fname if -x $fname;
    }
    return 0;
}

# Returns all active language specification entries except for .c
sub perl6_specs {
    my $self = shift;
    return grep { $_->[0] ne 'c' } @{ $self->cfg('perl6_specs') };
}

sub github_url {
    my $self = shift;
    my ( $protocol, $user, $repo ) = @_;
    $protocol = lc( $protocol // 'https' );
    if ( $protocol eq 'https' || $protocol eq 'git' ) {
        return sprintf '%s://github.com/%s/%s.git', $protocol, $user, $repo;
    }
    elsif ( $protocol eq 'ssh' ) {
        return sprintf 'git@github.com:%s/%s.git', $user, $repo;
    }
    else {
        die "Unknown protocol '$protocol' (fine are: ssh, https, git)";
    }
}

sub repo_url {
    my $self     = shift;
    my $repo     = shift;
    my %params   = @_;
    my $action   = $params{action} || 'pull';
    my $protocol = $params{protocol};
    my $config   = $self->config;

    die "Unknown repository type '$repo'" unless $self->{repo_maps}{$repo};
    die "Bad action type '$action'" unless $action =~ /^(push|pull)$/;

    my $gproto =
      $action eq 'push'
      ? 'ssh'
      : $protocol || $config->{git_protocol} || 'https';
    my $ckey     = "${repo}_${action}_url";
    my $repo_key = $repo . "_repo";

    # Return user defined repo if there is one
    if ( $config->{$repo_key} ) {
        return $config->{$ckey} = $config->{$repo_key};
    }

    # Return cached response.
    return $config->{$ckey} if $config->{$ckey} && !$protocol;

    my ( $guser, $grepo ) = @{ $self->{repo_maps}{$repo} };
    $guser = $config->{github_user} if $config->{github_user};
    my $url = $self->github_url( $gproto, $guser, $grepo );
    $config->{$ckey} = $url unless $protocol;
    return $url;
}

sub include_path {
    my $self = shift;

    my @incs;
    for my $ctx ( $self->contexts ) {
        next unless $ctx->{including_file};
        if (@incs) {
            push @incs, "\tincluded from $ctx->{including_file}";
        }
        else {
            push @incs, " in file $ctx->{including_file}";
        }
    }
    push @incs, " in template " . $self->prop('template_file');
    return join( "\n", @incs );
}

sub find_file_path {
    my $self   = shift;
    my $src    = shift;
    my %params = @_;
    my $config = $self->config;

    return $src if File::Spec->file_name_is_absolute($src);

    my @subdirs;

    push @subdirs, $params{subdir}       if $params{subdir};
    push @subdirs, @{ $params{subdirs} } if $params{subdirs};
    push @subdirs, "" unless $params{subdirs_only};

    my $ctx_subdir = $self->cfg('ctx_subdir');
    push @subdirs, $ctx_subdir if $ctx_subdir;

    my $where = $params{where} || 'templates';
    my $where_dir = $self->cfg( "${where}_dir", strict => 1 );
    my @suffixes  = ("");
    push @suffixes, $params{suffix}        if $params{suffix};
    push @suffixes, @{ $params{suffixes} } if $params{suffixes};

    for my $subdir (@subdirs) {
        my $try_dir = File::Spec->catdir( $where_dir, $subdir );
        for my $sfx (@suffixes) {

            # Don't append extension if it's already there.
            next if $sfx && $src =~ /\Q$sfx\E$/;
            my $tfile = File::Spec->catfile( $try_dir, $src . $sfx );
            return $tfile if -e $tfile;
        }
    }
    die "File '$src' not found in base directory $where_dir"
      . $self->include_path
      if $params{required};
    return "";
}

sub template_file_path {
    my $self = shift;
    return $self->find_file_path( shift, suffix => ".in", @_ );
}

sub build_file_path {
    my $self = shift;
    return $self->find_file_path(
        shift,
        where    => 'build',
        suffixes => [qw<.pl .nqp .p6>],
        @_
    );
}

sub fill_template_file {
    my $self = shift;
    my ( $infile, $outfile ) = @_;

    my $OUT;
    if ( ref $outfile ) {
        $OUT = $outfile;
    }
    else {
        print "\nCreating $outfile ...\n";
        open( $OUT, '>', $outfile )
          or die "Unable to write $outfile\n";
    }

    my @infiles = ref($infile) ? @$infile : $infile;
    for my $if (@infiles) {
        my $ifpath = $self->template_file_path( $if, required => 1, );
        my $s    = $self->push_ctx( { template_file => $ifpath, } );
        my $text = slurp($ifpath);
        print $OUT "\n# Generated from $ifpath\n";
        $text = $self->fill_template_text( $text, source => $ifpath );
        print $OUT $text;
        print $OUT "\n\n# (end of section generated from $ifpath)\n\n";
    }
    unless ( ref $outfile ) {
        close($OUT) or die "Error while writing '$outfile': $!";
    }
}

sub fixup_makefile {
    my $self = shift;
    my $text = shift;
    if ( $self->is_win ) {
        $text =~ s{/}{\\}g;
        $text =~ s{\\\*}{\\\\*}g;
        $text =~ s{(?:git|http):\S+}{ do {my $t = $&; $t =~ s'\\'/'g; $t} }eg;
        $text =~ s/.*curl.*/do {my $t = $&; $t =~ s'%'%%'g; $t}/meg;
    }
    if ( $self->cfg('makefile_timing') ) {
        $text =~ s{ (?<!\\\n)        # not after line ending in '\'
                        ^                # beginning of line
                        (\t(?>@?[ \t]*)) # capture tab, optional @, and hspace
                        (?!-)            # not before - (ignore error) lines
                        (?!cd)           # not before cd lines
                        (?!echo)         # not before echo lines
                        (?=\S)           # must be before non-blank
                      }
                      {$1time\ }mgx;
    }
    return $text;
}

sub fill_template_text {
    my $self   = shift;
    my $text   = shift;
    my %params = @_;
    my $config = $self->config;

    my sub on_fail {
        my $msg = shift;
        my $src = $params{source} ? " in template $params{source}" : "";
        die "$msg$src";
    }

    my $text_out =
      NQP::Configure::Macros->new( config => $self, on_fail => \&on_fail )
      ->expand($text);

    # XXX This is better be handled with makefile macros. Then the whole method
    # would be easily replaced with Macros->expand()
    if ( $text_out =~ /nqp::makefile/ ) {
        $text_out = $self->fixup_makefile($text_out);
    }
    $text_out;
}

sub reference_dir {
    my $self      = shift;
    my $reference = $self->cfg('reference');
    for my $d (@_) {
        my $dir = File::Spec->catdir( $reference, $d );
        return $dir if -d $dir;
    }
    return '';
}

sub git_checkout {
    my ( $self, $repo, $dir, $checkout ) = @_;

    die "Unknown repository '$repo' in call to git_checkout"
      unless $self->{repo_maps}{$repo};

    my $config  = $self->config;
    my $options = $self->{options};
    my $pwd     = cwd();

    # get an up-to-date repository
    if ( !-d $dir ) {
        my ( $pullurl, $pushurl ) = (
            $self->repo_url( $repo, action => 'pull' ),
            $self->repo_url( $repo, action => 'push' ),
        );
        my @args = ( 'git', 'clone' );
        if ( $config->{reference} ) {
            my $ref_dir =
              $self->reference_dir( $self->{repo_maps}{$repo}[1], $dir );
            die "Can't $repo repository directory in $config->{reference}"
              unless $ref_dir;
            push @args, "--reference=$ref_dir";
        }
        push @args, "--depth=$options->{'git-depth'}"
          if $options->{'git-depth'};
        push @args, $pullurl, $dir;
        system_or_die(@args);
        chdir($dir);

        system( 'git', 'config', 'remote.origin.pushurl', $pushurl )
          if defined $pushurl && $pushurl ne $pullurl;
    }
    else {
        chdir($dir);
        system_or_die( 'git', 'fetch' );

        # pre-git 1.9/2.0 `--tags` did not fetch tags in addition to normal
        # fetch https://stackoverflow.com/a/20608181/2410502 so do it separately
        system_or_die( 'git', 'fetch', '--tags' );
    }

    if ($checkout) {
        system_or_die( 'git', 'checkout', $checkout );
        system_or_die( 'git', 'pull' )
          if slurp('.git/HEAD') =~ /^ref:/;
    }

    my $git_describe;
    if ( open( my $GIT, '-|', "git describe --tags" ) ) {
        $git_describe = <$GIT>;
        close($GIT);
        chomp $git_describe;
    }
    chdir($pwd);
    $git_describe;
}

sub gen_nqp {
    my $self    = shift;
    my $options = $self->{options};
    my $config  = $self->config;

    my $nqp_bin      = $options->{'with-nqp'};
    my $gen_nqp      = $options->{'gen-nqp'};
    my $gen_moar     = $options->{'gen-moar'};
    my $prefix       = $config->{'prefix'};
    my $sdkroot      = $config->{'sdkroot'};
    my $startdir     = $config->{'base_dir'};
    my $bat          = $config->{bat};
    my $nqp_want     = $config->{nqp_want};
    my $git_protocol = $options->{'git-protocol'} // 'https';
    my @moar_options = @{ $options->{'moar-option'} // [] };
    my $impls        = $self->{impls};
    my $pwd          = cwd;

    my %need;

    for my $b ( $self->active_backends ) {
        my $postfix = $self->backend_abbr($b);
        my $bin     = nfp($nqp_bin)
          || (
            $sdkroot
            ? File::Spec->catfile( nfp($sdkroot), $prefix, 'bin',
                "nqp-$postfix$bat" )
            : File::Spec->catfile( $prefix, 'bin', "nqp-$postfix$bat" )
          );
        $impls->{$b}{bin} = $bin;
        my %c        = read_config($bin);
        my $nqp_have = $c{'nqp::version'} || '';
        $impls->{$b}{config} = \%c if %c;
        my $nqp_ok = $nqp_have && 0 <= cmp_rev( $nqp_have, $nqp_want );

        if ( $nqp_ok or $options->{'ignore-errors'} ) {
            $impls->{$b}{ok} = 1;
        }
        else {
            $need{$b} = 1;
        }
    }

    return unless %need;

    if ( defined $gen_nqp || defined $gen_moar ) {
        my $user = $options->{'github-user'} // 'perl6';
        $self->git_checkout( 'nqp', 'nqp', $gen_nqp || $nqp_want );
    }

    return unless defined($gen_nqp) || defined($gen_moar);

    my $backends = join ",", $self->active_backends;
    my @cmd      = (
        $^X, 'Configure.pl', "--prefix=$prefix",
        "--backends=$backends", "--make-install",
        "--git-protocol=$git_protocol",
    );

    for my $opt (qw<git-depth git-reference github-user nqp-repo moar-repo>) {
        push @cmd, "--$opt", $options->{$opt} if $options->{$opt};
    }

    if ( defined $gen_moar ) {
        push @cmd, $gen_moar ? "--gen-moar=$gen_moar" : '--gen-moar';

        if (@moar_options) {
            push @cmd, map( sprintf( '--moar-option=%s', $_ ), @moar_options );
        }
    }

    print "Building NQP ...\n";
    chdir("$startdir/nqp");
    print "@cmd\n";
    system_or_die(@cmd);
    chdir($pwd);

    for my $k ( keys %need ) {
        my %c = read_config( $impls->{$k}{bin} );
        %c = ( %{ $impls->{$k}{config} || {} }, %c );
        $impls->{$k}{config} = \%c;
        $impls->{$k}{ok}     = 1;
    }
    return;
}

sub _restore_ctx {
    my %params = @_;
    $params{obj}->pop_ctx;
}

sub contexts {
    my @c = reverse @{ $_[0]->{contexts} };
}

sub cur_ctx {
    return {} unless @{ $_[0]->{contexts} };
    $_[0]->{contexts}[-1];
}

sub push_ctx {
    my $self = shift;
    my $ctx  = shift;

    die "Context must be a hash" unless ref($ctx) eq 'HASH';

    warn "Context has 'config' key. Didn't you mean 'configs'?"
      if exists $ctx->{config};

    my @c = caller(1);

    $ctx->{".ctx"} = {
        from => {
            file => $c[1],
            line => $c[2],
            sub  => $c[3],
        },
    };

    if ( $ctx->{configs} ) {
        if ( ref( $ctx->{configs} ) ) {
            my $is_valid = 1;
            if ( ref( $ctx->{configs} ) eq 'ARRAY' ) {
                for my $cfg ( @{ $ctx->{configs} } ) {
                    if ( ref($cfg) ne 'HASH' ) {
                        $is_valid = 0;
                        last;
                    }
                }
            }
            else {
                $is_valid = 0;
            }
            die "'configs' key of context must be a list of hashes"
              unless $is_valid;
        }
        else {
            $ctx->{configs} = [ $ctx->{configs} ];
        }
    }
    else {
        $ctx->{configs} = [];
    }

    push @{ $self->{contexts} }, $ctx;

    return NQP::Config::_Scoping->new( \&_restore_ctx, obj => $self );
}

sub pop_ctx {
    my $self = shift;
    return pop @{ $self->{contexts} };
}

sub set_key {
    my $self = shift;
    my ( $key, $val, %params ) = @_;
    my $mname = "_ckey_$key";
    if ( $self->can($mname) ) {
        $val = $self->$mname($val);
    }
    $val //= $params{default};
    return $self->{config}{$key} = $val;
}

# Searches for a config variable in contexts (from latest pushed upwards) and
# then in the main config. If context contains more than one config hash in
# configs key then they're searched forward, from the first to the last.
sub cfg {
    my $self   = shift;
    my $var    = shift;
    my %params = @_;

    # Don't use config method for better performance.
    for my $ctx ( $self->contexts ) {
        my $configs = $ctx->{configs};
        for my $config (@$configs) {
            return $config->{$var} if exists $config->{$var};
        }
    }

    die "Can't find configuration variable '$var'"
      if $params{strict} && !exists $self->{config}{$var};

    return $self->{config}{$var};
}

# Same as cfg but looking for a property, i.e. a key on a context or config
# object itself.
sub prop {
    my $self   = shift;
    my $name   = shift;
    my %params = @_;

    for my $ctx ( $self->contexts ) {
        return $ctx->{$name} if exists $ctx->{$name};
    }

    die "Can't find property '$name'"
      if $params{strict} && !exists $self->{$name};

    return $self->{$name};
}

# $config->in_ctx(prop_name => "prop value")
sub in_ctx {
    my $self = shift;
    my ( $prop, $val ) = @_;
    my %params = @_;

    for my $ctx ( $self->contexts ) {
        return $ctx if $ctx->{$prop} eq $val;
    }

    return 0;
}

sub config {
    my $self   = shift;
    my %params = @_;

    return $self->{config} if $params{no_ctx};

    my %config = %{ $self->{config} };

    for my $ctx ( @{ $self->{contexts} } ) {

        # Reversing because the first must override the last.
        for my $ctx_cfg ( reverse @{ $ctx->{configs} } ) {
            @config{ keys %$ctx_cfg } = values %$ctx_cfg;
        }
    }

    return \%config;
}

#########################################################
### Non-method subs
#########################################################

sub slash {
    state $slash = File::Spec->catfile( '', '' );
    return $slash;
}

sub slurp {
    my $filename = shift;
    open my $fh, '<', $filename
      or die "Unable to read file '$filename'\n";
    local $/ = undef;
    my $text = <$fh>;
    close $fh or die $!;
    return $text;
}

sub nfp {
    my ( $vol, $dirs, $file ) = File::Spec->splitpath(shift);
    return File::Spec->catpath( $vol,
        File::Spec->catdir( File::Spec->splitdir($dirs) ), $file );
}

sub system_or_die {
    my @cmd = @_;
    system(@cmd) == 0
      or die "Command failed (status $?): @cmd\n";
}

sub parse_revision {
    my $rev       = shift;
    my $sep       = qr/[_.]/;
    my $rev_regex = qr/
        (?<year> \d+)
        $sep
        (?<month> \d+)
        (?:
            $sep
            (?<day> \d+)
        )?
        (?:
            -
            (?:
                (?<revno> \d+) - g[a-f0-9]*

                |

                RC (?<rcno> \d+)
            )
        )?
        $
    /x;
    if ( $rev =~ $rev_regex ) {
        return ( $+{year}, $+{month}, $+{day} // 0, $+{rcno} // 0,
            $+{revno} // 0 );
    }
    else {
        die "Unrecognized revision specifier '$rev'\n";
    }
}

sub cmp_rev {
    my ( $a, $b ) = @_;
    my @a   = parse_revision($a);
    my @b   = parse_revision($b);
    my $cmp = 0;
    for ( 0 .. 4 ) {
        $cmp = $a[$_] <=> $b[$_] if ( defined $a[$_] && defined $b[$_] );
        last if $cmp;
    }
    $cmp;
}

sub read_config {
    my @config_src = @_;
    my %config     = ();
    local $_;
    for my $file (@config_src) {
        no warnings;
        if ( !-f $file ) {
            print "No pre-existing installed file found at $file\n";
            next;
        }
        if ( open my $CONFIG, '-|', "\"$file\" --show-config" ) {
            while (<$CONFIG>) {
                if (/^([^\s=]+)=(.*)/) { $config{$1} = $2 }
            }
            close($CONFIG);
        }
        last if %config;
    }
    return %config;
}

### Handlers for configuration keys.
# A handler takes a value assigned to a key and must return same or another
# value which is considered valid for this key. I.e. it could set to a default
# if the assigned value is undefined, for example.

sub _ckey_libdir {
    my ( $self, $val ) = @_;
    unless ( defined $val ) {
        return File::Spec->catdir( $self->cfg('prefix'), 'share' );
    }
    $val;
}

### Tieing-related stuff
sub TIEHASH {
    my $class = shift;
    my $self  = bless {}, $class;
    return $self->init(@_);
}

sub FETCH {
    my $self = shift;
    my $key  = shift;
    return $self->{config}{$key};
}

sub STORE {
    my $self = shift;
    my ( $key, $val ) = @_;
    return $self->set_key( $key, $val );
}

sub DELETE {
    my $self = shift;
    my $key  = shift;
    delete $self->{config}{$key};
}

sub CLEAR {
    my $self = shift;
    $self->{config} = {};
}

sub EXISTS {
    my $self = shift;
    my $key  = shift;
    exists $self->{config}{$key};
}

sub FIRSTKEY {
    my $self = shift;
    each %{ $self->{config} };
}

sub NEXTKEY {
    my $self = shift;
    each %{ $self->{config} };
}

sub SCALAR {
    my $self = shift;
    %{ $self->{config} };
}

1;

# vim: ft=perl
