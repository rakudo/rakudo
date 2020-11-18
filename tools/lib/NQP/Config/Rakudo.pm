use v5.10.1;

package NQP::Config::Rakudo;
use strict;
use warnings;
use Cwd qw<abs_path cwd>;
use POSIX qw<strftime>;
use Digest::SHA;
use NQP::Config qw<slurp read_config cmp_rev system_or_die run_or_die>;
use IPC::Cmd qw<run>;
use NQP::Macros;

use base qw<NQP::Config>;

sub configure_nqp {
    my $self = shift;
    my $nqp_bin;

    my $have_dir_opts =
      defined( $self->opt('prefix') || $self->opt('sysroot') );

    if ( $nqp_bin = $self->opt('with-nqp') ) {
        $nqp_bin = File::Spec->rel2abs( $self->nfp($nqp_bin) );

        $self->sorry( "Could not find nqp '"
              . $self->opt('with-nqp')
              . "' defined with --with-nqp" )
          unless -x $nqp_bin;
        my $nqp_backend;
        run(
            command =>
              [ $nqp_bin, '-e', 'print(nqp::getcomp(\'nqp\').backend.name)' ],
            buffer => \$nqp_backend
          )
          or $self->sorry("Could not get backend information from '$nqp_bin'");
        $self->use_backend($nqp_backend);
        $self->backend_config( $nqp_backend, nqp_bin => $nqp_bin );
        my $passed_backends = $self->opt('backends');
        if ( defined $passed_backends && $nqp_backend ne $passed_backends ) {
            $self->sorry(
                "Passed value to --backends ($passed_backends) is overwritten ",
                "by the one infered by --with-nqp ($nqp_backend)"
            );
        }
    }

    #elsif ( !$have_dir_opts ) {
    #    # NQP from path would only be used to determine the implicit prefix.
    #    $nqp_bin = can_run('nqp');
    #}

    $self->set( nqp_default => $nqp_bin ) if $nqp_bin;
}

sub configure_backends {
    my $self = shift;

    my $prefix = $self->cfg('prefix');

    unless ( $self->active_backends ) {
        ### Consider what backends are to be built. ###
        my $passed_backends = $self->opt('backends');
        if ($passed_backends) {
            my @use_backends = $self->parse_backends($passed_backends);
            for my $b (@use_backends) {
                if ( $b eq 'parrot' ) {
                    $self->sorry(
                        "The Parrot backend has been suspended.\n",
                        "Please use Rakudo 2015.02 ",
                        "(which still supports parrot), ",
                        "or the MoarVM backend instead"
                    );
                }
                unless ( $self->known_backend($b) ) {
                    $self->sorry(
                            "Unknown backend '$b'; Supported backends are: "
                          . join( ", ", sort $self->known_backends )
                          . "\n" );
                }
                $self->use_backend($b);
            }
            unless ( $self->active_backends ) {
                $self->sorry("--prefix given, but no valid backend?!");
            }
        }
        else {
            if ($prefix) {
                for my $a ( $self->known_abbrs ) {
                    my $nqp_cmd = "nqp-$a";
                    my $nqp_bin = $self->is_executable("$prefix/bin/$nqp_cmd");
                    if ($nqp_bin) {
                        my $b = $self->abbr_to_backend($a);
                        $self->msg("Found $nqp_bin (backend $b)\n");
                        $self->use_backend($b);
                        $self->backend_config( $b, nqp_bin => $nqp_bin );
                    }
                }
            }

            $self->use_backend('moar') if $self->has_option('gen-moar');

            unless ( $self->active_backends or $self->has_option('with-nqp') ) {
                $self->sorry(
                    "No suitable nqp executables found! ",
                    "Please specify some --backends, ",
                    "or a --prefix that contains nqp-{js,j,m} executables\n\n",
                    "Example to build for all backends ",
                    "(which will take a while):\n",
                    "\tperl Configure.pl --backends=ALL --gen-moar\n\n",
                    "Example to build for MoarVM only:\n",
                    "\tperl Configure.pl --gen-moar\n\n",
                    "Example to build for JVM only:\n",
                    "\tperl Configure.pl --backends=jvm --gen-nqp\n\n"
                );
            }
        }
    }
}

sub configure_refine_vars {
    my $self = shift;

    my $config  = $self->{config};
    my $nqp_bin = $self->cfg('nqp_default');
    if ($nqp_bin) {
        my ( $vol, $dir, undef ) = File::Spec->splitpath($nqp_bin);
        my $nqp_prefix = File::Spec->catpath( $vol,
            File::Spec->catdir( $dir, File::Spec->updir ) );

        my $prefix = $self->cfg('prefix');

        if ( !$prefix ) {
            $self->set( prefix => $nqp_prefix );
            $self->note(
                "ATTENTION",
                "No --prefix supplied, ",
                "building and installing to $nqp_prefix\n",
                "Based on found executable $nqp_bin"
            );
        }
        elsif ( $self->cfg('relocatable') eq 'reloc'
            && abs_path($prefix) ne abs_path($nqp_prefix) )
        {
            $self->sorry(
                "Installation directory '$prefix' is different from '",
                abs_path($nqp_prefix),
                "' where nqp is installed.\n",
                "This does not work when creating a relocatable setup.\n",
                "A relocatable installation needs to have MoarVM, NQP and\n",
                "Rakudo all in the same prefix."
            );
        }
    }

    $self->SUPER::configure_refine_vars(@_);

    $config->{rakudo_home} = $self->nfp(
        File::Spec->rel2abs(
            $config->{rakudo_home}
              || File::Spec->catdir( $config->{'prefix'}, 'share', 'perl6' )
        )
    );

    $config->{nqp_home} = $self->nfp(
        File::Spec->rel2abs(
            $config->{nqp_home}
              || File::Spec->catdir( $config->{'prefix'}, 'share', 'nqp' )
        )
    );

    $config->{static_rakudo_home} =
      $config->{relocatable} eq 'reloc'
      ? ''
      : $config->{rakudo_home};
}

sub parse_lang_specs {
    my $self = shift;

    my $config = $self->{config};
    my $tmpl   = 'RAKU_SPECS';
    open my $sh, "<", $self->template_file_path( $tmpl, required => 1 )
      or self->sorry("Can't open $tmpl: $!");

    my @specs;    # Array to preserve the order or specs
    my $ln        = 0;
    my %flag_name = ( '-' => 'deprecate', '!' => 'require', );
    my $fail      = sub {
        $self->sorry( "$tmpl($ln): " . join( "", @_ ) );
    };
    for my $line (<$sh>) {
        $ln++;
        chomp $line;
        $line =~ s/\h*#.*$//;    # Cut off comment
        next unless $line;
        if ( $line =~ s/^\h*(?<default>\*)?(?<letter>[c-z])\b// ) {
            my $letter = $+{letter};
            if ( $+{default} ) {
                $fail->("duplicate default spec") if $config->{lang_spec};
                $config->{lang_spec} = $letter;
            }
            my @def = ($letter);
            push @specs, \@def;
            my @mods;            # Array to preserve the order of modificators
            while ($line) {
                unless ( $line =~ s/^\h+// ) {
                    $fail->("whitespace is missing");
                }
                next unless $line;
                if ( $line =~ s/^(?<flags>[\!\-]+)?(?<mod>\w+)\b// ) {
                    my ( $mod, $flag_str ) = @+{qw<mod flags>};
                    $flag_str //= "";
                    my %flags;
                    %flags = map { $flag_name{$_} => 1 } split( "", $flag_str );
                    push @mods, [ $mod, \%flags ];
                }
                else {
                    $fail->("expected a modificator");
                }
            }
            push @def, @mods;
        }
        else {
            $fail->("expected a revision letter");
        }
    }

    $self->{raku_specs} = \@specs;

    close $sh;
}

sub configure_misc {
    my $self   = shift;
    my $config = $self->{config};

    $self->SUPER::configure_misc(@_);

    # determine the version of NQP we want
    ( $config->{nqp_want} ) =
      split( ' ',
        slurp( $self->template_file_path( 'NQP_REVISION', required => 1, ) ) );

    $self->parse_lang_specs;

    #my $spec_line = sub {
    #    my @elems = split ' ', shift;
    #    if ( $elems[0] =~ s/^\*// ) {
    #        $config->{lang_spec} = $elems[0];
    #    }
    #    return \@elems;
    #};

    #$self->{raku_specs} = [
    #    map { $spec_line->($_) }
    #      grep { s/\s*#.*$//; length }
    #      split(
    #        /\n/s,
    #        slurp( $self->template_file_path( 'RAKU_SPECS', required => 1, ) )
    #      )
    #];

    # Get version info from VERSION template and git.
    my $VERSION = slurp( File::Spec->catfile( $self->cfg('base_dir'), 'VERSION' ) );
    chomp $VERSION;
    @{$config}{qw<version release codename>} = split( ' ', $VERSION, 3 );

    if ( -d File::Spec->catdir( $config->{base_dir}, '.git' )
        && open( my $GIT, '-|', q|git describe --match "2*"| ) )
    {
        my $git_version = <$GIT>;    # may be empty if we didn't fetch any tags
        $config->{version} = $git_version || "$config->{version}.0000.1";
        chomp $config->{version};
        close($GIT);
    }

    $config->{builddate} = strftime( '%Y-%m-%dT%H:%M:%SZ', gmtime );

    # NQP_LIB
    $config->{set_nqp_lib} = 'NQP_LIB=blib ';
    if ( $self->is_win ) {
        $config->{set_nqp_lib} = "set $config->{set_nqp_lib}\n\t";
    }
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
        my $java_version = run_or_die( [qw<java -version>] );
        $java_version =
          $java_version =~ /(?<v>[\d\._]+).+\n(?<n>\S+)/
          ? "$+{'n'} $+{'v'}"
          : 'no java version info available';

        $self->msg( "Using $bin"
              . " (version $nqp_config->{'nqp::version'}"
              . " / $java_version).\n" );

        $config->{'nqp_prefix'}   = $nqp_config->{'jvm::prefix'};
        $config->{'nqp_jars'}     = $nqp_config->{'jvm::runtime.jars'};
        $config->{'bld_nqp_jars'} = join(
            $config->{'cpsep'},
            map { $config->{'sysroot'} . $_ }
              split( $config->{'cpsep'}, $nqp_config->{'jvm::runtime.jars'} )
        );
        $config->{'nqp_classpath'} = $nqp_config->{'jvm::runtime.classpath'};
        $config->{'nqp::libdir'}   = $nqp_config->{'nqp::libdir'};
        $config->{'j_runner'}      = $self->batch_file('rakudo-j');

        $nqp_config->{static_nqp_home} = $nqp_config->{'nqp::static-nqp-home'};
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

    if ( $config->{relocatable} eq 'reloc' ) {
        $nqp_config->{static_nqp_home}           = '';
        $nqp_config->{static_nqp_home_define}    = '';
        $nqp_config->{static_rakudo_home_define} = '';
    }
    else {
        my $qchar = $config->{quote};
        $nqp_config->{static_nqp_home} = $nqp_config->{'nqp::static-nqp-home'};
        $nqp_config->{static_nqp_home_define} =
            '-DSTATIC_NQP_HOME='
          . $qchar
          . $self->c_escape_string( $nqp_config->{static_nqp_home} )
          . $qchar;
        $nqp_config->{static_rakudo_home_define} =
            '-DSTATIC_RAKUDO_HOME='
          . $qchar
          . $self->c_escape_string( $config->{static_rakudo_home} )
          . $qchar;
    }

    # Strip rpath from ldflags so we can set it differently ourself.
    $nqp_config->{ldflags} = $nqp_config->{'moar::ldflags'};
    $nqp_config->{ldflags} =~ s/\Q$nqp_config->{'moar::ldrpath'}\E ?//;
    $nqp_config->{ldflags} =~
      s/\Q$nqp_config->{'moar::ldrpath_relocatable'}\E ?//;
    if ( $config->{prefix} ne '/usr' ) {
        $nqp_config->{ldflags} .= ' '
          . (
              $config->{relocatable} eq 'reloc'
            ? $nqp_config->{'moar::ldrpath_relocatable'}
            : $nqp_config->{'moar::ldrpath'}
          );
    }
    $nqp_config->{mingw_unicode} = '';
    $nqp_config->{subsystem_windows_flag} = '';

    my @c_runner_libs;
    if ( $self->is_win ) {
        if ( File::Spec->catdir( $config->{prefix}, 'bin' ) ne
            $nqp_config->{'moar::libdir'} )
        {
            #$config->{'m_install'} = "\t"
            #  . '$(CP) '
            #  . $nqp_config->{'moar::libdir'}
            #  . $slash
            #  . $nqp_config->{'moar::moar'}
            #  . ' $(PREFIX)'
            #  . $slash . 'bin';
            $config->{m_install} = "\t"
              . q<$(NOECHO)$(CP) @nfpq(@moar::libdir@/@moar::moar@)@ @nfpq($(DESTDIR)$(PREFIX)/bin)@>;
        }
        if ( $nqp_config->{'moar::os'} eq 'mingw32' ) {
            $nqp_config->{mingw_unicode} = '-municode';
        }

        $nqp_config->{subsystem_win_cc_flags} = '-DSUBSYSTEM_WINDOWS';
        if ( $nqp_config->{'moar::ld'} eq 'link' ) {
            $nqp_config->{subsystem_win_ld_flags} = '/subsystem:windows';
        }
        else {
            $nqp_config->{subsystem_win_ld_flags} = '-mwindows';
        }

        push @c_runner_libs, sprintf( $nqp_config->{'moar::ldusr'}, 'Shlwapi' );
        push @c_runner_libs, sprintf( $nqp_config->{'moar::ldusr'}, 'Shell32' );
    }
    else {
        $imoar->{toolchains} = [qw<gdb lldb valgrind>];
    }
    $nqp_config->{c_runner_libs} = join( " ", @c_runner_libs );
    $nqp_config->{moar_lib}      = sprintf(
        (
              $nqp_config->{'moar::ldimp'}
            ? $nqp_config->{'moar::ldimp'}
            : $nqp_config->{'moar::ldusr'}
        ),
        $nqp_config->{'moar::name'}
    );

    $nqp_config->{'with_raku_alias'} =
      ( $self->has_option('raku-alias') && !$self->opt('raku-alias') )
      ? 'off'
      : 'on';

    unless ( $self->backend_error('moar') ) {
        $self->msg( "Using $config->{m_nqp}"
              . " (version $nqp_config->{'nqp::version'}"
              . " / MoarVM $nqp_config->{'moar::version'}).\n" );

        $nqp_config->{perl6_ops_dll} =
          sprintf( $nqp_config->{'moar::dll'}, 'perl6_ops_moar' );
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
      File::Spec->rel2abs( $self->nfp('src/vm/js/perl6-runtime') );
    $config->{'rakudo_js_runner'} =
      $self->batch_file( File::Spec->rel2abs('rakudo-js') );

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

    $self->backend_config(
        'js',
        js_build_dir => File::Spec->catdir( $config->{base_dir}, qw<gen js> ),
        js_blib => File::Spec->catdir( $config->{base_dir}, "node_modules" ),
        static_nqp_home => $nqp_config->{'nqp::static-nqp-home'},
    );
}

sub configure_active_backends {
    my $self = shift;

    my $impls = $self->{impls};
    for my $k ( $self->active_backends ) {
        my %c = read_config( $impls->{$k}{bin} );
        $self->backend_config( $k, \%c );
        $impls->{$k}{ok} = 1;
    }

    return $self->SUPER::configure_active_backends(@_);
}

sub opts_for_configure {
    my $self = shift;
    my @opts = $self->SUPER::opts_for_configure(@_);

    my $nqp_bin = $self->cfg('nqp_bin');
    push @opts, "--with-nqp=" . $self->cfg('nqp_bin') if $nqp_bin;

    return wantarray ? @opts : join( " ", @opts );
}

sub clean_old_p6_libs {
    my $self    = shift;
    my $is_moar = $self->active_backend('moar');
    if ($is_moar) {
        my $nqp_config = $self->{impls}{moar}->{config};
        my $lib_dir    = File::Spec->rel2abs(
            File::Spec->catdir(
                $nqp_config->{'nqp::prefix'},
                'share', 'nqp', 'lib', 'Perl6'
            )
        );

        return if !-d $lib_dir;

        my @files =
          qw(Actions.moarvm BOOTSTRAP.moarvm Compiler.moarvm Grammar.moarvm Metamodel.moarvm ModuleLoader.moarvm Ops.moarvm Optimizer.moarvm Pod.moarvm World.moarvm);

        my @notes;
        my @clean_rules;
        for (@files) {
            my $file = File::Spec->catdir( $lib_dir, $_ );
            next unless -f $file;
            push @notes,       "Will remove: $file\n";
            push @clean_rules, "\t\@noecho\@\$(RM_F) $file";
        }
        my $pp_pfx = $self->cfg('make_pp_pfx');
        if (@clean_rules) {
            unshift @clean_rules,
              "\t\@echo(+++ Cleaning up outdated MOAR files)\@\n";
        }

        # Don't try removing if DESTDIR is defined for the running make. It is
        # likely that a rakudo package is being built in a working environment.
        $self->{config}->{clean_old_p6_libs} =
          @clean_rules
          ? ( "${pp_pfx}ifndef DESTDIR\n"
              . join( "\n", @clean_rules )
              . "\n${pp_pfx}endif\n" )
          : "";
        $self->note(
            'NOTICE',
            "Found stale files in $lib_dir.\n",
            "These files were left by a previous install and cause breakage\n",
            "in this Rakudo version. The 'install' phase will try to remove\n",
            "them if possible.\n",
            "\n",
            @notes
        ) if @notes;
    }
}

# Returns all active language specification entries except for .c
sub raku_specs {
    my $self = shift;
    return @{ $self->{raku_specs} };
}

sub post_active_backends {
    my $self = shift;

    my @errors;

    for my $b ( $self->active_backends ) {
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

# Returns true if jvm or moar backend check produced errors and no gen-nqp
# specified
sub no_gen_nqp {
    my $self = shift;
    return ( $self->backend_error('jvm') || $self->backend_error('moar') )
      && !$self->{options}{'gen-nqp'};
}

sub gen_nqp {
    my $self    = shift;
    my $options = $self->{options};
    my $config  = $self->{config};

    #my $nqp_bin      = $options->{'with-nqp'};
    my $nqp_git_spec  = $options->{'gen-nqp'};
    my $gen_nqp       = $options->{'gen-nqp'};
    my $gen_moar      = $options->{'gen-moar'};
    my $force_rebuild = $options->{'force-rebuild'};
    my $prefix        = $config->{prefix};
    my $sdkroot       = $config->{'sdkroot'};
    my $startdir      = $config->{'base_dir'};
    my $nqp_want      = $config->{nqp_want};
    my $git_protocol  = $options->{'git-protocol'} // 'https';
    my @moar_options  = @{ $options->{'moar-option'} // [] };
    my $impls         = $self->{impls};
    my $pwd           = cwd;

    if ( $force_rebuild && !defined $gen_nqp ) {
        $self->note( "WARNING",
                "Command line option --force-rebuild"
              . " is ineffective without --gen-nqp" );
    }

    my %need;

    for my $b ( $self->active_backends ) {
        my $bconfig = $self->backend_config($b);
        my $postfix = $self->backend_abbr($b);
        my $exe     = $b eq 'moar' ? $config->{exe} : $config->{bat};
        my $tpath   = File::Spec->catfile( $prefix, 'bin', "nqp-$postfix$exe" );
        my $bin     = $bconfig->{nqp_bin}
          || (
            $sdkroot
            ? File::Spec->catfile( $self->nfp($sdkroot),
                $prefix, 'bin', "nqp-$postfix$exe" )
            : File::Spec->catfile( $prefix, 'bin', "nqp-$postfix$exe" )
          );
        $impls->{$b}{bin} = $bin;
        my %c        = read_config($bin);
        my $nqp_have = $c{'nqp::version'} || '';
        $self->backend_config( $b, \%c ) if %c;
        my $nqp_ver_ok =
          $nqp_have ? ( 0 <= cmp_rev( $nqp_have, $nqp_want ) ) : 0;
        my $nqp_ok = $nqp_have && $nqp_ver_ok;

        unless ( $force_rebuild
            || !$nqp_have
            || $nqp_ok
            || defined($gen_nqp))
        {
            my $say_sub = $options->{'ignore-errors'} ? 'note' : 'sorry';
            $self->$say_sub(
                "$bin version $nqp_have is outdated, $nqp_want expected.\n" );
        }

        if ( !$force_rebuild and ( $nqp_ok or $options->{'ignore-errors'} ) ) {
            $impls->{$b}{ok} = 1;
        }
        else {
            $need{$b} = 1;
        }
    }

    return unless %need;

    return unless defined($gen_nqp) || defined($gen_moar);

    {
        my $user = $options->{'github-user'} // 'Raku';

        # Don't expect any specific default commit in nqp/ if the repo is
        # already checked out and we're force-rebuilding.
        my $expected_spec =
          ( $force_rebuild
              && -d File::Spec->catdir( $self->cfg('base_dir'), 'nqp', '.git' )
          )
          ? undef
          : $nqp_want;
        $self->git_checkout( 'nqp', 'nqp', $nqp_git_spec || $expected_spec );
    }

    my @cmd = (
        $^X, 'Configure.pl', qq{--prefix=$prefix}, "--make-install",
        "--git-protocol=$git_protocol",
    );

    # Append only the options we'd like to pass down to NQP's Configure.pl
    for my $opt (
        qw<git-depth github-user nqp-repo moar-repo
        nqp-home relocatable ignore-errors with-moar silent-build
        force-rebuild>
      )
    {
        my $opt_str = $self->make_option( $opt, no_quote => 1 );
        push @cmd, $opt_str if $opt_str;
    }

    push @cmd,
      '--git-cache-dir=' . File::Spec->rel2abs( $options->{'git-cache-dir'} )
      if $options->{'git-cache-dir'};

    push @cmd, "--backends=" . join( ",", $self->active_backends );

    if ( defined $gen_moar ) {
        push @cmd, $gen_moar ? "--gen-moar=$gen_moar" : '--gen-moar';

        if (@moar_options) {
            push @cmd, map( sprintf( '--moar-option=%s', $_ ), @moar_options );
        }
    }

    $self->msg("Building NQP ...\n");
    chdir("$startdir/nqp");
    $self->msg("@cmd\n");
    system_or_die(@cmd);
    chdir($pwd);

    return;
}

package NQP::Macros::Rakudo;
use strict;
use warnings;
use File::Find;

# --- Rakudo-specific macro methods.
sub _specs_iterate {
    my $self   = shift;
    my $cb     = shift;
    my %params = @_;

    my $cfg = $self->{config_obj};

    $self->not_in_context( specs => 'spec' );

    my $prev_spec_char;
    for my $spec ( $cfg->raku_specs ) {
        my $spec_char   = $spec->[0];
        $prev_spec_char //= $spec_char; # Map c -> c
        my $spec_subdir = "6.$spec_char";
        my %config      = (
            ctx_subdir    => $spec_subdir,
            spec_subdir   => $spec_subdir,
            spec          => $spec_char,
            spec_with_mod => $spec_char,
            ucspec        => uc $spec_char,
            lcspec        => lc $spec_char,
            prevspec      => $prev_spec_char,
            ucprevspec    => uc $prev_spec_char,
            lcprevspec    => lc $prev_spec_char,
        );
        my $spec_ctx = {
            spec    => $spec,
            configs => [ \%config ],
        };
        my $s = $cfg->push_ctx($spec_ctx);
        $cb->(@_);
        if ( $params{with_mods} && @$spec > 1 ) {
            for my $mod ( @$spec[ 1 .. $#$spec ] ) {
                my %mod = (
                    spec_mod      => $mod->[0],
                    spec_dot_mod  => ".$mod->[0]",
                    spec_with_mod => "$spec_char.$mod->[0]",
                );
                my $mod_s = $cfg->push_ctx(
                    {
                        configs => [ \%mod ],
                    }
                );
                $cb->(@_);
            }
        }
        $prev_spec_char = $spec_char;
    }
}

# --- Utility methods

{
    my @all_sources;

    sub _all_sources {
        my $self     = shift;
        my $base_dir = $self->cfg->cfg('base_dir');
        unless (@all_sources) {
            find(
                sub {
                    push @all_sources,
                      File::Spec->abs2rel(
                        File::Spec->catfile( $File::Find::dir, $_ ), $base_dir )
                      if /\.(nqp|pm6)\z/;
                },
                File::Spec->catdir( $base_dir, "src" )
            );
            push @all_sources, 'gen/nqp-version';
        }
        return @all_sources;
    }
}

# --- Macro implementations

# for_specs(text)
# Iterates over active backends and expands text in the context of each backend.
sub _m_for_specs {
    my $self   = shift;
    my $text   = shift;
    my %params = @_;

    my $out = "";

    my $cb = sub {
        $out .= $self->_expand($text);
    };

    _specs_iterate( $self, $cb, %params );

    return $out;
}

# for_specmods(text)
# Same as for_specs but iterates including spec modifications, i.e. 6.x.PREVIEW
sub _m_for_specmods {
    my $self = shift;
    my $text = shift;
    return _m_for_specs( $self, $text, with_mods => 1 );
}

# for_toolchain(text)
# Iterate over tools
sub _m_for_toolchain {
    my $self = shift;
    my $text = shift;

    my $cfg = $self->cfg;

    $self->not_in_context( toolchain => 'toolchain' );

    my @tools = @{ $cfg->prop('toolchains') || [] };
    my $out   = "";

    for my $tool (@tools) {
        my %config = (
            toolchain   => $tool,
            uctoolchain => uc($tool),
        );
        my $tc_ctx = {
            toolchain => $tool,
            configs   => [ \%config ],
        };
        my $scope = $self->cfg->push_ctx($tc_ctx);
        $out .= $self->expand($text);
    }

    return $out;
}

# for_langalias(text)
# Iterate over perl6 and raku. Temporary, until the end of perl6 deprecation
# period.
sub _m_for_langalias {
    my $self = shift;
    my $text = shift;
    my $cfg  = $self->cfg;
    my $out  = "";

    $self->not_in_context( langalias => 'langalias' );

    for my $alias (qw<rakudo perl6>) {
        my %config = (
            langalias   => $alias,
            uclangalias => uc($alias),
            LangAlias   => ucfirst($alias),
        );
        my $la_ctx = {
            langalias => $alias,
            configs   => [ \%config ],
        };
        my $scope = $self->cfg->push_ctx($la_ctx);
        $out .= $self->expand($text);
    }

    return $out;
}

sub _m_source_digest_files {
    my $self   = shift;
    my $indent = " " x ( $self->cfg->{config}{filelist_indent} || 4 );
    return join( " \\\n$indent", _all_sources($self) );
}

sub _m_source_digest {
    my $self = shift;
    my $sha  = Digest::SHA->new;
    foreach my $src ( _all_sources($self) ) {
        $sha->addfile($src);
    }
    return $sha->hexdigest;
}

sub _m_gencat {
    my $self       = shift;
    my $text       = shift;
    my $all_prereq = $self->cfg->cfg('make_all_prereq');
    return $self->expand(<<TPL);
$text
\t\@echo(+++ Generating\t\$\@)@
\t\$(NOECHO\@nop())@\@bpm(NQP)\@ \@bpm(GEN_CAT)\@ $all_prereq > \$\@
TPL
}

sub _m_comp {
    my $self = shift;
    my $text = shift;
    return $self->expand(<<TPL);
$text
\t\@echo(+++ Compiling\t\$@)@
\t\$(NOECHO\@nop())@\@bpm(NQP)@ \@bpm(NQP_FLAGS)@ --target=\@btarget@ --output=\$@ \@prereqs\@
TPL
}

sub _m_comp_rr {
    my $self = shift;
    my $text = shift;
    return $self->expand(<<TPL);
$text
\t\@echo(+++ Compiling\t\$@)@
\t\$(NOECHO\@nop())@\@bpm(NQP_RR)@ \@bpm(NQP_FLAGS)@ --target=\@btarget@ --output=\$@ \@bpm(NQP_FLAGS_EXTRA)@ \@prereqs\@
TPL
}

NQP::Macros->register_macro( 'for_specs',           \&_m_for_specs );
NQP::Macros->register_macro( 'for_specmods',        \&_m_for_specmods );
NQP::Macros->register_macro( 'for_toolchain',       \&_m_for_toolchain );
NQP::Macros->register_macro( 'for_langalias',       \&_m_for_langalias );
NQP::Macros->register_macro( 'source_digest',       \&_m_source_digest );
NQP::Macros->register_macro( 'source_digest_files', \&_m_source_digest_files );
NQP::Macros->register_macro( 'gencat',              \&_m_gencat );
NQP::Macros->register_macro( 'comp', \&_m_comp, in_receipe => 1, );
NQP::Macros->register_macro( 'comp_rr', \&_m_comp_rr, in_receipe => 1, );

1;

# vim: ft=perl
