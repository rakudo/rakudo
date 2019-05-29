## Please see file perltidy.ERR
## Please see file perltidy.ERR
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
        $self->sorry( "Could not find nqp '"
              . $self->opt('with-nqp')
              . "' defined with --with-nqp" )
          unless -x $nqp_bin;
        my $nqp_backend;
        run(
            command =>
              [ $nqp_bin, '-e', 'print(nqp::getcomp("nqp").backend.name)' ],
            buffer => \$nqp_backend
        ) or self->sorry("Could not get backend information from '$nqp_bin'");
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
        elsif ( abs_path($prefix) ne abs_path($nqp_prefix) ) {
            $self->note(
                'WARNING!',
                "Installation directory '$prefix' is different from '",
                abs_path($nqp_prefix),
                "' where nqp is installed.\n",
                "This may result in non-functional perl6 binary.\n",
                "Make sure this is REALLY intended."
            );
        }
    }

    $self->SUPER::configure_refine_vars(@_);
}

sub configure_misc {
    my $self   = shift;
    my $config = $self->{config};

    # determine the version of NQP we want
    ( $config->{nqp_want} ) =
      split( ' ',
        slurp( $self->template_file_path( 'NQP_REVISION', required => 1, ) ) );

    # Get specs from PERL6_SPECS template
    my $spec_line = sub {
        my @elems = split ' ', shift;
        if ( $elems[0] =~ s/^\*// ) {
            $config->{lang_spec} = $elems[0];
        }
        return \@elems;
    };

    $config->{perl6_specs} = [
        map { $spec_line->($_) }
          grep { s/\s*#.*$//; length }
          split(
            /\n/s,
            slurp( $self->template_file_path( 'PERL6_SPECS', required => 1, ) )
          )
    ];

    # Get version info from VERSION template and git.
    my $VERSION = slurp( $self->template_file_path( 'VERSION', required => 1, ) );
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
    $config->{ldflags} = $nqp_config->{'moar::ldflags'};
    $config->{ldflags} =~ s/\Q$nqp_config->{'moar::ldrpath'}\E ?//;
    $config->{ldflags} =~ s/\Q$nqp_config->{'moar::ldrpath_relocatable'}\E ?//;
    $config->{ldflags} .= ' '
      . (
          $self->{no_relocatable}
        ? $nqp_config->{'moar::ldrpath'}
        : $nqp_config->{'moar::ldrpath_relocatable'}
      );
    $config->{ldlibs}          = $nqp_config->{'moar::ldlibs'};
    $config->{'mingw_unicode'} = '';

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
              . q<$(CP) @nfpq(@moar::libdir@/@moar::moar@) @nfpq($(DESTDIR)$(PREFIX)/bin)@>;
        }
        if ( $nqp_config->{'moar::os'} eq 'mingw32' ) {
            $config->{'mingw_unicode'} = '-municode';
        }
        push @c_runner_libs, sprintf( $nqp_config->{'moar::ldusr'}, 'Shlwapi' );
    }
    else {
        $config->{'m_cleanups'} =
"  \$(M_GDB_RUNNER) \\\n  \$(M_LLDB_RUNNER) \\\n  \$(M_VALGRIND_RUNNER)";
        $config->{'m_all'} =
          '$(M_GDB_RUNNER) $(M_LLDB_RUNNER) $(M_VALGRIND_RUNNER)';

        #$config->{'m_install'} = "\t"
        #  . '$(M_RUN_PERL6) '
        #  . $self->nfp("tools/build/create-moar-runner.p6")
        #  . ' perl6 $(M_RUNNER) $(DESTDIR)$(PREFIX)'
        #  . $self->nfp("/bin/perl6-gdb-m")
        #  . ' "gdb" "" "" ""' . "\n\t"
        #  . '$(M_RUN_PERL6) '
        #  . $self->nfp("tools/build/create-moar-runner.p6")
        #  . ' perl6 $(M_RUNNER) $(DESTDIR)$(PREFIX)'
        #  . $self->nfp("/bin/perl6-lldb-m")
        #  . ' "lldb" "" "" ""' . "\n\t"
        #  . '$(M_RUN_PERL6) '
        #  . $self->nfp("tools/build/create-moar-runner.p6")
        #  . ' perl6 $(M_RUNNER) $(DESTDIR)$(PREFIX)'
        #  . $self->nfp("/bin/perl6-valgrind-m")
        #  . ' "valgrind" "" "" ""';
        $config->{m_install} = '@insert(Makefile-install)@';
    }
    $config->{c_runner_libs} = join( " ", @c_runner_libs );
    $config->{moar_lib}      = sprintf(
        (
              $nqp_config->{'moar::ldimp'}
            ? $nqp_config->{'moar::ldimp'}
            : $nqp_config->{'moar::ldusr'}
        ),
        $nqp_config->{'moar::name'}
    );

    unless ( $self->backend_error('moar') ) {
        $self->msg( "Using $config->{'m_nqp'}"
              . " (version $nqp_config->{'nqp::version'}"
              . " / MoarVM $nqp_config->{'moar::version'}).\n" );

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
      File::Spec->rel2abs( $self->nfp('src/vm/js/perl6-runtime') );
    $config->{'perl6_lowlevel_libs'} =
      File::Spec->rel2abs('node_modules') . $slash;
    $config->{'perl6_js_runner'} =
      $self->batch_file( File::Spec->rel2abs('perl6-js') );

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
    );
}

sub opts_for_configure {
    my $self = shift;
    my @opts = $self->SUPER::opts_for_configure(@_);

    my $nqp_bin = $self->cfg('nqp_bin');
    push @opts, "--with-nqp=" . $self->cfg('nqp_bin') if $nqp_bin;

    return wantarray ? @opts : join( " ", @opts );
}

# Returns all active language specification entries except for .c
sub perl6_specs {
    my $self = shift;
    return grep { $_->[0] ne 'c' } @{ $self->cfg('perl6_specs') };
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
    my $nqp_git_spec = $options->{'gen-nqp'};
    my $gen_nqp      = defined $options->{'gen-nqp'};
    my $gen_moar     = $options->{'gen-moar'};
    my $prefix       = $config->{prefix};
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
        my $bconfig = $self->backend_config($b);
        my $postfix = $self->backend_abbr($b);
        my $tpath   = File::Spec->catfile( $prefix, 'bin', "nqp-$postfix$bat" );
        my $bin     = $bconfig->{nqp_bin}
          || (
            $sdkroot
            ? File::Spec->catfile( $self->nfp($sdkroot),
                $prefix, 'bin', "nqp-$postfix$bat" )
            : File::Spec->catfile( $prefix, 'bin', "nqp-$postfix$bat" )
          );
        $impls->{$b}{bin} = $bin;
        my %c        = read_config($bin);
        my $nqp_have = $c{'nqp::version'} || '';
        $self->backend_config( $b, \%c ) if %c;
        my $nqp_ok = $nqp_have && 0 <= cmp_rev( $nqp_have, $nqp_want );

        if ( $nqp_ok or $options->{'ignore-errors'} ) {
            $impls->{$b}{ok} = 1;
        }
        elsif ( $self->opt('with-nqp') ) {
            $self->sorry(
                "$bin version $nqp_have is outdated, $nqp_want expected.");
        }
        else {
            $need{$b} = 1;
        }
    }

    return unless %need;

    return unless defined($gen_nqp) || defined($gen_moar);

    if ( defined $gen_nqp || defined $gen_moar ) {
        my $user = $options->{'github-user'} // 'perl6';
        $self->git_checkout( 'nqp', 'nqp', $nqp_git_spec || $nqp_want );
    }

    my @cmd = (
        $^X, 'Configure.pl', qq{--prefix=$prefix}, "--make-install",
        "--git-protocol=$git_protocol",
    );

    # Append only the options we'd like to pass down to NQP's Configure.pl
    for my $opt (
        qw<git-depth git-reference github-user nqp-repo moar-repo
        no-relocatable ignore-errors with-moar>
      )
    {
        my $opt_str = $self->make_option( $opt, no_quote => 1 );
        push @cmd, $opt_str if $opt_str;
    }

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

    for my $k ( keys %need ) {
        my %c = read_config( $impls->{$k}{bin} );
        $self->backend_config( $k, \%c );
        $impls->{$k}{ok} = 1;
    }
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

    for my $spec ( $cfg->perl6_specs ) {
        my $spec_char   = $spec->[0];
        my $spec_subdir = "6.$spec_char";
        my %config      = (
            ctx_subdir    => $spec_subdir,
            spec_subdir   => $spec_subdir,
            spec          => $spec_char,
            spec_with_mod => $spec_char,
            ucspec        => uc $spec_char,
            lcspec        => lc $spec_char,
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
                    spec_mod      => $mod,
                    spec_dot_mod  => ".$mod",
                    spec_with_mod => "$spec_char.$mod",
                );
                my $mod_s = $cfg->push_ctx(
                    {
                        configs => [ \%mod ],
                    }
                );
                $cb->(@_);
            }
        }
    }
}

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

sub _m_source_digest {
    my $self = shift;
    my $sha  = Digest::SHA->new;
    find(
        sub {
            $sha->addfile($_) if /\.(nqp|pm6)\z/;
        },
        File::Spec->catdir( $self->cfg->cfg('base_dir'), "src" )
    );
    $sha->addfile('gen/nqp-version');
    return $sha->hexdigest;
}

NQP::Macros->register_macro( 'for_specs',     \&_m_for_specs );
NQP::Macros->register_macro( 'for_specmods',  \&_m_for_specmods );
NQP::Macros->register_macro( 'source_digest', \&_m_source_digest );

1;

# vim: ft=perl
