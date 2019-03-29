use v5.10.1;
package NQP::Configure;
use strict;
use warnings;
use Cwd;
use File::Copy qw(copy);
use File::Spec qw();
use NQP::Configure::Macros;

use base qw(Exporter);
our @EXPORT_OK = qw(sorry slurp system_or_die
                    cmp_rev
                    read_config
                    fill_template_file fill_template_text
                    git_checkout
                    verify_install gen_moar
                    github_url repo_url
                    gen_nqp );

our $exe = $^O eq 'MSWin32' ? '.exe' : '';
our $bat = $^O eq 'MSWin32' ? '.bat' : '';

our @required_nqp_files = qw(
    @bindir@/nqp-p@exe@
);


sub git_checkout {
    my $repo = shift;
    my $dir  = shift;
    my $checkout = shift;
    my $pushurl = shift;
    my $depth = shift;
    my $reference = shift;
    my $pwd = cwd();

    # get an up-to-date repository
    if (! -d $dir) {
        my @args = ('git', 'clone');
        push @args, $reference if $reference ne '';
        push @args, $depth if $depth ne '';
        push @args, $repo;
        push @args, $dir;
        system_or_die(@args);
        chdir($dir);
        system('git', 'config', 'remote.origin.pushurl', $pushurl)
            if defined $pushurl && $pushurl ne $repo;
    }
    else {
        chdir($dir);
        system_or_die('git', 'fetch');
        # pre-git 1.9/2.0 `--tags` did not fetch tags in addition to normal
        # fetch https://stackoverflow.com/a/20608181/2410502 so do it separately
        system_or_die('git', 'fetch', '--tags');
    }

    if ($checkout) {
        system_or_die('git', 'checkout', $checkout);
        system_or_die('git', 'pull')
            if slurp('.git/HEAD') =~ /^ref:/;
    }

    my $git_describe;
    if (open(my $GIT, '-|', "git describe --tags")) {
        $git_describe = <$GIT>;
        close($GIT);
        chomp $git_describe;
    }
    chdir($pwd);
    $git_describe;
}


sub verify_install {
    my $files = shift;
    my %config = @_;
    print "Verifying installation ...\n";
    my @missing;
    for my $reqfile ( @{$files} ) {
        my $f = fill_template_text($reqfile, %config);
        push @missing, "    $f" unless -e $f;
    }
    if (@missing) {
        unshift @missing, "I'm missing some needed files:";
    }
    @missing;
}


sub gen_nqp {
    my $nqp_want = shift;
    my %params = @_;
    my %options  = %{$params{options}};
    my $config = $params{config};

    my $backends = $options{'backends'};
    my $nqp_bin  = $options{'with-nqp'};
    my $gen_nqp     = $options{'gen-nqp'};
    my $gen_moar    = $options{'gen-moar'};
    my $prefix      = $options{'prefix'} || cwd().'/install';
    my $sdkroot     = $options{'sdkroot'} || '';
    my $startdir    = cwd();
    my $git_protocol = $options{'git-protocol'} // 'https';
    my @moar_options = @{ $options{'moar-option'} // [] };

    my (%impls, %need);

    for my $b (qw/jvm moar js/) {
        if ($backends =~ /$b/) {
            my %postfixes = (jvm => 'j', moar => 'm', js  => 'js');
            my $postfix = $postfixes{$b};
            my $bin = $nqp_bin || ($sdkroot
                ? File::Spec->catfile( $sdkroot, $prefix, 'bin', "nqp-$postfix$bat" )
                : File::Spec->catfile( $prefix, 'bin', "nqp-$postfix$bat" ));
            $impls{$b}{bin} = $bin;
            my %c = read_config($bin);
            my $nqp_have = $c{'nqp::version'} || '';
            $impls{$b}{config} = \%c if %c;
            my $nqp_ok   = $nqp_have && 0 <= cmp_rev($nqp_have, $nqp_want);
            if ($nqp_ok or $options{'ignore-errors'}) {
                $impls{$b}{ok} = 1;
            }
            else {
                $need{$b} = 1;
            }
        }
    }

    return %impls unless %need;

    if (defined $gen_nqp || defined $gen_moar) {
        my $user = $options{'github-user'} // 'perl6';
        git_checkout(
            repo_url('nqp', config => $config, action => 'pull', ),
            'nqp', $gen_nqp || $nqp_want,
            repo_url('nqp', config => $config, action => 'push', ),
            $options{'git-depth'} ? "--depth=$options{'git-depth'}" : '',
            $options{'git-reference'} ? "--reference=$options{'git-reference'}/nqp" : '',
        );
    }

    return %impls unless defined($gen_nqp) || defined($gen_moar);

    my $backends_to_build = join ',', sort keys %need;
    my @cmd = ($^X, 'Configure.pl', "--prefix=$prefix",
               "--backends=$backends", "--make-install",
               "--git-protocol=$git_protocol",
              );
    push @cmd, "--git-depth=" . $options{'git-depth'} if $options{'git-depth'};
    push @cmd, "--git-reference=" . $options{'git-reference'} if $options{'git-reference'};


    if (defined $gen_moar) {
        push @cmd, $gen_moar ? "--gen-moar=$gen_moar" : '--gen-moar';

        if (@moar_options) {
            push @cmd, map(sprintf('--moar-option=%s', $_), @moar_options);
        }
    }

    print "Building NQP ...\n";
    chdir("$startdir/nqp");
    print "@cmd\n";
    system_or_die(@cmd);
    chdir($startdir);

    for my $k (keys %need) {
        my %c = read_config($impls{$k}{bin});
        %c = (%{ $impls{$k}{config} || {} }, %c);
        $impls{$k}{config} = \%c;
        $impls{$k}{ok} = 1;
    }
    return %impls;
}

sub gen_moar {
    my $moar_want = shift;
    my %params = @_;
    my %options  = %{$params{options}};
    my $config = $params{config};

    my $prefix     = $options{'prefix'} || cwd()."/install";
    my $sdkroot    = $options{'sdkroot'} || '';
    my $gen_moar   = $options{'gen-moar'};
    my @opts       = @{ $options{'moar-option'} || [] };
    push @opts, "--optimize";
    my $startdir   = cwd();
    my $git_protocol = $options{'git-protocol'} || 'https';

    my $moar_exe   = $sdkroot
        ? File::Spec->catfile( $sdkroot, $prefix, 'bin', "moar$exe" )
        : File::Spec->catfile( $prefix, 'bin', "moar$exe" );
    my $moar_have  = qx{ $moar_exe --version };
    if ($moar_have) {
        $moar_have = $moar_have =~ /version (\S+)/ ? $1 : undef;
    }

    my $moar_ok   = $moar_have && cmp_rev($moar_have, $moar_want) >= 0;
    if ($moar_ok) {
        print "Found $moar_exe version $moar_have, which is new enough.\n";
        return $moar_exe;
    }
    elsif ($moar_have) {
        print "Found $moar_exe version $moar_have, which is too old. Wanted at least $moar_want\n";
    }

    return unless defined $gen_moar;

    my $moar_repo = git_checkout(
        repo_url('moar', config => $config, action => 'pull', ),
        'MoarVM', $gen_moar || $moar_want,
        repo_url('moar', config => $config, action => 'push', ),
        $options{'git-depth'} ? "--depth=$options{'git-depth'}" : '',
        $options{'git-reference'} ? "--reference=$options{'git-reference'}/MoarVM" : '',
    );

    unless (cmp_rev($moar_repo, $moar_want) >= 0) {
        die "You asked me to build $gen_moar, but $moar_repo is not new enough to satisfy version $moar_want\n";
    }

    chdir("$startdir/MoarVM") or die $!;

    $prefix =~ s{\\}{/}g;
    print "\nConfiguring and building MoarVM ...\n";
    my @cmd = ($^X, "Configure.pl", @opts, "--prefix=$prefix", '--make-install');
    print "@cmd\n";
    system_or_die(@cmd);

    chdir($startdir);

    return $moar_exe;
}

1;
