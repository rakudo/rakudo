package NQP::Configure;
use strict;
use warnings;
use Cwd;
use File::Copy qw(copy);
use File::Spec qw();

use base qw(Exporter);
our @EXPORT_OK = qw(sorry slurp system_or_die
                    cmp_rev
                    read_config
                    fill_template_file fill_template_text
                    git_checkout
                    verify_install gen_moar
                    github_url
                    gen_nqp );

our $exe = $^O eq 'MSWin32' ? '.exe' : '';
our $bat = $^O eq 'MSWin32' ? '.bat' : '';

our @required_nqp_files = qw(
    @bindir@/nqp-p@exe@
);

sub sorry {
    my @msg = @_;
    die join("\n", '', '===SORRY!===', @msg, "\n");
}

sub slurp {
    my $filename = shift;
    open my $fh, '<', $filename
        or die "Unable to read $filename\n";
    local $/ = undef;
    my $text = <$fh>;
    close $fh or die $!;
    return $text;
}


sub system_or_die {
    my @cmd = @_;
    system( @cmd ) == 0
        or die "Command failed (status $?): @cmd\n";
}


sub parse_revision {
    my $rev = shift;
    my $sep = qr/[_.]/;
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
    if ($rev =~ $rev_regex) {
        return ($+{year}, $+{month}, $+{day} // 0, $+{rcno} // 0, $+{revno} // 0);
    } else {
        die "Unrecognized revision specifier '$rev'\n";
    }
}

sub cmp_rev {
    my ($a, $b) = @_;
    my @a = parse_revision($a);
    my @b = parse_revision($b);
    my $cmp = 0;
    for (0..4) {
        $cmp = $a[$_] <=> $b[$_] if (defined $a[$_] && defined $b[$_]);
        last if $cmp;
    }
    $cmp;
}


sub read_config {
    my @config_src = @_;
    my %config = ();
    local $_;
    for my $file (@config_src) {
        no warnings;
        if (open my $CONFIG, '-|', "\"$file\" --show-config") {
            while (<$CONFIG>) {
                if (/^([^\s=]+)=(.*)/) { $config{$1} = $2 }
            }
            close($CONFIG);
        }
        last if %config;
    }
    return %config;
}

sub fill_template_file {
    my $infile = shift;
    my $outfile = shift;
    my %config = @_;

    my $OUT;
    if (ref $outfile) {
        $OUT = $outfile;
    }
    else {
        print "\nCreating $outfile ...\n";
        open($OUT, '>', $outfile)
            or die "Unable to write $outfile\n";
    }

    my @infiles = ref($infile) ? @$infile : $infile;
    for my $if (@infiles) {
        my $text = slurp( $if );
        print $OUT "\n# Generated from $if\n";
        $text = fill_template_text($text, %config);
        print $OUT $text;
        print $OUT "\n\n# (end of section generated from $if)\n\n";
    }
    unless (ref $outfile) {
        close($OUT) or die "Error while writing '$outfile': $!";
    }
}


sub fill_template_text {
    my $text = shift;
    my %config = @_;

    my $escape = sub {
        my $str = $_[0];
        $str =~ s{ }{\\ }g;
        $str;
    };

    $text =~ s/@@([:\w]+)@@/$escape->($config{$1} || '')/ge;
    $text =~ s/@([:\w]+)@/$config{$1} || ''/ge;
    if ($text =~ /nqp::makefile/) {
        if ($^O eq 'MSWin32') {
            $text =~ s{/}{\\}g;
            $text =~ s{\\\*}{\\\\*}g;
            $text =~ s{(?:git|http):\S+}{ do {my $t = $&; $t =~ s'\\'/'g; $t} }eg;
            $text =~ s/.*curl.*/do {my $t = $&; $t =~ s'%'%%'g; $t}/meg;
        }
        if ($config{'makefile-timing'}) {
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
    }
    $text;
}


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
    my %options  = @_;

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

    for my $b (qw/jvm moar/) {
        if ($backends =~ /$b/) {
            my $postfix = substr $b, 0, 1;
            my $bin = $nqp_bin || ($sdkroot
                ? File::Spec->catfile( $sdkroot, $prefix, 'bin', "nqp-$postfix$bat" )
                : File::Spec->catfile( $prefix, 'bin', "nqp-$postfix$bat" ));
            $impls{$b}{bin} = $bin;
            my %c = read_config($bin);
            my $nqp_have = $c{'nqp::version'} || '';
            $impls{$b}{config} = \%c if %c;
            my $nqp_ok   = $nqp_have && cmp_rev($nqp_have, $nqp_want) >= 0;
            if ($nqp_ok) {
                $impls{$b}{ok} = 1;
            }
            else {
                $need{$b} = 1;
            }
        }
    }

    return %impls unless %need;

    if (defined $gen_nqp || defined $gen_moar) {
        git_checkout(
            github_url($git_protocol, 'perl6', 'nqp'),
            'nqp', $gen_nqp || $nqp_want,
            github_url('ssh', 'perl6', 'nqp'),
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
    my %options  = @_;

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
        github_url($git_protocol, 'MoarVM', 'MoarVM'),
        'MoarVM', $gen_moar || $moar_want,
        github_url('ssh', 'MoarVM', 'MoarVM'),
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

sub github_url {
    my ($protocol, $user, $repo) = @_;
    $protocol = lc $protocol;
    if ($protocol eq 'https' || $protocol eq 'git') {
        return sprintf '%s://github.com/%s/%s.git', $protocol, $user, $repo;
    }
    elsif ($protocol eq 'ssh') {
        return sprintf 'git@github.com:%s/%s.git', $user, $repo;
    }
    else {
        die "Unknown protocol '$protocol' (fine are: ssh, https, git)";
    }
}


1;
