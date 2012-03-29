package NQP::Configure;
use strict;
use warnings;
use Cwd;

use base qw(Exporter);
our @EXPORT_OK = qw(sorry slurp system_or_die
                    cmp_rev 
                    read_parrot_config read_config
                    fill_template_file fill_template_text 
                    git_checkout
                    verify_install
                    gen_nqp gen_parrot);

our $exe = $^O eq 'MSWin32' ? '.exe' : '';

our @required_parrot_files = qw(
    @bindir@/parrot@exe@
    @bindir@/pbc_to_exe@exe@
    @bindir@/ops2c@exe@
    @libdir@@versiondir@/tools/build/pmc2c.pl
    @srcdir@@versiondir@/pmc
    @includedir@@versiondir@/pmc
);

our @required_nqp_files = qw(
    @bindir@/nqp@exe@
);

our $nqp_git = 'git://github.com/perl6/nqp.git';
our $par_git = 'git://github.com/parrot/parrot.git';

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
    $rev =~ /(\d+)$sep(\d+)(?:$sep(\d+))?(?:-(\d+)-g[a-f0-9]*)?$/
        or die "Unrecognized revision specifier '$rev'\n";
    return ($1, $2, $3 || 0, $4 || 0);
}


sub cmp_rev {
    my ($a, $b) = @_;
    my @a = parse_revision($a);
    my @b = parse_revision($b);
    my $cmp = 0;
    for (0..3) {
        $cmp = $a[$_] <=> $b[$_];
        last if $cmp;
    }
    $cmp;
}


sub read_config {
    my @config_src = @_;
    my %config = ();
    for my $file (@config_src) {
        no warnings;
        if (open my $CONFIG, '-|', "$file --show-config") {
            while (<$CONFIG>) {
                if (/^([\w:]+)=(.*)/) { $config{$1} = $2 }
            }
            close($CONFIG);
        }
        last if %config;
    }
    return %config;
}


sub read_parrot_config {
    my @parrot_config_src = @_;
    my %config = ();
    open my $CONFIG_PIR, '>', 'parrot-config.pir'
      or die "Unable to write parrot-config.pir\n";
    print $CONFIG_PIR <<'END';
        .include 'iglobals.pasm'
        .sub "main" :main
            .local pmc interp, config_hash, config_iter
            interp = getinterp
            config_hash = interp[.IGLOBALS_CONFIG_HASH]
            config_iter = iter config_hash
          config_loop:
            unless config_iter goto config_done
            $P0 = shift config_iter
            print "parrot::"
            $S0 = $P0.'key'()
            print $S0
            print "="
            $S0 = $P0.'value'()
            print $S0
            print "\n"
            goto config_loop
          config_done:
            .return ()
        .end
END
    close($CONFIG_PIR);
    
    for my $file (@parrot_config_src) {
        no warnings;
        if ($file =~ /.pir$/ && open my $PARROT_CONFIG, '<', $file) {
            while (<$PARROT_CONFIG>) {
                if (/P0\["(.*?)"\], "(.*?)"/) { $config{"parrot::$1"} = $2 }
            }
            close($PARROT_CONFIG) or die $!;
        }
        elsif (open my $PARROT, '-|', "$file parrot-config.pir") {
            while (<$PARROT>) {
                if (/^([\w:]+)=(.*)/) { $config{$1} = $2 }
            }
            close($PARROT);
        }
        last if %config;
    }
    unlink('parrot-config.pir');
    return %config;
}


sub fill_template_file {
    my $infile = shift;
    my $outfile = shift;
    my %config = @_;
    my $text = slurp( $infile );
    $text = fill_template_text($text, %config);
    print "\nCreating $outfile ...\n";
    open(my $OUT, '>', $outfile)
        or die "Unable to write $outfile\n";
    print $OUT $text;
    close($OUT) or die $!;
}

sub lookup_config {
    my ($key, $config) = @_;
    return $config->{$key} if defined $config->{$key};
    return $config->{"parrot::$key"} if defined $config->{"parrot::$key"};
    return '';
}

sub fill_template_text {
    my $text = shift;
    my %config = @_;

    $text =~ s/@([:\w]+)@/lookup_config("$1", \%config)/ge;
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
    my $pwd = cwd();

    # get an up-to-date repository
    if (! -d $dir) {
        system_or_die('git', 'clone', $repo, $dir);
        chdir($dir);
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

    my $gen_nqp     = $options{'gen-nqp'};
    my $with_parrot = $options{'with-parrot'};
    my $gen_parrot  = $options{'gen-parrot'};
    my $prefix      = $options{'prefix'} || cwd().'/install';
    my $startdir    = cwd();

    my $PARROT_REVISION = 'nqp/tools/build/PARROT_REVISION';

    my %config;
    my $nqp_exe;
    if ($with_parrot) {
        %config = read_parrot_config($with_parrot)
            or die "Unable to read parrot configuration from $with_parrot\n";
        $prefix  = $config{'parrot::prefix'};
        $nqp_exe = fill_template_text('@bindir@/nqp@ext@', %config);
        %config = read_config($nqp_exe);
    }
    elsif ($prefix) {
        $nqp_exe = "$prefix/bin/nqp$exe";
        %config = read_config($nqp_exe);
    }

    my $nqp_have = $config{'nqp::version'} || '';
    my $nqp_ok   = $nqp_have && cmp_rev($nqp_have, $nqp_want) >= 0;
    if ($gen_nqp) {
        my $nqp_repo = git_checkout($nqp_git, 'nqp', $gen_nqp);
        $nqp_ok = $nqp_have eq $nqp_repo;
    }
    elsif (!$nqp_ok || defined $gen_parrot && !-f $PARROT_REVISION) {
        git_checkout($nqp_git, 'nqp', $nqp_want);
    }

    if (defined $gen_parrot) {
        my ($par_want) = split(' ', slurp($PARROT_REVISION));
        $with_parrot = gen_parrot($par_want, %options, prefix => $prefix);
        %config = read_parrot_config($with_parrot);
    }
    elsif (!%config) {
        %config = read_parrot_config("$prefix/bin/parrot$exe", "parrot$exe");
        $with_parrot = fill_template_text('@bindir@/parrot@exe@', %config);
    }

    if ($nqp_ok && -M $nqp_exe < -M $with_parrot) {
        print "$nqp_exe is NQP $nqp_have.\n";
        return $nqp_exe;
    }

    my @cmd = ($^X, 'Configure.pl', "--with-parrot=$with_parrot",
               "--make-install");
    print "Building NQP ...\n";
    chdir("$startdir/nqp");
    print "@cmd\n";
    system_or_die(@cmd);
    chdir($startdir);
    return fill_template_text('@bindir@/nqp@exe@', %config);
}


sub gen_parrot {
    my $par_want = shift;
    my %options  = @_;

    my $prefix     = $options{'prefix'} || cwd()."/install";
    my $gen_parrot = $options{'gen-parrot'};
    my @opts       = @{ $options{'parrot-option'} || [] };
    push @opts, "--optimize";
    my $startdir   = cwd();

    my $par_exe  = "$options{'prefix'}/bin/parrot$exe";
    my %config   = read_parrot_config($par_exe);

    my $par_have = $config{'parrot::git_describe'} || '';
    my $par_ok   = $par_have && cmp_rev($par_have, $par_want) >= 0;
    if ($gen_parrot) {
        my $par_repo = git_checkout($par_git, 'parrot', $gen_parrot);
        $par_ok = $par_have eq $par_repo;
    }
    elsif (!$par_ok) {
        git_checkout($par_git, 'parrot', $par_want);
    }

    if ($par_ok) {
        print "$par_exe is Parrot $par_have.\n";
        return $par_exe;
    }
    chdir("$startdir/parrot") or die $!;
    if (-f 'Makefile') {
        %config = read_parrot_config('config_lib.pir');
        my $make = $config{'parrot::make'};
        if ($make) {
            print "\nPerforming '$make realclean' ...\n";
            system_or_die($make, 'realclean');
        }
    }

    $prefix =~ s{\\}{/}g;

    print "\nConfiguring Parrot ...\n";
    my @cmd = ($^X, "Configure.pl", @opts, "--prefix=$prefix");
    print "@cmd\n";
    system_or_die(@cmd);

    print "\nBuilding Parrot ...\n";
    %config = read_parrot_config('config_lib.pir');
    my $make = $config{'parrot::make'} or
        die "Unable to determine value for 'make' from parrot config\n";
    system_or_die($make, 'install-dev', @{$options{'parrot-make-option'}});
    chdir($startdir);

    print "Parrot installed.\n";
    return fill_template_text('@bindir@/parrot@exe@', %config);
}


1;
