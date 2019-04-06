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
use File::Spec::Win32;
use File::Spec::Unix;
use File::Basename;
use FindBin;
use Data::Dumper;
use NQP::Macros;
use IPC::Cmd qw<can_run>;
use Cwd;
use Carp;

$SIG{__DIE__} = sub { confess @_ };

use base qw<Exporter>;
our @EXPORT_OK = qw<
  nfp slash slurp system_or_die cmp_rev read_config
>;

sub init {
    my $self   = shift;
    my %params = @_;

    for my $rp (qw<lang>) {
        die "Missing required NQP::Config constructor parameter $rp"
          unless defined $params{$rp};
    }

    $self->{config} = {
        perl      => $^X,
        slash     => slash(),
        shell     => $^O eq 'solaris' ? '' : "SHELL = " . $self->shell_cmd,
        base_dir  => $FindBin::Bin,
        build_dir => File::Spec->catdir( $FindBin::Bin, 'tools', 'build' ),
        templates_dir =>
          File::Spec->catdir( $FindBin::Bin, 'tools', 'templates' ),

        # Number of spaces to indent filelists in a makefile
        filelist_indent => 4,
        lang            => $params{lang},
        lclang          => lc $params{lang},
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
        moar   => [qw<MoarVM MoarVM>],
        roast  => [qw<perl6 roast>],
    };

    $self->{impls} = {};
    for my $be ( @{ $self->{backends_order} } ) {
        $self->backend_config( $be, {} );
    }

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

    my $config = $self->{config};

    my $make = 'make';
    if ( $self->is_solaris ) {
        $make = can_run('gmake');
        unless ($make) {
            die
"gmake is required to compile rakudo. Please install by 'pkg install gnu-make'";
        }
        $make = 'gmake';
    }

    if ( $self->is_win ) {
        my $prefix    = $config->{prefix};
        my $has_nmake = 0 == system('nmake /? >NUL 2>&1');
        my $has_cl    = can_run('cl') && `cl 2>&1` =~ /Microsoft Corporation/;
        my $has_gmake = 0 == system('gmake --version >NUL 2>&1');
        my $has_gcc   = 0 == system('gcc --version >NUL 2>&1');
        if (
            -x "$prefix\\bin\\nqp-m.bat"
            && ( $_ =
                `$prefix\\bin\\nqp-m.bat -e "print(nqp::backendconfig()<make>)"`
            )
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
    my ( $self, $backend ) = ( shift, shift );
    if (@_) {
        my %config;
        if ( @_ == 1 && ref( $_[0] ) eq 'HASH' ) {
            %config = %{ $_[0] };
        }
        elsif ( @_ % 2 == 0 ) {
            %config = @_;
        }
        else {
            die "Bad configuration hash passed in to backend_config";
        }
        @{ $self->{impls}{$backend}{config} }{ keys %config } = values %config;
    }
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

# Takes a relative path
sub base_path {
    my $self     = shift;
    my @rel_path = @_;

    if ( @rel_path == 1 ) {
        return $rel_path[0]
          if File::Spec->file_name_is_absolute( $rel_path[0] );
        @rel_path = File::Spec->splitdir( $rel_path[0] );
    }

    return File::Spec->catfile( $self->{config}{base_dir}, @rel_path );
}

sub configure_jars {
    my $self    = shift;
    my $config  = $self->{config};
    my $options = $self->{options};
    if ( $options->{'with-asm'} ) {
        if ( $options->{'with-asm'} ne '-' ) {
            $config->{'asm'} = $options->{'with-asm'};
        }
    }
    else {
        $config->{'asm'} = $self->base_path(qw<3rdparty asm asm-4.1.jar>);
    }
    if ( $options->{'with-asm-tree'} ) {
        if ( $options->{'with-asm-tree'} ne '-' ) {
            $config->{'asmtree'} = $options->{'with-asm-tree'};
        }
    }
    else {
        $config->{'asmtree'} =
          $self->base_path(qw<3rdparty asm asm-tree-4.1.jar>);
    }
    if ( $options->{'with-jline'} ) {
        if ( $options->{'with-jline'} ne '-' ) {
            $config->{'jline'} = $options->{'with-jline'};
        }
    }
    else {
        $config->{'jline'} = $self->base_path("3rdparty/jline/jline-1.0.jar");
    }
    if ( $options->{'with-jna'} ) {
        if ( $options->{'with-jna'} ne '-' ) {
            $config->{'jna'} = $options->{'with-jna'};
        }
    }
    else {
        $config->{'jna'} = $self->base_path("3rdparty/jna/jna-4.0.0.jar");
    }

    $config->{asmfile}   = ( File::Spec->splitpath( $config->{asm} ) )[-1];
    $config->{jlinefile} = ( File::Spec->splitpath( $config->{jline} ) )[-1];
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

    $config->{make} = $self->make_cmd;

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

sub abstract {
    my @c = caller(1);
    die "Method $c[3] must be implemented by the language class";
}

sub configure_backends {
    abstract;
}

sub configure_misc {
    abstract;
}

sub configure_prefix {
    my $self   = shift;
    my $config = $self->{config};
    $config->{prefix} //= $self->option('prefix');
    unless ( $config->{prefix} ) {

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

sub parse_backends {
    my $self            = shift;
    my $passed_backends = shift;
    return uc($passed_backends) eq 'ALL'
      ? $self->known_backends
      : map { lc } split /,\s*/, $passed_backends;
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

    for my $b ( $self->active_backends ) {
        $self->{backend_errors}{$b} = [];
        my $method = "configure_${b}_backend";
        $self->$method();
    }

    $self->post_active_backends;
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

sub save_config_status {
    my $self   = shift;
    my $config = $self->{config};

    # Save options in config.status
    my $status_file = $self->base_path('config.status');
    unlink($status_file);
    if ( open( my $CONFIG_STATUS, '>', $status_file ) ) {
        my $ckey = $config->{lclang} . "_config_status";
        print $CONFIG_STATUS "$^X Configure.pl $config->{$ckey} \$*\n";
        close($CONFIG_STATUS);
    }
    else {
        warn "Can't write to $status_file: $!";
    }
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
    my $config   = $self->{config};

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
        $self->sorry("$msg$src");
    }

    my $text_out =
      NQP::Macros->new( config => $self, on_fail => \&on_fail )->expand($text);

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
        print "Cloning from $pullurl\n";
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
    my $class  = shift;
    my %params = @_;
    if ( $class eq __PACKAGE__ ) {
        if ( $params{lang} ) {
            $class .= "::$params{lang}";
            eval "require $class";
            if ($@) {
                die "===SORRY!=== Can't create an instance of "
                  . $class . ":\n"
                  . $@;
            }
        }
        else {
            die "Can't create instance of class " . $class
              . ", use a language sub-class instead";
        }
    }
    my $self = bless {}, $class;
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
