# The Distro class and its methods, underlying $*DISTRO, are a work in progress.
# It is very hard to capture data about a changing universe in a stable API.
# If you find errors for your hardware or OS distribution, please report them
# with the values that you expected and how to get them in your situation.

class Distro does Systemic {
    has Str $.release;
    has Bool $.is-win;
    has Str $.path-sep;

    submethod BUILD (
      :$name,
      :$version,
      :$!release,
      :$!auth,
      :$!path-sep,
      :$!signature  = Blob,
      :$!desc = Str
      --> Nil
    ) {
        $!name = $name.lc;    # lowercase
        $!name ~~ s:g/" "//;  # spaceless
        $!version = Version.new($version);
        $!is-win  = so $!name eq any <mswin32 mingw msys cygwin>;
    }

    # This is a temporary migration method needed for installation
    method cur-sep() { "," }
}

sub INITIALIZE-A-DISTRO-NOW() {
#?if jvm
    my $properties := INITIALIZE-A-VM-NOW.properties;
    my $name       := $properties<os.name>;
    my $version    := $properties<os.version>;
    my $path-sep   := $properties<path.separator>;
#?endif
#?if !jvm
    my $config   := INITIALIZE-A-VM-NOW.config;
    my $name     := $config<osname>;
    my $version  := $config<osvers>;
    my $path-sep := $name eq 'MSWin32' ?? ';' !! ':';
#?endif
    my Str $release := "unknown";
    my Str $auth    := "unknown";
    my Str $desc    := "unknown";

    # darwin specific info
    if $name eq 'darwin' {
        if qx/sw_vers/ ~~ m/^
        ProductName\: \s+ (<[\w\ ]>+) \s+
        ProductVersion\: \s+ (<[\d\.]>+) \s+
        BuildVersion\: \s+ (<[\w\d]>+)
        / {
            $name    := ~$0;
            $version := ~$1;
            $release := ~$2;
        }
        else {
            $name    := 'Mac OS X'; # we assume
            $version := "unknown";
            $release := qx/uname -r/.chomp;
        }
        $auth := 'Apple Computer, Inc.'; # presumably
    }
    elsif Rakudo::Internals.FILETEST-E('/etc/os-release') {
        $_ := '/etc/os-release'.IO.slurp.subst(:g, '"','');
        $auth    := ~$0 if m/^^ HOME_URL    \= (\N*) /;
        $name    := ~$0 if m/^^ ID          \= (\N*) /;
        $desc    := ~$0 if m/^^ PRETTY_NAME \= (\N*) /;
        $version := ~$0 if m/^^ VERSION     \= (\N*) /;
        $release := ~$0 if m/^^ VERSION_ID  \= (\N*) /;
    }
    elsif $name eq 'linux' {
        if try qx{lsb_release -a 2> /dev/null} ~~ m/
            Distributor \s+ ID\: \s+ (<[\w\ ]>+) \s+
            Description\: \s+ ((<[\w\ ]>+) \s+ (<[\w\.]>+)) \s+
            Release\: \s+ (<[\d\.]>+)
        / {
            $auth    := ~$0;
            $desc    := ~$1;
            $name    := ~$2;
            $version := ~$3;
            $release := ~$4;
        }
    }
    Distro.new(:$name, :$version, :$release, :$auth, :$path-sep, :$desc);
}

# set up $*DISTRO
Rakudo::Internals.REGISTER-DYNAMIC: '$*DISTRO', {
    PROCESS::<$DISTRO> := INITIALIZE-A-DISTRO-NOW();
}

# vim: ft=perl6 expandtab sw=4
