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
      :$!desc = Str,
    ) {
        $!name = TRANSPOSE-ONE($name.lc," ",""); # lowercase and spaceless
        $!version = Version.new($version);
        $!is-win  = so $!name eq any <mswin32 mingw msys cygwin>;
    }

    method release {
        $!release //= do {
            given $*DISTRO.name {
                when any <linux macosx freebsd openbsd netbsd> { # needs adapting
                    qx/uname -r/.chomp;
                }
                default {
                    "unknown";
                }
            }
        }
    }

    method !first-rwx-dir(\paths) {
        my $io;
        for paths.grep(*.defined) -> $path {
            $io := $path.IO;
            return $io if $io.d && $io.rwx;
        }

        # alas, nothing worked, use current dir
        ".".IO;
    }

    method tmpdir() {
        my %ENV := %*ENV;
        self!first-rwx-dir( self.is-win
          ?? <<
 %ENV<TMPDIR> %ENV<TEMP> %ENV<TMP> SYS:/temp C:/system/temp C:/temp /tmp /
             >>
          !! <<
 %ENV<TMPDIR> /tmp
             >>
        );
    }

    method homedir() {
        my %ENV  := %*ENV;
        my $USER := $*USER.Str;
        self!first-rwx-dir( self.is-win
          ?? << %ENV<HOME> %ENV<HOMEDIR> >>
          !! << %ENV<HOME> "/home/$USER" "/Users/$USER" >>
        );
    }
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
    elsif '/etc/os-release'.IO.e {
        $_ := '/etc/os-release'.IO.slurp.subst(:g, /'"'/,'');
        $auth    := ~$0 if m/^^ HOME_URL   \= (\N*) /;
        $name    := ~$0 if m/^^ ID         \= (\N*) /;
        $version := ~$0 if m/^^ VERSION    \= (\N*) /;
        $release := ~$0 if m/^^ VERSION_ID \= (\N*) /;
    }
    elsif $name eq 'linux' {
        if qx{lsb_release -a 2> /dev/null} ~~ m/
            Distributor \s+ ID\: \s+ (<[\w\ ]>+) \s+
            Description\: \s+ (<[\w\ ]>+) \s+ (<[\d\.]>+) \s+
            Release\: \s+ (<[\d\.]>+)
        / {
            $auth    := ~$0;
            $name    := ~$1;
            $version := ~$2;
            $release := ~$3;
        }
    }
    my $desc := DateTime.now.Str;
    Distro.new(:$name, :$version, :$release, :$auth, :$path-sep, :$desc);
}

# set up $*DISTRO and deprecated $*OS and $*OSVER
multi sub INITIALIZE_DYNAMIC('$*DISTRO') {
    PROCESS::<$DISTRO> := INITIALIZE-A-DISTRO-NOW();
}

multi sub INITIALIZE_DYNAMIC('$*OS') {
    PROCESS::<$OS> := Deprecation.obsolete(
      :name('$*OS'),
      :from<2014.09>,
      :removed<2015.09>,
      :value($*DISTRO.name),
      :instead('$*DISTRO.name'),
    );
}

multi sub INITIALIZE_DYNAMIC('$*OSVER') {
    PROCESS::<$OSVER> := Deprecation.obsolete(
      :name('$*OSVER'),
      :from<2014.09>,
      :removed<2015.09>,
      :value($*DISTRO.version),
      :instead('$*DISTRO.version'),
    );
}

# vim: ft=perl6 expandtab sw=4
