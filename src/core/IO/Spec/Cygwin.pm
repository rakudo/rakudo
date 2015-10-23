my class IO::Spec::Cygwin is IO::Spec::Unix {

    method canonpath (Cool:D $patharg) {
        my $path = $patharg.Str;
        $path.=subst(:g, '\\', '/');

        # Handle network path names beginning with double slash
        my $node = '';
        if $path ~~ s/^ ('//' <-[/]>+) [ '/' | $ ] /\// { #/
            $node = ~$0;
        }
        $node ~ IO::Spec::Unix.canonpath($path);
    }

    method catdir ( *@paths ) {
        my $result = IO::Spec::Unix.catdir(@paths);

        # Don't create something that looks like a //network/path
        $result.subst(/ <[\\\/]> ** 2..*/, '/');
    }

    method is-absolute ($file) {
        so $file ~~ / ^ [<[A..Z a..z]> ':']?  <[\\/]>/; # C:/test
    }

    method tmpdir {
        my %ENV := %*ENV;
        my $io;
        first( {
            if .defined {
                $io = .IO;
                $io.d && $io.r && $io.w && $io.x;
            }
          },
          %ENV<TMPDIR>,
          "/tmp",
          %ENV<TMP>,
          %ENV<TEMP>,
          'C:/temp',
        ) ?? $io !! IO::Path.new(".");
    }

    # Paths might have a volume, so we use Win32 splitpath and catpath instead
    method abs2rel(|c) {
        IO::Spec::Win32.abs2rel(|c).subst(:global, '\\', '/');
    }
    method rel2abs(|c) {
        IO::Spec::Win32.rel2abs(|c, :omit-volume).subst(:global, '\\', '/');
    }
    method splitpath(|c) {
        IO::Spec::Win32.splitpath(|c)>>.subst(:global, '\\', '/');
    }
    method catpath(|c) {
        IO::Spec::Win32.catpath(|c).subst(:global, '\\', '/');
    }
    multi method split(IO::Spec::Cygwin: Cool:D $path) {
        IO::Spec::Win32.split($path).map(
          { (.key => .value.subst(:global, '\\', '/')) }
        );
    }
    method join(|c) {
        IO::Spec::Win32.join(|c).subst(:global, '\\', '/');
    }
}

# vim: ft=perl6 expandtab sw=4
