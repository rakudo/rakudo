my class IO::Spec::Cygwin is IO::Spec::Unix {

    method canonpath ($patharg, :$parent) {
        my $path = $patharg.Str;
        $path.=subst(:g, '\\', '/');

        # Handle network path names beginning with double slash
        my $node = '';
        if $path ~~ s/^ ('//' <-[/]>+) [ '/' | $ ] /\// { #/
            $node = ~$0;
        }
        $node ~ IO::Spec::Unix.canonpath($path, :$parent);
    }

    method catdir ( *@paths ) {
        my $result = IO::Spec::Unix.catdir(@paths);

        # Don't create something that looks like a //network/path
        $result.subst(/ <[\\\/]> ** 2..*/, '/');
    }

    method is-absolute ($path) {
        nqp::hllbool(
          nqp::iseq_i(($_ := nqp::ord($path)), 92) # /^ ｢\｣ /
          || nqp::iseq_i($_, 47)                   # /^ ｢/｣ /
          || (nqp::eqat($path, ':', 1) # /^ <[A..Z a..z]> ':' [ ｢\｣ | ｢/｣ ] /
              && ( (nqp::isge_i($_, 65) && nqp::isle_i($_, 90)) # drive letter
                || (nqp::isge_i($_, 97) && nqp::isle_i($_, 122)))
              && ( nqp::iseq_i(($_ := nqp::ordat($path, 2)), 92) # slash
                || nqp::iseq_i($_, 47))))
    }

    method tmpdir {
        my %ENV := %*ENV;
        my $io;
        first( {
            if .defined {
                $io = .IO;
                $io.d && $io.rwx;
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
    method split(IO::Spec::Cygwin: Cool:D $path) {
        my $parts := IO::Spec::Win32.split($path);
        IO::Path::Parts.new:
          $parts.volume.subst(:global, '\\', '/'),
          $parts.dirname.subst(:global, '\\', '/'),
          $parts.basename.subst(:global, '\\', '/')
    }
    method join(|c) {
        IO::Spec::Win32.join(|c).subst(:global, '\\', '/');
    }
}

# vim: expandtab shiftwidth=4
