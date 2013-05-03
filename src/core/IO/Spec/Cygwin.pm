my class IO::Spec::Cygwin is IO::Spec::Unix {

    #| Any C<\> (backslashes) are converted to C</> (forward slashes),
    #| and then IO::Spec::Unix.canonpath() is called on the result.
    method canonpath (Cool:D $path is copy) {
        $path.=subst(:g, '\\', '/');

        # Handle network path names beginning with double slash
        my $node = '';
        if $path ~~ s/^ ('//' <-[/]>+) [ '/' | $ ] /\// { #/
            $node = ~$0;
        }
        $node ~ IO::Spec::Unix.canonpath($path);
    }

    #| Calls the Unix version, and additionally prevents
    #| accidentally creating a //network/path.
    method catdir ( *@paths ) {
        my $result = IO::Spec::Unix.catdir(@paths);

        # Don't create something that looks like a //network/path
        $result.subst(/ <[\\\/]> ** 2..*/, '/');
    }


    #| Tests if the file name begins with C<drive_letter:/> or a slash.
    method is-absolute ($file) {
        so $file ~~ / ^ [<[A..Z a..z]> ':']?  <[\\/]>/; # C:/test
    }

    method tmpdir {
        state $tmpdir;
        return $tmpdir if $tmpdir.defined;
        $tmpdir = self.canonpath: first( { .defined && .IO.d && .IO.w },
             %*ENV<TMPDIR>,
             "/tmp",
             %*ENV<TMP>,
             %*ENV<TEMP>,
             'C:/temp') 
          || self.curdir;
    }

    # Paths might have a volume, so we use Win32 splitpath and catpath instead
    method splitpath (|c)     { IO::Spec::Win32.splitpath(|c) }
    method catpath (|c)       { IO::Spec::Win32.catpath(|c).subst(:global, '\\', '/')  }
    method split ($path)      { IO::Spec::Win32.split($path).map:
                                  { (.key => .value.subst(:global, '\\', '/')) }  }
    method join (|c)          { IO::Spec::Win32.join(|c).subst(:global, '\\', '/')     }

}
