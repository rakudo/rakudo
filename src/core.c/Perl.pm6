class Perl does Systemic {
    has Compiler $.compiler;

    submethod BUILD(
      :$!name      = 'Perl 6',
      :$!auth      = "The Perl Foundation",
      :$!version   = Version.new(nqp::p6box_s(nqp::getcomp('perl6').language_version())),
      :$!compiler  = Compiler.new,
      --> Nil
    ) { }

    method VMnames { <moar jvm js> }

    method DISTROnames { <macosx linux freebsd mswin32 openbsd dragonfly netbsd browser> }
    method KERNELnames { <darwin linux freebsd openbsd netbsd  dragonfly win32 browser>  }

    my $version-cache      := nqp::hash;
    my $version-cache-lock := Lock.new;
    method version {
        $version-cache-lock.protect: {
            my $comp-ver := nqp::getcomp('perl6').language_version();
            nqp::existskey($version-cache,$comp-ver)
              ?? nqp::atkey($version-cache,$comp-ver)
              !! nqp::bindkey($version-cache,$comp-ver,Version.new($comp-ver))
        }
    }
}

# vim: ft=perl6 expandtab sw=4
