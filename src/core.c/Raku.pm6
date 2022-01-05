class Raku does Systemic {
    has Compiler $.compiler is built(:bind) = Compiler.new;

    submethod TWEAK(--> Nil) {
        # https://github.com/rakudo/rakudo/issues/3436
        nqp::bind($!name,'Raku');
        nqp::bind($!auth,'The Perl Foundation');
        nqp::bind($!version,nqp::getcomp('Raku').language_version.Version);
    }

    method VMnames { <moar jvm js> }

    method DISTROnames { <macosx linux freebsd mswin32 openbsd dragonfly netbsd browser> }
    method KERNELnames { <darwin linux freebsd openbsd netbsd dragonfly sunos win32 browser>  }

    my $version-cache      := nqp::hash;
    my $version-cache-lock := Lock.new;
    method version {
        $version-cache-lock.protect: {
            my $comp-ver := nqp::getcomp('Raku').language_version();
            nqp::existskey($version-cache,$comp-ver)
              ?? nqp::atkey($version-cache,$comp-ver)
              !! nqp::bindkey($version-cache,$comp-ver,Version.new($comp-ver))
        }
    }
}

class Perl is Raku { }  # indeed :-)

# vim: expandtab shiftwidth=4
