class Raku does Systemic {
    has Compiler $.compiler is built(:bind) = Compiler.new;

    submethod TWEAK(--> Nil) {
        # https://github.com/rakudo/rakudo/issues/3436
        nqp::bind($!name,'Raku');
        nqp::bind($!auth,'Yet Another Society');
        nqp::bind($!version,nqp::getcomp('Raku').language_version.Version);
    }

    method VMnames { <moar jvm> }

    method DISTROnames { <macos linux freebsd mswin32 openbsd dragonfly netbsd browser> }
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

    # until RakuAST becomes default
    method legacy(Raku:U:) is implementation-detail {
        nqp::gethllsym('Raku','COMPILER-FRONTEND') eq 'legacy'
    }

    # Change the above to this when RakuAST is default and legacy
    # is not available anymore
    #method legacy(Raku:U: --> False) is implementation-detail { }
}

class Perl is repr('Uninstantiable') {
    method new() {
        my $cf := callframe(1);
        DEPRECATED(
          "Raku.new", :what('Perl.new'), :file($cf.file), :line($cf.line)
        );
        Raku.new(|%_)
    }
}

# vim: expandtab shiftwidth=4
