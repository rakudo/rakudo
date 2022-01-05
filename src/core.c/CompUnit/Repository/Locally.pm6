role CompUnit::Repository::Locally {
    has Str        $.abspath;
    has IO::Path   $.prefix is required;
    has ValueObjAt $.WHICH  is required;

    my $lock = Lock.new;
    my %instances;

    method new(CompUnit::Repository::Locally: Any:D :$prefix is copy) {

        my $abspath;
        if $prefix ~~ IO::Path {
            $abspath := $prefix.absolute;
        }
        else {
            $abspath := $*SPEC.rel2abs($prefix.Str);
            $prefix = $abspath.IO;
        }

        my $WHICH := ValueObjAt.new(self.^name ~ '|' ~ $abspath);
        $lock.protect: {
            %instances{$WHICH} //= self.bless(:$abspath, :$prefix, :$WHICH, |%_)
        }
    }

    multi method Str(CompUnit::Repository::Locally:D:) { $!abspath }
    multi method gist(CompUnit::Repository::Locally:D:) {
        self.path-spec
    }
    multi method raku(CompUnit::Repository::Locally:D:) {
        $?CLASS.^name ~ '.new(prefix => ' ~ $!abspath.raku ~ ')';
    }

    multi method WHICH(CompUnit::Repository::Locally:D: --> ValueObjAt:D) {
        $!WHICH
    }

    method path-spec(CompUnit::Repository::Locally:D:) {
        self.short-id ~ '#' ~ $!abspath
    }

    method source-file(Str $name --> IO::Path:D) {
        self.prefix.add($name)
    }

    method id() {
        nqp::sha1(
          self.next-repo
            ?? self.path-spec ~ ',' ~ self.next-repo.id
            !! self.path-spec
        )
    }

    # stubs
    method short-id(CompUnit::Repository::Locally:D:) { ... }
}

# vim: expandtab shiftwidth=4
