role CompUnitRepo::Locally {
    has Lock     $!lock;
    has IO::Path $.prefix is required;
    has Str      $.WHICH;

    my %instances;

    method new(CompUnitRepo::Locally: Str:D :$prefix, CompUnit::Repository :$next-repo) {
        my $abspath := $*SPEC.rel2abs($prefix);
        my $IO      := IO::Path.new-from-absolute-path($abspath);

        %instances{$abspath} //=
          self.bless(:prefix($IO), :lock(Lock.new), :WHICH(self.^name ~ '|' ~ $abspath), :$next-repo);
    }

    multi method Str(CompUnitRepo::Locally:D:) { $!prefix.abspath }
    multi method gist(CompUnitRepo::Locally:D:) {
        self.path-spec
    }
    multi method perl(CompUnitRepo::Locally:D:) {
        $?CLASS.^name ~ ".new('$!prefix.abspath()')";
    }

    multi method WHICH(CompUnitRepo::Locally:D:) { $!WHICH }

    method path-spec(CompUnitRepo::Locally:D:) {
        self.short-id ~ '#' ~ $!prefix.abspath;
    }

    # stubs
    method files(CompUnitRepo::Locally:D: $file, :$name, :$auth, :$ver)   {...}
    method short-id(CompUnitRepo::Locally:D:)                             {...}

    method loaded() returns Iterable {
        return ();
    }
}

# vim: ft=perl6 expandtab sw=4
