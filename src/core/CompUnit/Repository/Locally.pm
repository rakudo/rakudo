role CompUnit::Repository::Locally {
    has Lock     $!lock;
    has IO::Path $.prefix is required;
    has IO::Path $.bindir is rw;
    has Str      $.WHICH;

    my %instances;

    method new(CompUnit::Repository::Locally: Str:D :$prefix, :$bindir, CompUnit::Repository :$next-repo) {
        my $abspath   := $*SPEC.rel2abs($prefix);
        my $IO-prefix := IO::Path.new-from-absolute-path($abspath);
        my $IO-bindir := $bindir ?? IO::Path.new-from-absolute-path($bindir) !! IO::Path;

        %instances{$abspath} //=
          self.bless(:prefix($IO-prefix), :bindir($IO-bindir), :lock(Lock.new), :WHICH(self.^name ~ '|' ~ $abspath), :$next-repo);
    }

    multi method Str(CompUnit::Repository::Locally:D:) { $!prefix.abspath }
    multi method gist(CompUnit::Repository::Locally:D:) {
        self.path-spec
    }
    multi method perl(CompUnit::Repository::Locally:D:) {
        $?CLASS.perl ~ '.new(' ~ $!prefix.abspath.perl ~ ')';
    }

    multi method WHICH(CompUnit::Repository::Locally:D:) { $!WHICH }

    method path-spec(CompUnit::Repository::Locally:D:) {
        self.short-id ~ '#' ~ $!prefix.abspath;
    }

    method prefix { "{$!prefix}".IO }

    method id() {
        my $name = self.path-spec;
        $name ~= ',' ~ self.next-repo.id if self.next-repo;
        return nqp::sha1($name);
    }

    # stubs
    method short-id(CompUnit::Repository::Locally:D:)                             {...}
}

# vim: ft=perl6 expandtab sw=4
