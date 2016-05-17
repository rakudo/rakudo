role CompUnit::Repository::Locally {
    has Lock     $!lock;
    has IO::Path $.prefix is required;
    has Str      $.WHICH;

    method new(CompUnit::Repository::Locally: Str:D :$prefix, CompUnit::Repository :$next-repo, *%args) {
        my $abspath := $*SPEC.rel2abs($prefix);
        my $IO      := IO::Path.new-from-absolute-path($abspath);

        state %instances;
        %instances{$abspath} //=
          self.bless(:prefix($IO), :lock(Lock.new), :WHICH(self.^name ~ '|' ~ $abspath), :$next-repo, |%args);
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

    method source-file(Str $name --> IO::Path) {
        self.prefix.child($name)
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
