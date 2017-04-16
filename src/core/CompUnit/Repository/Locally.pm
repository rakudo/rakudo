role CompUnit::Repository::Locally {
    has Lock     $!lock;
    has IO::Path $.prefix is required;
    has Str      $.WHICH;

    method new(CompUnit::Repository::Locally: Str:D :$prefix, CompUnit::Repository :$next-repo, *%args) {
        my $abspath := $*SPEC.rel2abs($prefix);
        my $IO      := $abspath.IO;

        state %instances;
        my $WHICH = self.^name ~ '|' ~ $abspath;
        %instances{$WHICH} //=
          self.bless(:prefix($IO), :lock(Lock.new), :$WHICH, :$next-repo, |%args);
    }

    multi method Str(CompUnit::Repository::Locally:D:) { $!prefix.absolute }
    multi method gist(CompUnit::Repository::Locally:D:) {
        self.path-spec
    }
    multi method perl(CompUnit::Repository::Locally:D:) {
        $?CLASS.perl ~ '.new(' ~ $!prefix.absolute.perl ~ ')';
    }

    multi method WHICH(CompUnit::Repository::Locally:D:) { $!WHICH }

    method path-spec(CompUnit::Repository::Locally:D:) {
        self.short-id ~ '#' ~ $!prefix.absolute;
    }

    method source-file(Str $name --> IO::Path:D) {
        self.prefix.add($name)
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
