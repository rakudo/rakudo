class Distribution::Resource {
    has $.repo;
    has $.repo-name;
    has $.dist-id;
    has $.key;

    # delegate appropriate IO::Path methods to the resource IO::Path object
    has IO::Path $.IO is built(False) handles <
      absolute is-absolute relative is-relative parts volume dirname
      basename extension open resolve slurp lines comb split words copy
    >;

    method TWEAK() {
        my $repo := self.repo-name
          ?? CompUnit::RepositoryRegistry.repository-for-name($!repo-name)
          !! CompUnit::RepositoryRegistry.repository-for-spec($!repo);
        $!IO := $repo.resource($!dist-id, "resources/$!key")
    }

    method platform-library-name() {
        my $library := self.IO;
        # Already a full name?
        ($library ~~ /\.<.alpha>+$/ or $library ~~ /\.so(\.<.digit>+)+$/)
            ??  $library
            !!  $*VM.platform-library-name($library)
    }

    # multis cannot be handled transparently by $!IO (yet)
    multi method Str(::?CLASS:D:  |c) { $!IO.Str(|c)  }
    multi method gist(::?CLASS:D: |c) { $!IO.gist(|c) }
    multi method raku(::?CLASS:D: |c) { $!IO.raku(|c) }
}

# vim: expandtab shiftwidth=4
