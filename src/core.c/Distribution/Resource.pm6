class Distribution::Resource {
    has $.repo;
    has $.repo-name;
    has $.dist-id;
    has $.key;

    method IO() {
        my $repo := self.repo-name
            ?? CompUnit::RepositoryRegistry.repository-for-name(self.repo-name)
            !! CompUnit::RepositoryRegistry.repository-for-spec(self.repo);
        $repo.resource(self.dist-id, "resources/$.key")
    }

    method platform-library-name() {
        my $library = self.IO;
        ($library ~~ /\.<.alpha>+$/ or $library ~~ /\.so(\.<.digit>+)+$/) #Already a full name?
            ??  $library
            !!  $*VM.platform-library-name($library)
    }

    # delegate appropriate IO::Path methods to the resource IO::Path object
    multi method Str(::?CLASS:D: |c) {
        self.IO.Str(|c)
    }
    multi method gist(::?CLASS:D: |c) {
        self.IO.gist(|c)
    }
    multi method raku(::?CLASS:D: |c) {
        self.IO.raku(|c)
    }
    method absolute(|c) {
        self.IO.absolute(|c)
    }
    method is-absolute(|c) {
        self.IO.is-absolute(|c)
    }
    method relative(|c) {
        self.IO.relative(|c)
    }
    method is-relative(|c) {
        self.IO.is-relative(|c)
    }
    method parts(|c) {
        self.IO.parts(|c)
    }
    method volume(|c) {
        self.IO.volume(|c)
    }
    method dirname(|c) {
        self.IO.dirname(|c)
    }
    method basename(|c) {
        self.IO.basename(|c)
    }
    method extension(|c) {
        self.IO.extension(|c)
    }
    method open(|c) {
        self.IO.open(|c)
    }
    method resolve(|c) {
        self.IO.resolve(|c)
    }
    method slurp(|c) {
        self.IO.slurp(|c)
    }
    method lines(|c) {
        self.IO.lines(|c)
    }
    method comb(|c) {
        self.IO.comb(|c)
    }
    method split(|c) {
        self.IO.split(|c)
    }
    method words(|c) {
        self.IO.words(|c)
    }
    method copy(|c) {
        self.IO.copy(|c)
    }
}

# vim: expandtab shiftwidth=4
