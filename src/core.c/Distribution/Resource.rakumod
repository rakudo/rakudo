class Distribution::Resource {
    has $.repo;
    has $.repo-name;
    has $.dist-id;
    has $.key;

    # NOTE: IO() **MUST** be determined at runtime. The result must not make
    # it into a precomp file.  See also commits 67906e4 and d4d6a99
    method IO() {
        my $repo := self.repo-name
            ?? CompUnit::RepositoryRegistry.repository-for-name(self.repo-name)
            !! CompUnit::RepositoryRegistry.repository-for-spec(self.repo);
        $repo.resource(self.dist-id, "resources/$.key")
    }

    method platform-library-name() {
        my $library = self.IO;
        # already a full name?
        ($library ~~ /\.<.alpha>+$/ or $library ~~ /\.so(\.<.digit>+)+$/)
            ??  $library
            !!  $*VM.platform-library-name($library)
    }

    # delegate appropriate IO::Handle methods
    method open(|c) {
        self.IO.open(|c)
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

    # treating a Distribution::Resource like an IO::Path (instead of an IO::Handle)
    # should be avoided, so we have deprecated all methods that assume IO::Path
    # behavior
    multi method Str(::?CLASS:D: |c)
      is DEPRECATED('%?RESOURCES<key> directly')
    {
        self.IO.Str(|c)
    }
    method absolute(|c)
      is DEPRECATED('%?RESOURCES<key> directly')
    {
        self.IO.absolute(|c)
    }
    method is-absolute(|c)
      is DEPRECATED('%?RESOURCES<key> directly')
    {
        self.IO.is-absolute(|c)
    }
    method relative(|c)
      is DEPRECATED('%?RESOURCES<key> directly')
    {
        self.IO.relative(|c)
    }
    method is-relative(|c)
      is DEPRECATED('%?RESOURCES<key> directly')
    {
        self.IO.is-relative(|c)
    }
    method parts(|c)
      is DEPRECATED('%?RESOURCES<key> directly')
    {
        self.IO.parts(|c)
    }
    method volume(|c)
      is DEPRECATED('%?RESOURCES<key> directly')
    {
        self.IO.volume(|c)
    }
    method dirname(|c)
      is DEPRECATED('%?RESOURCES<key> directly')
    {
        self.IO.dirname(|c)
    }
    method basename(|c)
      is DEPRECATED('%?RESOURCES<key> directly')
    {
        self.IO.basename(|c)
    }
    method extension(|c)
      is DEPRECATED('%?RESOURCES<key> directly')
    {
        self.IO.extension(|c)
    }
    method resolve(|c)
      is DEPRECATED('%?RESOURCES<key> directly')
    {
        self.IO.resolve(|c)
    }
}

# vim: expandtab shiftwidth=4
