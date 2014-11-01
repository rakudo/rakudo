# A class for directories that we know exist
my class IO::Dir does IO {
    has $!abspath;  # assumes we have a trailing slash *ALWAYS*
    has @!parts;

    my %nul = '..' => 1, '.' => 1, '' => 1;

    submethod BUILD(:$!abspath,:$check) {
        if $check {   # should really be .resolve, but we don't have that yet
            @!parts = MAKE-CLEAN-PARTS($!abspath);
            $!abspath = @!parts.join('/');
            fail "$!abspath is not a directory" unless FILETEST-D($!abspath);
        }
    }

    method !parts() {
        @!parts = $!abspath.split('/') unless @!parts;
    }

    method child(IO::Dir:D: $child) {
        $child
          ?? self.new(:abspath($!abspath ~ $child ~ '/'),:check)
          !! self;
    }

    method parent(IO::Dir:D: $levels = 1) {
        self!parts;
        @!parts <= $levels + 1
          ?? self.new(:abspath( @!parts[0] ~ '/' ))
          !! self.new(:abspath( @!parts[0 .. *-($levels + 2)].join('/') ~ '/'));
    }

    method chdir(IO::Dir:D: $path) {
        self.new( MAKE-ABSOLUTE-PATH($path,$!abspath), :check );
    }

    method open(IO::Dir:D: |c) {
        fail (X::IO::Directory.new(:$!abspath, :trying<open>));
    }
    method pipe(IO::Dir:D: |c) {
        fail (X::IO::Directory.new(:$!abspath, :trying<pipe>));
    }

    method volume(IO::Dir:D:)   { self!parts; @!parts[0] }
    method dirname(IO::Dir:D:)  { self!parts; @!parts[1 .. *-2].join('/') }
    method basename(IO::Dir:D:) { MAKE-BASENAME($!abspath.chop) }

    method Numeric(IO::Dir:D:) { self.basename.Numeric }
    method Bridge(IO::Dir:D:)  { self.basename.Bridge }
    method Int(IO::Dir:D:)     { self.basename.Int }

    method Str(IO::Dir:D:)  { $!abspath }
    method gist(IO::Dir:D:) { qq|"$!abspath".IO| }
    method perl(IO::Dir:D:) { "q|$!abspath|.IO" }

    method succ(IO::Dir:D:) { my $p = $!abspath.chop; ++$p ~ '/' }
    method pred(IO::Dir:D:) { my $p = $!abspath.chop; --$p ~ '/' }

    method e(IO::Dir:D:)   { True }
    method d(IO::Dir:D:)   { True }
    method f(IO::Dir:D:)   { False }
    method s(IO::Dir:D:)   { 0 }
    method l(IO::Dir:D:)   { False }
    method r(IO::Dir:D:)   { FILETEST-R(  $!abspath) }
    method w(IO::Dir:D:)   { FILETEST-W(  $!abspath) }
    method rw(IO::Dir:D:)  { FILETEST-RW( $!abspath) }
    method x(IO::Dir:D:)   { FILETEST-X(  $!abspath) }
    method rwx(IO::Dir:D:) { FILETEST-RWX($!abspath) }
    method z(IO::Dir:D:)   { True }
    method modified(IO::Dir:D:) { FILETEST-MODIFIED($!abspath) }
    method accessed(IO::Dir:D:) { FILETEST-ACCESSED($!abspath) }
    method changed(IO::Dir:D:)  { FILETEST-CHANGED( $!abspath) }
}
