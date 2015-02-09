# A class for directories that we know exist
my class IO::Dir is Cool does IO::Local {

    submethod BUILD(:$!abspath,:$check) {
        if $check {   # should really be .resolve, but we don't have that yet
            @!parts = MAKE-CLEAN-PARTS($!abspath);
            $!abspath = @!parts.join('/');
            fail "$!abspath is not a directory" unless FILETEST-D($!abspath);
        }
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

    method chdir(IO::Dir:D: Str() $path, :$test = 'r') {
        my $new := self.new( MAKE-ABSOLUTE-PATH($path,$!abspath), :check );
        $new // $new.throw;
        my $result := $new.all($test);
        $result // $result.throw;
        $new;
    }

    method rmdir(IO::Dir:D:) { REMOVE-DIR($!abspath) }

    method dirname(IO::Local:D:)   {
        self!parts;
        '/' ~ @!parts[1 .. *-3].join('/');
    }
    method basename(IO::Dir:D:)  { MAKE-BASENAME($!abspath.chop) }
    method extension(IO::Dir:D:) { MAKE-EXT(MAKE-BASENAME($!abspath.chop))}
    method succ(IO::Dir:D:) { $!abspath.chop.succ ~ '/' }
    method pred(IO::Dir:D:) { $!abspath.chop.pred ~ '/' }
    method d(IO::Dir:D:) { True }
    method f(IO::Dir:D:) { False }
    method s(IO::Dir:D:) { Nil }
    method l(IO::Dir:D:) { False }
    method z(IO::Dir:D:) { Nil }
}
