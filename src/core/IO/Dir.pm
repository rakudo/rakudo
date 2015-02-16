# A class for directories that we know exist
my class IO::Dir is Cool does IO::Pathy {

    submethod BUILD(:$!abspath,:$check = True) {
        if $check {
            @!parts = MAKE-CLEAN-PARTS($!abspath);
            $!abspath = @!parts.join('/');
            fail "$!abspath is not a directory" unless FILETEST-d($!abspath);
        }
    }

    method child(IO::Dir:D: $child) {
        $child
          ?? IOU.new($!abspath ~ '/' ~ $child)
          !! self;
    }

    proto method chdir(|) { * }
    multi method chdir(IO::Dir:D:) {
        CHANGE-DIRECTORY($!abspath);
    }
    multi method chdir(IO::Dir:D: Str() $path) {
        CHANGE-DIRECTORY( MAKE-ABSOLUTE-PATH($path,$!abspath) );
    }

    method mkdir(IO::Dir:D: Str() $path, Int $mode = 0o777) {
        MAKE-DIR( MAKE-ABSOLUTE-PATH($path,$!abspath), $mode );
    }

    proto method rmdir(|) { * }
    multi method rmdir(IO::Dir:D:) {
        REMOVE-DIR($!abspath);
    }
    multi method rmdir(IO::Dir:D: Str() $path) {
        REMOVE-DIR( MAKE-ABSOLUTE-PATH($path,$!abspath) );
    }

    multi method Str(IO::Dir:D:)  { $!abspath ~ '/' }  # string has trailing /

    method d(IO::Dir:D:) { True }
    method f(IO::Dir:D:) { False }
    method s(IO::Dir:D:) { Nil }
    method z(IO::Dir:D:) { Nil }
}

# vim: ft=perl6 expandtab sw=4
