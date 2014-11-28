# A class for file(path)s that we know exist
my class IO::File is Cool does IO::Local {

    submethod BUILD(:$!abspath) { }

    method open(IO::File:D: |c) { open( $!abspath, |c ) }

    method copy(IO::File:D: $to as Str, :$createonly) {
        COPY-FILE($!abspath, MAKE-ABSOLUTE-PATH($to,$*CWD), :$createonly);
    }

    method unlink(IO::File:D:) { UNLINK-PATH($!abspath) }

    method link(IO::File:D: $name as Str) {
        LINK-FILE($!abspath, MAKE-ABSOLUTE-PATH($name,$*CWD));
    }

    method slurp(IO::File:D: |c) { SLURP-PATH($!abspath,|c) }

    method spurt(IO::File:D: \what, :$bin, |c) {
        SPURT-PATH($!abspath, what, :bin(what ~~ Blob), |c );
    }

    method d(IO::File:D:) { False }
}
