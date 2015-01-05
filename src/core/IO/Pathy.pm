# A role for local file(path)s
my role IO::Locally {
    has $.abspath;
    has @!parts;

    submethod BUILD(:$abspath) { $!abspath = $abspath }

    multi method ACCEPTS(IO::Locally:D: \other) {
        nqp::p6bool(
          nqp::iseq_s(nqp::unbox_s($!abspath), nqp::unbox_s(~other))
        );
    }

    method chop(IO::Locally:D:)     { $!abspath.chop }
    method relpath(IO::Locally:D: $root?) {
        REMOVE-ROOT( $root // $*CWD.Str,$!abspath);
    }

    method !parts() { @!parts = $!abspath.split('/') unless @!parts }
    method volume(IO::Locally:D:)    {
#        self!parts;  # runtime failure
        @!parts = $!abspath.split('/') unless @!parts;  # remove if above ok
        @!parts[0];
    }
    method dirname(IO::Locally:D:)   {
        #self!parts;  # runtime failure
        @!parts = $!abspath.split('/') unless @!parts;  # remove if above ok
        @!parts[1 .. *-2].join('/');
    }
    method basename(IO::Locally:D:)  { MAKE-BASENAME($!abspath) }
    method extension(IO::Locally:D:) { MAKE-EXT(MAKE-BASENAME($!abspath))}

    method IO(IO::Locally:D:)      { self }
    method Numeric(IO::Locally:D:) { self.basename.Numeric }
    method Bridge(IO::Locally:D:)  { self.basename.Bridge }
    method Int(IO::Locally:D:)     { self.basename.Int }

    multi method Str(IO::Locally:D:)  { $!abspath }
    multi method gist(IO::Locally:D:) { qq|"{ self.relative }".IO| }
    multi method perl(IO::Locally:D:) { "q|$!abspath|.IO" }

    method succ(IO::Locally:D:) { $!abspath.succ }
    method pred(IO::Locally:D:) { $!abspath.pred }

    method e(IO::Locally:D:)   { True }
    method f(IO::Locally:D:)   { FILETEST-F(  $!abspath) }
    method s(IO::Locally:D:)   { FILETEST-S(  $!abspath) }
    method l(IO::Locally:D:)   { FILETEST-L(  $!abspath) }
    method r(IO::Locally:D:)   { FILETEST-R(  $!abspath) }
    method w(IO::Locally:D:)   { FILETEST-W(  $!abspath) }
    method rw(IO::Locally:D:)  { FILETEST-RW( $!abspath) }
    method x(IO::Locally:D:)   { FILETEST-X(  $!abspath) }
    method rx(IO::Locally:D:)  { FILETEST-RX( $!abspath) }
    method wx(IO::Locally:D:)  { FILETEST-WX( $!abspath) }
    method rwx(IO::Locally:D:) { FILETEST-RWX($!abspath) }
    method o(IO::Locally:D:)   { FILETEST-UID($!abspath) == +$*USER }
    method z(IO::Locally:D:)   { FILETEST-Z(  $!abspath) }
    method modified(IO::Locally:D:) { FILETEST-MODIFIED($!abspath) }
    method accessed(IO::Locally:D:) { FILETEST-ACCESSED($!abspath) }
    method changed(IO::Locally:D:)  { FILETEST-CHANGED( $!abspath) }

    method rename(IO::Locally:D: $to as Str, :$createonly) {
        RENAME-PATH($!abspath,MAKE-ABSOLUTE-PATH($to,$*CWD.Str),:$createonly);
    }
    method chmod(IO::Locally:D: $mode as Int) {
        CHMOD-PATH($!abspath, $mode);
    }
    method symlink(IO::Locally:D: $name as Str) {
        SYMLINK-PATH($!abspath, MAKE-ABSOLUTE-PATH($name,$*CWD.Str));
    }

#?if moar
    method watch(IO::Locally:D:) {
        IO::Notification.watch_path($!abspath);
    }
#?endif

    method directory(IO::Locally:D:) {
        DEPRECATED("dirname", |<2014.10 2015.10>);
        self.dirname;
    }
    method absolute(IO::Locally:D:) {
        DEPRECATED("abspath", |<2015.01 2016.01>);
        $!abspath;
    }
    method relative(IO::Locally:D: |c) {
        DEPRECATED("relpath", |<2015.01 2016.01>);
        self.relpath(|c);
    }
}

# vim: ft=perl6 expandtab sw=4
