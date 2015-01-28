# A role for local file(path)s
my role IO::Pathy {
    has $.abspath;
    has @!parts;

    submethod BUILD(:$abspath) { $!abspath = $abspath }

    multi method ACCEPTS(IO::Pathy:D: \other) {
        nqp::p6bool(
          nqp::iseq_s(nqp::unbox_s($!abspath), nqp::unbox_s(~other))
        );
    }

    method chop(IO::Pathy:D:)     { $!abspath.chop }
    method relpath(IO::Pathy:D: $root?) {
        REMOVE-ROOT( $root // $*CWD.Str,$!abspath);
    }

    method !parts() { @!parts = $!abspath.split('/') unless @!parts }
    method volume(IO::Pathy:D:)    {
#        self!parts;  # runtime failure
        @!parts = $!abspath.split('/') unless @!parts;  # remove if above ok
        @!parts[0];
    }
    method dirname(IO::Pathy:D:)   {
        #self!parts;  # runtime failure
        @!parts = $!abspath.split('/') unless @!parts;  # remove if above ok
        @!parts[1 .. *-2].join('/');
    }
    method basename(IO::Pathy:D:)  { MAKE-BASENAME($!abspath) }
    method extension(IO::Pathy:D:) { MAKE-EXT(MAKE-BASENAME($!abspath))}

    method IO(IO::Pathy:D:)      { self }
    method Numeric(IO::Pathy:D:) { self.basename.Numeric }
    method Bridge(IO::Pathy:D:)  { self.basename.Bridge }
    method Int(IO::Pathy:D:)     { self.basename.Int }

    multi method Str(IO::Pathy:D:)  { $!abspath }
    multi method gist(IO::Pathy:D:) { qq|"{ self.relative }".IO| }
    multi method perl(IO::Pathy:D:) { "q|$!abspath|.IO" }

    method succ(IO::Pathy:D:) { $!abspath.succ }
    method pred(IO::Pathy:D:) { $!abspath.pred }

    method e(IO::Pathy:D:)   { True }
    method f(IO::Pathy:D:)   { FILETEST-f(  $!abspath) }
    method s(IO::Pathy:D:)   { FILETEST-s(  $!abspath) }
    method i(IO::Pathy:D:)   { FILETEST-i(  $!abspath) }
    method v(IO::Pathy:D:)   { FILETEST-v(  $!abspath) }
    method l(IO::Pathy:D:)   { FILETEST-l(  $!abspath) }
    method r(IO::Pathy:D:)   { FILETEST-r(  $!abspath) }
    method w(IO::Pathy:D:)   { FILETEST-w(  $!abspath) }
    method rw(IO::Pathy:D:)  { FILETEST-rw( $!abspath) }
    method x(IO::Pathy:D:)   { FILETEST-x(  $!abspath) }
    method rx(IO::Pathy:D:)  { FILETEST-rx( $!abspath) }
    method wx(IO::Pathy:D:)  { FILETEST-wx( $!abspath) }
    method rwx(IO::Pathy:D:) { FILETEST-rwx($!abspath) }
    method o(IO::Pathy:D:)   { FILETEST-UID($!abspath) == +$*USER }
    method z(IO::Pathy:D:)   { FILETEST-z(  $!abspath) }
    method modified(IO::Pathy:D:) { FILETEST-MODIFIED($!abspath) }
    method accessed(IO::Pathy:D:) { FILETEST-ACCESSED($!abspath) }
    method changed(IO::Pathy:D:)  { FILETEST-CHANGED( $!abspath) }

    method rename(IO::Pathy:D: $to as Str, :$createonly) {
        my $topath := MAKE-ABSOLUTE-PATH($to,$*CWD.Str);
        RENAME-PATH($!abspath,$topath,:$createonly);
        $!abspath = $topath;  # have the object point to the new location
#        self!parts;  # runtime failure
        @!parts = $!abspath.split('/') unless @!parts;  # remove if above ok
    }
    method move(IO::Pathy:D: $to as Str, :$createonly) {
        my $topath := MAKE-ABSOLUTE-PATH($to,$*CWD.Str);
        MOVE-PATH($!abspath,$topath,:$createonly);
        $!abspath = $topath;  # have the object point to the new location
#        self!parts;  # runtime failure
        @!parts = $!abspath.split('/') unless @!parts;  # remove if above ok
    }
    method chmod(IO::Pathy:D: $mode as Int) {
        CHMOD-PATH($!abspath, $mode);
    }
    method symlink(IO::Pathy:D: $name as Str) {
        SYMLINK-PATH($!abspath, MAKE-ABSOLUTE-PATH($name,$*CWD.Str));
    }

#?if moar
    method watch(IO::Pathy:D:) {
        IO::Notification.watch_path($!abspath);
    }
#?endif

    method directory(IO::Pathy:D:) {
        DEPRECATED("dirname", |<2014.10 2015.10>);
        self.dirname;
    }
    method absolute(IO::Pathy:D:) {
        DEPRECATED("abspath", |<2015.01 2016.01>);
        $!abspath;
    }
    method relative(IO::Pathy:D: |c) {
        DEPRECATED("relpath", |<2015.01 2016.01>);
        self.relpath(|c);
    }
}

# vim: ft=perl6 expandtab sw=4
