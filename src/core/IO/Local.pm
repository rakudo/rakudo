# A role for local file(path)s that we know exist
my role IO::Local {
    has $!abspath;
    has @!parts;

    multi method ACCEPTS(IO::Local:D: \other) {
        nqp::p6bool(
          nqp::iseq_s(nqp::unbox_s($!abspath), nqp::unbox_s(~other))
        );
    }

    method absolute(IO::Local:D:) { $!abspath }

    proto method relative(|) { * }
    multi method relative(IO::Local:D:) { REMOVE-ROOT($*CWD ~ '/',$!abspath) }
    multi method relative(IO::Local:D: $root) { REMOVE-ROOT($root,$!abspath) }

    method !parts() { @!parts = $!abspath.split('/') unless @!parts }
    method volume(IO::Local:D:)    {
#        self!parts;  # runtime failure
        @!parts = $!abspath.split('/') unless @!parts;  # remove if above ok
        @!parts[0];
    }
    method dirname(IO::Local:D:)   {
        #self!parts;  # runtime failure
        @!parts = $!abspath.split('/') unless @!parts;  # remove if above ok
        @!parts[1 .. *-2].join('/');
    }
    method basename(IO::Local:D:)  { MAKE-BASENAME($!abspath) }
    method extension(IO::Local:D:) { MAKE-EXT(MAKE-BASENAME($!abspath))}

    method IO(IO::Local:D:)      { self }
    method Numeric(IO::Local:D:) { self.basename.Numeric }
    method Bridge(IO::Local:D:)  { self.basename.Bridge }
    method Int(IO::Local:D:)     { self.basename.Int }

    method Str(IO::Local:D:)  { $!abspath }
    multi method gist(IO::Local:D:) { qq|"{ self.relative }".IO| }
    multi method perl(IO::Local:D:) { "q|$!abspath|.IO" }

    method succ(IO::Local:D:) { $!abspath.succ }
    method pred(IO::Local:D:) { $!abspath.pred }

    method e(IO::Local:D:)   { True }
    method f(IO::Local:D:)   { FILETEST-F(  $!abspath) }
    method s(IO::Local:D:)   { FILETEST-S(  $!abspath) }
    method l(IO::Local:D:)   { FILETEST-L(  $!abspath) }
    method r(IO::Local:D:)   { FILETEST-R(  $!abspath) }
    method w(IO::Local:D:)   { FILETEST-W(  $!abspath) }
    method rw(IO::Local:D:)  { FILETEST-RW( $!abspath) }
    method x(IO::Local:D:)   { FILETEST-X(  $!abspath) }
    method rwx(IO::Local:D:) { FILETEST-RWX($!abspath) }
    method z(IO::Local:D:)   { FILETEST-Z(  $!abspath) }
    method modified(IO::Local:D:) { FILETEST-MODIFIED($!abspath) }
    method accessed(IO::Local:D:) { FILETEST-ACCESSED($!abspath) }
    method changed(IO::Local:D:)  { FILETEST-CHANGED( $!abspath) }

    method rename(IO::Local:D: Str() $to, :$createonly) {
        RENAME-PATH($!abspath,MAKE-ABSOLUTE-PATH($to,$*CWD ~ '/'),:$createonly);
    }
    method chmod(IO::Local:D: Int(Any) $mode) {
        CHMOD-PATH($!abspath, $mode);
    }
    method symlink(IO::Local:D: Str() $name) {
        SYMLINK-PATH($!abspath, MAKE-ABSOLUTE-PATH($name,$*CWD ~ '/'));
    }

#?if moar
    method watch(IO::Local:D:) {
        IO::Notification.watch_path($!abspath);
    }
#?endif
}
