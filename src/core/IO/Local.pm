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
    multi method relative(IO::Local:D:) {
        Rakudo::Internals.REMOVE-ROOT($*CWD ~ '/',$!abspath)
    }
    multi method relative(IO::Local:D: $root) {
        Rakudo::Internals.REMOVE-ROOT($root,$!abspath)
    }

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
    method basename(IO::Local:D:)  {
        Rakudo::Internals.MAKE-BASENAME($!abspath)
    }
    method extension(IO::Local:D:) {
        Rakudo::Internals.MAKE-EXT(Rakudo::Internals.MAKE-BASENAME($!abspath))
    }

    method IO(IO::Local:D:)      { self }
    method Numeric(IO::Local:D:) { self.basename.Numeric }
    method Bridge(IO::Local:D:)  { self.basename.Bridge }
    method Int(IO::Local:D:)     { self.basename.Int }

    method Str(IO::Local:D:)  { $!abspath }
    multi method gist(IO::Local:D:) { qq|"{ self.relative }".IO| }
    multi method perl(IO::Local:D:) { $!abspath.perl ~ '.IO' }

    method succ(IO::Local:D:) { $!abspath.succ }
    method pred(IO::Local:D:) { $!abspath.pred }

    method e(IO::Local:D:)   { True }
    method f(IO::Local:D:)   { ?Rakudo::Internals.FILETEST-F(  $!abspath) }
    method s(IO::Local:D:)   {  Rakudo::Internals.FILETEST-S(  $!abspath) }
    method l(IO::Local:D:)   { ?Rakudo::Internals.FILETEST-L(  $!abspath) }
    method r(IO::Local:D:)   { ?Rakudo::Internals.FILETEST-R(  $!abspath) }
    method w(IO::Local:D:)   { ?Rakudo::Internals.FILETEST-W(  $!abspath) }
    method rw(IO::Local:D:)  { ?Rakudo::Internals.FILETEST-RW( $!abspath) }
    method x(IO::Local:D:)   { ?Rakudo::Internals.FILETEST-X(  $!abspath) }
    method rwx(IO::Local:D:) { ?Rakudo::Internals.FILETEST-RWX($!abspath) }
    method z(IO::Local:D:)   { ?Rakudo::Internals.FILETEST-Z(  $!abspath) }
    method modified(IO::Local:D:) {
        Instant.from-posix(Rakudo::Internals.FILETEST-MODIFIED($!abspath))
    }
    method accessed(IO::Local:D:) { 
        Instant.from-posix(Rakudo::Internals.FILETEST-ACCESSED($!abspath))
    }
    method changed(IO::Local:D:)  { FILETEST-CHANGED( $!abspath) }

    method rename(IO::Local:D: Str() $to, :$createonly) {
        RENAME-PATH($!abspath,
          Rakudo::Internals.MAKE-ABSOLUTE-PATH($to,$*CWD ~ '/'),:$createonly);
    }
    method chmod(IO::Local:D: Int() $mode) {
        CHMOD-PATH($!abspath, $mode);
    }
    method symlink(IO::Local:D: Str() $name) {
        SYMLINK-PATH($!abspath,
          Rakudo::Internals.MAKE-ABSOLUTE-PATH($name,$*CWD ~ '/'));
    }

#?if moar
    method watch(IO::Local:D:) {
        IO::Notification.watch-path($!abspath);
    }
#?endif
}
