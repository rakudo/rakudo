# A role for local file(path)s that are known to exist
my role IO::Pathy {
    has $.abspath;
    has @!parts;

    submethod BUILD(:$abspath) { $!abspath = $abspath }

    multi method ACCEPTS(IO::Pathy:D: \other) {
        nqp::p6bool(
          nqp::iseq_s(nqp::unbox_s($!abspath), nqp::unbox_s(~other))
        );
    }

    method relpath(IO::Pathy:D: Str() $root = $*CWD) {
        REMOVE-ROOT($root,$!abspath);
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

    proto method basename (|) { * }
    multi method basename(IO::Pathy:D:)  { MAKE-BASENAME($!abspath) }
    multi method basename(IO::Pathy:D: :$strip!)  {
        my $basename := MAKE-BASENAME($!abspath);
        my $ext      := MAKE-EXT($basename);
        $ext ~~ $strip.any
          ?? $basename.substr(0,*-($ext.chars + 1))
          !! $basename;
    }
    method extension(IO::Pathy:D:) { MAKE-EXT(MAKE-BASENAME($!abspath))}

    method parent(IO::Pathy:D: $levels = 1) {
        #self!parts;  # runtime failure
        @!parts = $!abspath.split('/') unless @!parts;  # remove if above ok
        @!parts <= $levels + 1
          ?? self.new(:abspath( @!parts[0] ~ '/' ))
          !! self.new(:abspath( @!parts[0 .. *-($levels + 2)].join('/') ~ '/'));
    }

    method resolve(IO::Pathy:D:) {
        FILETEST-l($!abspath)
          ?? self.new(MAKE-CLEAN-PARTS(READLINK-ABSPATH($!abspath)).join('/'))
          !! self;
    }

    method IO(IO::Pathy:D:)      { self }
    method Numeric(IO::Pathy:D:) { self.basename.Numeric }
    method Bridge(IO::Pathy:D:)  { self.basename.Bridge }
    method Int(IO::Pathy:D:)     { self.basename.Int }

    multi method Str(IO::Pathy:D:)  { $!abspath }
    multi method gist(IO::Pathy:D:) { qq|"{ self.relpath }".IO| }
    multi method perl(IO::Pathy:D:) { "q|$!abspath|.IO" }

    method succ(IO::Pathy:D:) { $!abspath.succ }
    method pred(IO::Pathy:D:) { $!abspath.pred }

    method e(IO::Pathy:D:)   { True }
    method f(IO::Pathy:D:)   { FILETEST-f(  $!abspath) }
    method d(IO::Pathy:D:)   { FILETEST-d(  $!abspath) }
    method s(IO::Pathy:D:)   { FILETEST-s(  $!abspath) }
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
    method device(IO::Pathy:D:)   { FILETEST-DEVICE(  $!abspath) }
    method inode(IO::Pathy:D:)    { FILETEST-INODE(   $!abspath) }

    method l-e(IO::Pathy:D:)        { True }
    method l-s(IO::Pathy:D:)        { FILETEST-ls(  $!abspath) }
    method l-modified(IO::Pathy:D:) { FILETEST-LMODIFIED($!abspath) }
    method l-accessed(IO::Pathy:D:) { FILETEST-LACCESSED($!abspath) }
    method l-changed(IO::Pathy:D:)  { FILETEST-LCHANGED( $!abspath) }
    method l-device(IO::Pathy:D:)   { FILETEST-LDEVICE(  $!abspath) }
    method l-inode(IO::Pathy:D:)    { FILETEST-LINODE(   $!abspath) }

    method rename(IO::Pathy:D: Str() $to, :$createonly) {
        my $topath := MAKE-ABSOLUTE-PATH($to,$*CWD.Str);
        RENAME-PATH($!abspath,$topath,:$createonly);
        $!abspath = $topath;  # have the object point to the new location
#        self!parts;  # runtime failure
        @!parts = $!abspath.split('/') unless @!parts;  # remove if above ok
    }
    method move(IO::Pathy:D: Str() $to, :$createonly) {
        my $topath := MAKE-ABSOLUTE-PATH($to,$*CWD.Str);
        MOVE-PATH($!abspath,$topath,:$createonly);
        $!abspath = $topath;  # have the object point to the new location
#        self!parts;  # runtime failure
        @!parts = $!abspath.split('/') unless @!parts;  # remove if above ok
    }
    method chmod(IO::Pathy:D: Int() $mode) {
        CHMOD-PATH($!abspath, $mode);
    }
    method symlink(IO::Pathy:D: Str() $name) {
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
