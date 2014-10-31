my class Instant { ... }
my role IO {
    method umask { state $ = :8( qx/umask/.chomp ) }
}

# Primitives that may live in a role someday, if we figure out how to do that
# in a sensible and performant way.  Until then, these global subs with slightly
# obfuscated names, will have to do.  They should also provide excellent
# optimizing targets.

sub MAKE-ABSOLUTE-PATH($path,$abspath) {
    if $path.ord == 47 {              # 4x faster $path.substr(0,1) eq "/"
        return $path;
    }
    elsif $path.substr(1,1) eq ':' {  # assume C: something
        if $path.substr(2,1) eq "/" { #  assume C:/ like prefix
            return $path;
        }
        elsif $abspath.substr(0,2) ne $path.substr(0,2) {
            die "Can not set relative dir from different roots";
        }
        else {
            return $abspath ~ $path.substr(2);
        }
    }
    else {                            # assume relative path
        return $abspath ~ $path;
    }
}

sub FILETEST-E($abspath) {
    nqp::p6bool( nqp::stat(nqp::unbox_s($abspath),nqp::const::STAT_EXISTS) );
}
sub FILETEST-D($abspath) {
    nqp::p6bool( nqp::stat(nqp::unbox_s($abspath),nqp::const::STAT_ISDIR) );
}
sub FILETEST-F($abspath) {
    nqp::p6bool( nqp::stat(nqp::unbox_s($abspath),nqp::const::STAT_ISREG) );
}
sub FILETEST-S($abspath) {
    nqp::p6box_i(nqp::stat(nqp::unbox_s($abspath),nqp::const::STAT_FILESIZE) );
}
sub FILETEST-L($abspath) {
    nqp::p6bool(nqp::fileislink(nqp::unbox_s($abspath)));
}
sub FILETEST-R($abspath) {
    nqp::p6bool(nqp::filereadable(nqp::unbox_s($abspath)));
}
sub FILETEST-W($abspath) {
    nqp::p6bool(nqp::filewritable(nqp::unbox_s($abspath)));
}
sub FILETEST-RW($abspath) {
    my str $p = nqp::unbox_s($abspath);
    nqp::p6bool(nqp::filereadable($p) & nqp::filewritable($p));
}
sub FILETEST-X($abspath) {
    nqp::p6bool(nqp::fileexecutable(nqp::unbox_s($abspath)));
}
sub FILETEST-RWX($abspath) {
    my str $p = nqp::unbox_s($abspath);
    nqp::p6bool(
      nqp::filereadable($p) & nqp::filewritable($p) & nqp::fileexecutable($p)
    );
}
sub FILETEST-Z($abspath) {
    nqp::p6bool(nqp::stat(nqp::unbox_s($abspath),nqp::const::STAT_FILESIZE)==0);
}
sub FILETEST-MODIFIED($abspath) {
    Instant.new( nqp::p6box_i(
      nqp::stat(nqp::unbox_s($abspath), nqp::const::STAT_MODIFYTIME)
    ));
}
sub FILETEST-ACCESSED($abspath) {
    Instant.new( nqp::p6box_i(
      nqp::stat(nqp::unbox_s($abspath), nqp::const::STAT_ACCESSTIME)
    ));
}
sub FILETEST-CHANGED($abspath) {
    Instant.new( nqp::p6box_i(
      nqp::stat(nqp::unbox_s($abspath), nqp::const::STAT_CHANGETIME)
    ));
}
