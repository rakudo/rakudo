my class Instant { ... }
my role IO {
    method umask { state $ = :8( qx/umask/.chomp ) }
}

#===============================================================================
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

sub MAKE-BASENAME($abspath) {
    my str $abspath_s = nqp::unbox_s($abspath);
    my int $index     = nqp::rindex($abspath_s,'/');
    nqp::p6bool($index == -1)
      ?? $abspath
      !! nqp::box_s(nqp::substr($abspath_s,$index + 1),Str);
}

sub MAKE-EXTENSION($basename) {
    my str $basename_s = nqp::unbox_s($basename);
    my int $index      = nqp::rindex($basename_s,'.');
    nqp::p6bool($index == -1)
      ?? ''
      !! nqp::box_s(nqp::substr($basename_s,$index + 1),Str);
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

#?if moar
sub MAKE-DIR-LIST($abspath, Mu :$test) {

    CATCH { default {
        fail X::IO::Dir.new(
          :path(nqp::box_s($abspath,Str)), :os-error(.Str) );
    } }

    my str $cwd = nqp::cwd();
    nqp::chdir(nqp::unbox_s($abspath.chop));
    my Mu $dirh := nqp::opendir(nqp::unbox_s($abspath));
    nqp::chdir($cwd);

    if $test {
        gather { loop {
            my str $elem_s = nqp::nextfiledir($dirh);
            if nqp::isnull_s($elem_s) || nqp::chars($elem_s) == 0 {
                nqp::closedir($dirh);
                last;
            }
            my Str $elem = nqp::box_s($elem_s,Str);
            take $abspath ~ $elem if $test.ACCEPTS($elem);
        } }
    }
    else {
        my str $abspath_s = $abspath;
        gather { loop {
            my str $elem_s = nqp::nextfiledir($dirh);
            if nqp::isnull_s($elem_s) || nqp::chars($elem_s) == 0 {
                nqp::closedir($dirh);
                last;
            }
            take nqp::box_s(nqp::concat($abspath_s,$elem_s),Str)
              if nqp::isne_s($elem_s,'.') && nqp::isne_s($elem_s,'..');
        } }
    }
}
#?endif

#?if jvm
sub MAKE-DIR-LIST($abspath, Mu :$test) {

    CATCH { default {
        fail X::IO::Dir.new(
          :path(nqp::box_s($abspath,Str)), :os-error(.Str) );
    } }

    my Mu $dirh := nqp::opendir(nqp::unbox_s($abspath.chop));
    gather {
        if $test {
            for <. ..> -> $elem {
                take $abspath ~ $elem if $test.ACCEPTS($elem);
            }
            loop {
                my str $elem_s = nqp::nextfiledir($dirh);
                if nqp::isnull_s($elem_s) || nqp::chars($elem_s) == 0 {
                    nqp::closedir($dirh);
                    last;
                }
                my Str $elem = nqp::box_s($elem_s,Str);
                take $elem if $test.ACCEPTS(MAKE-BASENAME($elem));
            }
        }
        else {
            loop {
                my str $elem_s = nqp::nextfiledir($dirh);
                if nqp::isnull_s($elem_s) || nqp::chars($elem_s) == 0 {
                    nqp::closedir($dirh);
                    last;
                }
                take nqp::box_s($elem_s,Str);
            }
        }
    }
}
#?endif

#?if parrot
sub MAKE-DIR-LIST($abspath, Mu :$test) {

    CATCH { default {
        fail X::IO::Dir.new(
          :path(nqp::box_s($abspath,Str)), :os-error(.Str) );
    } }

    my Mu $RSA := pir::new__PS('OS').readdir(nqp::unbox_s($abspath));
    my int $elems = nqp::elems($RSA);

    if $test {
        gather loop (my int $i = 0; $i < $elems; $i = $i + 1) {
            my Str $elem = nqp::p6box_s(pir::trans_encoding__Ssi(
              nqp::atpos_s($RSA, $i),
              pir::find_encoding__Is('utf8')));
            take $abspath ~ $elem if $test.ACCEPTS($elem);
        }
    }
    else {
        my str $abspath_s = nqp::unbox_s($abspath);
        gather loop (my int $i = 0; $i < $elems; $i = $i + 1) {
            my str $elem_s = pir::trans_encoding__Ssi(
              nqp::atpos_s($RSA, $i),
              pir::find_encoding__Is('utf8'));
            take nqp::box_s(nqp::concat($abspath_s,$elem_s),Str)
              if nqp::isne_s($elem_s,'.') && nqp::isne_s($elem_s,'..');
        }
    }
}
#?endif
