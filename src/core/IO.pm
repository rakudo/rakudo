my class Instant  { ... }
my class IO::Dir  { ... }
my class IO::File  { ... }

my role IO {
    method umask { state $ = :8( qx/umask/.chomp ) }
}

enum SeekType (
  :SeekFromBeginning(0),
  :SeekFromCurrent(1),
  :SeekFromEnd(2),
);
enum ProtocolFamily (
  :PF_LOCAL(0),
  :PF_UNIX(1),
  :PF_INET(2),
  :PF_INET6(3),
  :PF_MAX(4),
);
enum SocketType (
  :SOCK_PACKET(0),
  :SOCK_STREAM(1),
  :SOCK_DGRAM(2),
  :SOCK_RAW(3),
  :SOCK_RDM(4),
  :SOCK_SEQPACKET(5),
  :SOCK_MAX(6),
);
enum ProtocolType (
  :PROTO_TCP(6),
  :PROTO_UDP(17),
);

#===============================================================================
# Primitives that may live in a role someday, if we figure out how to do that
# in a sensible and performant way.  Until then, these global subs with slightly
# obfuscated names, will have to do.  They should also provide excellent
# optimizing targets.

sub CHANGE-DIRECTORY($path,$base,&test) {

    my $abspath = Rakudo::Internals.MAKE-CLEAN-PARTS(
      Rakudo::Internals.MAKE-ABSOLUTE-PATH($path,$base)).join('/');
    Rakudo::Internals.FILETEST-E($abspath)
      && Rakudo::Internals.FILETEST-D($abspath)
      && test($abspath)
      ?? IO::Path.new-from-absolute-path($abspath.chop)
      !! fail X::IO::Chdir.new(
           :$path, :os-error( "does not exist, is not a dir or no access" ))
}

sub COPY-FILE(Str $from, Str $to, :$createonly --> True) {
    if $createonly && Rakudo::Internals.FILETEST-E($to) {
        fail X::IO::Copy.new(
          :$from,
          :$to,
          :os-error(':createonly specified and destination exists'),
        );
    }

    nqp::copy(nqp::unbox_s($from), nqp::unbox_s($to));
    CATCH { default {
        fail X::IO::Copy.new( :$from, :$to, :os-error(.Str) );
    } }
}

sub RENAME-PATH(Str $from, Str $to, :$createonly --> True) {
    if $createonly && Rakudo::Internals.FILETEST-E($to) {
        fail X::IO::Rename.new(
          :$from,
          :$to,
          :os-error(':createonly specified and destination exists'),
        );
    }

    nqp::rename(nqp::unbox_s($from), nqp::unbox_s($to));
    CATCH { default {
        fail X::IO::Rename.new( :$from, :$to, :os-error(.Str) );
    } }
}

sub CHMOD-PATH(Str $path, Int $mode --> True) {
    nqp::chmod(nqp::unbox_s($path), nqp::unbox_i($mode));
    CATCH { default {
        fail X::IO::Chmod.new( :$path, :$mode, :os-error(.Str) );
    } }
}

sub UNLINK-PATH(Str $path --> True) {
    nqp::unlink(nqp::unbox_s($path));
    CATCH { default {
        fail X::IO::Unlink.new( :$path, :os-error(.Str) );
    } }
}

sub SYMLINK-PATH(Str $target, Str $name --> True) {
    nqp::symlink(nqp::unbox_s($name), nqp::unbox_s($target));
    CATCH { default {
        fail X::IO::Symlink.new( :$target, :$name, :os-error(.Str) );
    } }
}

sub LINK-FILE(Str $target, Str $name --> True) {
    nqp::link(nqp::unbox_s($name), nqp::unbox_s($target));
    CATCH { default {
        fail X::IO::Link.new( :$target, :$name, :os-error(.Str) );
    } }
}

sub MAKE-DIR(Str $path, Int $mode --> True) {
    nqp::mkdir(nqp::unbox_s($path), nqp::unbox_i($mode));
    CATCH { default {
        fail X::IO::Mkdir.new(:$path, :$mode, os-error => .Str);
    } }
}

sub REMOVE-DIR(Str $path --> True) {
    nqp::rmdir(nqp::unbox_s($path));
    CATCH { default {
        fail X::IO::Rmdir.new(:$path, os-error => .Str);
    } }
}

sub DIR-GATHER(Str $abspath,Mu $test) {
    gather {
        for MAKE-DIR-LIST($abspath,$test) -> $elem {
            take Rakudo::Internals.FILETEST-D($elem)
              ?? IO::Dir.new(:abspath($elem ~ '/'))
              !! IO::File.new(:abspath($elem));
        }
    }
}

sub DIR-GATHER-STR(Str $abspath,Mu $test) {
    gather {
        for MAKE-DIR-LIST($abspath,$test) -> $elem {
            take Rakudo::Internals.FILETEST-D($elem)
              ?? $elem ~ '/'
              !! $elem;
        }
    }
}

#?if moar
sub MAKE-DIR-LIST(Str $abspath, Mu $test) {

    CATCH { default {
        fail X::IO::Dir.new(
          :path(nqp::box_s($abspath,Str)), :os-error(.Str) );
    } }

    my str $cwd = nqp::cwd();
    nqp::chdir(nqp::unbox_s($abspath.chop));
    my Mu $dirh := nqp::opendir(nqp::unbox_s($abspath));
    nqp::chdir($cwd);

    if $test.defined {
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
sub MAKE-DIR-LIST(Str $abspath, Mu $test) {

    CATCH { default {
        fail X::IO::Dir.new(
          :path(nqp::box_s($abspath,Str)), :os-error(.Str) );
    } }

    my Mu $dirh := nqp::opendir(nqp::unbox_s($abspath.chop));
    gather {
        if $test.defined {
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
                take $elem
                  if $test.ACCEPTS(Rakudo::Internals.MAKE-BASENAME($elem));
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

