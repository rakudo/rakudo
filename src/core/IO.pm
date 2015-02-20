my class Instant   { ... }
my class IO::File  { ... }
my class IO::Dir   { ... }
my class IO::Local { ... }

my role IO {
    method umask { state $ = :8( qx/umask/.chomp ) }
}

#===============================================================================
# Primitives that may live in a role someday, if we figure out how to do that
# in a sensible and performant way.  Until then, these global subs with slightly
# obfuscated names, will have to do.  They should also provide excellent
# optimizing targets.

# create absolute path from forward slashed paths, abspath must be / terminated
sub MAKE-ABSOLUTE-PATH(Str $path, Str $abspath) {
    my str $spath    = nqp::unbox_s($path);
    my str $sabspath = nqp::unbox_s($abspath);

    # simple /path
    return nqp::chars($sabspath) > 2   #      can have C:/
      && nqp::ordat($sabspath,1) == 58 # ":", assume C: something
      && nqp::ordat($sabspath,2) == 47 # "/", assume C:/ like prefix
      ?? nqp::box_s(nqp::concat(nqp::substr($sabspath,0,2),$spath),Str)
      !! $path
      if nqp::ordfirst($spath) == 47;  # "/"

    # need trailing slash for abspath now if there is none yet
    $sabspath = nqp::concat($abspath,'/')
      if nqp::ordat($sabspath,nqp::chars($sabspath) - 1) != 47;  # "/"

    # assume relative path
    return nqp::box_s(nqp::concat($sabspath,$spath),Str)
      if nqp::chars($spath) <= 2 || nqp::ordat($spath,1) != 58; # ":", like C:..

    # absolute path
    return $path if nqp::ordat($spath,2) == 47;           # "/", like C:/...

    die "Can not set relative dir from different roots"
      if nqp::isne_s(nqp::substr($sabspath,0,2),nqp::substr($spath,0,2));

    # relative path on same drive
    nqp::box_s(nqp::concat($sabspath,nqp::substr($spath,2)),Str);
}

sub FORWARD-SLASH(Str \path)  { TRANSPOSE-ONE(path,'\\','/') }
sub BACKWARD-SLASH(Str \path) { TRANSPOSE-ONE(path,'/','\\') }

sub MAKE-BASENAME(Str $abspath) {
    my str $abspath_s = nqp::unbox_s($abspath);
    my int $offset    = nqp::rindex($abspath_s,'/');
    nqp::p6bool($offset == -1)
      ?? $abspath
      !! nqp::box_s(nqp::substr($abspath_s,$offset + 1),Str);
}

sub MAKE-PARENT(Str $abspath) {
    my str $abspath_s = nqp::unbox_s($abspath);
    my int $offset    = nqp::rindex($abspath_s,'/');
    nqp::p6bool($offset == -1)
      ?? $abspath
      !! nqp::box_s(nqp::substr($abspath_s,0,$offset + 1),Str);
}

sub MAKE-EXT(Str $basename) {
    my str $basename_s = nqp::unbox_s($basename);
    my int $offset     = nqp::rindex($basename_s,'.');
    nqp::p6bool($offset == -1)
      ?? ''
      !! nqp::box_s(nqp::substr($basename_s,$offset + 1),Str);
}

my %CLEAN-PARTS-NUL = 'Str|..' => 1, 'Str|.' => 1, 'Str|' => 1;
sub MAKE-CLEAN-PARTS(Str $abspath) {
    my @parts = $abspath.split('/');

    # handle //unc/ on win
    @parts.unshift( @parts.splice(0,3).join('/') )
      if @parts.elems >= 3        # //unc
      && @parts.at_pos(1) eq ''   # //
      && @parts.at_pos(0) eq '';  # and no C: like stuff

    # front part cleanup
    @parts.splice(1,1)
      while %CLEAN-PARTS-NUL.exists_key(@parts.at_pos(1).WHICH);

    # recursive ".." and "." handling
    sub updirs($index is copy) {

        # the end
        if $index == 1 {
            @parts.splice(1,1);
            return 1;
        }

        # something to check
        elsif @parts.at_pos($index - 1) -> $part {
            if $part.ord == 46 { # fast $part.substr(0,1) eq '.'
                if $part eq '..' {
                    return updirs($index - 1);
                }
                elsif $part eq '.' {
                    @parts.splice($index,1);
                    return updirs($index - 1);
                }
            }
            @parts.splice(--$index,2);
            return $index;
        }

        # nul, just ignore
        else {
            @parts.splice($index--,1);
            return updirs($index);
        }
    }

    # back part cleanup
    my Int $checks = @parts.end;
    while $checks > 1 {
        if @parts.at_pos($checks) -> $part {
            $part eq '..'
              ?? $checks = updirs($checks)
              !! $part eq '.'
                ?? @parts.splice($checks--, 1)
                !! $checks--;
        }
        else {
            @parts.splice($checks--, 1);
        }
    }


    # back part cleanup
    @parts.pop
      while $checks
        && %CLEAN-PARTS-NUL.exists_key(@parts.at_pos($checks--).WHICH);

    # need (at least) / at the end
    my $elems := @parts.elems;
    if $elems == 1 {
        @parts.push("");
    }
    elsif @parts.at_pos($elems - 1) eq "" {
        @parts.pop;
    }
    @parts;
}

sub REMOVE-ROOT(Str $r, Str $p) {
    my str $root  = nqp::unbox_s($r);
    my str $path  = nqp::unbox_s($p);
    my int $chars = nqp::chars($root);

    return $p if $chars >= nqp::chars($path);  # makes no sense

    my int $i;
    while $i < $chars && nqp::ordat($root,$i) == nqp::ordat($path,$i) {
        $i = $i + 1;
    }

    $i == $chars ?? nqp::box_s(nqp::substr($path,$i),Str) !! $p;
}

sub CHANGE-DIRECTORY($path,$base,&test) {

    my $abspath := MAKE-CLEAN-PARTS(
      MAKE-ABSOLUTE-PATH(FORWARD-SLASH($path),$base)
    ).join('/');
    FILETEST-e($abspath) && FILETEST-d($abspath) && test($abspath)
      ?? IO::Dir.new(:$abspath)
      !! fail X::IO::Chdir.new(
           :$path,
           :os-error( "does not exist, is not a dir or no access" ),
         );
}

sub COPY-FILE(Str $from, Str $to, :$createonly) {
    if $createonly and FILETEST-e($to) {
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
    True;
}

sub RENAME-PATH(Str $from, Str $to, :$createonly) {
    if $createonly and FILETEST-e($to) {
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
    True;
}

sub MOVE-PATH(Str \from, Str \to, :$createonly) {
    my str $to = nqp::unbox_s(to);
    if FILETEST-e(to) {
        $to = $to ~ '/' ~ MAKE-BASENAME(from) if FILETEST-d(to);
        fail X::IO::Move.new(
          :from(from),
          :$to,
          :os-error(':createonly specified and destination exists'),
        ) if $createonly and FILETEST-e(nqp::p6box_s($to));
    }

    fail X::IO::Move.new(
      :from(from),
      :$to,
      :os-error('Can only move regular files'),
    ) if FILETEST-e(from) && !FILETEST-f(from);

    my str $from = nqp::unbox_s(from);
    return True unless nqp::rename($from,$to);

    CATCH { default {
        nqp::copy($from,$to);
        if FILETEST-e($to) {
            nqp::unlink($from);
            return True;
        }
        CATCH { default {
            fail X::IO::Move.new( :$from, :$to, :os-error(.Str) );
        } }
    } }
    True;
}

sub CHMOD-PATH(Str $path, Int $mode) {
    nqp::chmod(nqp::unbox_s($path), nqp::unbox_i($mode));
    CATCH { default {
        fail X::IO::Chmod.new( :$path, :$mode, :os-error(.Str) );
    } }
    True;
}

sub UNLINK-PATH(Str $path) {
    nqp::unlink(nqp::unbox_s($path));
    CATCH { default {
        fail X::IO::Unlink.new( :$path, :os-error(.Str) );
    } }
    True;
}

sub SYMLINK-PATH(Str $target, Str $name) {
    nqp::symlink(nqp::unbox_s($name), nqp::unbox_s($target));
    CATCH { default {
        fail X::IO::Symlink.new( :$target, :$name, :os-error(.Str) );
    } }
    True;
}

sub LINK-FILE(Str $target, Str $name) {
    nqp::link(nqp::unbox_s($name), nqp::unbox_s($target));
    CATCH { default {
        fail X::IO::Link.new( :$target, :$name, :os-error(.Str) );
    } }
    True;
}

sub SLURP-PATH(Str $path,:$encoding,:$bin,:$enc,|c) {
    my $handle = open($path,:encoding($encoding // $enc // 'utf8'),:$bin,:r,|c);
    $handle // $handle.throw;

    my \slurped = $handle.slurp-rest(:$bin,:$encoding,:$enc);
    $handle.close;
    slurped;
}

sub SPURT-PATH(Str $path,\what,:$encoding,:$append,:$createonly,:$bin,:$enc,|c) {
    if $createonly and FILETEST-e($path) {
        fail("File '$path' already exists, and :createonly was specified");
    }
    my $mode = $append ?? :a !! :w;
    my $handle =
      open($path,:encoding($encoding // $enc // 'utf8'),:$bin,|$mode,|c);
    $handle // $handle.throw;

    my $spurt := $bin
      ?? $handle.write(what)
      !! $handle.print(what);
    $handle.close;  # can't use LEAVE in settings :-(
    $spurt;
}

sub READLINK-ABSPATH(Str $abspath) {
    MAKE-ABSOLUTE-PATH(
      nqp::p6box_s(nqp::readlink(nqp::unbox_s($abspath))),
      MAKE-PARENT($abspath),
    );
}

sub MAKE-DIR(Str $path, Int $mode) {
    nqp::mkdir(nqp::unbox_s($path), nqp::unbox_i($mode));
    CATCH { default {
        fail X::IO::Mkdir.new(:$path, :$mode, os-error => .Str);
    } }
    True;
}

sub REMOVE-DIR(Str $path) {
    nqp::rmdir(nqp::unbox_s($path));
    CATCH { default {
        fail X::IO::Rmdir.new(:$path, os-error => .Str);
    } }
    True;
}

sub FILETEST-e(Str $abspath) {
    nqp::p6bool( nqp::stat(nqp::unbox_s($abspath),nqp::const::STAT_EXISTS) );
}
sub FILETEST-d(Str $abspath) {
    nqp::p6bool( nqp::stat(nqp::unbox_s($abspath),nqp::const::STAT_ISDIR) );
}
sub FILETEST-f(Str $abspath) {
    nqp::p6bool( nqp::stat(nqp::unbox_s($abspath),nqp::const::STAT_ISREG) );
}
sub FILETEST-s(Str $abspath) {
    nqp::p6box_i(nqp::stat(nqp::unbox_s($abspath),nqp::const::STAT_FILESIZE) );
}
sub FILETEST-l(Str $abspath) {
#?if moar
    nqp::p6bool(nqp::lstat(nqp::unbox_s($abspath),nqp::const::STAT_ISLNK));
#?endif
#?if !moar
    nqp::p6bool(nqp::fileislink(nqp::unbox_s($abspath)));  # no nqp::lstat yet
#?endif
}
sub FILETEST-r(Str $abspath) {
    nqp::p6bool(nqp::filereadable(nqp::unbox_s($abspath)));
}
sub FILETEST-w(Str $abspath) {
    nqp::p6bool(nqp::filewritable(nqp::unbox_s($abspath)));
}
sub FILETEST-rw(Str $abspath) {
    my str $p = nqp::unbox_s($abspath);
    nqp::p6bool(nqp::filereadable($p) && nqp::filewritable($p));
}
sub FILETEST-x(Str $abspath) {
    nqp::p6bool(nqp::fileexecutable(nqp::unbox_s($abspath)));
}
sub FILETEST-rx(Str $abspath) {
    my str $p = nqp::unbox_s($abspath);
    nqp::p6bool(nqp::filereadable($p) && nqp::fileexecutable($p));
}
sub FILETEST-wx(Str $abspath) {
    my str $p = nqp::unbox_s($abspath);
    nqp::p6bool(nqp::filewritable($p) && nqp::fileexecutable($p));
}
sub FILETEST-rwx(Str $abspath) {
    my str $p = nqp::unbox_s($abspath);
    nqp::p6bool(
      nqp::filereadable($p) && nqp::filewritable($p) && nqp::fileexecutable($p)
    );
}
sub FILETEST-z(Str $abspath) {
    nqp::p6bool(nqp::stat(nqp::unbox_s($abspath),nqp::const::STAT_FILESIZE)==0);
}
sub FILETEST-MODIFIED(Str $abspath) {
    Instant.new( nqp::p6box_i(
      nqp::stat(nqp::unbox_s($abspath), nqp::const::STAT_MODIFYTIME)
    ));
}
sub FILETEST-ACCESSED(Str $abspath) {
    Instant.new( nqp::p6box_i(
      nqp::stat(nqp::unbox_s($abspath), nqp::const::STAT_ACCESSTIME)
    ));
}
sub FILETEST-CHANGED(Str $abspath) {
    Instant.new( nqp::p6box_i(
      nqp::stat(nqp::unbox_s($abspath), nqp::const::STAT_CHANGETIME)
    ));
}
sub FILETEST-UID(Str $abspath) {
    nqp::p6box_i(nqp::stat(nqp::unbox_s($abspath),nqp::const::STAT_UID) );
}
sub FILETEST-GID(Str $abspath) {
    nqp::p6box_i(nqp::stat(nqp::unbox_s($abspath),nqp::const::STAT_GID) );
}
sub FILETEST-INODE(Str $abspath) {
    nqp::p6box_i(nqp::stat(nqp::unbox_s($abspath),nqp::const::STAT_PLATFORM_INODE) );
}
sub FILETEST-DEVICE(Str $abspath) {
    nqp::p6box_i(nqp::stat(nqp::unbox_s($abspath),nqp::const::STAT_PLATFORM_DEV) );
}

sub FILETEST-le(Str $abspath) {
#?if moar
    nqp::p6bool( nqp::lstat(nqp::unbox_s($abspath),nqp::const::STAT_EXISTS) );
#?endif
#?if !moar
    FILETEST-e($abspath);  # no nqp::lstat yet
#?endif
}
sub FILETEST-ls(Str $abspath) {
#?if moar
    nqp::p6box_i(nqp::lstat(nqp::unbox_s($abspath),nqp::const::STAT_FILESIZE) );
#?endif
#?if !moar
    FILETEST-s($abspath);  # no nqp::lstat yet
#?endif
}
sub FILETEST-LMODIFIED(Str $abspath) {
#?if moar
    Instant.new( nqp::p6box_i(
      nqp::lstat(nqp::unbox_s($abspath), nqp::const::STAT_MODIFYTIME)
    ));
#?endif
#?if !moar
    FILETEST-MODIFIED($abspath);  # no nqp::lstat yet
#?endif
}
sub FILETEST-LACCESSED(Str $abspath) {
#?if moar
    Instant.new( nqp::p6box_i(
      nqp::lstat(nqp::unbox_s($abspath), nqp::const::STAT_ACCESSTIME)
    ));
#?endif
#?if !moar
    FILETEST-ACCESSED($abspath);  # no nqp::lstat yet
#?endif
}
sub FILETEST-LCHANGED(Str $abspath) {
#?if moar
    Instant.new( nqp::p6box_i(
      nqp::lstat(nqp::unbox_s($abspath), nqp::const::STAT_CHANGETIME)
    ));
#?endif
#?if !moar
    FILETEST-CHANGED($abspath);  # no nqp::lstat yet
#?endif
}
sub FILETEST-LINODE(Str $abspath) {
#?if moar
    nqp::p6box_i(nqp::lstat(nqp::unbox_s($abspath),nqp::const::STAT_PLATFORM_INODE) );
#?endif
#?if !moar
    FILETEST-INODE($abspath);  # no nqp::lstat yet
#?endif
}
#?if moar
sub FILETEST-LDEVICE(Str $abspath) {
    nqp::p6box_i(nqp::lstat(nqp::unbox_s($abspath),nqp::const::STAT_PLATFORM_DEV) );
#?endif
#?if !moar
    FILETEST-DEVICE($abspath);  # no nqp::lstat yet
#?endif
}

sub OBJECTIFY-ABSPATH(Str $abspath, :$check = True) {
    FILETEST-f($abspath)
      ?? IO::File.new(:$abspath)
      !! FILETEST-d($abspath)
        ?? IO::Dir.new(:abspath($abspath ~ '/'), :$check)
        !! IO::Local.new(:$abspath);
}
sub DIR-GATHER(Str $abspath,Mu $test) {
    gather {
        for MAKE-DIR-LIST($abspath,$test) -> $elem {
            take OBJECTIFY-ABSPATH($elem,:!check);
        }
    }
}

sub DIR-GATHER-STR(Str $abspath,Mu $test) {
    gather {
        for MAKE-DIR-LIST($abspath,$test) -> $elem {
            take FILETEST-d($elem) ?? $elem ~ '/' !! $elem;
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
    nqp::chdir(nqp::unbox_s($abspath));
    my Mu $dirh := nqp::opendir(nqp::unbox_s($abspath));
    nqp::chdir($cwd);

    if $test.defined {
        gather { loop {
            my str $elem_s = nqp::nextfiledir($dirh);
            if nqp::isnull_s($elem_s) || nqp::chars($elem_s) == 0 {
                nqp::closedir($dirh);
                last;
            }
            my Str $elem = FORWARD-SLASH(nqp::box_s($elem_s,Str));
            take $abspath ~ $elem if $test.ACCEPTS($elem);
        } }
    }
    else {
        gather { loop {
            my str $elem_s = nqp::nextfiledir($dirh);
            if nqp::isnull_s($elem_s) || nqp::chars($elem_s) == 0 {
                nqp::closedir($dirh);
                last;
            }
            take $abspath ~ FORWARD-SLASH(nqp::box_s($elem_s,Str))
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

    my Mu $dirh := nqp::opendir(nqp::unbox_s($abspath));
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
                my Str $elem = FORWARD-SLASH(nqp::box_s($elem_s,Str));
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
                take FORWARD-SLASH(nqp::box_s($elem_s,Str));
            }
        }
    }
}
#?endif

# vim: ft=perl6 expandtab sw=4
