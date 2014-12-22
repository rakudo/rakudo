my class Instant  { ... }
my class IO::Dir  { ... }
my class IO::File  { ... }

my role IO {
    method umask { state $ = :8( qx/umask/.chomp ) }
}

#===============================================================================
# Primitives that may live in a role someday, if we figure out how to do that
# in a sensible and performant way.  Until then, these global subs with slightly
# obfuscated names, will have to do.  They should also provide excellent
# optimizing targets.

# create absolute path from forward slashed paths, abspath must be / terminated
sub MAKE-ABSOLUTE-PATH($path,$abspath) {
    my str $spath    = nqp::unbox_s($path);
    my str $sabspath = nqp::unbox_s($abspath);

    # simple /path
    if nqp::ordfirst($spath) == 47 {       # "/"
        return nqp::chars($sabspath) > 2   #      can have C:/
          && nqp::ordat($sabspath,1) == 58 # ":", assume C: something
          && nqp::ordat($sabspath,2) == 47 # "/", assume C:/ like prefix
          ?? nqp::box_s(nqp::concat(nqp::substr($sabspath,0,2),$spath),Str)
          !! $path;
    }

    # potential relative path
    if nqp::chars($spath) > 2 && nqp::ordat($spath,1) == 58 { # ":", like C:...
        return $path if nqp::ordat($spath,2) == 47;           # "/", like C:/...

        die "Can not set relative dir from different roots"
          if nqp::isne_s(nqp::substr($sabspath,0,2),nqp::substr($spath,0,2));

        return nqp::box_s(nqp::concat($sabspath,nqp::substr($spath,2)),Str);
    }

    # assume relative path
    nqp::box_s(nqp::concat($sabspath,$spath),Str);
}

sub FORWARD-SLASH(Str \path)  { TRANSPOSE-ONE(path,'\\','/') }
sub BACKWARD-SLASH(Str \path) { TRANSPOSE-ONE(path,'/','\\') }

sub TRANSPOSE-ONE(Str \path,\original,\final) {  # 500x faster than .trans
    my str $str   = nqp::unbox_s(path);
    my int $chars = nqp::chars($str);
    my int $ordinal = ord(original);
    my int $from;
    my int $to;
    my $parts := nqp::list_s();

    while $to < $chars {
        if nqp::ordat($str,$to) == $ordinal {
            nqp::push_s($parts, $to > $from
              ?? nqp::substr($str,$from,$to - $from)
              !! ''
            );
            $from = $to + 1;
        }
        $to = $to + 1;
    }
    nqp::push_s( $parts, $from < $chars
      ?? nqp::substr($str,$from,$chars - $from)
      !! ''
    );

    nqp::elems($parts)
      ?? nqp::box_s(nqp::join(nqp::unbox_s(final),$parts),Str)
      !! path;
}

sub MAKE-BASENAME(Str $abspath) {
    my str $abspath_s = nqp::unbox_s($abspath);
    my int $offset    = nqp::rindex($abspath_s,'/');
    nqp::p6bool($offset == -1)
      ?? $abspath
      !! nqp::box_s(nqp::substr($abspath_s,$offset + 1),Str);
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
    @parts.push("") if $elems == 1 || @parts.at_pos($elems - 1) ne "";
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
    FILETEST-E($abspath) && FILETEST-D($abspath) && test($abspath)
      ?? IO::Dir.new(:$abspath)
      !! fail X::IO::Chdir.new(
           :$path,
           :os-error( "does not exist, is not a dir or no access" ),
         );
}

sub COPY-FILE(Str $from, Str $to, :$createonly) {
    if $createonly and FILETEST-E($to) {
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
    if $createonly and FILETEST-E($to) {
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
    if $createonly and FILETEST-E($path) {
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

sub FILETEST-E(Str $abspath) {
    nqp::p6bool( nqp::stat(nqp::unbox_s($abspath),nqp::const::STAT_EXISTS) );
}
sub FILETEST-D(Str $abspath) {
    nqp::p6bool( nqp::stat(nqp::unbox_s($abspath),nqp::const::STAT_ISDIR) );
}
sub FILETEST-F(Str $abspath) {
    nqp::p6bool( nqp::stat(nqp::unbox_s($abspath),nqp::const::STAT_ISREG) );
}
sub FILETEST-S(Str $abspath) {
    nqp::p6box_i(nqp::stat(nqp::unbox_s($abspath),nqp::const::STAT_FILESIZE) );
}
sub FILETEST-L(Str $abspath) {
    nqp::p6bool(nqp::fileislink(nqp::unbox_s($abspath)));
}
sub FILETEST-R(Str $abspath) {
    nqp::p6bool(nqp::filereadable(nqp::unbox_s($abspath)));
}
sub FILETEST-W(Str $abspath) {
    nqp::p6bool(nqp::filewritable(nqp::unbox_s($abspath)));
}
sub FILETEST-RW(Str $abspath) {
    my str $p = nqp::unbox_s($abspath);
    nqp::p6bool(nqp::filereadable($p) && nqp::filewritable($p));
}
sub FILETEST-X(Str $abspath) {
    nqp::p6bool(nqp::fileexecutable(nqp::unbox_s($abspath)));
}
sub FILETEST-RX(Str $abspath) {
    my str $p = nqp::unbox_s($abspath);
    nqp::p6bool(nqp::filereadable($p) && nqp::fileexecutable($p));
}
sub FILETEST-WX(Str $abspath) {
    my str $p = nqp::unbox_s($abspath);
    nqp::p6bool(nqp::filewritable($p) && nqp::fileexecutable($p));
}
sub FILETEST-RWX(Str $abspath) {
    my str $p = nqp::unbox_s($abspath);
    nqp::p6bool(
      nqp::filereadable($p) && nqp::filewritable($p) && nqp::fileexecutable($p)
    );
}
sub FILETEST-Z(Str $abspath) {
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

sub DIR-GATHER(Str $abspath,Mu $test) {
    gather {
        for MAKE-DIR-LIST($abspath,$test) -> $elem {
            take FILETEST-D($elem)
              ?? IO::Dir.new(:abspath($elem ~ '/'))
              !! IO::File.new(:abspath($elem));
        }
    }
}

sub DIR-GATHER-STR(Str $abspath,Mu $test) {
    gather {
        for MAKE-DIR-LIST($abspath,$test) -> $elem {
            take FILETEST-D($elem)
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

#?if parrot
sub MAKE-DIR-LIST(Str $abspath, Mu $test) {

    CATCH { default {
        fail X::IO::Dir.new(
          :path(nqp::box_s($abspath,Str)), :os-error(.Str) );
    } }

    my Mu $RSA := pir::new__PS('OS').readdir(nqp::unbox_s($abspath));
    my int $elems = nqp::elems($RSA);

    if $test.defined {
        gather loop (my int $i = 0; $i < $elems; $i = $i + 1) {
            my Str $elem = FORWARD-SLASH(nqp::p6box_s(pir::trans_encoding__Ssi(
              nqp::atpos_s($RSA, $i),
              pir::find_encoding__Is('utf8'))));
            take $abspath ~ $elem if $test.ACCEPTS($elem);
        }
    }
    else {
        gather loop (my int $i = 0; $i < $elems; $i = $i + 1) {
            my str $elem_s = pir::trans_encoding__Ssi(
              nqp::atpos_s($RSA, $i),
              pir::find_encoding__Is('utf8'));
            take $abspath ~ FORWARD-SLASH(nqp::box_s($elem_s,Str))
              if nqp::isne_s($elem_s,'.') && nqp::isne_s($elem_s,'..');
        }
    }
}
#?endif

# vim: ft=perl6 expandtab sw=4
