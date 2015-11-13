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

sub MAKE-ABSOLUTE-PATH($path,$abspath) {
    if $path.ord == 47 {              # 4x faster substr($path,0,1) eq "/"
        return $path;
    }
    elsif $path.substr-eq(":",1) {  # assume C: something
        if $path.substr-eq("/",2) { #  assume C:/ like prefix
            return $path;
        }
        elsif !$abspath.starts-with(substr($path,0,2)) {
            die "Can not set relative dir from different roots";
        }
        else {
            return $abspath ~ substr($path,2);
        }
    }
    else {                            # assume relative path
        return $abspath ~ $path;
    }
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
      if @parts.AT-POS(1) eq ''    # //
      and @parts.AT-POS(0) eq '';  # and no C: like stuff

    # front part cleanup
    @parts.splice(1,1)
      while %CLEAN-PARTS-NUL.EXISTS-KEY(@parts.AT-POS(1).WHICH);

    # recursive ".." and "." handling
    sub updirs($index is copy) {

        # the end
        if $index == 1 {
            @parts.splice(1,1);
            return 1;
        }

        # something to check
        elsif @parts.AT-POS($index - 1) -> $part {
            if $part.ord == 46 { # fast substr($part,0,1) eq '.'
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
            @parts.splice($index,1);
            return updirs($index);
        }
    }

    # back part cleanup
    my Int $checks = @parts.end;
    while $checks > 1 {
        if @parts.AT-POS($checks) -> $part {
            $part eq '..'
              ?? ($checks = updirs($checks))
              !! $part eq '.'
                ?? @parts.splice($checks--, 1)
                !! $checks--;
        }
        else {
            @parts.splice($checks--, 1);
        }
    }

    # need / at the end
    @parts.push("");
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

    my $abspath = MAKE-CLEAN-PARTS(MAKE-ABSOLUTE-PATH($path,$base)).join('/');
    FILETEST-E($abspath) && FILETEST-D($abspath) && test($abspath)
      ?? IO::Path.new-from-absolute-path($abspath.chop)
      !! fail X::IO::Chdir.new(
           :$path,
           :os-error( "does not exist, is not a dir or no access" ),
         );
}

sub COPY-FILE(Str $from, Str $to, :$createonly --> True) {
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
}

sub RENAME-PATH(Str $from, Str $to, :$createonly --> True) {
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

my %FILETEST-HASH =
  e => -> $p { True },
  d => -> $p { nqp::p6bool(nqp::stat(nqp::unbox_s($p),nqp::const::STAT_ISDIR)) },
  f => -> $p { nqp::p6bool(nqp::stat(nqp::unbox_s($p),nqp::const::STAT_ISREG)) },
  s => -> $p { %FILETEST-HASH.AT-KEY("f")($p)
    && nqp::box_i(nqp::stat(nqp::unbox_s($p),nqp::const::STAT_FILESIZE),Int) },
  l => -> $p { nqp::p6bool(nqp::fileislink(nqp::unbox_s($p))) },
  r => -> $p { nqp::p6bool(nqp::filereadable(nqp::unbox_s($p))) },
  w => -> $p { nqp::p6bool(nqp::filewritable(nqp::unbox_s($p))) },
  x => -> $p { nqp::p6bool(nqp::fileexecutable(nqp::unbox_s($p))) },
  z => -> $p { %FILETEST-HASH.AT-KEY("f")($p)
    && nqp::p6bool(nqp::stat(nqp::unbox_s($p),nqp::const::STAT_FILESIZE) == 0) },

  "!e" => -> $p { False },
  "!d" => -> $p { !%FILETEST-HASH.AT-KEY("d")($p) },
  "!f" => -> $p { !%FILETEST-HASH.AT-KEY("f")($p) },
  "!l" => -> $p { !%FILETEST-HASH.AT-KEY("l")($p) },
  "!r" => -> $p { !%FILETEST-HASH.AT-KEY("r")($p) },
  "!w" => -> $p { !%FILETEST-HASH.AT-KEY("w")($p) },
  "!x" => -> $p { !%FILETEST-HASH.AT-KEY("x")($p) },
  "!z" => -> $p { !%FILETEST-HASH.AT-KEY("z")($p) },
;

sub FILETEST-ALL(Str $path, *@tests) {

    # most common cases
    if @tests.join -> $tests {
        return FILETEST-R($path)   if $tests eq "r";
        return FILETEST-RW($path)  if $tests eq "rw";
        return FILETEST-RWX($path) if $tests eq "rwx";
    }

    # nothing to check
    else {
        return False;
    }

    my $result = True;
    for @tests -> $t {
        die "Unknown test $t" unless %FILETEST-HASH.EXISTS-KEY($t);
        last unless $result = $result && %FILETEST-HASH.AT-KEY($t)($path);
    }

    $result;
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

