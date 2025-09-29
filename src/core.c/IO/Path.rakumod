my class IO::Path is Cool does IO {
    has IO::Spec $.SPEC;  # the associated IO::Spec
    has Str      $.CWD;   # the associated CWD
    has Str      $.path;  # the path as specified
    has $!is-absolute;    # Bool:D if we know $!path is an absolute path
    has $!os-path;        # the absolute path associated with path/SPEC/CWD
    has $!parts;          # IO::Path::Parts object, if any

    sub empty-path-message() is hidden-from-backtrace {
        die "Must specify a non-empty string as a path"
    }
    sub null-in-path() is hidden-from-backtrace {
        X::IO::Null.new.throw
    }

    multi method ACCEPTS(IO::Path:D: Cool:D \other) {
        nqp::hllbool(nqp::iseq_s($.absolute, nqp::unbox_s(other.IO.absolute)));
    }

    method !SET-SELF(str $path, IO::Spec $SPEC, $CWD) {
        empty-path-message
          unless nqp::chars($path);

        null-in-path
          if nqp::isne_i(nqp::index($path,"\0"),-1)
          || nqp::isne_i(nqp::index($CWD, "\0"),-1);

        $!path := $path;
        $!SPEC := $SPEC;
        $!CWD  := $CWD;
        $!is-absolute := $!os-path := $!parts := nqp::null;

        self
    }

    proto method new(|) {*}
    multi method new(IO::Path:
      Str:D $path, :$CWD!, IO::Spec :$SPEC = $*SPEC
    --> IO::Path:D) {
        nqp::create(self)!SET-SELF($path, $SPEC, $CWD.Str)
    }
    multi method new(IO::Path: Str:D $path, IO::Spec :$SPEC! --> IO::Path:D) {
        nqp::create(self)!SET-SELF($path, $SPEC, $*CWD.Str)
    }
    multi method new(IO::Path: Str:D $path --> IO::Path:D) {
        nqp::create(self)!SET-SELF($path, $*SPEC, $*CWD.Str)
    }
    multi method new(IO::Path:
      Cool:D $path, IO::Spec :$SPEC = $*SPEC, :$CWD = $*CWD
    --> IO::Path:D) {
        nqp::create(self)!SET-SELF($path.Str, $SPEC, $CWD.Str)
    }
    multi method new(IO::Path:
      :$basename!,
      :$dirname  = '',
      :$volume   = '',
      :$SPEC     = $*SPEC,
      :$CWD      = $*CWD,
    ) {
        nqp::create(self)!SET-SELF(
          $SPEC.join($volume,$dirname,$basename), $SPEC, $CWD.Str)
    }
    multi method new(IO::Path:) { empty-path-message }

    method is-absolute(--> Bool:D) {
        nqp::ifnull(
          $!is-absolute,
          $!is-absolute := nqp::hllbool($!SPEC.is-absolute: $!path)
        )
    }
    method is-relative(--> Bool:D) {
        self.is-absolute ?? False !! True
    }

    method parts {
        nqp::ifnull(
          $!parts,
          $!parts := $!SPEC.split($!path)
        )
    }
    method volume(IO::Path:D:)   { self.parts.volume   }
    method dirname(IO::Path:D:)  { self.parts.dirname  }
    method basename(IO::Path:D:) { self.parts.basename }

    my sub EXTENSION-MK-EXTENSION (
        str $name, $no-ext, int $part-min, int $part-max = $part-min
    ) is pure {
      my int $offset = nqp::chars($name);
      my int $next-offset;
      my int $parts;
      nqp::while(
        nqp::if(
          nqp::isne_i( -1,
            ($next-offset = nqp::rindex($name, '.', nqp::sub_i($offset, 1)))),
          nqp::if($offset, nqp::islt_i($parts, $part-max))
        ),
        nqp::stmts(
          ($offset = $next-offset),
          ++$parts
        ),
      );
      nqp::if(
        nqp::if(nqp::isle_i($part-min, $parts), nqp::isle_i($parts, $part-max)),
        nqp::substr($name, nqp::add_i($offset, 1)),
        $no-ext,
      )
    }
    my sub EXTENSION-SUBST ($ext, $base, $subst, $joiner) is pure {
      nqp::if(
        nqp::defined($ext),
        nqp::unless(
          nqp::concat(
            nqp::if(
              nqp::unless( # if extension is empty, check $base to find out if...
                nqp::chars($ext), #... it's a missing ext. or empty string ext.
                nqp::eqat($base, '.', nqp::sub_i(nqp::chars($base), 1))
              ),
              nqp::substr($base, 0,
                nqp::sub_i(nqp::chars($base), nqp::add_i(nqp::chars($ext), 1))
              ),
              $base,
            ),
            nqp::concat($joiner, $subst)
          ), '.' # use `.` as basename if we ended up with it being empty
        ),
        $base,
      )
    }
    proto method extension(|) {*}
    multi method extension(IO::Path:D:) {
      nqp::if(
        nqp::iseq_i(-1, (my int $offset = nqp::rindex(
          (my str $basename = nqp::unbox_s(self.basename)),'.'))),
        '', nqp::substr($basename, nqp::add_i($offset, 1))
      )
    }
    multi method extension(IO::Path:D: Int :$parts!) {
      EXTENSION-MK-EXTENSION self.basename, '',
        nqp::if(
          nqp::islt_I(nqp::decont($parts), -2**63), -2**63,
          nqp::if( nqp::isgt_I(nqp::decont($parts),  2**63-1), 2**63-1,
            nqp::unbox_i($parts),
          ),
        )
    }
    multi method extension(IO::Path:D: Range :$parts!) {
      my ($min, $max) := Rakudo::Internals.RANGE-AS-ints:
        $parts, "Can only use numeric, non-NaN Ranges as :parts";
      EXTENSION-MK-EXTENSION self.basename, '', $min, $max
    }
    multi method extension(IO::Path:D:
      Str $subst,
      Int :$parts = ?self.extension,
      Str :$joiner = nqp::chars($subst) ?? '.' !! ''
    ) {
      self.new: :dirname(self.dirname), :volume(self.volume),
       :$!SPEC, :$!CWD, basename => EXTENSION-SUBST
            EXTENSION-MK-EXTENSION(
              (my str $base = nqp::unbox_s(self.basename)),
              Any, nqp::if(
                nqp::islt_I(nqp::decont($parts), -2**63), -2**63,
                nqp::if( nqp::isgt_I(nqp::decont($parts),  2**63-1), 2**63-1,
                  nqp::unbox_i($parts),
                ),
              )
            ), $base, $subst, $joiner;
    }
    multi method extension(
      Str $subst,
      Range :$parts,
      Str :$joiner = nqp::chars($subst) ?? '.' !! ''
    ) {
      my ($min, $max) := Rakudo::Internals.RANGE-AS-ints:
        $parts, "Can only use numeric, non-NaN Ranges as :parts";
      self.new: :dirname(self.dirname), :volume(self.volume),
       :$!SPEC, :$!CWD, basename => EXTENSION-SUBST
        EXTENSION-MK-EXTENSION(
            (my str $base = nqp::unbox_s(self.basename)), Any, $min, $max
        ), $base, $subst, $joiner
    }

    method Numeric(IO::Path:D:) { self.basename.Numeric }

    multi method Str (IO::Path:D:) { $!path }
    multi method gist(IO::Path:D:) {
        $!is-absolute
          ?? qq|"$.absolute".IO|
          !! qq|"$.path".IO|
    }
    multi method raku(IO::Path:D:) {
        self.^name ~ ".new({$.path.raku}, {:$!SPEC.raku}, {:$!CWD.raku})"
    }

    method sibling(IO::Path:D: Str() \sibling) {
        $_ := self.parts;
        nqp::clone(self).cloned-with-path:
          $!SPEC.join(.<volume>, .<dirname>, sibling)
    }

    method succ(IO::Path:D:) {
        my int $i = nqp::index($!path,".");
        $i = nqp::iseq_i($i,-1) ?? nqp::chars($!path) !! $i;
        nqp::clone(self).cloned-with-path(Rakudo::Internals.SUCC($!path,$i - 1))
    }
    method pred(IO::Path:D:) {
        my int $i = nqp::index($!path,".");
        $i = nqp::iseq_i($i,-1) ?? nqp::chars($!path) !! $i;
        nqp::clone(self).cloned-with-path(Rakudo::Internals.PRED($!path,$i - 1))
    }

    multi method IO() { self }
    method open(IO::Path:D: |c) { IO::Handle.new(:path(self)).open(|c) }

#?if moar
    method watch(IO::Path:D:) {
        IO::Notification.watch-path($.absolute);
    }
#?endif

    proto method absolute(|) {*}
    multi method absolute (IO::Path:D: --> Str:D) {
        nqp::ifnull(
          $!os-path,
          $!os-path := $!SPEC.rel2abs($!path,$!CWD)
        )
    }
    multi method absolute (IO::Path:D: $CWD --> Str:D) {
        $!is-absolute
          ?? $!os-path
          !! $!SPEC.rel2abs($!path, $CWD)  # do *not* set because different CWD
    }

    method relative (IO::Path:D: $CWD = $*CWD --> Str:D) {
        $!SPEC.abs2rel($.absolute, $CWD);
    }

    method cleanup (IO::Path:D:) {
        nqp::clone(self).cloned-with-path($!SPEC.canonpath($!path))
    }
    method resolve (IO::Path:D: :$completely) {
        # XXXX: Not portable yet; assumes POSIX semantics
        my int $max-depth = 256;
        my str $sep       = $!SPEC.dir-sep;
        my str $cur       = $!SPEC.curdir;
        my str $up        = $!SPEC.updir;
        my str $empty     = '';
        my Mu  $res-list := nqp::list_s();

        my $vdb          := $!SPEC.split: self.absolute;
        my str $volume    = $vdb.volume;
        my str $resolved  = $volume;
        my $path         := $!SPEC.catpath: '', $vdb.dirname, $vdb.basename;

#?if jvm
        # Apparently JVM doesn't know how to decode to utf8-c8 yet
        # so it's still afflicted by the bug that, say, "/\[x308]" in the path
        # doesn't get recognized as a path separator
        my $parts := nqp::split($sep, nqp::unbox_s($path));
#?endif
#?if !jvm
        # In this bit, we work with bytes, converting $sep (and assuming it's
        # 1-char long) in the path to nul bytes and then splitting the path
        # on nul bytes. This way, even if we get some weird paths like
        # "/\x[308]", we still split on the /, leaving the lone combiner as
        # part of the path part.
        nqp::stmts(
          (my $p := nqp::encode(
            nqp::unbox_s($path), 'utf8-c8', nqp::create(buf8.^pun))),
          (my int $ord-sep = nqp::ord($sep)),
          (my int $els = nqp::elems($p)),
          (my int $i = -1),
          nqp::while(
            nqp::isne_i($els,++$i),
            nqp::if(
              nqp::iseq_i(nqp::atpos_u($p, $i), $ord-sep),
              nqp::atposref_u($p, $i) = 0)),
          my $parts := nqp::split("\0", nqp::decode($p, 'utf8-c8')));
#?endif

        while $parts {
            fail "Resolved path too deep!"
                if $max-depth < nqp::elems($res-list) + nqp::elems($parts);

            # Grab next unprocessed part, check for '', '.', '..'
            my str $part = nqp::shift($parts);

            next if nqp::iseq_s($part, $empty) || nqp::iseq_s($part, $cur);
            if nqp::iseq_s($part, $up) {
                next unless $res-list;
                nqp::pop_s($res-list);
                $resolved = $res-list
                  ?? nqp::concat(nqp::concat($volume, $sep), nqp::join($sep, $res-list))
                  !! $empty;
                next;
            }

            # Normal part, set as next path to test
            my str $next = nqp::concat($resolved, nqp::concat($sep, $part));

            # Path part doesn't exist...
            if nqp::not_i(nqp::stat($next, nqp::const::STAT_EXISTS)) {
                # fail() if we were asked for complete resolution and we still
                # have further parts to resolve. If it's the last part,
                # don't fail; it can be a yet-to-be-created file or dir
                $completely
                  && nqp::elems($parts)
                  && X::IO::Resolve.new(:path(self)).fail;

                # ...or handle rest in non-resolving mode if not
                $resolved = $next;
                while $parts {
                    $part = nqp::shift($parts);
                    next if nqp::iseq_s($part, $empty) || nqp::iseq_s($part, $cur);
                    $resolved = nqp::concat($resolved, nqp::concat($sep, $part));
                }
            }
            # Symlink; read it and act on absolute or relative link
            elsif nqp::fileislink($next) {
                my str $link        = nqp::readlink($next);
                my Mu  $link-parts := nqp::split($sep, $link);
                next unless $link-parts;

                # Symlink to absolute path
                if nqp::iseq_s($link-parts[0], $empty) {
                    $resolved  = nqp::shift($link-parts);
                    $res-list := nqp::list_s();
                }

                nqp::unshift($parts, nqp::pop($link-parts))
                    while $link-parts;
            }
            # Just a plain old path part, so append it and go on
            else {
                $resolved = $next;
                nqp::push_s($res-list, $part);
            }
        }
        $resolved = $volume ~ $sep if $resolved eq $volume;
        nqp::p6bindattrinvres(
          nqp::create(self)!SET-SELF($resolved, $!SPEC, $volume ~ $sep),
          IO::Path,'$!is-absolute',True
        )
    }

    proto method parent(|) {*}
    multi method parent(IO::Path:D: Int:D $depth is copy) {
        if $depth > 0 {
            my $io := self;
            nqp::while(
              $depth--,
              $io := $io.parent
            );
            $io
        }
        else {
            $depth
              ?? X::OutOfRange.new(
                   what  => 'Depth of .parent',
                   got   => $depth,
                   range => "0..*"
                 ).throw
              !! self
        }
    }
    multi method parent(IO::Path:D:) {
        my $curdir := $!SPEC.curdir;
        my $updir  := $!SPEC.updir;

        nqp::clone(self).cloned-with-path: self.is-absolute
          ?? $!SPEC.join($.volume, $.dirname, '')
          !! $.dirname eq $curdir && $.basename eq $curdir
            ?? $!SPEC.join($.volume,$curdir,$updir)
            !! $.basename eq $updir && ($.dirname eq $curdir
                || !$!SPEC.splitdir($.dirname).first(* ne $updir))
              ?? $!SPEC.join($.volume,$!SPEC.catdir($.dirname,$updir),$updir)
              !! $!SPEC.join($.volume, $.dirname, '')
    }

    method child (IO::Path:D: \child) {
        nqp::clone(self).cloned-with-path:
          $!SPEC.join('', $!path, child.Str)
    }

    method add (IO::Path:D: *@children) {
        nqp::clone(self).cloned-with-path:
          $!SPEC.join: '', $!path, @children.join($!SPEC.dir-sep)
    }

    proto method chdir(|) {*}
    multi method chdir(IO::Path:D: IO $path, |c) {
        self.chdir: $path.absolute, |c
    }
    multi method chdir(
        IO::Path:D: Str() $path is copy, :$d = True, :$r, :$w, :$x,
    ) {
        unless $!SPEC.is-absolute($path) {
            my ($volume,$dirs) = $!SPEC.splitpath(self.absolute, :nofile);
            my @dirs = $!SPEC.splitdir($dirs);
            @dirs.shift; # the first is always empty for absolute dirs
            for $!SPEC.splitdir($path) -> $dir {
                if $dir eq '..' {
                    @dirs.pop if @dirs;
                }
                elsif $dir ne '.' {
                    @dirs.push: $dir;
                }
            }
            @dirs.push('') if !@dirs;  # need at least the rootdir
            $path = join($!SPEC.dir-sep, $volume, @dirs);
        }
        my $dir := nqp::p6bindattrinvres(
          nqp::create(self)!SET-SELF($path, $!SPEC, $!path),
          IO::Path,'$!is-absolute',True
        );

        nqp::stmts(
            nqp::unless(
                nqp::unless(nqp::isfalse($d), $dir.d),
                fail X::IO::Chdir.new: :$path, :os-error(
                    $dir.e ?? 'is not a directory' !! 'does not exist'
                )
            ),
            nqp::unless(
                nqp::unless(nqp::isfalse($r), $dir.r),
                fail X::IO::Chdir.new: :$path, :os-error("did not pass :r test")
            ),
            nqp::unless(
                nqp::unless(nqp::isfalse($w), $dir.w),
                fail X::IO::Chdir.new: :$path, :os-error("did not pass :w test")
            ),
            nqp::unless(
                nqp::unless(nqp::isfalse($x), $dir.x),
                fail X::IO::Chdir.new: :$path, :os-error("did not pass :x test")
            ),
            $dir
        )
    }

    method rename(IO::Path:D: IO() $to, :$createonly --> True) {
        CATCH { default {
            fail X::IO::Rename.new:
                :from($!os-path), :to($to.absolute), :os-error(.Str);
        }}
        $createonly and $to.e and fail X::IO::Rename.new:
            :from($.absolute),
            :to($to.absolute),
            :os-error(':createonly specified and destination exists');

        nqp::rename($.absolute, nqp::unbox_s($to.absolute));
    }

    method copy(IO::Path:D: IO() $to, :$createonly --> True) {
        CATCH { default {
            fail X::IO::Copy.new:
                :from($!os-path), :to($to.absolute), :os-error(.Str)
        }}
        # add fix for issue #3971 where attempt to copy a dir
        # to a file clobbers the file.
        self.d and $to.f and fail X::IO::Copy.new:
            :from($.absolute),
            :to($to.absolute),
            :os-error('cannot copy a directory to a file');

        $createonly and $to.e and fail X::IO::Copy.new:
            :from($.absolute),
            :to($to.absolute),
            :os-error(':createonly specified and destination exists');

        # XXX TODO: maybe move the sameness check to the nqp OP/VM
        nqp::if(
            nqp::iseq_s(
                (my $from-abs :=   $.absolute),
                (my $to-abs   := $to.absolute)),
            X::IO::Copy.new(:from($from-abs), :to($to-abs),
                :os-error('source and target are the same')).fail,
            nqp::copy($from-abs, $to-abs));
    }

    method move(IO::Path:D: |c --> True) {
        self.copy(|c) orelse fail X::IO::Move.new: :from(.exception.from),
            :to(.exception.to), :os-error(.exception.os-error);
        self.unlink   orelse fail X::IO::Move.new: :from(.exception.from),
            :to(.exception.to), :os-error(.exception.os-error);
    }

    method chmod(IO::Path:D: Int() $mode --> True) {
        CATCH { default {
            fail X::IO::Chmod.new(
              :path($!os-path), :$mode, :os-error(.Str) );
        }}
        nqp::chmod($.absolute, nqp::unbox_i($mode));
    }

    method chown(IO::Path:D: :$uid is copy, :$gid is copy --> True) {
        CATCH { default {
            fail X::IO::Chown.new(
              :path($!os-path), :$uid, :$gid, :os-error(.Str) );
        }}
        my str $path = self.absolute;
#?if moar
        my $stat := nqp::syscall("file-stat", nqp::decont_s($path), 0);
        die "Path does not exist" unless nqp::syscall("stat-flags", $stat, nqp::const::STAT_EXISTS);
#?endif
        $uid = $uid.defined
          ?? $uid.UInt
#?if moar
          !! nqp::syscall("stat-flags", $stat, nqp::const::STAT_UID);
#?endif
#?if !moar
          !! nqp::stat($path,nqp::const::STAT_UID);
#?endif
        $gid = $gid.defined
          ?? $gid.UInt
#?if moar
          !! nqp::syscall("stat-flags", $stat, nqp::const::STAT_GID);
#?endif
#?if !moar
          !! nqp::stat($path,nqp::const::STAT_GID);
#?endif
        nqp::chown($path, nqp::unbox_u($uid), nqp::unbox_u($gid))
    }

    method unlink(IO::Path:D: --> True) {
        CATCH { default {
            fail X::IO::Unlink.new( :path($!os-path), os-error => .Str );
        }}
        nqp::unlink($.absolute);
    }

    method symlink(IO::Path:D: IO() $name, Bool :$absolute = True --> True) {
        CATCH { default {
            fail X::IO::Symlink.new:
                :target($!os-path), :name($name.absolute), :os-error(.Str);
        }}
        nqp::symlink($absolute ?? $.absolute !! ~self , nqp::unbox_s($name.absolute));
    }

    method link(IO::Path:D: IO() $name --> True) {
        CATCH { default {
            fail X::IO::Link.new:
                :target($!os-path), :name($name.absolute), :os-error(.Str);
        }}
        nqp::link($.absolute, $name.absolute);
    }

    method mkdir(IO::Path:D: Int() $mode = 0o777) {
        CATCH { default {
            fail X::IO::Mkdir.new(:path($!os-path), :$mode, os-error => .Str);
        }}
        my str $abspath = $.absolute;
#?if moar
        my $stat := nqp::syscall("file-stat", nqp::decont_s($abspath), 0);
#?endif
        nqp::unless(
#?if moar
          nqp::syscall("stat-flags", $stat, nqp::const::STAT_EXISTS) && nqp::syscall("stat-flags", $stat, nqp::const::STAT_ISDIR),
#?endif
#?if !moar
          nqp::stat($abspath,nqp::const::STAT_EXISTS)
            && nqp::stat($abspath,nqp::const::STAT_ISDIR),
#?endif
          nqp::mkdir($abspath,$mode)
        );
        self
    }

    method rmdir(IO::Path:D: --> True) {
        CATCH { default {
            fail X::IO::Rmdir.new(:path($!os-path), os-error => .Str);
        }}
        nqp::rmdir($.absolute);
    }


    # Call with cloned object, update path, keep "is-absolute" setting
    # and reset the rest.  If the source object was an absolute path,
    # then the given path should also be an absolute path, and vice-versa.
    method cloned-with-path(Str:D $path) is implementation-detail {
        X::IO::Null.new.throw if nqp::isne_i(nqp::index($path, "\0"), -1);
        $!path := $path;
        $!os-path := $!parts := nqp::null;
        self
    }

    # create prefix to be added to each directory entry
    method prefix-for-dir() is implementation-detail {
        my str $dir-sep = $!SPEC.dir-sep;
        nqp::iseq_s($!path,'.')
          ?? ''
          !! $!path.ends-with($dir-sep)
            ?? $!path
            !! $!path.ends-with("$dir-sep.")
              ?? nqp::substr($!path,0,nqp::chars($!path) - 1)
              !! nqp::concat($!path,$dir-sep)
    }

    proto method dir(|) {*} # make it possible to augment with multies from modulespace
    multi method dir(IO::Path:D: Mu :$test!) {
        CATCH { default {
            X::IO::Dir.new(:path(self.absolute), :os-error(.Str)).throw
        } }

        Seq.new: Rakudo::Iterator.Dir(self, $test)
    }

    multi method dir(IO::Path:D:) {
        CATCH { default {
            X::IO::Dir.new(:path(self.absolute), :os-error(.Str)).throw
        } }

        # if default tester is system default, use implicit no . .. iterator
        Seq.new: nqp::eqaddr($!SPEC.curupdir,IO::Spec::Unix.curupdir)
          ?? Rakudo::Iterator.Dir(self)
          !! Rakudo::Iterator.Dir(self, $!SPEC.curupdir)
    }

    constant fallback-slurp-size = 0x100000;

    # slurp contents of low level handle
    sub slurp-PIO(Mu \PIO, :$size = fallback-slurp-size) is raw {
        my \slurp-size = $size;
        nqp::readfh(PIO,(my $blob := nqp::create(buf8.^pun)),slurp-size);

        # if the size is set to the fallback value, then
        # run the old path...
        # enough to read entire buffer, assume there's more
        if slurp-size == fallback-slurp-size && nqp::iseq_i(nqp::elems($blob),slurp-size) {
            nqp::while(
              nqp::elems(
                nqp::readfh(PIO,(my $part := nqp::create(buf8.^pun)),slurp-size)
              ),
              $blob.append($part)
            );
        }
        $blob
    }

    # slurp STDIN in binary mode
    sub slurp-stdin-bin() is raw { slurp-PIO(nqp::getstdin) }

    # slurp given path in binary mode
    sub slurp-path-bin(str $path, int $size) is raw {
        my $PIO  := nqp::open($path,'r');
        my $blob := slurp-PIO($PIO, :$size);
        nqp::closefh($PIO);
        $blob
    }

    # slurp STDIN with given normalized encoding
    sub slurp-stdin-with-encoding(str $encoding) {
        nqp::join("\n",
          nqp::split("\r\n",nqp::decode(slurp-stdin-bin,$encoding))
        )
    }

    # slurp given path with given normalized encoding
    sub slurp-path-with-encoding(str $path, str $encoding, int $size) {
        CATCH { return $_.Failure }

        nqp::elems(my $blob := slurp-path-bin($path, $size))
          ?? nqp::join("\n",nqp::split("\r\n",nqp::decode($blob,$encoding)))
          !! ""
    }

    proto method slurp() {*}
    multi method slurp(IO::Path:D: :$bin!) {
        my $size = ((try self.s) // 0), fallback-slurp-size;
        nqp::iseq_s($!path,"-")
          ?? $bin
            ?? slurp-stdin-bin()
            !! slurp-stdin-with-encoding('utf8')
          !! $bin
            ?? slurp-path-bin(self.absolute,$size)
            !! slurp-path-with-encoding(self.absolute,'utf8',$size)
    }
    multi method slurp(IO::Path:D: :$enc!) {
        my $size = max try self.s, fallback-slurp-size;
        nqp::iseq_s($!path,"-")
          ?? slurp-stdin-with-encoding(
               Rakudo::Internals.NORMALIZE_ENCODING($enc))
          !! slurp-path-with-encoding(self.absolute,
               Rakudo::Internals.NORMALIZE_ENCODING($enc),$size)
    }
    multi method slurp(IO::Path:D:) {
        my $size = max try self.s, fallback-slurp-size;
        nqp::iseq_s($!path,"-")
          ?? slurp-stdin-with-encoding('utf8')
          !! slurp-path-with-encoding(self.absolute,'utf8',$size)
    }

    # spurt data to given path and file mode
    sub spurt-blob(str $path, str $mode, Blob:D \data) {
        CATCH { .fail }

        my $PIO := nqp::open($path,$mode);
        nqp::writefh($PIO,nqp::decont(data));
        nqp::closefh($PIO);
        True
    }

    # spurt text to given path and file mode with given encoding
    sub spurt-string(str $path, str $mode, str $text, $encoding) {
        my $blob := nqp::encode(
          $text,
          (my str $enc = Rakudo::Internals.NORMALIZE_ENCODING($encoding)),
          nqp::create(buf8.^pun)
        );

        # check if we need a BOM
        if $enc eq 'utf16' {

            # add a BOM if (over)writing
            if $mode eq 'w' {
                nqp::unshift_i($blob,254);
                nqp::unshift_i($blob,255);
            }
            # or appending to a new or existing, but zero-length, file
            else {
#?if moar
                my $stat := nqp::syscall("file-stat", nqp::decont_s($path), 0);
                if nqp::not_i(nqp::syscall("stat-flags", $stat, nqp::const::STAT_EXISTS)) ||
                   nqp::not_i(nqp::syscall("stat-flags", $stat, nqp::const::STAT_FILESIZE)) {
#?endif
#?if !moar
                if nqp::not_i(nqp::stat($path,nqp::const::STAT_EXISTS)) ||
                   nqp::not_i(nqp::stat($path,nqp::const::STAT_FILESIZE)) {
#?endif
                    nqp::unshift_i($blob,254);
                    nqp::unshift_i($blob,255);
                }
            }
        }

        spurt-blob($path, $mode, $blob)
    }

    method user(IO::Path:D:) {
#?if moar
        my $stat := nqp::syscall("file-stat", nqp::decont_s(self.absolute), 0);
        nqp::syscall("stat-flags", $stat, nqp::const::STAT_EXISTS)
          ?? nqp::syscall("stat-flags", $stat, nqp::const::STAT_UID)
#?endif
#?if !moar
        Rakudo::Internals.FILETEST-E(my str $path = self.absolute)
          ?? nqp::stat($path, nqp::const::STAT_UID)
#?endif
          !! self!does-not-exist("user")
    }

    method group(IO::Path:D:) {
#?if moar
        my $stat := nqp::syscall("file-stat", nqp::decont_s(self.absolute), 0);
        nqp::syscall("stat-flags", $stat, nqp::const::STAT_EXISTS)
          ?? nqp::syscall("stat-flags", $stat, nqp::const::STAT_GID)
#?endif
#?if !moar
        Rakudo::Internals.FILETEST-E(my str $path = self.absolute)
          ?? nqp::stat($path, nqp::const::STAT_GID)
#?endif
          !! self!does-not-exist("group")
    }

    proto method spurt(|) {*}
    multi method spurt(IO::Path:D: --> Bool:D) {
        self.open(:w).close
    }
    multi method spurt(IO::Path:D: Blob:D \data, :$append! --> Bool:D) {
        spurt-blob(self.absolute, $append ?? 'wa' !! 'w', data)
    }
    multi method spurt(IO::Path:D: Blob:D \data, :$createonly! --> Bool:D) {
        nqp::stat(self.absolute,nqp::const::STAT_EXISTS)  # sets $!os-path
          ?? "Failed to open file $!os-path: File exists".Failure
          !! spurt-blob($!os-path, 'w', data)
    }
    multi method spurt(IO::Path:D: Blob:D \data --> Bool:D) {
        spurt-blob(self.absolute, 'w', data)
    }
    multi method spurt(IO::Path:D: \text, :$append!, :$enc --> Bool:D) {
        spurt-string(self.absolute, $append ?? 'wa' !! 'w', text.Str, $enc)
    }
    multi method spurt(IO::Path:D: \text, :$createonly!, :$enc --> Bool:D) {
        nqp::stat(self.absolute,nqp::const::STAT_EXISTS)  # sets $!os-path
          ?? "Failed to open file $!os-path: File exists".Failure
          !! spurt-string($!os-path, 'w', text.Str, $enc)
    }
    multi method spurt(IO::Path:D: \text, :$enc --> Bool:D) {
        spurt-string(self.absolute, 'w', text.Str, $enc)
    }

    # XXX TODO: when we get definedness-based defaults in core, use them in
    # IO::Handle.open and get rid of duplication of default values here
    method lines(IO::Path:D:
        :$chomp = True, :$enc = 'utf8', :$nl-in = ["\x0A", "\r\n"], |c
    ) {
        self.open(:$chomp, :$enc, :$nl-in).lines: |c, :close
    }
    method comb(IO::Path:D:
        :$chomp = True, :$enc = 'utf8', :$nl-in = ["\x0A", "\r\n"], |c
    ) {
        self.open(:$chomp, :$enc, :$nl-in).comb:  |c, :close
    }
    method split(IO::Path:D:
        :$chomp = True, :$enc = 'utf8', :$nl-in = ["\x0A", "\r\n"], |c
    ) {
        self.open(:$chomp, :$enc, :$nl-in).split: |c, :close
    }
    method words(IO::Path:D:
        :$chomp = True, :$enc = 'utf8', :$nl-in = ["\x0A", "\r\n"], |c
    ) {
        self.open(:$chomp, :$enc, :$nl-in).words: |c, :close
    }

    method !does-not-exist(
      Str:D $trying
    --> Failure) is hidden-from-backtrace {
        X::IO::DoesNotExist.new(:path($!os-path),:$trying).Failure
    }

    method e(IO::Path:D: --> Bool:D) {
        nqp::hllbool(Rakudo::Internals.FILETEST-E(self.absolute))
    }
    method d(IO::Path:D: --> Bool:D) {
#?if moar
        my $stat := nqp::syscall("file-stat", nqp::decont_s(self.absolute), 0);
        nqp::syscall("stat-flags", $stat, nqp::const::STAT_EXISTS)
          ?? nqp::hllbool(nqp::syscall("stat-flags", $stat, nqp::const::STAT_ISDIR))
#?endif
#?if !moar
        Rakudo::Internals.FILETEST-E(self.absolute)  # sets $!os-path
          ?? nqp::hllbool(Rakudo::Internals.FILETEST-D($!os-path))
#?endif
          !! self!does-not-exist("d")
    }

    method f(IO::Path:D: --> Bool:D) {
#?if moar
        my $stat := nqp::syscall("file-stat", nqp::decont_s(self.absolute), 0);
        nqp::syscall("stat-flags", $stat, nqp::const::STAT_EXISTS)
          ?? nqp::hllbool(nqp::syscall("stat-flags", $stat, nqp::const::STAT_ISREG))
#?endif
#?if !moar
        Rakudo::Internals.FILETEST-E(self.absolute)  # sets $!os-path
          ?? nqp::hllbool(Rakudo::Internals.FILETEST-F($!os-path))
#?endif
          !! self!does-not-exist("f")
    }

    method s(IO::Path:D: --> Int:D) {
#?if moar
        my $stat := nqp::syscall("file-stat", nqp::decont_s(self.absolute), 0);
        nqp::syscall("stat-flags", $stat, nqp::const::STAT_EXISTS)
          ?? nqp::syscall("stat-flags", $stat, nqp::const::STAT_FILESIZE)
#?endif
#?if !moar
        Rakudo::Internals.FILETEST-E(self.absolute)  # sets $!os-path
          ?? Rakudo::Internals.FILETEST-S($!os-path)
#?endif
          !! self!does-not-exist("s")
    }

    method l(IO::Path:D: --> Bool:D) {
#?if moar
        my $stat := nqp::syscall("file-stat", nqp::decont_s(self.absolute), 1);
        nqp::syscall("stat-flags", $stat, nqp::const::STAT_EXISTS)
          ?? nqp::hllbool(nqp::syscall("stat-flags", $stat, nqp::const::STAT_ISLNK))
#?endif
#?if !moar
        Rakudo::Internals.FILETEST-LE(self.absolute)  # sets $!os-path
          ?? nqp::hllbool(Rakudo::Internals.FILETEST-L($!os-path))
#?endif
          !! self!does-not-exist("l")
    }

    method r(IO::Path:D: --> Bool:D) {
#?if moar
        my $stat := nqp::syscall("file-stat", nqp::decont_s(self.absolute), 0);
        nqp::syscall("stat-flags", $stat, nqp::const::STAT_EXISTS)
          ?? nqp::hllbool(nqp::syscall("stat-is-readable", $stat))
#?endif
#?if !moar
        Rakudo::Internals.FILETEST-E(self.absolute)  # sets $!os-path
          ?? nqp::hllbool(Rakudo::Internals.FILETEST-R($!os-path))
#?endif
          !! self!does-not-exist("r")
    }

    method w(IO::Path:D: --> Bool:D) {
#?if moar
        my $stat := nqp::syscall("file-stat", nqp::decont_s(self.absolute), 0);
        nqp::syscall("stat-flags", $stat, nqp::const::STAT_EXISTS)
          ?? nqp::hllbool(nqp::syscall("stat-is-writable", $stat))
#?endif
#?if !moar
        Rakudo::Internals.FILETEST-E(self.absolute)  # sets $!os-path
          ?? nqp::hllbool(Rakudo::Internals.FILETEST-W($!os-path))
#?endif
          !! self!does-not-exist("w")
    }

    method rw(IO::Path:D: --> Bool:D) {
#?if moar
        my $stat := nqp::syscall("file-stat", nqp::decont_s(self.absolute), 0);
        nqp::syscall("stat-flags", $stat, nqp::const::STAT_EXISTS)
          ?? nqp::hllbool(nqp::syscall("stat-is-readable", $stat) && nqp::syscall("stat-is-writable", $stat))
#?endif
#?if !moar
        Rakudo::Internals.FILETEST-E(self.absolute)  # sets $!os-path
          ?? nqp::hllbool(Rakudo::Internals.FILETEST-RW($!os-path))
#?endif
          !! self!does-not-exist("rw")
    }

    method x(IO::Path:D: --> Bool:D) {
#?if moar
        my $stat := nqp::syscall("file-stat", nqp::decont_s(self.absolute), 0);
        nqp::syscall("stat-flags", $stat, nqp::const::STAT_EXISTS)
          ?? nqp::hllbool(nqp::syscall("stat-is-executable", $stat))
#?endif
#?if !moar
        Rakudo::Internals.FILETEST-E(self.absolute)  # sets $!os-path
          ?? nqp::hllbool(Rakudo::Internals.FILETEST-X($!os-path))
#?endif
          !! self!does-not-exist("x")
    }

    method rwx(IO::Path:D: --> Bool:D) {
#?if moar
        my $stat := nqp::syscall("file-stat", nqp::decont_s(self.absolute), 0);
        nqp::syscall("stat-flags", $stat, nqp::const::STAT_EXISTS)
          ?? nqp::hllbool(nqp::syscall("stat-is-readable", $stat) && nqp::syscall("stat-is-writable", $stat) && nqp::syscall("stat-is-executable", $stat))
#?endif
#?if !moar
        Rakudo::Internals.FILETEST-E(self.absolute)  # sets $!os-path
          ?? nqp::hllbool(Rakudo::Internals.FILETEST-RWX($!os-path))
#?endif
          !! self!does-not-exist("rwx")
    }

    method z(IO::Path:D: --> Bool:D) {
#?if moar
        my $stat := nqp::syscall("file-stat", nqp::decont_s(self.absolute), 0);
        nqp::syscall("stat-flags", $stat, nqp::const::STAT_EXISTS)
          ?? nqp::syscall("stat-flags", $stat, nqp::const::STAT_FILESIZE) == 0
#?endif
#?if !moar
        Rakudo::Internals.FILETEST-E(self.absolute)  # sets $!os-path
          ?? nqp::hllbool(Rakudo::Internals.FILETEST-Z($!os-path))
#?endif
          !! self!does-not-exist("z")
    }

    method created(IO::Path:D: --> Instant:D) {
#?if moar
        my $stat := nqp::syscall("file-stat", nqp::decont_s(self.absolute), 0);
        nqp::syscall("stat-flags", $stat, nqp::const::STAT_EXISTS)
          ?? Instant.from-posix-nanos(nqp::syscall("stat-time-nanos", $stat, nqp::const::STAT_CREATETIME))
#?endif
#?if !moar
        Rakudo::Internals.FILETEST-E(self.absolute)  # sets $!os-path
          ?? Instant.from-posix(Rakudo::Internals.FILETEST-CREATED($!os-path))
#?endif
          !! self!does-not-exist("created")
    }

    method modified(IO::Path:D: --> Instant:D) {
#?if moar
        my $stat := nqp::syscall("file-stat", nqp::decont_s(self.absolute), 0);
        nqp::syscall("stat-flags", $stat, nqp::const::STAT_EXISTS)
          ?? Instant.from-posix-nanos(nqp::syscall("stat-time-nanos", $stat, nqp::const::STAT_MODIFYTIME))
#?endif
#?if !moar
        Rakudo::Internals.FILETEST-E(self.absolute)  # sets $!os-path
          ?? Instant.from-posix(Rakudo::Internals.FILETEST-MODIFIED($!os-path))
#?endif
          !! self!does-not-exist("modified")
    }

    method accessed(IO::Path:D: --> Instant:D) {
#?if moar
        my $stat := nqp::syscall("file-stat", nqp::decont_s(self.absolute), 0);
        nqp::syscall("stat-flags", $stat, nqp::const::STAT_EXISTS)
          ?? Instant.from-posix-nanos(nqp::syscall("stat-time-nanos", $stat, nqp::const::STAT_ACCESSTIME))
#?endif
#?if !moar
        Rakudo::Internals.FILETEST-E(self.absolute)  # sets $!os-path
          ?? Instant.from-posix(Rakudo::Internals.FILETEST-ACCESSED($!os-path))
#?endif
          !! self!does-not-exist("accessed")
    }

    method changed(IO::Path:D: --> Instant:D) {
#?if moar
        my $stat := nqp::syscall("file-stat", nqp::decont_s(self.absolute), 0);
        nqp::syscall("stat-flags", $stat, nqp::const::STAT_EXISTS)
          ?? Instant.from-posix-nanos(nqp::syscall("stat-time-nanos", $stat, nqp::const::STAT_CHANGETIME))
#?endif
#?if !moar
        Rakudo::Internals.FILETEST-E(self.absolute)  # sets $!os-path
          ?? Instant.from-posix(Rakudo::Internals.FILETEST-CHANGED($!os-path))
#?endif
          !! self!does-not-exist("changed")
    }

    method mode(IO::Path:D: --> IntStr:D) {
#?if moar
        my $stat := nqp::syscall("file-stat", nqp::decont_s(self.absolute), 0);
        if nqp::syscall("stat-flags", $stat, nqp::const::STAT_EXISTS) {  # sets $!os-path
            my Int $mode := nqp::bitand_i(nqp::syscall("stat-flags", $stat, nqp::const::STAT_PLATFORM_MODE), 0o7777);
#?endif
#?if !moar
        if Rakudo::Internals.FILETEST-E(self.absolute) {  # sets $!os-path
            my Int $mode := Rakudo::Internals.FILETEST-MODE($!os-path);
#?endif
            my str $str   = nqp::base_I($mode,8);
            IntStr.new(
              $mode,
              nqp::concat(nqp::x('0',4 - nqp::chars($str)),$str)
            )
        }
        else {
            self!does-not-exist("mode")
        }
    }

    method inode(IO::Path:D: --> Int:D) {
#?if moar
        my $stat := nqp::syscall("file-stat", nqp::decont_s(self.absolute), 0);
        nqp::syscall("stat-flags", $stat, nqp::const::STAT_EXISTS)
          ?? nqp::syscall("stat-flags", $stat, nqp::const::STAT_PLATFORM_INODE)
#?endif
#?if !moar
        Rakudo::Internals.FILETEST-E(self.absolute)  # sets $!os-path
          ?? nqp::stat($!os-path, nqp::const::STAT_PLATFORM_INODE)
#?endif
          !! self!does-not-exist("inode")
    }

    method dev(IO::Path:D: --> Int:D) {
#?if moar
        my $stat := nqp::syscall("file-stat", nqp::decont_s(self.absolute), 0);
        nqp::syscall("stat-flags", $stat, nqp::const::STAT_EXISTS)
          ?? nqp::syscall("stat-flags", $stat, nqp::const::STAT_ISDEV)
#?endif
#?if !moar
        Rakudo::Internals.FILETEST-E(self.absolute)  # sets $!os-path
          ?? nqp::stat($!os-path, nqp::const::STAT_PLATFORM_DEV)
#?endif
          !! self!does-not-exist("dev")
    }

    method devtype(IO::Path:D: --> Int:D) {
#?if moar
        my $stat := nqp::syscall("file-stat", nqp::decont_s(self.absolute), 0);
        nqp::syscall("stat-flags", $stat, nqp::const::STAT_EXISTS)
          ?? nqp::syscall("stat-flags", $stat, nqp::const::STAT_PLATFORM_DEV)
#?endif
#?if !moar
        Rakudo::Internals.FILETEST-E(self.absolute)  # sets $!os-path
          ?? nqp::stat($!os-path, nqp::const::STAT_PLATFORM_DEVTYPE)
#?endif
          !! self!does-not-exist("devtype")
    }

    proto method dir-with-entries(|) {*}
    multi method dir-with-entries(IO::Path:D: --> Bool:D) {
        my $handle := nqp::opendir(self.absolute);
        nqp::while(
          (my str $entry = nqp::nextfiledir($handle))
            && (nqp::iseq_s($entry,'.') || nqp::iseq_s($entry,'..')),
          nqp::null()
        );
        nqp::closedir($handle);
        nqp::hllbool(nqp::chars($entry))
    }
    multi method dir-with-entries(IO::Path:D: :$test! --> Bool:D) {
        my $handle := nqp::opendir(self.absolute);
        nqp::while(
          (my str $entry = nqp::nextfiledir($handle))
            && !$test.ACCEPTS($entry),
          nqp::null()
        );
        nqp::closedir($handle);
        nqp::hllbool(nqp::chars($entry))
    }

    method CHECKSUM(IO::Path:D: --> Str:D) is implementation-detail {
        my \slurped := self.slurp(:enc<iso-8859-1>);
        nqp::istype(slurped,Failure)
          ?? slurped
          !! nqp::sha1(slurped)
    }
}

my role IO::Path::Spec[$SPEC] is IO::Path {
    method new(|c) { self.IO::Path::new(|c, :$SPEC) }
    multi method raku(::?CLASS:D:) {
        self.^name ~ ".new({$.path.raku}, {:$.CWD.raku})"
    }
}

my class IO::Path::Cygwin does IO::Path::Spec[IO::Spec::Cygwin] { }
my class IO::Path::QNX    does IO::Path::Spec[IO::Spec::QNX   ] { }
my class IO::Path::Unix   does IO::Path::Spec[IO::Spec::Unix  ] { }
my class IO::Path::Win32  does IO::Path::Spec[IO::Spec::Win32 ] { }

# vim: expandtab shiftwidth=4
