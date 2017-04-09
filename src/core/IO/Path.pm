my class Instant { ... }

my class IO::Path is Cool {
    has IO::Spec $.SPEC;
    has Str      $.CWD;
    has Str      $.path;
    has Bool $!is-absolute;
    has Str  $!abspath;
    has %!parts;

    multi method ACCEPTS(IO::Path:D: Cool:D \other) {
        nqp::p6bool(nqp::iseq_s($.absolute, nqp::unbox_s(other.IO.absolute)));
    }

    submethod BUILD(:$!path!, :$!SPEC!, :$!CWD! --> Nil) {
        nqp::unless($!path,
            die "Must specify something as a path: did you mean '.' for the current directory?"
        )
    }

    method new-from-absolute-path($path, :$SPEC = $*SPEC, Str() :$CWD = $*CWD) {
        method !set-absolute() {
            $!is-absolute = True;
            $!abspath := $path;
            self;
        }

        self.bless(:$path, :$SPEC, :$CWD)!set-absolute;
    }

    proto method new(|) {*}
    multi method new(IO::Path: Str $path, :$SPEC = $*SPEC, Str:D :$CWD) {
        self.bless(:$path, :$SPEC, :$CWD);
    }
    multi method new(IO::Path: Str $path, :$SPEC = $*SPEC, :$CWD = $*CWD) {
        self.bless(:$path, :$SPEC, :CWD($CWD.Str));
    }
    multi method new(IO::Path: Cool $path, :$SPEC = $*SPEC, :$CWD = $*CWD) {
        self.bless(:path($path.Str), :$SPEC, :CWD($CWD.Str));
    }
    multi method new(IO::Path:
      :$basename!,
      :$dirname  = '',
      :$volume   = '',
      :$SPEC     = $*SPEC,
      Str() :$CWD = $*CWD,
    ) {
        self.bless(:path($SPEC.join($volume,$dirname,$basename)),:$SPEC,:$CWD);
    }
    multi method new(IO::Path:) {
        die "Must specify something as a path: did you mean '.' for the current directory?";
    }

    method is-absolute() {
        $!is-absolute //= $!SPEC.is-absolute($!path);
    }
    method is-relative() {
        !( $!is-absolute //= $!SPEC.is-absolute($!path) );
    }

    method parts                  {
        %!parts ||= $!SPEC.split($!path);
    }
    method volume(IO::Path:D:)   { %.parts<volume>   }
    method dirname(IO::Path:D:)  { %.parts<dirname>  }
    method basename(IO::Path:D:) { %.parts<basename> }


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
          ($parts = nqp::add_i($parts, 1))
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
      Int :$parts = 1, Str :$joiner = nqp::if(nqp::chars($subst), '.', '')
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
      Range :$parts, Str :$joiner = nqp::if(nqp::chars($subst), '.', '')
    ) {
      my ($min, $max) := Rakudo::Internals.RANGE-AS-ints:
        $parts, "Can only use numeric, non-NaN Ranges as :parts";
      self.new: :dirname(self.dirname), :volume(self.volume),
       :$!SPEC, :$!CWD, basename => EXTENSION-SUBST
        EXTENSION-MK-EXTENSION(
            (my str $base = nqp::unbox_s(self.basename)), Any, $min, $max
        ), $base, $subst, $joiner
    }


    # core can't do 'basename handles <Numeric Int>'
    method Numeric(IO::Path:D:) { self.basename.Numeric }
    method Int    (IO::Path:D:) { self.basename.Int     }

    multi method Str (IO::Path:D:) { $!path }
    multi method gist(IO::Path:D:) {
        $!is-absolute
          ?? qq|"$.absolute".IO|
          !! qq|"$.path".IO|
    }
    multi method perl(IO::Path:D:) {
        $!is-absolute  # attribute now set
          ?? "{$.absolute.perl}.IO({:$!SPEC.perl})"
          !! "{$.path.perl}.IO({:$!SPEC.perl},{:$!CWD.perl})"
    }

    method succ(IO::Path:D:) {
        self.bless(
          :path($!SPEC.join($.volume,$.dirname,$.basename.succ)),
          :$!SPEC,
          :$!CWD,
        );
    }
    method pred(IO::Path:D:) {
        self.bless(
          :path($!SPEC.join($.volume,$.dirname,$.basename.pred)),
          :$!SPEC,
          :$!CWD,
        );
    }

    multi method IO(IO::Path:D:) { self }
    method open(IO::Path:D: |c) { IO::Handle.new(:path(self)).open(|c) }

#?if moar
    method watch(IO::Path:D:) {
        IO::Notification.watch-path($.absolute);
    }
#?endif

    proto method absolute(|) { * }
    multi method absolute (IO::Path:D:) {
        $!abspath //= $!SPEC.rel2abs($!path,$!CWD)
    }
    multi method absolute (IO::Path:D: $CWD) {
        self.is-absolute
          ?? self.absolute
          !! $!SPEC.rel2abs($!path, $CWD);
    }

    method relative (IO::Path:D: $CWD = $*CWD) {
        $!SPEC.abs2rel($.absolute, $CWD);
    }

    method cleanup (IO::Path:D:) {
        self.bless(:path($!SPEC.canonpath($!path)), :$!SPEC, :$!CWD);
    }
    method resolve (IO::Path:D:) {
        # XXXX: Not portable yet; assumes POSIX semantics
        my int $max-depth = 256;
        my str $sep       = $!SPEC.dir-sep;
        my str $cur       = $!SPEC.curdir;
        my str $up        = $!SPEC.updir;
        my str $empty     = '';
        my str $resolved  = $empty;
        my Mu  $res-list := nqp::list_s();

        my Mu $parts := nqp::split($sep, nqp::unbox_s(self.absolute));
        while $parts {
            fail "Resolved path too deep!"
                if $max-depth < nqp::elems($res-list) + nqp::elems($parts);

            # Grab next unprocessed part, check for '', '.', '..'
            my str $part = nqp::shift($parts);

            next if nqp::iseq_s($part, $empty) || nqp::iseq_s($part, $cur);
            if nqp::iseq_s($part, $up) {
                next unless $res-list;
                nqp::pop_s($res-list);
                $resolved = $res-list ?? $sep ~ nqp::join($sep, $res-list)
                                      !! $empty;
                next;
            }

            # Normal part, set as next path to test
            my str $next = nqp::concat($resolved, nqp::concat($sep, $part));

            # Path part doesn't exist, so bail out
            if !nqp::stat($next, nqp::const::STAT_EXISTS) {
                fail X::IO::DoesNotExist.new(
                    :path($next),
                    :trying('resolve'),
                    :os-error("path part doesn't exist"),
                );
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
        $resolved = $sep unless nqp::chars($resolved);
        IO::Path.new-from-absolute-path($resolved,:$!SPEC,:CWD(self));
    }

    method parent(IO::Path:D:) {    # XXX needs work
        my $curdir := $!SPEC.curdir;
        my $updir  := $!SPEC.updir;

        if self.is-absolute {
            return self.bless(
              :path($!SPEC.join($.volume, $.dirname, '')),
              :$!SPEC,
              :$!CWD,
            );
        }
        elsif $.dirname eq $curdir and $.basename eq $curdir {
            return self.bless(
              :path($!SPEC.join($.volume,$curdir,$updir)),
              :$!SPEC,
              :$!CWD,
            );
        }
        elsif $.dirname eq $curdir && $.basename eq $updir
           or !grep({$_ ne $updir}, $!SPEC.splitdir($.dirname)) {
            return self.bless(    # If all updirs, then add one more
              :path($!SPEC.join($.volume,$!SPEC.catdir($.dirname,$updir),$.basename)),
              :$!SPEC,
              :$!CWD,
            );
        }
        else {
            return self.bless(
              :path($!SPEC.join($.volume, $.dirname, '')),
              :$!SPEC,
              :$!CWD,
            );
        }
    }

    method child (IO::Path:D: \child) {
        self.bless(
            :path( $!SPEC.join('', $!path,
                nqp::if(nqp::istype(child, Str), child, child.Str)
            )),
            :$!SPEC, :$!CWD
        );
    }

    method concat-with (IO::Path:D: Str() \what) {
        self.bless: :path($!SPEC.join: '', $!path, what), :$!SPEC, :$!CWD;
    }

    proto method chdir(|) { * }
    multi method chdir(IO::Path:D: Str() $path, :$test!) {
        DEPRECATED(
            :what<:$test argument>,
            'individual named parameters (e.g. :r, :w, :x)',
            "v2017.03.101.ga.5800.a.1", "v6.d", :up(*),
        );
        self.chdir: $path, |$test.words.map(* => True).Hash;
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
        my $dir = IO::Path.new-from-absolute-path($path,:$!SPEC,:CWD(self));

        nqp::stmts(
            nqp::unless(
                nqp::unless(nqp::isfalse($d), $dir.d),
                fail X::IO::Chdir.new: :$path, :os-error(
                    nqp::if($dir.e, 'is not a directory', 'does not exist')
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

    proto method rename(|) { * }
    multi method rename(IO::Path:D: IO::Path:D $to, :$createonly) {
        if $createonly and $to.e {
            fail X::IO::Rename.new(
              :from($.absolute),
              :$to,
              :os-error(':createonly specified and destination exists'),
            );
        }
        nqp::rename($.absolute, nqp::unbox_s($to.absolute));
        CATCH { default {
            fail X::IO::Rename.new(
              :from($!abspath), :to($to.absolute), :os-error(.Str) );
        } }
        True;
    }
    multi method rename(IO::Path:D: $to, :$CWD = $*CWD, |c) {
        self.rename($to.IO(:$!SPEC,:$CWD),|c);
    }

    proto method copy(|) { * }
    multi method copy(IO::Path:D: IO::Path:D $to, :$createonly) {
        if $createonly and $to.e {
            fail X::IO::Copy.new(
              :from($.absolute),
              :$to,
              :os-error(':createonly specified and destination exists'),
            );
        }
        nqp::copy($.absolute, nqp::unbox_s($to.absolute));
        CATCH { default {
            fail X::IO::Copy.new(
              :from($!abspath), :$to, :os-error(.Str) );
        } }
        True;
    }
    multi method copy(IO::Path:D: $to, :$CWD  = $*CWD, |c) {
        self.copy($to.IO(:$!SPEC,:$CWD),|c);
    }

    method move(IO::Path:D: |c) {
        my $result = self.copy(|c);

        fail X::IO::Move.new(
            :from($result.exception.from),
            :to($result.exception.to),
            :os-error($result.exception.os-error),
        ) unless $result.defined;

        $result = self.unlink();

        fail X::IO::Move.new(
            :from($result.exception.from),
            :to($result.exception.to),
            :os-error($result.exception.os-error),
        ) unless $result.defined;

        True
    }

    method chmod(IO::Path:D: Int() $mode) {
        nqp::chmod($.absolute, nqp::unbox_i($mode));
        CATCH { default {
            fail X::IO::Chmod.new(
              :path($!abspath), :$mode, :os-error(.Str) );
        } }
        True;
    }
    method unlink(IO::Path:D:) {
        nqp::unlink($.absolute);
        CATCH { default {
            fail X::IO::Unlink.new( :path($!abspath), os-error => .Str );
        } }
        True;
    }

    method symlink(IO::Path:D: $name is copy, :$CWD  = $*CWD) {
        $name = $name.IO(:$!SPEC,:$CWD).path;
        nqp::symlink(nqp::unbox_s($name), $.absolute);
        CATCH { default {
            fail X::IO::Symlink.new(:target($!abspath), :$name, os-error => .Str);
        } }
        True;
    }

    method link(IO::Path:D: $name is copy, :$CWD  = $*CWD) {
        $name = $name.IO(:$!SPEC,:$CWD).path;
        nqp::link(nqp::unbox_s($name), $.absolute);
        CATCH { default {
            fail X::IO::Link.new(:target($!abspath), :$name, os-error => .Str);
        } }
        True;
    }

    method mkdir(IO::Path:D: $mode = 0o777) {
        nqp::mkdir($.absolute, $mode);
        CATCH { default {
            fail X::IO::Mkdir.new(:path($!abspath), :$mode, os-error => .Str);
        } }
        True;
    }

    method rmdir(IO::Path:D:) {
        nqp::rmdir($.absolute);
        CATCH { default {
            fail X::IO::Rmdir.new(:path($!abspath), os-error => .Str);
        } }
        True;
    }

    proto method dir(|) {*} # make it possible to augment with multies from modulespace
    multi method dir(IO::Path:D:
        Mu :$test = $*SPEC.curupdir,
        :$absolute,
        :$Str,
        :$CWD = $*CWD,
    ) {

        CATCH { default {
            fail X::IO::Dir.new(
              :path($.absolute), :os-error(.Str) );
        } }

        my str $dir-sep  = $!SPEC.dir-sep;
        my int $relative = !$absolute && !$.is-absolute;

        my str $abspath = $.absolute.ends-with($dir-sep)
          ?? $.absolute
          !! $.absolute ~ $dir-sep;

        my str $path = $!path eq '.' || $!path eq $dir-sep
          ?? ''
          !! $!path.ends-with($dir-sep)
            ?? $!path
            !! $!path ~ $dir-sep;

        my Mu $dirh := nqp::opendir(nqp::unbox_s($.absolute));
        gather {
#?if jvm
            for <. ..> -> $elem {
                if $test.ACCEPTS($elem) {
                    $Str
                      ?? !$absolute
                        ?? take $path ~ $elem
                        !! take $abspath ~ $elem
                      !! !$absolute
                        ?? take IO::Path.new($path ~ $elem,:$!SPEC,:$CWD)
                        !! take IO::Path.new-from-absolute-path($abspath ~ $elem,:$!SPEC,:$CWD);
                }
            }
#?endif
            nqp::until(
              nqp::isnull_s(my str $str_elem = nqp::nextfiledir($dirh))
                || nqp::iseq_i(nqp::chars($str_elem),0),
              nqp::if(
                $test.ACCEPTS($str_elem),
                nqp::if(
                  $Str,
                  (take
                    nqp::concat(nqp::if($relative,$path,$abspath),$str_elem)),
                  nqp::if(
                    $relative,
                    (take IO::Path.new(
                      nqp::concat($path,$str_elem),:$!SPEC,:$CWD)),
                    (take IO::Path.new-from-absolute-path(
                      nqp::concat($abspath,$str_elem),:$!SPEC,:$CWD))
                  )
                )
              )
            );
            nqp::closedir($dirh);
        }
    }

    proto method slurp() { * }
    multi method slurp(IO::Path:D:) {

        # clean call, try the fast way
        if nqp::iseq_i(nqp::elems(nqp::getattr(%_,Map,'$!storage')),0)
            && nqp::open(self.absolute,"r") -> $PIO {
            LEAVE nqp::closefh(nqp::decont($PIO));
            nqp::p6box_s(nqp::readallfh(nqp::decont($PIO)))
        }

        # need to do the slow way
        else {
            my $handle = self.open;
            $handle // $handle.throw;
            LEAVE $handle.close;

            my Mu $PIO := nqp::getattr(nqp::decont($handle),IO::Handle,'$!PIO');
            if %_<bin> {
                my $res;
                # normal file
                if Rakudo::Internals.FILETEST-S(self.absolute) -> int $size {
                    $res := nqp::readfh($PIO,buf8.new,$size)
                }
                # spooky file with zero size?
                else {
                    $res := buf8.new();
                    loop {
                        my $buf := nqp::readfh($PIO,buf8.new,0x100000);
                        last unless nqp::elems($buf);
                        $res.append($buf);
                    }
                }
                $res
            }
            else {
                $handle.encoding($_) with %_<enc>;
                nqp::p6box_s(nqp::readallfh($PIO))
            }
        }
    }

    method spurt(IO::Path:D: $data, :$enc = 'utf8', :$append, :$createonly) {
        my $fh := self.open:
            :$enc,     :bin(nqp::istype($data, Blob)),
            :mode<wo>, :create, :exclusive($createonly),
            :$append,  :truncate(
                nqp::if(nqp::isfalse($append), nqp::isfalse($createonly))
            );
        nqp::if( nqp::istype($fh, Failure), $fh, $fh.spurt($data, :close) )
    }

    proto method lines(|) { * }
    multi method lines(IO::Path:D: $limit = Whatever, |c) {
        my $handle = self.open(|c);
        LEAVE $handle.close;
        my $buf := nqp::create(IterationBuffer);
        nqp::istype($limit,Whatever) || $limit == Inf
          ?? $handle.iterator.push-all($buf)
          !! $handle.iterator.push-exactly($buf,$limit.Int);
        Seq.new(Rakudo::Iterator.ReifiedList($buf))
    }

    proto method comb(|) { * }
    multi method comb(IO::Path:D: Cool:D $comber = "", |c) {
        self.open(|c).comb($comber, :close);
    }
    multi method comb(IO::Path:D: Int:D $size, |c) {
        self.open(|c).comb($size, :close);
    }
    multi method comb(IO::Path:D: Regex:D $comber, |c) {
        self.open(|c).comb($comber, :close);
    }

    multi method split(IO::Path:D: Str:D $splitter = "", |c) {
        self.open(|c).split($splitter, :close);
    }
    multi method split(IO::Path:D: Regex:D $splitter, |c) {
        self.open(|c).split($splitter, :close);
    }

    proto method words(|) { * }
    multi method words(IO::Path:D: |c) {
        self.open(|c).words(:close);
    }

    method e(--> Bool:D) {
        ?Rakudo::Internals.FILETEST-E($.absolute) # must be $.absolute
    }
    method d(--> Bool:D) {
        $.e
          ?? ?Rakudo::Internals.FILETEST-D($!abspath)
          !! Failure.new(X::IO::DoesNotExist.new(:path(~self),:trying<d>))
    }

    method f(--> Bool:D) {
        $.e
          ?? ?Rakudo::Internals.FILETEST-F($!abspath)
          !! Failure.new(X::IO::DoesNotExist.new(:path(~self),:trying<f>))
    }

    method s(--> Int:D) {
        $.e
          ?? Rakudo::Internals.FILETEST-S($!abspath)
          !! Failure.new(X::IO::DoesNotExist.new(:path(~self),:trying<s>))
    }

    method l(--> Bool:D) {
        ?Rakudo::Internals.FILETEST-LE($.absolute)
          ?? ?Rakudo::Internals.FILETEST-L($!abspath)
          !! Failure.new(X::IO::DoesNotExist.new(:path(~self),:trying<l>))
    }

    method r(--> Bool:D) {
        $.e
          ?? ?Rakudo::Internals.FILETEST-R($!abspath)
          !! Failure.new(X::IO::DoesNotExist.new(:path(~self),:trying<r>))
    }

    method w(--> Bool:D) {
        $.e
          ?? ?Rakudo::Internals.FILETEST-W($!abspath)
          !! Failure.new(X::IO::DoesNotExist.new(:path(~self),:trying<w>))
    }

    method rw(--> Bool:D) {
        $.e
          ?? ?Rakudo::Internals.FILETEST-RW($!abspath)
          !! Failure.new(X::IO::DoesNotExist.new(:path(~self),:trying<rw>))
    }

    method x(--> Bool:D) {
        $.e
          ?? ?Rakudo::Internals.FILETEST-X($!abspath)
          !! Failure.new(X::IO::DoesNotExist.new(:path(~self),:trying<x>))
    }

    method rwx(--> Bool:D) {
        $.e
          ?? ?Rakudo::Internals.FILETEST-RWX($!abspath)
          !! Failure.new(X::IO::DoesNotExist.new(:path(~self),:trying<rwx>))
    }

    method z(--> Bool:D) {
        $.e
          ?? $.f
            ?? ?Rakudo::Internals.FILETEST-Z($!abspath)
            !! Failure.new( X::IO::NotAFile.new(:path(~self),:trying<z>))
          !! Failure.new(X::IO::DoesNotExist.new(:path(~self),:trying<z>))
    }

    method modified(--> Instant:D) {
        $.e
          ?? Instant.from-posix(Rakudo::Internals.FILETEST-MODIFIED($!abspath))
          !! Failure.new(X::IO::DoesNotExist.new(:path(~self),:trying<modified>))
    }

    method accessed(--> Instant:D) {
        $.e
          ?? Instant.from-posix(Rakudo::Internals.FILETEST-ACCESSED($!abspath))
          !! Failure.new(X::IO::DoesNotExist.new(:path(~self),:trying<accessed>))
    }

    method changed(--> Instant:D) {
        $.e
          ?? Instant.from-posix(Rakudo::Internals.FILETEST-CHANGED($!abspath))
          !! Failure.new(X::IO::DoesNotExist.new(:path(~self),:trying<changed>))
    }

    method mode(--> IntStr:D) {
        $.e
          ?? nqp::stmts(
              (my int $mode = nqp::stat($!abspath, nqp::const::STAT_PLATFORM_MODE) +& 0o7777),
              IntStr.new($mode, sprintf('%04o', $mode))
            )
          !! Failure.new(X::IO::DoesNotExist.new(:path(~self),:trying<mode>))
    }
}

my class IO::Path::Cygwin is IO::Path {
    method new(|c) { self.IO::Path::new(|c, :SPEC(IO::Spec::Cygwin) ) }
}
my class IO::Path::QNX is IO::Path {
    method new(|c) { self.IO::Path::new(|c, :SPEC(IO::Spec::QNX) ) }
}
my class IO::Path::Unix is IO::Path {
    method new(|c) { self.IO::Path::new(|c, :SPEC(IO::Spec::Unix) ) }
}
my class IO::Path::Win32 is IO::Path {
    method new(|c) { self.IO::Path::new(|c, :SPEC(IO::Spec::Win32) ) }
}

# vim: ft=perl6 expandtab sw=4
