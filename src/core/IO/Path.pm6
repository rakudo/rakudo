my class IO::Path is Cool does IO {
    has IO::Spec $.SPEC;
    has Str      $.CWD;
    has Str      $.path;
    has Bool     $!is-absolute;
    has Str      $!abspath;
    has          %!parts;

    multi method ACCEPTS(IO::Path:D: Cool:D \other --> Bool:D) {
        nqp::stmts(
          (my IO $io := other.IO),
          nqp::if(
            nqp::istype($io, IO::Path),
            nqp::hllbool(nqp::iseq_s(nqp::unbox_s($.absolute), nqp::unbox_s($io.absolute))),
            # Getting the native descriptors of paths requires opening files,
            # so let's just return False instead.
            False
          )
        )
    }

    submethod BUILD(:$!path!, :$!SPEC!, :$!CWD! --> Nil) {
        nqp::unless(nqp::chars($!path), # could be an IntStr, so check chars
            die "Must specify something as a path: did you mean '.' for the current directory?"
        );
        nqp::if(
               nqp::isne_i(nqp::index($!path, "\0"), -1)
            || nqp::isne_i(nqp::index($!CWD,  "\0"), -1),
            X::IO::Null.new.throw
        );
    }

    method !new-from-absolute-path($path, :$SPEC = $*SPEC, Str() :$CWD = $*CWD) {
        self.bless(:$path, :$SPEC, :$CWD)!set-absolute($path);
    }

    method !set-absolute($path) {
        $!is-absolute = True;
        $!abspath := $path;
        self;
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
        nqp::if(
          nqp::isconcrete($!is-absolute),
          $!is-absolute,
          $!is-absolute = nqp::hllbool($!SPEC.is-absolute: $!path))
    }
    method is-relative() {
        nqp::hllbool(
          nqp::not_i(
            nqp::if(
              nqp::isconcrete($!is-absolute),
              $!is-absolute,
              $!is-absolute = nqp::hllbool($!SPEC.is-absolute: $!path))))
    }

    method parts {
        %!parts || (%!parts := nqp::create(Map).STORE:
          $!SPEC.split($!path), :INITIALIZE)
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

    method Numeric(IO::Path:D:) { self.basename.Numeric }

    multi method IO(IO::Path:D: --> IO::Path:D) { self }

    multi method Str (IO::Path:D: --> Str:D) { $!path }
    multi method gist(IO::Path:D: --> Str:D) {
        $!is-absolute
          ?? qq|"$.absolute".IO|
          !! qq|"$.path".IO|
    }
    multi method perl(IO::Path:D: --> Str:D) {
        self.^name ~ ".new({$.path.perl}, {:$!SPEC.perl}, {:$!CWD.perl})"
    }

    method sibling(IO::Path:D: Str() \sibling) {
        $_ := self.parts;
        self.bless: :path($!SPEC.join: .<volume>, .<dirname>, sibling),
            :$!SPEC, :$!CWD;
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

    proto method absolute(|) {*}
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
    method resolve (IO::Path:D: :$completely) {
        # XXXX: Not portable yet; assumes POSIX semantics
        my int $max-depth = 256;
        my str $sep       = $!SPEC.dir-sep;
        my str $cur       = $!SPEC.curdir;
        my str $up        = $!SPEC.updir;
        my str $empty     = '';
        my Mu  $res-list := nqp::list_s();

        my %parts         = $!SPEC.split: self.absolute;
        my str $volume    = %parts<volume>;
        my str $resolved  = $volume;
        my $path          = $!SPEC.catpath: '', |%parts<dirname  basename>;

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
            nqp::unbox_s($path), 'utf8-c8', buf8.new)),
          (my int $ord-sep = nqp::ord($sep)),
          (my int $els = nqp::elems($p)),
          (my int $i = -1),
          nqp::while(
            nqp::isne_i($els, $i = nqp::add_i($i, 1)),
            nqp::if(
              nqp::iseq_i(nqp::atpos_i($p, $i), $ord-sep),
              nqp::atposref_i($p, $i) = 0)),
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
            if !nqp::stat($next, nqp::const::STAT_EXISTS) {
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
        IO::Path!new-from-absolute-path($resolved,:$!SPEC,:CWD($volume ~ $sep));
    }
    proto method parent(|) {*}
    multi method parent(IO::Path:D: UInt:D $depth) {
        my $io = self;
        $io .= parent xx $depth;
        $io;
    }
    multi method parent(IO::Path:D:) {    # XXX needs work
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

    method child (IO::Path:D: Str() \child) {
        self.bless: :path($!SPEC.join: '', $!path, child), :$!SPEC, :$!CWD
    }

    method add (IO::Path:D: Str() \what) {
        self.bless: :path($!SPEC.join: '', $!path, what), :$!SPEC, :$!CWD;
    }

    multi method e       (IO::Path:D: --> Bool)    {
        ?Rakudo::Internals.FILETEST-E: $.absolute # must be $.absolute
    }
    multi method d       (IO::Path:D: --> Bool)    {
        ?Rakudo::Internals.FILETEST-D: $!abspath
    }
    multi method f       (IO::Path:D: --> Bool)    {
        ?Rakudo::Internals.FILETEST-F: $!abspath
    }
    multi method s       (IO::Path:D: --> Int)     {
        nqp::p6box_i(Rakudo::Internals.FILETEST-S: $!abspath)
    }
    multi method l       (IO::Path:D: --> Bool)    {
        ?Rakudo::Internals.FILETEST-LE($.absolute)
          ?? ?Rakudo::Internals.FILETEST-L($!abspath)
          !! Failure.new: X::IO::DoesNotExist.new: :at($!abspath), :trying<l>
    }
    multi method z       (IO::Path:D: --> Bool)    {
        ?Rakudo::Internals.FILETEST-Z: $!abspath
    }
    multi method r       (IO::Path:D: --> Bool)    {
        ?Rakudo::Internals.FILETEST-R: $!abspath
    }
    multi method w       (IO::Path:D: --> Bool)    {
        ?Rakudo::Internals.FILETEST-W: $!abspath
    }
    multi method x       (IO::Path:D: --> Bool)    {
        ?Rakudo::Internals.FILETEST-X: $!abspath
    }
    multi method rw      (IO::Path:D: --> Bool)    {
        ?Rakudo::Internals.FILETEST-RW: $!abspath
    }
    multi method rwx     (IO::Path:D: --> Bool)    {
        ?Rakudo::Internals.FILETEST-RWX: $!abspath
    }
    multi method modified(IO::Path:D: --> Instant) {
        Instant.from-posix: Rakudo::Internals.FILETEST-MODIFIED: $!abspath
    }
    multi method accessed(IO::Path:D: --> Instant) {
        Instant.from-posix: Rakudo::Internals.FILETEST-ACCESSED: $!abspath
    }
    multi method changed (IO::Path:D: --> Instant) {
        Instant.from-posix: Rakudo::Internals.FILETEST-CHANGED: $!abspath
    }
    multi method mode    (IO::Path:D: --> IntStr)  {
        nqp::stmts(
          (my int $mode = nqp::stat($!abspath, nqp::const::STAT_PLATFORM_MODE) +& 0o7777),
          IntStr.new: $mode, sprintf '%04o', $mode
        )
    }

    multi method open-native(IO::Path:D:
        :$mode, :$create, :$append, :$truncate, :$exclusive --> Mu
    ) {
        nqp::open(
          $.absolute,
          nqp::concat(
            nqp::if(nqp::iseq_s($mode, 'ro'), 'r',
            nqp::if(nqp::iseq_s($mode, 'wo'), '-',
            nqp::if(nqp::iseq_s($mode, 'rw'), '+',
              die "Unknown mode '$mode'"))),
            nqp::concat(nqp::if($create,      'c', ''),
            nqp::concat(nqp::if($append,      'a', ''),
            nqp::concat(nqp::if($truncate,    't', ''),
                        nqp::if($exclusive,   'x', ''))))))
    }

    multi method open   (IO::Path:D: |c --> IO::Handle)               {
        IO::Handle.new(:file(self)).open(|c)
    }
#?if moar
    multi method watch  (IO::Path:D: --> IO::Notification)            {
        IO::Notification.watch-path: $.absolute
    }
#?endif
    multi method chmod  (IO::Path:D: Int() $mode --> True)            {
        nqp::chmod($.absolute, nqp::unbox_i($mode))
    }
    multi method rename (IO::Path:D: IO() $to, :$createonly --> True) {
        nqp::rename($.absolute, nqp::unbox_s($to.absolute));
    }
    multi method copy   (IO::Path:D: IO() $to, :$createonly --> True) {
        # XXX TODO: maybe move the sameness check to the nqp OP/VM
        nqp::if(
            nqp::iseq_s(
                (my $from-abs :=   $.absolute),
                (my $to-abs   := $to.absolute)),
            X::IO::Copy.new(:from($from-abs), :to($to-abs),
                :os-error('source and target are the same')).fail,
            nqp::copy($from-abs, $to-abs));
    }
    multi method move   (IO::Path:D: |c --> True)                     {
        self.copy(|c) orelse fail X::IO::Move.new: :from(.exception.from),
            :to(.exception.to), :os-error(.exception.os-error);
        self.unlink   orelse fail X::IO::Move.new: :from(.exception.from),
            :to(.exception.to), :os-error(.exception.os-error);
    }

    multi method unlink (IO::Path:D: --> True)          {
        nqp::unlink($.absolute);
    }
    multi method symlink(IO::Path:D: IO() $to --> True) {
        nqp::symlink($.absolute, $to.absolute);
    }
    multi method link   (IO::Path:D: IO() $to --> True) {
        nqp::link($.absolute, $to.absolute);
    }

    multi method chdir  (IO::Path:D: Str() $path, :$test!)           {
        Rakudo::Deprecations.DEPRECATED(
            :what<:$test argument>,
            'individual named parameters (e.g. :r, :w, :x)',
            "v2017.03.101.ga.5800.a.1", "v6.d", :up(*),
        );
        self.chdir: $path, |$test.words.map(* => True).Hash
    }
    multi method chdir  (IO::Path:D: IO() $path, |c --> IO::Path)    {
        self.chdir: $path.absolute, |c
    }
    multi method chdir  (
        IO::Path:D: Str() $path is copy, :$d = True, :$r, :$w, :$x --> IO::Path
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
        my $dir = IO::Path!new-from-absolute-path($path,:$!SPEC,:CWD(self));

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
    multi method mkdir(IO::Path:D: Int() $mode = 0o777 --> IO::Path) {
        nqp::mkdir($.absolute, $mode);
        self
    }
    multi method rmdir(IO::Path:D: --> IO::Path)                     {
        nqp::rmdir($.absolute);
        self
    }
    multi method dir(IO::Path:D: Mu :$test = $*SPEC.curupdir)        {
        my str $dir-sep  = $!SPEC.dir-sep;
        my int $absolute = $.is-absolute;

        my str $abspath;
        $absolute && nqp::unless( # calculate $abspath only when we'll need it
            nqp::eqat(($abspath = $.absolute), $dir-sep,
                nqp::sub_i(nqp::chars($abspath), 1)),
            ($abspath = nqp::concat($abspath, $dir-sep)));

        my str $path = nqp::iseq_s($!path, '.') || nqp::iseq_s($!path, $dir-sep)
          ?? ''
          !! nqp::eqat($!path, $dir-sep, nqp::sub_i(nqp::chars($!path), 1))
            ?? $!path
            !! nqp::concat($!path, $dir-sep);

        my Mu $dirh := nqp::opendir(nqp::unbox_s($.absolute));
        gather {
          # set $*CWD inside gather for $test.ACCEPTS to use correct
          # $*CWD the user gave us, instead of whatever $*CWD is
          # when the gather is actually evaluated. We use a temp var
          # so that .IO coercer doesn't use the nulled `$*CWD` for
          # $!CWD attribute and we don't use `temp` for this, because
          # it's about 2x slower than using a temp var.
          my $cwd = $!CWD.IO;
          { my $*CWD = $cwd;
#?if jvm
            for <. ..> -> $elem {
                $test.ACCEPTS($elem) && (
                  $absolute
                    ?? take IO::Path!new-from-absolute-path(
                        $abspath ~ $elem,:$!SPEC,:$!CWD)
                    !! take IO::Path.new($path ~ $elem,:$!SPEC,:$!CWD)
                );
            }
#?endif
            nqp::until(
              nqp::isnull_s(my str $str-elem = nqp::nextfiledir($dirh))
                || nqp::iseq_i(nqp::chars($str-elem),0),
              nqp::if(
                $test.ACCEPTS($str-elem),
                nqp::if(
                  $absolute,
                  (take IO::Path!new-from-absolute-path(
                    nqp::concat($abspath,$str-elem),:$!SPEC,:$!CWD)),
                  (take IO::Path.new(
                    nqp::concat($path,$str-elem),:$!SPEC,:$!CWD)),)));
            nqp::closedir($dirh);
          }
        }
    }
}

my class IO::Path::Cygwin is IO::Path {
    method new(|c) { self.IO::Path::new(|c, :SPEC(IO::Spec::Cygwin) ) }
    multi method perl(::?CLASS:D: --> Str:D) {
        self.^name ~ ".new({$.path.perl}, {:$.CWD.perl})"
    }
}
my class IO::Path::QNX is IO::Path {
    method new(|c) { self.IO::Path::new(|c, :SPEC(IO::Spec::QNX) ) }
    multi method perl(::?CLASS:D: --> Str:D) {
        self.^name ~ ".new({$.path.perl}, {:$.CWD.perl})"
    }
}
my class IO::Path::Unix is IO::Path {
    method new(|c) { self.IO::Path::new(|c, :SPEC(IO::Spec::Unix) ) }
    multi method perl(::?CLASS:D: --> Str:D) {
        self.^name ~ ".new({$.path.perl}, {:$.CWD.perl})"
    }
}
my class IO::Path::Win32 is IO::Path {
    method new(|c) { self.IO::Path::new(|c, :SPEC(IO::Spec::Win32) ) }
    multi method perl(::?CLASS:D: --> Str:D) {
        self.^name ~ ".new({$.path.perl}, {:$.CWD.perl})"
    }
}

# vim: ft=perl6 expandtab sw=4
