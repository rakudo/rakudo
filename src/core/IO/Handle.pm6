my class IO::Path { ... }
my class Proc { ... }

my class IO::Handle {
    has $.path;
    has $!PIO;
    has $.chomp is rw = Bool::True;
    has $.nl-in = ["\x0A", "\r\n"];
    has Str:D $.nl-out is rw = "\n";
    has Str $.encoding;
    has Encoding::Decoder $!decoder;
    has Encoding::Encoder $!encoder;
    has int $!out-buffer;

    submethod TWEAK (:$encoding, :$bin, IO() :$!path = Nil) {
        nqp::if(
          $bin,
          nqp::isconcrete($encoding) && X::IO::BinaryAndEncoding.new.throw,
          $!encoding = $encoding || 'utf8')
    }

#?if moar
    # Make sure we close any open files on exit
    my $opened := nqp::list;
    my $opened-locker = Lock.new;
    method !remember-to-close(--> Nil) {
        $opened-locker.protect: {
            nqp::stmts(
              nqp::if(
                nqp::isge_i(
                  (my int $fileno = nqp::filenofh($!PIO)),
                  (my int $elems = nqp::elems($opened))
                ),
                nqp::setelems($opened,nqp::add_i($elems,1024))
              ),
              nqp::bindpos($opened,$fileno,$!PIO)
            )
        }
    }
    method !forget-about-closing(int $fileno --> Nil) {
        $opened-locker.protect: {
            nqp::bindpos($opened,$fileno,nqp::null)
        }
    }
    method !close-all-open-handles() {
        my int $i = 2;
        my int $elems = nqp::elems($opened);
        nqp::while(
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
          nqp::unless(
            nqp::isnull(my $PIO := nqp::atpos($opened,$i)),
            nqp::closefh($PIO)
          )
        )
    }
#?endif

    method open(IO::Handle:D:
      :$r, :$w, :$x, :$a, :$update,
      :$rw, :$rx, :$ra,
      :$mode is copy,
      :$create is copy,
      :$append is copy,
      :$truncate is copy,
      :$exclusive is copy,
      :$bin,
      :$enc is copy,
      :$chomp = $!chomp,
      :$nl-in is copy = $!nl-in,
      Str:D :$nl-out is copy = $!nl-out,
      :$out-buffer is copy,
    ) {

        nqp::if(
          $bin,
          nqp::stmts(
            nqp::isconcrete($enc) && X::IO::BinaryAndEncoding.new.throw,
            $!encoding = Nil),
          nqp::unless(
            nqp::isconcrete($enc),
            $enc = $!encoding));

        $mode = nqp::if(
          $mode, nqp::if(nqp::istype($mode, Str), $mode, $mode.Str),
          nqp::if($w && $r || $rw, nqp::stmts(($create              = True), 'rw'),
          nqp::if($x && $r || $rx, nqp::stmts(($create = $exclusive = True), 'rw'),
          nqp::if($a && $r || $ra, nqp::stmts(($create = $append    = True), 'rw'),
          nqp::if($r,                                                        'ro',
          nqp::if($w,              nqp::stmts(($create = $truncate  = True), 'wo'),
          nqp::if($x,              nqp::stmts(($create = $exclusive = True), 'wo'),
          nqp::if($a,              nqp::stmts(($create = $append    = True), 'wo'),
          nqp::if($update,                                                   'rw',
                                                                             'ro')))))))));

        nqp::if(
          nqp::iseq_s($!path.Str, '-'),
          nqp::if(
            nqp::iseq_s($mode, 'ro'),
            nqp::if(
              $*IN.opened,
              nqp::stmts(
                $*IN.encoding($enc),
                return $*IN),
              nqp::stmts(
                nqp::if(
                  nqp::iseq_s($*IN.path.Str, '-'),
                  $*IN = IO::Handle.new: :path(IO::Special.new: '<STDIN>')),
                return $*IN.open: :$enc,
                  :bin(nqp::isfalse(nqp::isconcrete($enc))))),
            nqp::if(
              nqp::iseq_s($mode, 'wo'),
              nqp::if(
                $*OUT.opened,
                nqp::stmts(
                  $*OUT.encoding($enc),
                  return $*OUT),
                nqp::stmts(
                  nqp::if(
                    nqp::iseq_s($*OUT.path.Str, '-'),
                    $*OUT = IO::Handle.new: :path(IO::Special.new: '<STDOUT>')),
                  return $*OUT.open: :w, :$enc,
                    :bin(nqp::isfalse(nqp::isconcrete($enc))))),
              die("Cannot open standard stream in mode '$mode'"))));

        if nqp::istype($!path, IO::Special) {
            my $what := $!path.what;
            if $what eq '<STDIN>' {
                $!PIO := nqp::getstdin();
            }
            elsif $what eq '<STDOUT>' {
                $!PIO := nqp::getstdout();
            }
            elsif $what eq '<STDERR>' {
                $!PIO := nqp::getstderr();
            }
            else {
                die "Don't know how to open '$_' especially";
            }
            $!chomp = $chomp;
            $!nl-out = $nl-out;
            if nqp::isconcrete($enc) {
                my $encoding = Encoding::Registry.find($enc);
                $!decoder := $encoding.decoder(:translate-nl);
                $!decoder.set-line-separators(($!nl-in = $nl-in).list);
                $!encoder := $encoding.encoder(:translate-nl);
                $!encoding = $encoding.name;
            }
            self!set-out-buffer-size($out-buffer);
            return self;
        }

        fail X::IO::Directory.new(:$!path, :trying<open>) if $!path.d;

        {
            CATCH { .fail }
            $!PIO := nqp::open(
              $!path.absolute,
              nqp::concat(
                nqp::if(nqp::iseq_s($mode, 'ro'), 'r',
                nqp::if(nqp::iseq_s($mode, 'wo'), '-',
                nqp::if(nqp::iseq_s($mode, 'rw'), '+',
                  die "Unknown mode '$mode'"))),
                nqp::concat(nqp::if($create,      'c', ''),
                nqp::concat(nqp::if($append,      'a', ''),
                nqp::concat(nqp::if($truncate,    't', ''),
                            nqp::if($exclusive,   'x', ''))))));
#?if moar
            self!remember-to-close;
#?endif
        }

        $!chomp = $chomp;
        $!nl-out = $nl-out;
        if nqp::isconcrete($enc) {
            my $encoding = Encoding::Registry.find($enc);
            $!decoder := $encoding.decoder(:translate-nl);
            $!decoder.set-line-separators(($!nl-in = $nl-in).list);
            $!encoder := $encoding.encoder(:translate-nl);
            $!encoding = $encoding.name;
        }
        self!set-out-buffer-size($out-buffer);
        self;
    }

    method out-buffer is rw {
        Proxy.new: :FETCH{ $!out-buffer }, STORE => -> $, \buffer {
            self!set-out-buffer-size: buffer;
        }
    }

    method !set-out-buffer-size($buffer is copy) {
        $buffer //= !nqp::isttyfh($!PIO);
        $!out-buffer = nqp::istype($buffer, Bool)
            ?? ($buffer ?? 8192 !! 0)
            !! $buffer.Int;
        nqp::setbuffersizefh($!PIO, $!out-buffer);
        $!out-buffer
    }

    method nl-in is rw {
        Proxy.new(
          FETCH => {
              $!nl-in
          },
          STORE => -> $, $nl-in {
            $!nl-in = $nl-in;
            $!decoder && $!decoder.set-line-separators($nl-in.list);
            $nl-in
          }
        );
    }

    method close(IO::Handle:D: --> True) {
        nqp::if(
          nqp::defined($!PIO),
          nqp::stmts(
#?if !moar
            nqp::closefh($!PIO), # TODO: catch errors
            $!PIO := nqp::null
#?endif
#?if moar
            (my int $fileno = nqp::filenofh($!PIO)),
            nqp::closefh($!PIO), # TODO: catch errors
            ($!PIO := nqp::null),
            self!forget-about-closing($fileno)
#?endif
          )
        )
    }

    method eof(IO::Handle:D:) {
        nqp::p6bool($!decoder
            ?? $!decoder.is-empty && self.eof-internal
            !! self.eof-internal)
    }

    method eof-internal() {
        nqp::eoffh($!PIO)
    }

    method read-internal(Int:D $bytes) {
        nqp::readfh($!PIO,buf8.new,nqp::unbox_i($bytes))
    }

    method get(IO::Handle:D:) {
        $!decoder or die X::IO::BinaryMode.new(:trying<get>);
        $!decoder.consume-line-chars(:$!chomp) // self!get-line-slow-path()
    }

    method !get-line-slow-path() {
        my $line := Nil;
        unless self.eof-internal && $!decoder.is-empty {
            loop {
                my $buf := self.read-internal(0x100000);
                if $buf.elems {
                    $!decoder.add-bytes($buf);
                    $line := $!decoder.consume-line-chars(:$!chomp);
                    last if nqp::isconcrete($line);
                }
                else {
                    $line := $!decoder.consume-line-chars(:$!chomp, :eof)
                        unless self.eof-internal && $!decoder.is-empty;
                    last;
                }
            }
        }
        $line
    }

    method getc(IO::Handle:D:) {
        $!decoder or die X::IO::BinaryMode.new(:trying<getc>);
        $!decoder.consume-exactly-chars(1) || (self!readchars-slow-path(1) || Nil)
    }

    # XXX TODO: Make these routine read handle lazily when we have Cat type
    method comb (IO::Handle:D: :$close, |c) {
        $!decoder or die X::IO::BinaryMode.new(:trying<comb>);
        self.slurp(:$close).comb: |c
    }
    method split(IO::Handle:D: :$close, |c) {
        $!decoder or die X::IO::BinaryMode.new(:trying<split>);
        self.slurp(:$close).split: |c
    }

    proto method words (|) {*}
    multi method words(IO::Handle:D \SELF: $limit, :$close) {
        $!decoder or die X::IO::BinaryMode.new(:trying<words>);
        nqp::istype($limit,Whatever) || $limit == Inf
          ?? self.words(:$close)
          !! $close
            ?? Seq.new(Rakudo::Iterator.FirstNThenSinkAll(
                self.words.iterator, $limit.Int, {SELF.close}))
            !! self.words.head($limit.Int)
    }
    multi method words(IO::Handle:D: :$close) {
        $!decoder or die X::IO::BinaryMode.new(:trying<words>);
        Seq.new(class :: does Iterator {
            has $!handle;
            has $!close;
            has str $!str;
            has int $!pos;
            has int $!searching;

            method !SET-SELF(\handle, $!close) {
                $!handle   := handle;
                $!searching = 1;
                $!str       = ""; # RT #126492
                self!next-chunk;
                self
            }
            method new(\handle, \close) {
                nqp::create(self)!SET-SELF(handle, close);
            }
            method !next-chunk() {
                my int $chars = nqp::chars($!str);
                $!str = $!pos < $chars ?? nqp::substr($!str,$!pos) !! "";
                $chars = nqp::chars($!str);

                while $!searching {
                    $!str = nqp::concat($!str,$!handle.readchars);
                    my int $new = nqp::chars($!str);
                    $!searching = 0 if $new == $chars; # end
                    $!pos = ($chars = $new)
                      ?? nqp::findnotcclass(
                           nqp::const::CCLASS_WHITESPACE, $!str, 0, $chars)
                      !! 0;
                    last if $!pos < $chars;
                }
            }
            method pull-one() {
                my int $chars;
                my int $left;
                my int $nextpos;

                while ($chars = nqp::chars($!str)) && $!searching {
                    while ($left = $chars - $!pos) > 0 {
                        $nextpos = nqp::findcclass(
                          nqp::const::CCLASS_WHITESPACE,$!str,$!pos,$left);
                        last unless $left = $chars - $nextpos; # broken word

                        my str $found =
                          nqp::substr($!str, $!pos, $nextpos - $!pos);
                        $!pos = nqp::findnotcclass(
                          nqp::const::CCLASS_WHITESPACE,$!str,$nextpos,$left);

                        return nqp::p6box_s($found);
                    }
                    self!next-chunk;
                }
                if $!pos < $chars {
                    my str $found = nqp::substr($!str,$!pos);
                    $!pos = $chars;
                    nqp::p6box_s($found)
                }
                else {
                    $!handle.close if $!close;
                    IterationEnd
                }
            }
            method push-all($target --> IterationEnd) {
                my int $chars;
                my int $left;
                my int $nextpos;

                while ($chars = nqp::chars($!str)) && $!searching {
                    while ($left = $chars - $!pos) > 0 {
                        $nextpos = nqp::findcclass(
                          nqp::const::CCLASS_WHITESPACE,$!str,$!pos,$left);
                        last unless $left = $chars - $nextpos; # broken word

                        $target.push(nqp::p6box_s(
                          nqp::substr($!str, $!pos, $nextpos - $!pos)
                        ));

                        $!pos = nqp::findnotcclass(
                          nqp::const::CCLASS_WHITESPACE,$!str,$nextpos,$left);
                    }
                    self!next-chunk;
                }
                $target.push(nqp::p6box_s(nqp::substr($!str,$!pos)))
                  if $!pos < $chars;
                $!handle.close if $close;
            }
        }.new(self, $close));
    }

    my role PIOIterator does Iterator {
        has $!handle;
        has $!chomp;
        has $!decoder;
        method new(\handle) {
            my \res = nqp::create(self);
            nqp::bindattr(res, self.WHAT, '$!handle', handle);
            nqp::bindattr(res, self.WHAT, '$!chomp',
                nqp::getattr(handle, IO::Handle, '$!chomp'));
            nqp::p6bindattrinvres(res, self.WHAT, '$!decoder',
                nqp::getattr(handle, IO::Handle, '$!decoder'))
        }
        method sink-all(--> IterationEnd) {
            nqp::seekfh(nqp::getattr($!handle, IO::Handle, '$!PIO'), 0, 2)  # seek to end
        }
    }

    method !LINES-ITERATOR (IO::Handle:D:) {
        $!decoder or die X::IO::BinaryMode.new(:trying<lines>);
        (nqp::eqaddr(self.WHAT,IO::Handle)
            ?? (class :: does PIOIterator { # exact type, can shortcircuit get
                method pull-one() {
                    # Slow path falls back to .get on the handle, which will
                    # replenish the buffer once we exhaust it.
                    $!decoder.consume-line-chars(:$!chomp) // ($!handle.get // IterationEnd)
                }
                method push-all($target --> IterationEnd) {
                    nqp::while(
                        nqp::isconcrete(my $line :=
                            $!decoder.consume-line-chars(:$!chomp) // $!handle.get),
                        $target.push($line)
                    )
                }
            })
            !! (class :: does Iterator {    # can *NOT* shortcircuit .get
                has $!handle;
                method new(\handle) {
                    nqp::p6bindattrinvres(
                      nqp::create(self),self.WHAT,'$!handle',handle)
                }
                method pull-one() {
                    nqp::if(
                      nqp::isconcrete(my $line := $!handle.get),
                      $line,
                      IterationEnd
                    )
                }
                method push-all($target --> IterationEnd) {
                    nqp::while(
                      nqp::isconcrete(my $line := $!handle.get),
                      $target.push($line)
                    )
                }
                method sink-all(--> IterationEnd) {
                    # can't seek pipes, so need the `try`
                    try $!handle.seek(0,SeekFromEnd)  # seek to end
                }
            })
        ).new(self)
    }

    proto method lines (|) {*}
    multi method lines(IO::Handle:D \SELF: $limit, :$close) {
        nqp::istype($limit,Whatever) || $limit == Inf
          ?? self.lines(:$close)
          !! $close
            ?? Seq.new(Rakudo::Iterator.FirstNThenSinkAll(
                self!LINES-ITERATOR, $limit.Int, {SELF.close}))
            !! self.lines.head($limit.Int)
    }
    multi method lines(IO::Handle:D \SELF: :$close!) {
      Seq.new(
        $close # use -1 as N in FirstNThenSinkAllSeq to get all items
          ?? Rakudo::Iterator.FirstNThenSinkAll(
              self!LINES-ITERATOR, -1, {SELF.close})
          !! self!LINES-ITERATOR
      )
    }
    multi method lines(IO::Handle:D:) { Seq.new(self!LINES-ITERATOR) }

    method read(IO::Handle:D: Int(Cool:D) $bytes = $*DEFAULT-READ-ELEMS) {
        # If we have one, read bytes via. the decoder to support mixed-mode I/O.
        $!decoder
            ?? ($!decoder.consume-exactly-bytes($bytes) // self!read-slow-path($bytes))
            !! self.read-internal($bytes)
    }

    method !read-slow-path($bytes) {
        if self.eof-internal && $!decoder.is-empty {
            buf8.new
        }
        else {
            $!decoder.add-bytes(self.read-internal($bytes max 0x100000));
            $!decoder.consume-exactly-bytes($bytes)
                // $!decoder.consume-exactly-bytes($!decoder.bytes-available)
                // buf8.new
        }
    }

    method readchars(Int(Cool:D) $chars = $*DEFAULT-READ-ELEMS) {
        $!decoder or die X::IO::BinaryMode.new(:trying<readchars>);
        $!decoder.consume-exactly-chars($chars) // self!readchars-slow-path($chars)
    }

    method !readchars-slow-path($chars) {
        my $result := '';
        unless self.eof-internal && $!decoder.is-empty {
            loop {
                my $buf := self.read-internal(0x100000);
                if $buf.elems {
                    $!decoder.add-bytes($buf);
                    $result := $!decoder.consume-exactly-chars($chars);
                    last if nqp::isconcrete($result);
                }
                else {
                    $result := $!decoder.consume-exactly-chars($chars, :eof);
                    last;
                }
            }
        }
        $result
    }

    multi method Supply(IO::Handle:D: :$size = $*DEFAULT-READ-ELEMS --> Supply:D) {
        if $!decoder { # handle is in character mode
            supply {
                my int $chars = $size;
                my str $str = self.readchars($chars);
                nqp::while(
                  nqp::chars($str),
                  nqp::stmts(
                    (emit nqp::p6box_s($str)),
                    ($str = self.readchars($chars))
                  )
                );
                done;
            }
        }
        else {
            supply {
                my $buf := self.read($size);
                nqp::while(
                  nqp::elems($buf),
                  nqp::stmts(
                    (emit $buf),
                    ($buf := self.read($size))
                  )
                );
                done;
            }
        }
    }

    proto method seek(|) {*}
    multi method seek(IO::Handle:D: Int:D $offset, SeekType:D $whence = SeekFromBeginning --> True) {
        my int $rewind = 0;
        if $!decoder {
            # consider bytes we pre-read, when seeking from current position:
            $rewind = $!decoder.bytes-available if
                nqp::eqaddr(nqp::decont($whence), SeekFromCurrent);

            # Freshen decoder, so we won't have stuff left over from earlier reads
            # that were in the wrong place.
            $!decoder := Encoding::Registry.find($!encoding).decoder(:translate-nl);
            $!decoder.set-line-separators($!nl-in.list);
        }
        nqp::seekfh($!PIO, $offset - $rewind, +$whence);
    }

    method tell(IO::Handle:D: --> Int:D) {
        nqp::tellfh($!PIO) - ($!decoder ?? $!decoder.bytes-available !! 0)
    }

    method write(IO::Handle:D: Blob:D $buf --> True) {
        self.write-internal($buf)
    }

    method write-internal(IO::Handle:D: Blob:D $buf --> True) {
        nqp::writefh($!PIO, nqp::decont($buf));
    }

    method opened(IO::Handle:D:) {
        nqp::p6bool(nqp::istrue($!PIO));
    }

    method t(IO::Handle:D:) {
        self.opened && nqp::p6bool(nqp::isttyfh($!PIO))
    }

    method lock(IO::Handle:D:
        Bool:D :$non-blocking = False, Bool:D :$shared = False --> True
    ) {
#?if moar
        self!forget-about-closing(nqp::filenofh($!PIO));
#?endif
        nqp::lockfh($!PIO, 0x10*$non-blocking + $shared);
        CATCH { default {
#?if moar
            self!remember-to-close;
#?endif
            fail X::IO::Lock.new: :os-error(.Str),
                :lock-type( 'non-' x $non-blocking ~ 'blocking, '
                    ~ ($shared ?? 'shared' !! 'exclusive') );
        }}
    }

    method unlock(IO::Handle:D: --> True) {
#?if moar
        self!remember-to-close;
#?endif
        nqp::unlockfh($!PIO);
    }

    method printf(IO::Handle:D: |c) {
        self.print(sprintf |c);
    }

    proto method print(|) {*}
    multi method print(IO::Handle:D: Str:D \x --> True) {
        $!decoder or die X::IO::BinaryMode.new(:trying<print>);
        self.write-internal($!encoder.encode-chars(x));
    }
    multi method print(IO::Handle:D: **@list is raw --> True) { # is raw gives List, which is cheaper
        self.print(@list.join);
    }
    multi method print(Junction:D \j) { j.THREAD: {self.print: $_} }

    proto method put(|) {*}
    multi method put(IO::Handle:D: Str:D \x --> True) {
        $!decoder or die X::IO::BinaryMode.new(:trying<put>);
        self.write-internal($!encoder.encode-chars(
            nqp::concat(nqp::unbox_s(x), nqp::unbox_s($!nl-out))))
    }
    multi method put(IO::Handle:D: **@list is raw --> True) { # is raw gives List, which is cheaper
        self.put(@list.join);
    }
    multi method put(Junction:D \j) { j.THREAD: {self.put: $_} }

    multi method say(IO::Handle:D: Str:D $x --> True) {
        $!decoder or die X::IO::BinaryMode.new(:trying<say>);
        self.write-internal($!encoder.encode-chars(
            nqp::concat(nqp::unbox_s($x), nqp::unbox_s($!nl-out))));
    }
    multi method say(IO::Handle:D: \x --> True) {
        $!decoder or die X::IO::BinaryMode.new(:trying<say>);
        self.write-internal($!encoder.encode-chars(
            nqp::concat(nqp::unbox_s(x.gist), nqp::unbox_s($!nl-out))))
    }
    multi method say(IO::Handle:D: |) {
        $!decoder or die X::IO::BinaryMode.new(:trying<say>);
        my Mu $args := nqp::p6argvmarray();
        nqp::shift($args);
        my str $conc = '';
        $conc = nqp::concat($conc, nqp::shift($args).gist) while $args;
        self.print(nqp::concat($conc, $!nl-out));
    }

    method print-nl(IO::Handle:D: --> True) {
        $!decoder or die X::IO::BinaryMode.new(:trying<print-nl>);
        self.write-internal($!encoder.encode-chars($!nl-out));
    }

    proto method slurp-rest(|) {*}
    multi method slurp-rest(IO::Handle:D: :$bin! where *.so, :$close --> Buf:D) {
        # NOTE: THIS METHOD WILL BE DEPRECATED IN 6.d in favour of .slurp()
        # Testing of it in roast master has been removed and only kept in 6.c
        # If you're changing this code for whatever reason, test with 6.c-errata
        LEAVE self.close if $close;
        my $res := buf8.new;
        loop {
            my $buf := self.read(0x100000);
            nqp::elems($buf)
              ?? $res.append($buf)
              !! return $res
        }
    }
    multi method slurp-rest(IO::Handle:D: :$enc, :$bin, :$close --> Str:D) {
        # NOTE: THIS METHOD WILL BE DEPRECATED IN 6.d in favour of .slurp()
        # Testing of it in roast master has been removed and only kept in 6.c
        # If you're changing this code for whatever reason, test with 6.c-errata
        $!decoder or die X::IO::BinaryMode.new(:trying<slurp-rest>);
        LEAVE self.close if $close;
        self.encoding($enc) if $enc.defined;
        self!slurp-all-chars()
    }

    method slurp(IO::Handle:D: :$close, :$bin) {
        nqp::stmts(
          (my $res),
          nqp::if(
            $!decoder,
            nqp::if(
              $bin,
              nqp::stmts(
                ($res := buf8.new),
                nqp::if(
                  $!decoder.bytes-available,
                  $res.append($!decoder.consume-exactly-bytes(
                    $!decoder.bytes-available)))),
              ($res := self!slurp-all-chars())),
            ($res := buf8.new)),
          nqp::if(
            nqp::isfalse($!decoder) || $bin,
            nqp::while(
              nqp::elems(my $buf := self.read-internal(0x100000)),
              $res.append($buf))),
          # don't sink result of .close; it might be a failed Proc
          nqp::if($close, my $ = self.close),
          $res)
    }

    method !slurp-all-chars() {
        while nqp::elems(my $buf := self.read-internal(0x100000)) {
            $!decoder.add-bytes($buf);
        }
        $!decoder.consume-all-chars()
    }

    proto method spurt(|) {*}
    multi method spurt(IO::Handle:D: Blob $data, :$close) {
        LEAVE self.close if $close;
        self.write-internal($data);
    }
    multi method spurt(IO::Handle:D: Cool $data, :$close) {
        LEAVE self.close if $close;
        self.print($data);
    }

    method path(IO::Handle:D:)      { $!path.IO }
    method IO(IO::Handle:D:)        { $!path.IO }

    # use $.path, so IO::Pipe picks it up
    multi method Str(IO::Handle:D:) { $.path.Str }

    multi method gist(IO::Handle:D:) {
        "{self.^name}<$!path.gist()>({self.opened ?? 'opened' !! 'closed'})"
    }

    method flush(IO::Handle:D: --> True) {
        CATCH { default { fail X::IO::Flush.new: :os-error(.Str) } }
        nqp::defined($!PIO) or die 'File handle not open, so cannot flush';
        nqp::flushfh($!PIO);
    }

    proto method encoding(|) {*}
    multi method encoding(IO::Handle:D:) { $!encoding // Nil }
    multi method encoding(IO::Handle:D: $new-encoding is copy, :$replacement, :$strict, Bool:D :$translate-nl = True) {
        with $new-encoding {
            if $_ eq 'bin' {
                $_ = Nil;
            }
            else {
                return $!encoding if $!encoding && $!encoding eq $_;
            }
        }
        with $!decoder {
            # We're switching encoding, or back to binary mode. First grab any
            # bytes the current decoder is holding on to but has not yet done
            # decoding of.
            my $available = $!decoder.bytes-available;
            with $new-encoding {
                my $prev-decoder := $!decoder;
                my $encoding = Encoding::Registry.find($new-encoding);
                $!decoder := $encoding.decoder(:$translate-nl, :$replacement, :$strict);
                $!decoder.set-line-separators($!nl-in.list);
                $!decoder.add-bytes($prev-decoder.consume-exactly-bytes($available))
                    if $available;
                $!encoder := $encoding.encoder(:$translate-nl, :$replacement, :$strict);
                $!encoding = $encoding.name;
            }
            else {
                nqp::seekfh($!PIO, -$available, SeekFromCurrent) if $available;
                $!decoder := Encoding::Decoder;
                $!encoder := Encoding::Encoder;
                $!encoding = Nil;
                Nil
            }
        }
        else {
            # No previous decoder; make a new one if needed, otherwise no change.
            with $new-encoding {
                my $encoding = Encoding::Registry.find($new-encoding);
                $!decoder := $encoding.decoder(:$translate-nl, :$replacement, :$strict);
                $!decoder.set-line-separators($!nl-in.list);
                $!encoder := $encoding.encoder(:$translate-nl, :$replacement, :$strict);
                $!encoding = $encoding.name;
            }
            else {
                Nil
            }
        }
    }

    submethod DESTROY(IO::Handle:D:) {
        # Close handles with any file descriptor larger than 2. Those below
        # are our $*IN, $*OUT, and $*ERR, and we don't want them closed
        # implicitly via DESTROY, since you can't get them back again.
        nqp::if(
          nqp::defined($!PIO)
            && nqp::isgt_i((my int $fileno = nqp::filenofh($!PIO)), 2),
          nqp::stmts(
            nqp::closefh($!PIO),  # don't bother checking for errors
#?if !moar
            $!PIO := nqp::null
#?endif
#?if moar
            ($!PIO := nqp::null),
            self!forget-about-closing($fileno)
#?endif
          )
        )
    }

    method native-descriptor(IO::Handle:D:) {
        nqp::defined($!PIO) or die 'File handle not open, so cannot get native descriptor';
        nqp::filenofh($!PIO)
    }
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*DEFAULT-READ-ELEMS', {
    PROCESS::<$DEFAULT-READ-ELEMS> := %*ENV<RAKUDO_DEFAULT_READ_ELEMS> // 65536;
}

# vim: ft=perl6 expandtab sw=4
