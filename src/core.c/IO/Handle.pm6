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
        $bin
          ?? nqp::isconcrete($encoding) && X::IO::BinaryAndEncoding.new.throw
          !! ($!encoding = $encoding || 'utf8')
    }

#?if moar
    # Make sure we close any open files on exit
    my $opened := nqp::list;
    my $opened-locker := Lock.new;
    method !remember-to-close(--> Nil) {
        $opened-locker.protect: {
            nqp::setelems($opened,nqp::elems($opened) + 1024)
              if nqp::isge_i(nqp::filenofh($!PIO),nqp::elems($opened));
            nqp::bindpos($opened,nqp::filenofh($!PIO),$!PIO);
        }
    }
    method !forget-about-closing(--> Nil) {
        $opened-locker.protect: {
            nqp::bindpos($opened,nqp::filenofh($!PIO),nqp::null)
        }
    }
    method !close-all-open-handles(--> Nil) {
        if nqp::elems($opened) -> int $elems {
            my int $i = 2;  # skip STDIN, STDOUT, STDERR
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              nqp::unless(
                nqp::isnull(my $PIO := nqp::atpos($opened,$i)),
                nqp::closefh($PIO)
              )
            )
        }
    }

    method do-not-close-automatically(IO::Handle:D: --> Bool:D) {
        if nqp::defined($!PIO) {
            self!forget-about-closing;
            True
        }
        else {
            False
        }
    }
#?endif
#?if !moar
    method do-not-close-automatically(IO::Handle:D: --> False) { }
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

            # Add a byte order mark to the start of the file for utf16
            nqp::if(nqp::iseq_s($!encoding, 'utf16'), (
                if $create && !$exclusive && (!$append || $append && $!path.s == 0) {
                  self.write: Buf[uint16].new(0xFEFF);
                })
            );
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
            nqp::if(
              nqp::isconcrete($!decoder),
              ($!decoder := Encoding::Decoder)
            ),
#?if moar
            self!forget-about-closing,  # mark as closed
#?endif
            nqp::closefh($!PIO),        # close, ignore errors
            $!PIO := nqp::null          # mark HLL handle now also closed
          )
        )
    }

    method eof(IO::Handle:D:) {
        nqp::hllbool($!decoder
            ?? $!decoder.is-empty && self.EOF
            !! self.EOF)
    }

    method EOF() {
        nqp::not_i(nqp::defined($!PIO)) || nqp::eoffh($!PIO)
    }

    method READ(Int:D $bytes) {
        nqp::readfh($!PIO,nqp::create(buf8.^pun),$bytes)
    }

    method !failed($trying) {
        ($!PIO ?? X::IO::BinaryMode !! X::IO::Closed).new(:$trying).throw
    }

    method get(IO::Handle:D:) {
        $!decoder
          ?? $!decoder.consume-line-chars(:$!chomp) // self!get-line-slow-path()
          !! self!failed('get')
    }

    method !get-line-slow-path() {
        my $line := Nil;
        unless self.EOF && $!decoder.is-empty {
            loop {
                my $buf := self.READ(0x100000);
                if $buf.elems {
                    $!decoder.add-bytes($buf);
                    $line := $!decoder.consume-line-chars(:$!chomp);
                    last if nqp::isconcrete($line);
                }
                else {
                    $line := $!decoder.consume-line-chars(:$!chomp, :eof)
                        unless self.EOF && $!decoder.is-empty;
                    last;
                }
            }
        }
        $line
    }

    method getc(IO::Handle:D:) {
        $!decoder
          ?? $!decoder.consume-exactly-chars(1)
               || (self!readchars-slow-path(1) || Nil)
          !! self!failed('getc')
    }

    # XXX TODO: Make these routine read handle lazily when we have Cat type
    method comb (IO::Handle:D: :$close, |c) {
        $!decoder
          ?? self.slurp(:$close).comb( |c )
          !! self!failed('comb')
    }
    method split(IO::Handle:D: :$close, |c) {
        $!decoder
          ?? self.slurp(:$close).split( |c )
          !! self!failed('split')
    }

    proto method words (|) {*}
    multi method words(IO::Handle:D \SELF: $limit, :$close) {
        $!decoder
          ?? nqp::istype($limit,Whatever) || $limit == Inf
            ?? self.words(:$close)
            !! $close
              ?? Seq.new(Rakudo::Iterator.FirstNThenSinkAll(
                  self.words.iterator, $limit.Int, {SELF.close}))
              !! self.words.head($limit.Int)
          !! self!failed('words')
    }

    my class Words does Iterator {
        has $!handle is built(:bind);
        has $!close  is built(:bind);
        has str $!str= ""; # https://github.com/Raku/old-issue-tracker/issues/4690;
        has int $!searching = 1;
        has int $!pos;

        method TWEAK() { self!next-chunk }
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
        method push-all(\target --> IterationEnd) {
            my int $chars;
            my int $left;
            my int $nextpos;

            while ($chars = nqp::chars($!str)) && $!searching {
                while ($left = $chars - $!pos) > 0 {
                    $nextpos = nqp::findcclass(
                      nqp::const::CCLASS_WHITESPACE,$!str,$!pos,$left);
                    last unless $left = $chars - $nextpos; # broken word

                    target.push(nqp::p6box_s(
                      nqp::substr($!str, $!pos, $nextpos - $!pos)
                    ));

                    $!pos = nqp::findnotcclass(
                      nqp::const::CCLASS_WHITESPACE,$!str,$nextpos,$left);
                }
                self!next-chunk;
            }
            target.push(nqp::p6box_s(nqp::substr($!str,$!pos)))
              if $!pos < $chars;
            $!handle.close if $!close;
        }
    }
    multi method words(IO::Handle:D: :$close) {
        $!decoder
          ?? Seq.new(Words.new(:handle(self), :$close))
          !! self!failed('words')
    }

    my class GetLineFast does Iterator {
        has $!handle;
        has $!chomp;
        has $!decoder;
        has $!close;
        method new(\handle,\close) {
            my \res = nqp::create(self);
            nqp::bindattr(res, self.WHAT, '$!handle', handle);
            nqp::bindattr(res, self.WHAT, '$!close', close);
            nqp::bindattr(res, self.WHAT, '$!chomp',
                nqp::getattr(handle, IO::Handle, '$!chomp'));
            nqp::p6bindattrinvres(res, self.WHAT, '$!decoder',
                nqp::getattr(handle, IO::Handle, '$!decoder'))
        }
        method pull-one() {
            # Slow path falls back to .get on the handle, which will
            # replenish the buffer once we exhaust it.
            nqp::if(
              nqp::isconcrete(
                my \consumed := $!decoder.consume-line-chars(:$!chomp)
              ),
              consumed,
              nqp::if(
                nqp::isconcrete(my \got := $!handle.get),
                got,
                nqp::stmts(
                  nqp::if($!close,$!handle.close),
                  IterationEnd
                )
              )
            )
        }
        method push-all(\target --> IterationEnd) {
            nqp::while(
              nqp::if(
                nqp::isconcrete(
                  my \consumed := $!decoder.consume-line-chars(:$!chomp)
                ),
                nqp::stmts(
                  target.push(consumed),
                  1
                ),
                nqp::if(
                  nqp::isconcrete(my \got := $!handle.get),
                  nqp::stmts(
                    target.push(got),
                    1
                  )
                )
              ),
              nqp::null
            );
            $!handle.close if $!close;
        }
        method sink-all(--> IterationEnd) {
            $!close
              ?? $!handle.close
              # can't seek pipes, so need the `try`
              !! try $!handle.seek(0,SeekFromEnd)  # seek to end
        }
    }
    my class GetLineSlow does Iterator {
        has $!handle;
        has $!close;
        method new(\handle,\close) {
            my \res = nqp::create(self);
            nqp::bindattr(res, self.WHAT, '$!close', close);
            nqp::p6bindattrinvres(res,self.WHAT,'$!handle',handle)
        }
        method pull-one() {
            nqp::if(
              nqp::isconcrete(my \line := $!handle.get),
              line,
              nqp::stmts(
                nqp::if($!close,$!handle.close),
                IterationEnd
              )
            )
        }
        method push-all(\target --> IterationEnd) {
            nqp::while(
              nqp::isconcrete(my \line := $!handle.get),
              target.push(line)
            );
            $!handle.close if $!close;
        }
        method sink-all(--> IterationEnd) {
            $!close
              ?? $!handle.close
              # can't seek pipes, so need the `try`
              !! try $!handle.seek(0,SeekFromEnd)  # seek to end
        }
    }
    method !LINES-ITERATOR(IO::Handle:D: $close) {
        $!decoder
          ?? nqp::eqaddr(self.WHAT,IO::Handle)
            ?? GetLineFast.new(self,$close)   # exact type, can shortcircuit
            !! GetLineSlow.new(self,$close)   # can *NOT* shortcircuit .get
          !! self!failed('lines')
    }

    proto method lines (|) {*}
    multi method lines(IO::Handle:D \SELF: $limit, :$close) {
        nqp::istype($limit,Whatever) || $limit == Inf
          ?? Seq.new(self!LINES-ITERATOR($close))
          !! $close
            ?? Seq.new(Rakudo::Iterator.FirstNThenSinkAll(
                self!LINES-ITERATOR(0), $limit.Int, {SELF.close}))
            !! self.lines.head($limit.Int)
    }
    multi method lines(IO::Handle:D \SELF: :$close) {
        Seq.new(self!LINES-ITERATOR($close))
    }

    method read(IO::Handle:D: Int(Cool:D) $bytes = $*DEFAULT-READ-ELEMS) {
        # If we have one, read bytes via. the decoder to support mixed-mode I/O.
        $!decoder
            ?? ($!decoder.consume-exactly-bytes($bytes) // self!read-slow-path($bytes))
            !! nqp::eqaddr(nqp::what(self),IO::Handle)
              ?? nqp::readfh($!PIO,nqp::create(buf8.^pun),$bytes)
              !! self.READ($bytes)
    }

    method !read-slow-path($bytes) {
        if self.EOF && $!decoder.is-empty {
            nqp::create(buf8.^pun)
        }
        else {
            $!decoder.add-bytes(self.READ($bytes max 0x100000));
            $!decoder.consume-exactly-bytes($bytes)
                // $!decoder.consume-exactly-bytes($!decoder.bytes-available)
                // nqp::create(buf8.^pun)
        }
    }

    method readchars(Int(Cool:D) $chars = $*DEFAULT-READ-ELEMS) {
        $!decoder
          ?? $!decoder.consume-exactly-chars($chars) // self!readchars-slow-path($chars)
          !! self!failed('readchars')
    }

    method !readchars-slow-path($chars) {
        my $result := '';
        unless self.EOF && $!decoder.is-empty {
            loop {
                my $buf := self.READ(0x100000);
                if $buf.elems {
                    $!decoder.add-bytes($buf);
                    $result := $!decoder.consume-exactly-chars($chars);
                    last if nqp::isconcrete($result);
                }
                else {
                    $result := $!decoder.consume-exactly-chars($chars, :eof)
                        unless self.EOF && $!decoder.is-empty;
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
        self.WRITE($buf)
    }

    method WRITE(IO::Handle:D: Blob:D $buf --> True) {
        nqp::writefh($!PIO, nqp::decont($buf));
    }

    method opened(IO::Handle:D:) {
        nqp::hllbool(nqp::istrue($!PIO));
    }

    method t(IO::Handle:D:) {
        self.opened && nqp::hllbool(nqp::isttyfh($!PIO))
    }

    method lock(IO::Handle:D:
        Bool:D :$non-blocking = False, Bool:D :$shared = False --> True
    ) {
#?if moar
        self!forget-about-closing;
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

    multi method print(IO::Handle:D: Junction:D \j) {
        j.THREAD: { self.print: $_ }
    }
    multi method print(IO::Handle:D: Str:D \x --> True) {
        $!decoder
          ?? self.WRITE($!encoder.encode-chars(x))
          !! self!failed('print')
    }
    multi method print(IO::Handle:D: \x --> True) {
        $!decoder
          ?? self.WRITE($!encoder.encode-chars(x.Str))
          !! self!failed('print')
    }
    multi method print(IO::Handle:D: | --> True) {
        self!failed('print') unless $!decoder;

        my Mu $args := nqp::p6argvmarray;
        nqp::shift($args);
        self.WRITE($!encoder.encode-chars(
          nqp::join("",Rakudo::Internals.StrList2list_s($args))))
    }

    multi method put(IO::Handle:D: Junction:D \j) {
        j.THREAD: { self.print: nqp::concat(.Str,$!nl-out) }
    }
    multi method put(IO::Handle:D: --> True) {
        $!decoder
          ?? self.WRITE($!encoder.encode-chars($!nl-out))
          !! self!failed('say')
    }
    multi method put(IO::Handle:D: \x --> True) {
        $!decoder
          ?? self.WRITE($!encoder.encode-chars(nqp::concat(x.Str, $!nl-out)))
          !! self!failed('put')
    }
    multi method put(IO::Handle:D: | --> True) {
        self!failed('put') unless $!decoder;

        my Mu $args := nqp::p6argvmarray;
        nqp::shift($args);
        my $parts := Rakudo::Internals.StrList2list_s($args);
        nqp::push_s($parts,$!nl-out);
        self.WRITE($!encoder.encode-chars(nqp::join("",$parts)))
    }

    multi method say(IO::Handle:D: --> True) {
        $!decoder
          ?? self.WRITE($!encoder.encode-chars($!nl-out))
          !! self!failed('say')
    }
    multi method say(IO::Handle:D: \x --> True) {
        $!decoder
          ?? self.WRITE($!encoder.encode-chars(nqp::concat(x.gist,$!nl-out)))
          !! self!failed('say')
    }
    multi method say(IO::Handle:D: | --> True) {
        self!failed('say') unless $!decoder;

        my Mu $args := nqp::p6argvmarray;
        nqp::shift($args);
        my $parts := Rakudo::Internals.GistList2list_s($args);
        nqp::push_s($parts,$!nl-out);
        self.WRITE($!encoder.encode-chars(nqp::join("",$parts)))
    }

    # there is no special handling, since it is supposed to give a
    # human readable version of the Junction.  Leaving the method here
    # so that future optimizers will not try to add it.
    # multi method say(Junction:D \j) { j.THREAD: { self.say: $_ } }

    method print-nl(IO::Handle:D: --> True) {
        $!decoder
          ?? self.WRITE($!encoder.encode-chars($!nl-out))
          !! self!failed('print-nl')
    }

    proto method slurp-rest(|) {*}
    multi method slurp-rest(IO::Handle:D: :$bin! where *.so, :$close --> Buf:D) {
        # NOTE: THIS METHOD WILL BE DEPRECATED IN 6.d in favour of .slurp()
        # Testing of it in roast master has been removed and only kept in 6.c
        # If you're changing this code for whatever reason, test with 6.c-errata
        LEAVE self.close if $close;
        my $res := nqp::create(buf8.^pun);
        loop {
            my $buf := self.read(0x100000);
            nqp::elems($buf)
              ?? $res.append($buf)
              !! return $res
        }
    }
    multi method slurp-rest(IO::Handle:D: :$enc, :$bin, :$close --> Str:D) {
        self!failed('slurp-rest') unless $!decoder;

        # NOTE: THIS METHOD WILL BE DEPRECATED IN 6.d in favour of .slurp()
        # Testing of it in roast master has been removed and only kept in 6.c
        # If you're changing this code for whatever reason, test with 6.c-errata
        LEAVE self.close if $close;
        self.encoding($enc) if $enc.defined;
        self!slurp-all-chars()
    }

    method slurp(IO::Handle:D: :$close, :$bin) {
        my $res;
        nqp::if(
          $!decoder,
          nqp::if(
            $bin,
            nqp::stmts(
              ($res := nqp::create(buf8.^pun)),
              nqp::if(
                $!decoder.bytes-available,
                $res.append(
                  $!decoder.consume-exactly-bytes($!decoder.bytes-available)
                )
              )
            ),
            ($res := self!slurp-all-chars)
          ),
          ($res := nqp::create(buf8.^pun))
        );

        nqp::if(
          nqp::isfalse($!decoder) || $bin,
          nqp::while(
            nqp::elems(my $buf := self.READ(0x100000)),
            $res.append($buf)
          )
        );

        # don't sink result of .close; it might be a failed Proc
#?if jvm
        nqp::stmts(
          nqp::if($close, my $ = self.close),
          $res
        )
#?endif
#?if !jvm
        my $ = self.close if $close;
        $res
#?endif
    }

    method !slurp-all-chars() {
        while nqp::elems(my $buf := self.READ(0x100000)) {
            $!decoder.add-bytes($buf);
        }
        $!decoder.consume-all-chars()
    }

    proto method spurt(|) {*}
    multi method spurt(IO::Handle:D: Blob $data, :$close) {
        LEAVE self.close if $close;
        self.WRITE($data);
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
                return $!encoding if $!decoder and $!encoding and $!encoding eq $_;
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

    submethod DESTROY(IO::Handle:D: --> Nil) {
        # Close handles with any file descriptor larger than 2. Those below
        # are our $*IN, $*OUT, and $*ERR, and we don't want them closed
        # implicitly via DESTROY, since you can't get them back again.

        self.close
          if nqp::defined($!PIO)                   # not closed yet
          && nqp::isgt_i(nqp::filenofh($!PIO),2)   # not a standard handle
#?if moar
          && nqp::not_i(                           # marked for closing
               nqp::isnull(nqp::atpos($opened,nqp::filenofh($!PIO)))
             )
#?endif
    }

    method native-descriptor(IO::Handle:D:) {
        nqp::defined($!PIO)
          ?? nqp::filenofh($!PIO)
          !! X::AdHoc.new( payload => 'File handle not open, so cannot get native descriptor' ).throw
    }
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*DEFAULT-READ-ELEMS', {
    PROCESS::<$DEFAULT-READ-ELEMS> := %*ENV<RAKUDO_DEFAULT_READ_ELEMS> // 65536;
}

# vim: expandtab shiftwidth=4
