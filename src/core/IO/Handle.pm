my class IO::Path { ... }
my class IO::Special { ... }
my class Proc { ... }

my class IO::Handle {
    has $.path;
    has $!PIO;
    has $.chomp is rw = Bool::True;
    has $.nl-in = ["\x0A", "\r\n"];
    has Str:D $.nl-out is rw = "\n";
    has Str $.encoding;
    has Rakudo::Internals::VMBackedDecoder $!decoder;

    method open(IO::Handle:D:
      :$r, :$w, :$x, :$a, :$update,
      :$rw, :$rx, :$ra,
      :$mode is copy,
      :$create is copy,
      :$append is copy,
      :$truncate is copy,
      :$exclusive is copy,
      :$bin,
      :$chomp = $!chomp,
      :$enc = $!encoding,
      :$nl-in is copy = $!nl-in,
      Str:D :$nl-out is copy = $!nl-out,
    ) {
        $mode = nqp::if(
          $mode,
          nqp::if(nqp::istype($mode, Str), $mode, $mode.Str),
          nqp::if(
            nqp::unless(nqp::if($r, $w), $rw), # $r && $w || $rw
            nqp::stmts(($create = True), 'rw'),
            nqp::if(
              nqp::unless(nqp::if($r, $x), $rx),
              nqp::stmts(($create = $exclusive = True), 'rw'),
              nqp::if(
                nqp::unless(nqp::if($r, $a), $ra),
                nqp::stmts(($create = $append = True), 'rw'),
                nqp::if(
                  $r, 'ro',
                  nqp::if(
                    $w,
                    nqp::stmts(($create = $truncate = True), 'wo'),
                    nqp::if(
                      $x,
                      nqp::stmts(($create = $exclusive = True), 'wo'),
                      nqp::if(
                        $a,
                        nqp::stmts(($create = $append = True), 'wo'),
                        nqp::if(
                          $update, 'rw',
                          'ro'
                        ),
                      ),
                    ),
                  ),
                ),
              ),
            ),
          ),
        );

        nqp::if(
            nqp::iseq_s($!path.Str, '-'),
            nqp::stmts(
                nqp::if(
                    nqp::iseq_s($mode, 'ro'),
                    (return $*IN),
                    nqp::if(
                        nqp::iseq_s($mode, 'wo'),
                        (return $*OUT),
                        die("Cannot open standard stream in mode '$mode'"),
                    ),
                ),
            ),
        );

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
            if $bin {
                die X::IO::BinaryAndEncoding.new if nqp::isconcrete($enc);
            }
            else {
                $!encoding = Rakudo::Internals.NORMALIZE_ENCODING($enc || 'utf-8');
                # XXX Remove next two lines after streaming decoder is in use
                nqp::setencoding($!PIO, $!encoding);
#?if !jvm
                Rakudo::Internals.SET_LINE_ENDING_ON_HANDLE($!PIO, $!nl-in = $nl-in);
#?endif
                $!decoder := Rakudo::Internals::VMBackedDecoder.new($!encoding);
                $!decoder.set-line-separators($!nl-in.list);
            }
            return self;
        }

        fail X::IO::Directory.new(:$!path, :trying<open>) if $!path.d;

        {
            CATCH { .fail }
            $!PIO := nqp::open(
                $!path.absolute,
                nqp::concat(
                    nqp::if(
                        nqp::iseq_s($mode, 'ro'), 'r',
                        nqp::if(
                            nqp::iseq_s($mode, 'wo'), '-',
                            nqp::if(
                                nqp::iseq_s($mode, 'rw'), '+',
                                die("Unknown mode '$mode'")
                            ),
                        ),
                    ),
                    nqp::concat(
                        nqp::if($create, 'c', ''),
                        nqp::concat(
                            nqp::if($append, 'a', ''),
                            nqp::concat(
                                nqp::if($truncate,  't', ''),
                                nqp::if($exclusive, 'x', ''),
                            ),
                        ),
                    )
                ),
            );
        }

        $!chomp = $chomp;
        $!nl-out = $nl-out;
        if $bin {
            die X::IO::BinaryAndEncoding.new if nqp::isconcrete($enc);
        }
        else {
            $!encoding = Rakudo::Internals.NORMALIZE_ENCODING($enc || 'utf-8');
            # XXX Remove next two lines after streaming decoder is in use
            nqp::setencoding($!PIO, $!encoding);
            Rakudo::Internals.SET_LINE_ENDING_ON_HANDLE($!PIO, $!nl-in = $nl-in);
            $!decoder := Rakudo::Internals::VMBackedDecoder.new($!encoding);
            $!decoder.set-line-separators($!nl-in.list);
        }
        self;
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
            nqp::closefh($!PIO), # TODO: catch errors
            $!PIO := nqp::null
          )
        )
    }

    method eof(IO::Handle:D:) {
        nqp::p6bool($!decoder
            ?? $!decoder.is-empty && nqp::eoffh($!PIO)
            !! nqp::eoffh($!PIO));
    }

    method get(IO::Handle:D:) {
        $!decoder or die X::IO::BinaryMode.new(:trying<get>);
        $!decoder.consume-line-chars(:$!chomp) // self!get-line-slow-path()
    }

    method !get-line-slow-path() {
        my $line := Str;
        unless nqp::eoffh($!PIO) && $!decoder.is-empty {
            loop {
                my $buf := nqp::readfh($!PIO, buf8.new, 0x100000);
                if $buf.elems {
                    $!decoder.add-bytes($buf);
                    $line := $!decoder.consume-line-chars(:$!chomp);
                    last if nqp::isconcrete($line);
                }
                else {
                    $line := $!decoder.consume-line-chars(:$!chomp, :eof);
                    last;
                }
            }
        }
        $line
    }

    method getc(IO::Handle:D:) {
        $!decoder or die X::IO::BinaryMode.new(:trying<getc>);
        $!decoder.consume-exactly-chars(1) || self!getc-slow-path()
    }

    method !getc-slow-path() {
        if nqp::eoffh($!PIO) && $!decoder.is-empty {
            Str
        }
        else {
            $!decoder.add-bytes(nqp::readfh($!PIO, buf8.new, 0x100000));
            $!decoder.consume-exactly-chars(1) // $!decoder.consume-all-chars()
        }
    }

    # XXX TODO: Make these routine read handle lazily when we have Cat type
    method comb (IO::Handle:D: :$close, |c) { self.slurp(:$close).comb:  |c }
    method split(IO::Handle:D: :$close, |c) { self.slurp(:$close).split: |c }

    proto method words (|) { * }
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
                    $!decoder.consume-line-chars(:$!chomp) // $!handle.get // IterationEnd
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
                      (my $line := $!handle.get).DEFINITE,
                      $line,
                      IterationEnd
                    )
                }
                method push-all($target --> IterationEnd) {
                    nqp::while(
                      (my $line := $!handle.get).DEFINITE,
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

    proto method lines (|) { * }
    multi method lines(IO::Handle:D \SELF: $limit, :$close) {
        nqp::istype($limit,Whatever) || $limit == Inf
          ?? self.lines(:$close)
          !! $close
            ?? Seq.new(Rakudo::Iterator.FirstNThenSinkAll(
                self!LINES-ITERATOR, $limit.Int, {SELF.close}))
            !! self.lines.head($limit.Int)
    }
    multi method lines(IO::Handle:D \SELF: :$close) {
      Seq.new(
        $close # use -1 as N in FirstNThenSinkAllSeq to get all items
          ?? Rakudo::Iterator.FirstNThenSinkAll(
              self!LINES-ITERATOR, -1, {SELF.close})
          !! self!LINES-ITERATOR
      )
    }
    multi method lines(IO::Handle:D:) { Seq.new(self!LINES-ITERATOR) }

    method read(IO::Handle:D: Int(Cool:D) $bytes) {
        # If we have on, read bytes via. the decoder to support mixed-mode I/O.
        $!decoder
            ?? ($!decoder.consume-exactly-bytes($bytes) // self!read-slow-path($bytes))
            !! nqp::readfh($!PIO,buf8.new,nqp::unbox_i($bytes))
    }

    method !read-slow-path($bytes) {
        if nqp::eoffh($!PIO) && $!decoder.is-empty {
            buf8.new
        }
        else {
            $!decoder.add-bytes(nqp::readfh($!PIO, buf8.new, $bytes max 0x10000));
            $!decoder.consume-exactly-bytes($bytes)
                // $!decoder.consume-exactly-bytes($!decoder.bytes-available)
                // buf8.new
        }
    }

    method readchars(Int(Cool:D) $chars = $*DEFAULT-READ-ELEMS) {
        $!decoder or die X::IO::BinaryMode.new(:trying<readchars>);
#?if jvm
        my Buf $buf := Buf.new;   # nqp::readcharsfh doesn't work on the JVM
        # a char = 2 bytes
        nqp::readfh($!PIO, $buf, nqp::unbox_i($chars + $chars));
        nqp::unbox_s($buf.decode);
#?endif
#?if !jvm
        nqp::readcharsfh($!PIO, nqp::unbox_i($chars));
#?endif
    }

    method Supply(IO::Handle:D: :$size = $*DEFAULT-READ-ELEMS --> Supply:D) {
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

    proto method seek(|) { * }
    multi method seek(IO::Handle:D: Int:D $offset, SeekType:D $whence = SeekFromBeginning) {
        nqp::seekfh($!PIO, $offset, +$whence);
    }

    method tell(IO::Handle:D: --> Int:D) {
        nqp::p6box_i(nqp::tellfh($!PIO));
    }

    method write(IO::Handle:D: Blob:D $buf --> True) {
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
        nqp::lockfh($!PIO, 0x10*$non-blocking + $shared);
        CATCH { default {
            fail X::IO::Lock.new: :os-error(.Str),
                :lock-type( 'non-' x $non-blocking ~ 'blocking, '
                    ~ ($shared ?? 'shared' !! 'exclusive') );
        }}
    }

    method unlock(IO::Handle:D: --> True) {
        nqp::unlockfh($!PIO);
    }

    method printf(IO::Handle:D: |c) {
        self.print(sprintf |c);
    }

    proto method print(|) { * }
    multi method print(IO::Handle:D: Str:D \x --> True) {
        $!decoder or die X::IO::BinaryMode.new(:trying<print>);
        nqp::writefh($!PIO, x.encode($!encoding));
    }
    multi method print(IO::Handle:D: **@list is raw --> True) { # is raw gives List, which is cheaper
        self.print(@list.join);
    }

    proto method put(|) { * }
    multi method put(IO::Handle:D: Str:D \x --> True) {
        $!decoder or die X::IO::BinaryMode.new(:trying<put>);
        nqp::writefh($!PIO,
          nqp::concat(nqp::unbox_s(x), nqp::unbox_s($!nl-out)).encode($!encoding))
    }
    multi method put(IO::Handle:D: **@list is raw --> True) { # is raw gives List, which is cheaper
        self.put(@list.join);
    }

    multi method say(IO::Handle:D: \x --> True) {
        $!decoder or die X::IO::BinaryMode.new(:trying<say>);
        nqp::writefh($!PIO,
          nqp::concat(nqp::unbox_s(x.gist), nqp::unbox_s($!nl-out)).encode($!encoding))
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
        nqp::writefh($!PIO, $!nl-out.encode($!encoding));
    }

    proto method slurp-rest(|) { * }
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

    method slurp(IO::Handle:D: :$close) {
        my $res;
        nqp::if(
          $!decoder,
          ($res := self!slurp-all-chars()),
          nqp::stmts(
            ($res := buf8.new),
            nqp::while(
              nqp::elems(my $buf := nqp::readfh($!PIO, buf8.new, 0x100000)),
              $res.append($buf)
            )
          )
        );

        # don't sink result of .close; it might be a failed Proc
        $ = self.close if $close;
        $res
    }

    method !slurp-all-chars() {
        while nqp::elems(my $buf := nqp::readfh($!PIO, buf8.new, 0x100000)) {
            $!decoder.add-bytes($buf);
        }
        $!decoder.consume-all-chars()
    }

    proto method spurt(|) { * }
    multi method spurt(IO::Handle:D: Blob $data, :$close) {
        LEAVE self.close if $close;
        self.write($data);
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

    proto method encoding(|) { * }
    multi method encoding(IO::Handle:D:) { $!encoding }
    multi method encoding(IO::Handle:D: $new-encoding is copy) {
        with $new-encoding {
            if $_ eq 'bin' {
                $_ = Nil;
            }
            else {
                $_ = Rakudo::Internals.NORMALIZE_ENCODING(.Str);
                return $!encoding if $!encoding eq $_;
            }
        }
        with $!decoder {
            # We're switching encoding, or back to binary mode. First grab any
            # bytes the current decoder is holding on to but has not yet done
            # decoding of.
            my $available = $!decoder.bytes-available;
            with $new-encoding {
                $!decoder := Rakudo::Internals::VMBackedDecoder.new($new-encoding);
                $!decoder.set-line-separators($!nl-in.list);
                $!decoder.add-bytes($!decoder.consume-exactly-bytes($available)) if $available;
                $!encoding = $new-encoding;
            }
            else {
                nqp::seekfh($!PIO, -$available, SeekFromCurrent) if $available;
                $!decoder := Rakudo::Internals::VMBackedDecoder;
                $!encoding = Nil;
            }
        }
        else {
            # No previous decoder; make a new one if needed, otherwise no change.
            with $new-encoding {
                $!decoder := Rakudo::Internals::VMBackedDecoder.new($new-encoding);
                $!decoder.set-line-separators($!nl-in.list);
                $!encoding = $new-encoding;
            }
            else {
                Nil
            }
        }
    }

    submethod DESTROY(IO::Handle:D:) {
        nqp::if(
          nqp::defined($!PIO),
          nqp::stmts(
            nqp::closefh($!PIO),  # don't bother checking for errors
            $!PIO := nqp::null
          )
        )
    }

    method native-descriptor(IO::Handle:D:) {
        nqp::filenofh($!PIO)
    }
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*DEFAULT-READ-ELEMS', {
    PROCESS::<$DEFAULT-READ-ELEMS> := %*ENV<RAKUDO_DEFAULT_READ_ELEMS> // 65536;
}

# vim: ft=perl6 expandtab sw=4
