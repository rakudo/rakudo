my class IO::CatHandle is IO::Handle {
    has @!handles       is built(:bind);
    has $!iterator;
    has $!active-handle is default(Nil);

    has Str  $.encoding;
    has      &.on-switch is rw;
    has Bool $.chomp     is rw = True;
    has      $.nl-in           = ["\x0A", "\r\n"];

    multi method raku(::?CLASS:D:) {
        my $parts = join ', ',
            @!handles.List.raku,
            (':!chomp' if not $!chomp),
            (":nl-in({$!nl-in.list.raku})" if $!nl-in !eqv ["\x0A", "\r\n"]),
            (nqp::isconcrete($!encoding)
                ?? ":encoding({$!encoding.raku})"
                !! ':bin'),
            (':&.on-switch({;})' if &!on-switch); # can't .raku Callables :(

        "{self.^name}.new($parts)"
    }

    method new(*@handles) { self.bless(:@handles, |%_) }
    submethod TWEAK(:$bin) {
        $bin
          ?? nqp::isconcrete($!encoding) && X::IO::BinaryAndEncoding.new.throw
          !! ($!encoding //= 'utf8');

        $!iterator := @!handles.iterator;
        self.next-handle;
    }

    # Set $!active-handle to the next handle in line, opening it if necessary
    method next-handle() is implementation-detail {
        my $old-handle is default(Nil) = $!active-handle;

        # Don't sink the result, since it might be an IO::Pipe that
        # returns a Proc that might throw
        my $ = .close with $!active-handle;

        # Logic for actually activating the next handle
        sub open($what) {
            my $handle = $what.open:
              :r, :$!chomp, :$!nl-in, :enc($!encoding),
              :bin(nqp::hllbool(nqp::isfalse($!encoding)));
            nqp::istype($handle,Failure)
              ?? $handle.throw
              !! $handle
        }

        if nqp::eqaddr((my $handle := $!iterator.pull-one),IterationEnd) {
            $!active-handle = Nil;
        }
        elsif nqp::istype($handle,IO::Handle) {
            if $handle.opened {
                # These aren't the attribute assignment inconsistencies
                # you're looking for!
                $handle.encoding: $!encoding; # *Jedi wave*
                $handle.nl-in   = $!nl-in;
                $handle.chomp   = $!chomp;
                $!active-handle = $handle;
            }
            else {
                $!active-handle = open($handle);
            }
        }
        else {
            $!active-handle = open($handle.IO);
        }

        if &!on-switch {
            if &!on-switch.count -> $c {
                $c == Inf || $c == 2
                  ?? &!on-switch($!active-handle, $old-handle)
                  !! $c == 1
                    ?? &!on-switch($!active-handle)
                    !! die ':&on-switch must have .count 0, 1, 2, or Inf';
            }
            else {
                &!on-switch();
            }
        }

        $!active-handle
    }

    my class Handles does Iterator {
        has $!cat is built(:bind);
        has $!gave-active;

        method pull-one {
            nqp::if(
              $!gave-active,
              nqp::if(
                nqp::defined(my $h := $!cat.next-handle),
                $h,
                IterationEnd),
              nqp::stmts(
                ($!gave-active := True),
                nqp::defined(my $ah := nqp::decont(
                  nqp::getattr($!cat, IO::CatHandle, '$!active-handle')))
                ?? $ah !! IterationEnd))
        }
    }
    method handles(IO::Handle:D: --> Seq:D) {
        Seq.new(Handles.new(cat => self))
    }

    method chomp (::?CLASS:D:) is rw {
        Proxy.new:
          :FETCH{ $!chomp },
          :STORE( -> $, $chomp {
              $!active-handle && $!active-handle.chomp = $chomp;
              $!chomp = $chomp
          })
    }

    # XXX TODO: Make these routine read handle lazily when we have Cat type
    method comb (::?CLASS:D: |c) { self.slurp.comb:  |c }
    method split(::?CLASS:D: |c) { self.slurp.split: |c }

    method !WORDS {
      nqp::if(
        nqp::defined($!active-handle),
        (flat $!active-handle.words, gather {
          nqp::while(
            nqp::defined(self.next-handle),
            take $!active-handle.words)}),
        Seq.new(Rakudo::Iterator.Empty)
      )
    }
    multi method words(::?CLASS:D \SELF: $limit, :$close) {
        nqp::istype($limit,Whatever) || $limit == Inf
          ?? self.words(:$close)
          !! $close
            ?? Seq.new(Rakudo::Iterator.FirstNThenSinkAll(
                self!WORDS.iterator, $limit.Int, {SELF.close}))
            !! self.words.head($limit.Int)
    }
    multi method words(::?CLASS:D \SELF: :$close!) {
      $close # use -1 as N in FirstNThenSinkAllSeq to get all items
        ?? Seq.new(Rakudo::Iterator.FirstNThenSinkAll(
            self!WORDS.iterator, -1, {SELF.close}))
        !! self!WORDS
    }
    multi method words(::?CLASS:D:) { self!WORDS }

    method !LINES {
      nqp::if(
        nqp::defined($!active-handle),
        (flat $!active-handle.lines, gather {
          nqp::while(
            nqp::defined(self.next-handle),
            take $!active-handle.lines)}),
        Seq.new(Rakudo::Iterator.Empty)
      )
    }
    multi method lines(::?CLASS:D \SELF: $limit, :$close) {
        nqp::istype($limit,Whatever) || $limit == Inf
          ?? self.lines(:$close)
          !! $close
            ?? Seq.new(Rakudo::Iterator.FirstNThenSinkAll(
                self!LINES.iterator, $limit.Int, {SELF.close}))
            !! self.lines.head($limit.Int)
    }
    multi method lines(::?CLASS:D \SELF: :$close!) {
      $close # use -1 as N in FirstNThenSinkAllSeq to get all items
        ?? Seq.new(Rakudo::Iterator.FirstNThenSinkAll(
            self!LINES.iterator, -1, {SELF.close}))
        !! self!LINES
    }
    multi method lines(::?CLASS:D:) { self!LINES }

    multi method Supply (::?CLASS:D: :$size = $*DEFAULT-READ-ELEMS --> Supply:D) {
        nqp::if(
          nqp::isconcrete($!encoding),
          (supply nqp::stmts(
            (my str $str = self.readchars: $size),
            nqp::while(
              nqp::chars($str),
              nqp::stmts(
                (emit nqp::p6box_s($str)),
                ($str = self.readchars: $size))),
            done)),
          (supply nqp::stmts(
            (my $buf := self.read: $size),
            nqp::while(
              nqp::elems($buf),
              nqp::stmts(
                (emit $buf),
                ($buf := self.read: $size))),
            done)))
    }

    # Get a single result, going to the next handle on EOF
    method get (::?CLASS:D:) {
        nqp::if(
          nqp::defined($!active-handle),
          nqp::stmts(
            nqp::while(
              nqp::eqaddr(Nil, my $res := $!active-handle.get)
              && nqp::defined(self.next-handle),
              nqp::null),
            $res),
          Nil)
    }
    method getc (::?CLASS:D:) {
        nqp::if(
          nqp::defined($!active-handle),
          nqp::stmts(
            nqp::while(
              nqp::eqaddr(Nil, my $res := $!active-handle.getc)
              && nqp::defined(self.next-handle),
              nqp::null),
            $res),
          Nil)
    }
    method read (::?CLASS:D: Int(Cool:D) $bytes = $*DEFAULT-READ-ELEMS) {
        # The logic is:
        #   read some stuff
        #     do we have enough stuff?
        #     -> [yes] -> return stuff
        #     -> [no]
        #       is current handle EOF or did we read zero stuff on last chunk?
        #       -> [yes] -> switch handle -> repeat from start
        #       -> [no] return stuff
        # The extra gymnastics are due to:
        #   (a) possibility of TTY handles returning
        #       fewer than requested number of bytes without being entirely
        #       exhausted. This means when we read fewer than $bytes bytes, we
        #      don't yet know whether we should switch the handle and thus,
        #       if we read at least some bytes in a chunk and don't have EOF,
        #       we gotta return whatever we managed to read
        #   (b) XXX TODO: (this actually seems to be a bug)
        #       possibility of .seek being used on current handle. In such a
        #       case we can read a zero-sized chunk and EOF would still be false
        nqp::unless(
          nqp::defined($!active-handle),
          nqp::create(buf8.^pun),
          nqp::stmts(
            (my $ret := nqp::create(buf8.^pun)),
            (my int $stop = 0),
            nqp::until(
              $stop,
              nqp::stmts(
                (my $chunk := buf8.new: $!active-handle.read:
                  nqp::sub_i($bytes,nqp::elems($ret))),
                $ret.append($chunk),
                nqp::if(
                  nqp::isge_i(nqp::elems($ret),$bytes),
                  ($stop = 1),
                  nqp::if(
                    $!active-handle.eof || nqp::isfalse(nqp::elems($chunk)),
                    nqp::unless(
                      nqp::defined(self.next-handle),
                      $stop = 1),
                    $stop = 1)))),
            $ret))
    }
    method readchars (::?CLASS:D: Int(Cool:D) $chars = $*DEFAULT-READ-ELEMS) {
        nqp::if(
          nqp::defined($!active-handle),
          nqp::stmts(
            (my $ret := $!active-handle.readchars: $chars),
            nqp::while(
              nqp::islt_i(nqp::chars($ret), $chars)
              && nqp::defined(self.next-handle),
              $ret := nqp::concat($ret, $!active-handle.readchars:
                nqp::sub_i($chars, nqp::chars($ret)))),
            $ret
          ),
          '')
    }

    method slurp (::?CLASS:D: :$bin) {
        # we don't take a :close arg, because we close exhausted handles
        # and .slurp isn't lazy, so all handles will get exhausted
        nqp::if(
          nqp::defined($!active-handle),
          ([~] gather nqp::stmts( # the [~] takes care of both Str and Blobs
            (take $!active-handle.slurp(:$bin, :close)),
            nqp::while(
              nqp::defined(self.next-handle),
              take $!active-handle.slurp(:$bin, :close)))),
          Nil)
    }
    method slurp-rest (|) {
        # We inherit deprecated .slurp-rest from IO::Handle. Pull the
        # plug on it in this class, since no one is using this yet.
        # The old IO::ArgFiles used .slurp
        X::Obsolete.new( :old<slurp-rest>, :replacement<slurp>,
            :when('with IO::CatHandle')).throw
    }
    method DESTROY { self.close }

    method close (::?CLASS:D: --> True) {
        # Note: our IO::Handles might be IO::Pipes, whose .close
        # method returns the Proc object, which will explode when sunk if the
        # process exited unsuccessfully. So here, we ensure we never sink it.

        my $unsink = .close with $!active-handle;
        nqp::until(
          nqp::eqaddr((my $pulled := $!iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype($pulled,IO::Handle),
            ($unsink = $pulled.close)
          )
        );
        $!active-handle = Nil;
    }

    proto method encoding(|) {*}
    multi method encoding(::?CLASS:D:) { $!encoding || Nil }
    multi method encoding(::?CLASS:D: $enc) {
        $!encoding = nqp::if(
          nqp::defined($!active-handle),
          $!active-handle.encoding($enc),
          nqp::if(
            nqp::isfalse($enc.defined) || nqp::iseq_s($enc.Str, 'bin'),
            Nil,
            Encoding::Registry.find($enc.Str).name))
    }

    method eof (::?CLASS:D: --> Bool:D) {
        nqp::hllbool(
          nqp::stmts(
            nqp::while(
              $!active-handle
              && $!active-handle.eof
              && self.next-handle,
              nqp::null),
            nqp::isfalse($!active-handle)
            || False))
    }
    multi method gist (::?CLASS:D:) {
        "{self.^name}({self.opened ?? "opened on {$.path.gist}" !! 'closed'})"
    }
    multi method Str (::?CLASS:D:) {
        $!active-handle ?? $.path.Str !! '<closed IO::CatHandle>'
    }
    method IO (::?CLASS:D:) {
        $!active-handle ?? $!active-handle.IO !! Nil
    }
    method path (::?CLASS:D:) {
        $!active-handle ?? $!active-handle.path !! Nil
    }
    method opened(::?CLASS:D: --> Bool:D) { nqp::hllbool(nqp::istrue($!active-handle)) }
    method lock(::?CLASS:D: |c) {
        $!active-handle ?? $!active-handle.lock(|c) !! Nil
    }
    method nl-in (::?CLASS:D:) is rw {
        Proxy.new:
          :FETCH{ $!nl-in },
          :STORE( -> $, $nl-in {
              $!active-handle && $!active-handle.nl-in = $nl-in;
              $!nl-in = $nl-in
          })
    }
    method seek(::?CLASS:D: |c) {
        $!active-handle ?? $!active-handle.seek(|c) !! Nil
    }
    method tell(::?CLASS:D: --> Int:D) {
        $!active-handle ?? $!active-handle.tell !! Nil
    }
    method t (::?CLASS:D: --> Bool:D) {
        $!active-handle ?? $!active-handle.t !! False
    }
    method unlock(::?CLASS:D:) {
        $!active-handle ?? $!active-handle.unlock !! Nil
    }
    method native-descriptor (::?CLASS:D: --> Int:D) {
        $!active-handle ?? $!active-handle.native-descriptor !! Nil
    }
    method open (::?CLASS:D: --> ::?CLASS:D) {
        # The idea behind cat handle's open is to fake .open in code that
        # doesn't know it's dealing with a cat handle, so we accept any args
        # IO::Handle.open accepts and then just return self. Since that .open
        # takes only named args methods have `*%_` in sigs, we don't put any
        # args in our sig. If that ever changes, then ensure cat handle's .open
        # can be called with any of the IO::Handle.open's args
        self
    }

    #                           __________________________________________
    #                          / I don't know what the write methods      \
    #                         | should do in a CatHandle, so I'll mark    |
    #                         | these as NYI, for now.... Has anyone      |
    #                         \ seen my cocoon? I always lose that thing! /
    #                         |  -----------------------------------------
    #                         | /
    #                         |/
    #                       (⛣)
    proto method flush      (|) {*}
    multi method flush      (|) { NYI('flush').throw      }
    proto method out-buffer (|) {*}
    multi method out-buffer (|) { NYI('out-buffer').throw }
    multi method print      (|) { NYI('print').throw      }
    proto method printf     (|) {*}
    multi method printf     (|) { NYI('printf').throw     }
    proto method print-nl   (|) {*}
    multi method print-nl   (|) { NYI('print-nl').throw   }
    multi method put        (|) { NYI('put').throw        }
    multi method say        (|) { NYI('say').throw        }
    proto method write      (|) {*}
    multi method write      (|) { NYI('write').throw      }
    proto method WRITE      (|) {*}
    multi method WRITE      (|) { NYI('WRITE').throw      }
    proto method READ       (|) {*}
    multi method READ       (|) { NYI('READ').throw       }
    proto method EOF        (|) {*}
    multi method EOF        (|) { NYI('EOF').throw        }
    #                       /|\

    # Don't die on this one, as doing so breaks .Capture
    # proto method nl-out  (|) {*}
    # multi method nl-out  (|) {
    #     die X::NYI.new: :feature<nl-out>
    # }
}

# vim: expandtab shiftwidth=4
