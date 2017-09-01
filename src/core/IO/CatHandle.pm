my class IO::CatHandle is IO::Handle {
    has $!handles;
    has $!active-handle is default(Nil);

    has $.chomp is rw;
    has $.nl-in;
    has Str $.encoding;
    has &.on-switch is rw;

    multi method perl(::?CLASS:D:) {
        my @handles =
            ($!active-handle if $!active-handle),
            |nqp::p6bindattrinvres((), List, '$!reified', $!handles);

        my $parts = join ', ',
            (@handles.List.perl if @handles),
            (':!chomp' if not $!chomp),
            (":nl-in({$!nl-in.list.perl})" if $!nl-in !eqv ["\x0A", "\r\n"]),
            (nqp::isconcrete($!encoding)
                ?? ":encoding({$!encoding.perl})"
                !! ':bin'),
            (':&.on-switch({;})' if &!on-switch); # can't .perl Callables :(

        "{self.^name}.new($parts)"
    }

    method !SET-SELF (
        @handles, &!on-switch, $!chomp, $!nl-in, $encoding, $bin
    ) {
        nqp::if(
          $bin,
          nqp::isconcrete($encoding) && X::IO::BinaryAndEncoding.new.throw,
          $!encoding = $encoding || 'utf8');

        @handles.elems; # reify
        $!handles := nqp::getattr(@handles || [], List, '$!reified');
        self.next-handle;
        self
    }
    method new (
        *@handles, :&on-switch,
        :$chomp = True, :$nl-in = ["\x0A", "\r\n"], Str :$encoding, Bool :$bin
    ) {
        self.bless!SET-SELF:
            @handles, &on-switch, $chomp, $nl-in, $encoding, $bin
    }
    method next-handle {
      # Set $!active-handle to the next handle in line, opening it if necessary
      nqp::stmts(
        (my $old-handle is default(Nil) = $!active-handle),
        nqp::if(
          nqp::defined($!active-handle),
          ($ = $!active-handle.close)), # don't sink the result, since it might
          # .. be an IO::Pipe that returns a Proc that might throw
        nqp::if(
          nqp::elems($!handles),
          nqp::if(
            nqp::istype(($_ := nqp::shift($!handles)), IO::Handle),
            nqp::if(
              .opened,
              nqp::stmts(
                (.encoding: $!encoding), # *Jedi wave*
                (.nl-in = $!nl-in),      # These aren't the attribute assignment
                (.chomp = $!chomp),      # inconsistencies you're looking for!
                $!active-handle = $_),
              nqp::if(
                nqp::istype(
                  ($_ = .open: :r, :$!chomp, :$!nl-in, :enc($!encoding),
                    :bin(nqp::isfalse($!encoding))),
                  Failure),
                .throw,
                ($!active-handle = $_))),
            nqp::if(
              nqp::istype(
                ($_ := .IO.open: :r, :$!chomp, :$!nl-in, :enc($!encoding),
                  :bin(nqp::isfalse($!encoding))),
                Failure),
              .throw,
              ($!active-handle = $_))),
          ($!active-handle = Nil)),
        nqp::if(
          &!on-switch,
          nqp::stmts(
            (my $c := &!on-switch.count),
            nqp::if(
              $c,
              nqp::if(
                nqp::istype($c, Num) || nqp::iseq_i($c, 2), # inf or 2
                &!on-switch($!active-handle, $old-handle),
                nqp::if(
                  nqp::iseq_i($c, 1),
                  &!on-switch($!active-handle),
                  die ':&on-switch must have .count 0, 1, 2, or Inf')),
              &!on-switch()))),
        $!active-handle)
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
        Seq.new: Rakudo::Iterator.Empty)
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
        Seq.new: Rakudo::Iterator.Empty)
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
        nqp::if(
          nqp::defined($!active-handle),
          nqp::stmts(
            (my $ret := buf8.new: $!active-handle.read: $bytes),
            nqp::while(
              nqp::islt_i(nqp::elems($ret), $bytes)
              && nqp::defined(self.next-handle),
              $ret.append: $!active-handle.read:
                nqp::sub_i($bytes, nqp::elems($ret))),
            $ret
          ),
          buf8.new)
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

    method slurp (::?CLASS:D:) {
        # we don't take a :close arg, because we close exhausted handles
        # and .slurp isn't lazy, so all handles will get exhausted
        nqp::if(
          nqp::defined($!active-handle),
          ([~] gather nqp::stmts( # the [~] takes care of both Str and Blobs
            (take $!active-handle.slurp),
            nqp::while(
              nqp::defined(self.next-handle),
              take $!active-handle.slurp))),
          Nil)
    }
    method slurp-rest (|) {
        # We inherit deprecated .slurp-rest from IO::Handle. Pull the
        # plug on it in this class, since no one is using this yet.
        # The old IO::ArgFiles used .slurp
        die X::Obsolete.new: :old<slurp-rest>, :replacement<slurp>,
            :when('with IO::CatHandle')
    }
    method DESTROY { self.close }

    method close (::?CLASS:D: --> True) {
        # Note: our IO::Handles might be IO::Pipes, whose .close
        # method returns the Proc object, which will explode when sunk if the
        # process exited unsuccessfully. So here, we ensure we never sink it.
        nqp::stmts(
          nqp::if(
            nqp::defined($!active-handle),
            $ = $!active-handle.close),
          (my int $i = -1),
          (my int $els = nqp::elems($!handles)),
          nqp::while(
            nqp::isgt_i($els, $i = nqp::add_i($i, 1)),
            nqp::if(
              nqp::istype(($_ := nqp::atpos($!handles, $i)), IO::Handle),
              $ = .close)),
          ($!handles := nqp::list),
          ($!active-handle = Nil))
    }

    proto method encoding(|) { * }
    multi method encoding(::?CLASS:D:) { $!encoding || Nil }
    multi method encoding(::?CLASS:D: $enc is copy) {
        $!encoding = nqp::if(
          nqp::defined($!active-handle),
          $!active-handle.encoding($enc),
          nqp::if(
            nqp::isfalse($enc.defined) || nqp::iseq_s($enc.Str, 'bin'),
            Nil,
            Encoding::Registry.find($enc.Str).name))
    }

    method eof (::?CLASS:D: --> Bool:D) {
        nqp::p6bool(
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
        nqp::if($!active-handle, $.path.Str, '<closed IO::CatHandle>')
    }
    method IO (::?CLASS:D:) {
        nqp::if($!active-handle, $!active-handle.IO, Nil)
    }
    method path (::?CLASS:D:) {
        nqp::if($!active-handle, $!active-handle.path, Nil)
    }
    method opened(::?CLASS:D: --> Bool:D) { nqp::p6bool($!active-handle) }
    method lock(::?CLASS:D: |c) {
        nqp::if($!active-handle, $!active-handle.lock(|c), Nil)
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
        nqp::if($!active-handle, $!active-handle.seek(|c), Nil)
    }
    method tell(::?CLASS:D: --> Int:D) {
        nqp::if($!active-handle, $!active-handle.tell, Nil)
    }
    method t (::?CLASS:D: --> Bool:D) {
        nqp::if($!active-handle, $!active-handle.t, False)
    }
    method unlock(::?CLASS:D:) {
        nqp::if($!active-handle, $!active-handle.unlock, Nil)
    }
    method native-descriptor (::?CLASS:D: --> Int:D) {
        nqp::if($!active-handle, $!active-handle.native-descriptor, Nil)
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

    #                        __________________________________________
    #                       / I don't know what the write methods      \
    #                      | should do in a CatHandle, so I'll mark    |
    #                      | these as NYI, for now.... Has anyone      |
    #                      \ seen my cocoon? I always lose that thing! /
    #                      |  -----------------------------------------
    #                      | /
    #                      |/
    #                    (⛣)
    proto method flush   (|) { * }
    multi method flush   (|) { die X::NYI.new: :feature<flush>    }
    proto method nl-out  (|) { * }
    multi method nl-out  (|) { die X::NYI.new: :feature<nl-out>   }
    proto method print   (|) { * }
    multi method print   (|) { die X::NYI.new: :feature<print>    }
    proto method printf  (|) { * }
    multi method printf  (|) { die X::NYI.new: :feature<printf>   }
    proto method print-nl(|) { * }
    multi method print-nl(|) { die X::NYI.new: :feature<print-nl> }
    proto method put     (|) { * }
    multi method put     (|) { die X::NYI.new: :feature<put>      }
    proto method say     (|) { * }
    multi method say     (|) { die X::NYI.new: :feature<say>      }
    proto method write   (|) { * }
    multi method write   (|) { die X::NYI.new: :feature<write>    }
    #                    /|\
}

# vim: ft=perl6 expandtab sw=4
