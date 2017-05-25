my class IO::CatHandle is IO::Handle {
    has $!handles;
    has IO::Handle $!active-handle;

    has $.path;
    has $.chomp is rw;
    has $.nl-in;
    has str $.encoding;

    method !SET-SELF (@handles, $!chomp, $!nl-in, $!encoding) {
        # reify:
        @handles.elems
          or die 'Must have at least one item to create IO::CatHandle from';
        $!handles := nqp::getattr(@handles, List, '$!reified');
        self.next-handle;
        self
    }
    method new (*@handles,
        :$chomp = True, :$nl-in = ["\x0A", "\r\n"], :$encoding = 'utf8',
    ) {
        self.bless!SET-SELF(@handles, $chomp, $nl-in, $encoding)
    }
    method next-handle {
      # Set $!active-handle to the next handle in line, opening it if necessary
      nqp::stmts(
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
              ($!active-handle = $_),
              nqp::if(
                nqp::istype(
                  ($_ = .open: :r, :$!chomp, :$!nl-in, :$!encoding),
                  Failure),
                .throw,
                ($!active-handle = $_))),
            nqp::if(
              nqp::istype(($_ := .IO.open: :r, :$!chomp, :$!nl-in, :$!encoding),
                Failure),
              .throw,
              ($!active-handle = $_))),
          ($!active-handle = Nil)))
    }

    # Produce a Seq with an iterator that walks through all handles
    method comb (::?CLASS:D: |c) {}
    method split (::?CLASS:D: |c) {}

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
    multi method words(::?CLASS:D \SELF: :$close) {
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
    multi method lines(::?CLASS:D \SELF: :$close) {
      $close # use -1 as N in FirstNThenSinkAllSeq to get all items
        ?? Seq.new(Rakudo::Iterator.FirstNThenSinkAll(
            self!LINES.iterator, -1, {SELF.close}))
        !! self!LINES
    }
    multi method lines(::?CLASS:D:) { self!LINES }

    method Supply (::?CLASS:D: |c) {}

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
    method read (::?CLASS:D: |c) {}
    method readchars (::?CLASS:D: |c) {}

    # Walk through all handles, producing single result
    method slurp (::?CLASS:D: |c) {

    }
    method slurp-rest (|) {
        # We inherit deprecated .slurp-rest from IO::Handle. Pull the
        # plug on it in this class, since no one is using this yet.
        # The old IO::ArgFiles used .slurp
        die X::Obsolete.new: :old<slurp-rest>, :new<slurp>,
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
              $ = .close)))
    }
    method encoding {}
    method eof      {}
    method gist     {}
    method Str      {}
    method IO       {}
    method path     {}
    method open     {}
    method opened   {}
    method lock     {}
    method nl-in    {}
    method seek     {}
    method tell     {}
    method t        {}
    method unlock   {}
    method native-descriptor {}

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
