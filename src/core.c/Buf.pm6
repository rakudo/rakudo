my class Kernel                 { ... }
my class X::Assignment::RO      { ... }
my class X::Buf::AsStr          { ... }
my class X::Buf::Pack           { ... }
my class X::Buf::Pack::NonASCII { ... }
my class X::Experimental        { ... }

# externalize the endian indicators
enum Endian (
  NativeEndian => nqp::box_i(nqp::const::BINARY_ENDIAN_NATIVE,Int),
  LittleEndian => nqp::box_i(nqp::const::BINARY_ENDIAN_LITTLE,Int),
  BigEndian    => nqp::box_i(nqp::const::BINARY_ENDIAN_BIG,Int),
);

my role Blob[::T = uint8] does Positional[T] does Stringy is repr('VMArray') is array_type(T) {
    die "Can only parameterize with native int types, not '{T.^name}'."
      unless nqp::objprimspec(T) == 1 || nqp::objprimspec(T) == 4 || nqp::objprimspec(T) == 5;

    # other then *8 not supported yet
    my int $bpe = try {
#?if jvm
        # https://colabti.org/irclogger/irclogger_log/perl6-dev?date=2017-01-20#l202
        CATCH { default { Nil } }
#?endif
        (T.^nativesize / 8).Int
    } // 1;

    multi method WHICH(Blob:D: --> ValueObjAt:D) {
        nqp::box_s(
          nqp::concat(
            nqp::if(
              nqp::eqaddr(self.WHAT,Blob),
              'Blob|',
            nqp::concat(nqp::unbox_s(self.^name), '|')
            ),
            nqp::sha1(self.decode("latin-1"))
          ),
          ValueObjAt
        )
    }

    multi method new(Blob:) { nqp::create(self) }
    multi method new(Blob: Blob:D $blob) is default {
        nqp::splice(nqp::create(self),$blob,0,0)
    }
    multi method new(Blob: int @values) {
        nqp::splice(nqp::create(self),@values,0,0)
    }
    multi method new(Blob: @values) {
        nqp::create(self).STORE(@values, :INITIALIZE)
    }
    multi method new(Blob: *@values) {
        nqp::create(self).STORE(@values, :INITIALIZE)
    }

    # Because it is (apparently) impossible to stub the Buf role in the
    # setting, the lookup for Buf needs to be done at runtime, hence the
    # ::<Buf> rather than just Buf.
    method Buf(Blob:D:) {
        (nqp::eqaddr(T,uint8) ?? ::<Buf> !! ::<Buf>.^parameterize(T)).new: self
    }

    proto method STORE(Blob:D: |) {*}
    multi method STORE(Blob:D: Iterable:D \iterable, :$INITIALIZE) {
        $INITIALIZE
          ?? iterable.is-lazy
            ?? self.throw-iterator-cannot-be-lazy('store')
            !! self!push-list("initializ",self,iterable)
          !! X::Assignment::RO.new(:value(self)).throw
    }
    multi method STORE(Blob:D: Any:D \non-iterable, :$INITIALIZE) {
        X::Assignment::RO.new(:value(self)).throw unless $INITIALIZE;
        my int $elems = non-iterable.elems;
        nqp::push_i(self,non-iterable.AT-POS($_)) for ^$elems;
        self
    }

    proto method allocate(|) {*}
    multi method allocate(Blob:U: Int:D $elements) {
        nqp::setelems(nqp::create(self),$elements)
    }
    multi method allocate(Blob:U: Int:D $elements, int $value) {
        my int $elems = $elements;
        my $blob     := nqp::setelems(nqp::create(self),$elems);
        my int $i     = -1;
        nqp::bindpos_i($blob,$i,$value) while nqp::islt_i(++$i,$elems);
        $blob;
    }
    multi method allocate(Blob:U: Int:D $elements, Int:D \value) {
        my int $value = value;
        self.allocate($elements,$value)
    }
    multi method allocate(Blob:U: Int:D $elements, Mu:D $got) {
        self!fail-typecheck('allocate',$got)
    }
    multi method allocate(Blob:U: Int:D $elements, int @values) {
        self!spread(nqp::setelems(nqp::create(self),$elements),@values)
    }
    multi method allocate(Blob:U: Int:D $elements, Blob:D $blob) {
        self!spread(nqp::setelems(nqp::create(self),$elements),$blob)
    }
    multi method allocate(Blob:U: Int:D $elements, @values) {
        self!spread(nqp::setelems(nqp::create(self),$elements),Blob.new(@values))
    }

    multi method EXISTS-POS(Blob:D: int \pos) {
        nqp::hllbool(
          nqp::islt_i(pos,nqp::elems(self)) && nqp::isge_i(pos,0)
        );
    }
    multi method EXISTS-POS(Blob:D: Int:D \pos) {
        nqp::hllbool(
          nqp::islt_i(pos,nqp::elems(self)) && nqp::isge_i(pos,0)
        );
    }

    multi method AT-POS(Blob:D: int \pos) {
        nqp::isge_i(pos,nqp::elems(self)) || nqp::islt_i(pos,0)
          ?? self!fail-range(pos)
          !! nqp::atpos_i(self,pos)
    }
    multi method AT-POS(Blob:D: Int:D \pos) {
        nqp::isge_i(pos,nqp::elems(self)) || nqp::islt_i(pos,0)
          ?? self!fail-range(pos)
          !! nqp::atpos_i(self,pos)
    }

#?if moar
    # for simplicity's sake, these are not multis
    method read-int8(::?ROLE:D: int $offset, Endian $? --> int) is raw {
        nqp::readint(self,$offset,
          nqp::bitor_i(
            nqp::const::BINARY_SIZE_8_BIT,
            nqp::const::BINARY_ENDIAN_NATIVE
          )
        )
    }
    method read-int16(::?ROLE:D:
      int $offset, Endian $endian = NativeEndian --> int
    ) is raw {
        nqp::readint(self,$offset,
          nqp::bitor_i(nqp::const::BINARY_SIZE_16_BIT,$endian))
    }
    method read-int32(::?ROLE:D:
      int $offset, Endian $endian = NativeEndian --> int
    ) is raw {
        nqp::readint(self,$offset,
          nqp::bitor_i(nqp::const::BINARY_SIZE_32_BIT,$endian))
    }
    method read-int64(::?ROLE:D:
      int $offset, Endian $endian = NativeEndian --> int
    ) is raw {
        nqp::readint(self,$offset,
          nqp::bitor_i(nqp::const::BINARY_SIZE_64_BIT,$endian))
    }
    method read-int128(::?ROLE:D:
      int $offset, Endian $endian = NativeEndian --> Int
    ) is raw {
        my \unsigned := self.read-uint128($offset,$endian);
        unsigned >= 1 +< 127 ?? unsigned - 1 +< 128 !! unsigned
    }

    method read-uint8(::?ROLE:D: int $offset, Endian $? --> uint) is raw {
        nqp::readuint(self,$offset,
          nqp::bitor_i(
            nqp::const::BINARY_SIZE_8_BIT,
            nqp::const::BINARY_ENDIAN_NATIVE
          )
        )
    }
    method read-uint16(::?ROLE:D:
      int $offset, Endian $endian = NativeEndian --> uint
    ) is raw {
        nqp::readuint(self,$offset,
          nqp::bitor_i(nqp::const::BINARY_SIZE_16_BIT,$endian))
    }
    method read-uint32(::?ROLE:D:
      int $offset, Endian $endian = NativeEndian --> uint
    ) is raw {
        nqp::readuint(self,$offset,
          nqp::bitor_i(nqp::const::BINARY_SIZE_32_BIT,$endian))
    }
    method read-uint64(::?ROLE:D:
      int $offset, Endian $endian = NativeEndian --> uint
    ) is raw {
        my \signed := nqp::readuint(self,$offset,
          nqp::bitor_i(nqp::const::BINARY_SIZE_64_BIT,$endian));
        signed < 0 ?? signed + 1 +< 64 !! signed

    }
    method read-uint128(::?ROLE:D:
      int $offset, Endian $endian = NativeEndian --> uint
    ) is raw {
        my \first  := self.read-uint64($offset,     $endian);
        my \second := self.read-uint64($offset + 8, $endian);
        $endian == BigEndian
          || ($endian == NativeEndian && Kernel.endian == BigEndian)
          ?? first +< 64 +| second
          !! second +< 64 +| first
    }

    method read-num32(::?ROLE:D:
      int $offset, Endian $endian = NativeEndian --> num
    ) is raw {
        nqp::readnum(self,$offset,
          nqp::bitor_i(nqp::const::BINARY_SIZE_32_BIT,$endian))
    }
    method read-num64(::?ROLE:D:
      int $offset, Endian $endian = NativeEndian --> num
    ) is raw {
        nqp::readnum(self,$offset,
          nqp::bitor_i(nqp::const::BINARY_SIZE_64_BIT,$endian))
    }
#?endif

    method read-bits(::?ROLE:D \SELF: int $pos, Int:D $bits --> Int:D) {
        my $result := SELF.read-ubits($pos, $bits);
        $result > 1 +< ($bits - 1) - 1
          ?? $result - 1 +< $bits
          !! $result
    }

    method read-ubits(::?ROLE:D \SELF: int $pos, Int:D $bits --> UInt:D) {

        # sanity checking
        die "Can only read from position 0..{
            nqp::elems(self) * 8 - 1
        } in buffer{
            " '" ~ SELF.VAR.name ~ "'" if nqp::iscont(SELF)
        }, you tried: $pos"
          if $pos < 0;

        die "Can only read 1..{
            nqp::elems(self) * 8 - $pos
        } bits from position $pos in buffer{
            " '" ~ SELF.VAR.name ~ "'" if nqp::iscont(SELF)
        }, you tried: $bits"
          if ($pos + $bits - 1) +> 3 >= nqp::elems(self);

        # set up stuff to work with
        my int $first-bit = $pos +& 7;             # 0 = left-aligned
        my int $last-bit  = ($pos + $bits) +& 7;   # 0 = right-aligned
        my int $first-byte = $pos +> 3;
        my int $last-byte  = ($pos + $bits - 1) +> 3;

# l=least significant byte, m=most significant byte
# 00010010 00110100 01011100 01111000 10011010
# ________ mmmmmmmm llllllll ________ ________   8,16 mmmmmmmm llllllll
# ________ __mmmmmm llllllll ________ ________   8,16   mmmmmm llllllll
# ________ mmmmmmll llllll__ ________ ________   8,16   mmmmmm llllllll
# ________ __mmmmmm mmllllll ll______ ________  10,16 mmmmmmmm llllllll
# ________ ________ ______ll lll_____ ________  21, 5             lllll
# ________ ________ ________ __lllll_ ________  26, 5             lllll

        nqp::if(
          nqp::iseq_i($first-byte,$last-byte),
          (my $result := nqp::atpos_i(self,$first-byte)),
          nqp::stmts(
            ($result  := 0),
            (my int $i = $first-byte - 1),
            nqp::while(
              nqp::isle_i(++$i,$last-byte),
              ($result :=
                nqp::bitshiftl_I($result,8,Int) +| nqp::atpos_i(self,$i))
            )
          )
        );

        $last-bit
          ?? ($result +> (8 - $last-bit)) # not right-aligned, so
               +& (1 +< $bits - 1)         # shift and mask
          !! $first-bit                   # right-aligned
            ?? $result +& (1 +< $bits - 1) # but not left-aligned, so mask
            !! $result                     # also left-aligned, already done
    }

    multi method Bool(Blob:D:) { nqp::hllbool(nqp::elems(self)) }
    method Capture(Blob:D:) { self.List.Capture }

    multi method elems(Blob:D:) { nqp::elems(self) }

    method Numeric(Blob:D:) { nqp::p6box_i(nqp::elems(self)) }
    method Int(Blob:D:)     { nqp::p6box_i(nqp::elems(self)) }

    method bytes(Blob:D:) { nqp::mul_i(nqp::elems(self),$bpe) }

    method chars(Blob:D:) {
        X::Buf::AsStr.new(object => self, method => 'chars').throw
    }
    method codes(Blob:D:) {
        X::Buf::AsStr.new(object => self, method => 'codes').throw
    }
    multi method Str(Blob:D:) {
        X::Buf::AsStr.new(object => self, method => 'Str'  ).throw
    }
    multi method Stringy(Blob:D:) {
        X::Buf::AsStr.new(object => self, method => 'Stringy' ).throw
    }

    proto method decode(|) {*}
    multi method decode(Blob:D: $encoding = self.encoding // "utf-8") {
        nqp::p6box_s(
          nqp::decode(self, Rakudo::Internals.NORMALIZE_ENCODING($encoding))
        )
    }
#?if !jvm
    multi method decode(Blob:D: $encoding, Str :$replacement!, Bool:D :$strict = False) {
        nqp::p6box_s(
          nqp::decoderepconf(self,
            Rakudo::Internals.NORMALIZE_ENCODING($encoding),
            $replacement.defined ?? $replacement !! nqp::null_s(),
            $strict ?? 0 !! 1))
    }
    multi method decode(Blob:D: $encoding, Bool:D :$strict = False) {
        nqp::p6box_s(
          nqp::decodeconf(self,
            Rakudo::Internals.NORMALIZE_ENCODING($encoding),
            $strict ?? 0 !! 1))
    }
#?endif
#?if jvm
    multi method decode(Blob:D: $encoding, Bool:D :$strict = False) {
        nqp::p6box_s(
          nqp::decode(self, Rakudo::Internals.NORMALIZE_ENCODING($encoding)))
    }
    multi method decode(Blob:D: $encoding, Str:D :$replacement!, Bool:D :$strict = False) {
        X::NYI.new(:feature<decode-with-replacement>).throw
    }
#?endif

    multi method list(Blob:D:) {
        my int $elems = nqp::elems(self);

        # presize memory, but keep it empty, so we can just push
        my $buffer := nqp::setelems(
          nqp::setelems(nqp::create(IterationBuffer),$elems),
          0
        );

        my int $i = -1;
        nqp::while(
          nqp::islt_i(++$i,$elems),
          nqp::push($buffer,nqp::atpos_i(self,$i))
        );
        $buffer.List
    }

    my $char := nqp::list_s(
      '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'
    );

    multi method gist(Blob:D:) {
        my int $todo =
          nqp::elems(self) min nqp::div_i(200,nqp::div_i(T.^nativesize,4) || 1);
        my int $i   = -1;
        my $chunks := nqp::list_s;

        nqp::while(
          nqp::islt_i($i = nqp::add_i($i,1),$todo),
          nqp::stmts(
            (my int $elem   = nqp::atpos_i(self,$i)),
            (my     $chunk := nqp::list_s),
            (my int $size   = nqp::div_i(T.^nativesize,4) || 1),
            nqp::while(
              nqp::isgt_i($size,0),
              nqp::stmts(
                nqp::unshift_s(
                  $chunk,
                  nqp::atpos_s($char,nqp::bitand_i($elem,0xF))
                ),
                ($elem = nqp::bitshiftr_i($elem,4)),
                ($size = nqp::sub_i($size,1))
              )
            ),
            nqp::push_s($chunks,nqp::join('',$chunk))
          )
        );

        nqp::push_s($chunks,"...")
         if nqp::isgt_i(nqp::elems(self),$todo);

        nqp::join('',nqp::list_s(self.^name,':0x<',nqp::join(" ",$chunks),'>'))
    }
    multi method raku(Blob:D:) {
        self.^name ~ '.new(' ~ self.join(',') ~ ')';
    }

    # Made this a sub instead of a private method so that the optimizer
    # doesn't need to put in IntLexRef's for the native int parameters.
    # Since we're not using any attributes, just self, that was an easy
    # choice to make.
    sub subbuf-end(\SELF, int $start, int $end, int $elems) {
        nqp::if(
          nqp::islt_i($start,0) || nqp::isgt_i($start,$elems),
          Failure.new( X::OutOfRange.new(
            what  => '"From argument to subbuf',
            got   => $start,
            range => "0.." ~ $elems
          )),
          nqp::if(
            nqp::isle_i(
              (my int $last = nqp::if(nqp::isge_i($end,$elems),$elems-1,$end)),
              $start - 1
            ),                                # 0 elements to return
            nqp::create(SELF),                # just create a new one
            nqp::slice(SELF,$start,$last)     # do the actual slice
          )
        )
    }
    sub subbuf-length(\SELF, int $from, int $length, int $elems) {
        nqp::islt_i($length,0)
          ?? Failure.new(
               X::OutOfRange.new(
                 what  => 'Len element to subbuf',
                 got   => $length,
                 range => "0.." ~ $elems
               )
             )
          !! subbuf-end(SELF, $from, $from + $length - 1, $elems)
    }

    proto method subbuf(|) {*}
    multi method subbuf(Blob:D: Range:D $fromto) {
        nqp::if(
          nqp::getattr_i(nqp::decont($fromto),Range,'$!is-int'),
          nqp::stmts(
            (my int $start = nqp::add_i(
              nqp::unbox_i(nqp::getattr(nqp::decont($fromto),Range,'$!min')),
              nqp::getattr_i(nqp::decont($fromto),Range,'$!excludes-min')
            )),
            (my int $end = nqp::sub_i(
              nqp::unbox_i(nqp::getattr(nqp::decont($fromto),Range,'$!max')),
              nqp::getattr_i(nqp::decont($fromto),Range,'$!excludes-max')
            )),
            subbuf-end(self, $start, $end, nqp::elems(self))
          ),
          Failure.new( X::AdHoc.new(
            payload => "Must specify a Range with integer bounds to subbuf"
          ))
        )
    }
    multi method subbuf(Blob:D: Int:D $From) {
        my int $elems = nqp::elems(self);
        my int $from  = $From;
        subbuf-end(self, $from, $elems, $elems)
    }
    multi method subbuf(Blob:D: &From) {
        my int $elems = nqp::elems(self);
        my int $from  = From(nqp::box_i($elems,Int));
        subbuf-end(self, $from, $elems, $elems)
    }
    multi method subbuf(Blob:D: Int:D $From, Int:D $Length) {
        my int $from   = $From;
        my int $length = $Length;
        subbuf-length(self, $from, $length, nqp::elems(self))
    }
    multi method subbuf(Blob:D: Int:D $From, &End) {
        my int $elems  = nqp::elems(self);
        my int $from   = $From;
        my int $end    = End(nqp::box_i($elems,Int));
        subbuf-end(self, $from, $end, $elems)
    }
    multi method subbuf(Blob:D: &From, Int:D $Length) {
        my int $elems  = nqp::elems(self);
        my int $from   = From(nqp::box_i($elems,Int));
        my int $length = $Length;
        subbuf-length(self, $from, $length, $elems)
    }
    multi method subbuf(Blob:D: &From, &End) {
        my int $elems  = nqp::elems(self);
        my int $from   = From(nqp::box_i($elems,Int));
        my int $end    = End(nqp::box_i($elems,Int));
        subbuf-end(self, $from, $end, $elems)
    }
    multi method subbuf(Blob:D: \from, Whatever) {
        self.subbuf(from)
    }
    multi method subbuf(Blob:D: \from, Numeric \length) {
        length == Inf ?? self.subbuf(from) !! self.subbuf(from,length.Int)
    }

    method reverse(Blob:D:) {
        my int $elems = nqp::elems(self);
        my int $last  = nqp::sub_i($elems,1);
        my $reversed := nqp::setelems(nqp::create(self),$elems);
        my int $i     = -1;
        nqp::while(
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
          nqp::bindpos_i($reversed,nqp::sub_i($last,$i),
            nqp::atpos_i(self,$i))
        );
        $reversed
    }

    method COMPARE(Blob:D: Blob:D \other) is implementation-detail {
        nqp::unless(
          nqp::cmp_i(
            (my int $elems = nqp::elems(self)),
            nqp::elems(my $other := nqp::decont(other))
          ),
          nqp::stmts(                            # same number of elements
            (my int $i = -1),
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
                && nqp::not_i(
                     nqp::cmp_i(nqp::atpos_i(self,$i),nqp::atpos_i($other,$i))
                   ),
              nqp::null
            ),
            nqp::if(
              nqp::isne_i($i,$elems),
              nqp::cmp_i(nqp::atpos_i(self,$i),nqp::atpos_i($other,$i))
            )
          )
        )
    }

    method SAME(Blob:D: Blob:D \other) is implementation-detail {
        nqp::if(
          nqp::iseq_i(
            (my int $elems = nqp::elems(self)),
            nqp::elems(my $other := nqp::decont(other))
          ),
          nqp::stmts(                            # same number of elements
            (my int $i = -1),
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
                && nqp::iseq_i(nqp::atpos_i(self,$i),nqp::atpos_i($other,$i)),
              nqp::null
            ),
            nqp::iseq_i($i,$elems)
          )
        )
    }

    method join(Blob:D: $delim = '') {
        my int $elems = nqp::elems(self);
        my int $i     = -1;
        my $list := nqp::setelems(nqp::setelems(nqp::list_s,$elems),0);

        nqp::while(
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
          nqp::push_s($list,nqp::atpos_i(self,$i))
        );

        nqp::join($delim.Str,$list)
    }

    proto method unpack(|) {*}
    multi method unpack(Blob:D: Str:D $template) {
        nqp::isnull(nqp::getlexcaller('EXPERIMENTAL-PACK'))
          ?? X::Experimental.new(
               feature => "the 'unpack' method",
               use     => "pack"
             ).throw
          !! self.unpack($template.comb(/<[a..zA..Z]>[\d+|'*']?/))
    }
    multi method unpack(Blob:D: @template) {
        nqp::isnull(nqp::getlexcaller('EXPERIMENTAL-PACK'))
          ?? X::Experimental.new(
               feature => "the 'unpack' method",
               use     => "pack"
             ).throw
          !! nqp::getlexcaller('EXPERIMENTAL-PACK')(self, @template)
    }

    # XXX: the pack.t spectest file seems to require this method
    # not sure if it should be changed to list there...
    method contents(Blob:D:) { self.list }

    method encoding() { Any }

    method !push-list(\action,\to,\from) {
        if nqp::istype(from,List) {
            my Mu $from := nqp::getattr(from,List,'$!reified');
            if nqp::defined($from) {
                my int $elems = nqp::elems($from);
                my int $j     = nqp::elems(to);
                nqp::setelems(to, $j + $elems);  # presize for efficiency
                my int $i = -1;
                my $got;
                nqp::while(
                  nqp::islt_i(++$i,$elems),
                  nqp::stmts(
                    ($got := nqp::atpos($from,$i)),
                    nqp::istype(nqp::hllize($got),Int)
                      ?? nqp::bindpos_i(to,$j++,$got)
                      !! self!fail-typecheck-element(action,$i,$got).throw))
            }
        }
        else {
            my $iter := from.iterator;
            my int $i = 0;
            nqp::until(
              nqp::eqaddr((my $got := $iter.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(nqp::hllize($got),Int),
                nqp::stmts(
                  nqp::push_i(to,$got),
                  ($i = nqp::add_i($i,1))
                ),
                self!fail-typecheck-element(action,$i,$got).throw
              )
            )
        }
        to
    }
    method !unshift-list(\action,\to,\from) {
        if nqp::istype(from,List) {
            my Mu $from := nqp::getattr(from,List,'$!reified');
            if nqp::defined($from) {
                my int $i = nqp::elems($from);
                nqp::istype((my $got := nqp::atpos($from,$i)),Int)
                  ?? nqp::unshift_i(to,$got)
                  !! self!fail-typecheck-element(action,$i,$got).throw
                  while nqp::isge_i(--$i,0);
            }
            to
        }
        else {
            nqp::splice(to,self!push-list(action,nqp::create(self),from),0,0)
        }
    }
    method !spread(\to,\from) {
        if nqp::elems(from) -> int $values { # something to init with
            my int $elems = nqp::elems(to) - $values;
            my int $i     = -$values;
            nqp::splice(to,from,$i,$values)
              while nqp::isle_i($i = $i + $values,$elems);

            if nqp::isgt_i($i,$elems) {  # something left to init
                --$i;                    # went one too far
                $elems = $elems + $values;
                my int $j = -1;
                nqp::bindpos_i(to,$i,nqp::atpos_i(from,$j = ($j + 1) % $values))
                  while nqp::islt_i(++$i,$elems);
            }
        }
        to
    }
    method !fail-range($got) {
        Failure.new(X::OutOfRange.new(
          :what($*INDEX // 'Index'),
          :$got,
          :range("0..{nqp::elems(self)-1}")
        ))
    }
    method !fail-typecheck-element(\action,\i,\got) {
        self!fail-typecheck(action ~ "ing element #" ~ i,got);
    }
    method !fail-typecheck($action,$got) {
        Failure.new(X::TypeCheck.new(
          operation => $action ~ " to " ~ self.^name,
          got       => $got,
          expected  => T,
        ))
    }
    multi method ACCEPTS(Blob:D: Blob:D \Other) {
        nqp::hllbool(
          nqp::unless(
            nqp::eqaddr(self,my \other := nqp::decont(Other)),
            nqp::if(
              nqp::iseq_i(
                (my int $elems = nqp::elems(self)),
                nqp::elems(other)
              ),
              nqp::stmts(
                (my int $i = -1),
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
                    && nqp::iseq_i(nqp::atpos_i(self,$i),nqp::atpos_i(other,$i)
                       ),
                  nqp::null
                ),
                nqp::iseq_i($i,$elems)
              )
            )
          )
        )
    }
}

constant blob8 = Blob[uint8];
constant blob16 = Blob[uint16];
constant blob32 = Blob[uint32];
constant blob64 = Blob[uint64];

my class utf8 does Blob[uint8] is repr('VMArray') {
    method encoding(--> "utf-8") { }
    multi method Str(utf8:D:) { self.decode }
    multi method Stringy(utf8:D:) { self.decode }
}

my class utf16 does Blob[uint16] is repr('VMArray') {
    method encoding(--> "utf-16") { }
    multi method Str(utf16:D:) { self.decode }
    multi method Stringy(utf16:D:) { self.decode }
}

my class utf32 does Blob[uint32] is repr('VMArray') {
    method encoding(--> "utf-32") { }
    multi method Str(utf32:D:) { self.decode }
    multi method Stringy(utf32:D:) { self.decode }
}

my role Buf[::T = uint8] does Blob[T] is repr('VMArray') is array_type(T) {

    multi method WHICH(Buf:D:) { self.Mu::WHICH }

    method Blob(Blob:D:) {
        (nqp::eqaddr(T,uint8) ?? Blob !! Blob.^parameterize(T)).new: self
    }

    multi method AT-POS(Buf:D: int \pos) is raw {
        nqp::islt_i(pos,0)
          ?? Failure.new(X::OutOfRange.new(
               :what($*INDEX // 'Index'),:got(pos),:range<0..^Inf>))
          !! nqp::atposref_i(self, pos)
    }
    multi method AT-POS(Buf:D: Int:D \pos) is raw {
        my int $pos = nqp::unbox_i(pos);
        nqp::islt_i($pos,0)
          ?? Failure.new(X::OutOfRange.new(
               :what($*INDEX // 'Index'),:got(pos),:range<0..^Inf>))
          !! nqp::atposref_i(self,$pos)
    }

    multi method ASSIGN-POS(Buf:D: int \pos, Mu \assignee) {
        nqp::islt_i(pos,0)
          ?? Failure.new(X::OutOfRange.new(
               :what($*INDEX // 'Index'),:got(pos),:range<0..^Inf>))
          !! nqp::bindpos_i(self,pos,assignee)
    }
    multi method ASSIGN-POS(Buf:D: Int:D \pos, Mu \assignee) {
        my int $pos = nqp::unbox_i(pos);
        nqp::islt_i($pos,0)
          ?? Failure.new(X::OutOfRange.new(
               :what($*INDEX // 'Index'),:got(pos),:range<0..^Inf>))
          !! nqp::bindpos_i(self,$pos,assignee)
    }

    multi method STORE(Buf:D: Blob:D $blob) {
        nqp::splice(nqp::setelems(self,0),$blob,0,0)
    }
    # The "is default" is needed to prevent runtime dispatch errors
    multi method STORE(Buf:D: int @values) is default {
        nqp::splice(nqp::setelems(self,0),@values,0,0)
    }
    multi method STORE(Buf:D: Iterable:D \iterable) {
        iterable.is-lazy
          ?? self.throw-iterator-cannot-be-lazy('store')
          !! self!push-list("initializ",nqp::setelems(self,0),iterable);
    }
    multi method STORE(Buf:D: Any:D \non-iterable) {
        my int $elems = non-iterable.elems;
        nqp::setelems(self,0);
        nqp::push_i(self,non-iterable.AT-POS($_)) for ^$elems;
        self
    }

#?if moar
    # for simplicity's sake, these are not multis
    method write-int8(::?ROLE:
      int $offset, int8 $value, Endian $endian = NativeEndian
    ) is raw {
        my $self := nqp::isconcrete(self) ?? self !! nqp::create(self);
        nqp::writeint($self,$offset,$value,
          nqp::bitor_i(nqp::const::BINARY_SIZE_8_BIT,$endian));
        $self
    }
    method write-int16(::?ROLE:
      int $offset, int16 $value, Endian $endian = NativeEndian
    ) is raw {
        my $self := nqp::isconcrete(self) ?? self !! nqp::create(self);
        nqp::writeint($self,$offset,$value,
          nqp::bitor_i(nqp::const::BINARY_SIZE_16_BIT,$endian));
        $self
    }
    method write-int32(::?ROLE:
      int $offset, int32 $value, Endian $endian = NativeEndian
    ) is raw {
        my $self := nqp::isconcrete(self) ?? self !! nqp::create(self);
        nqp::writeint($self,$offset,$value,
          nqp::bitor_i(nqp::const::BINARY_SIZE_32_BIT,$endian));
        $self
    }
    method write-int64(::?ROLE:
      int $offset, Int:D $value, Endian $endian = NativeEndian
    ) is raw {
        my $self := nqp::isconcrete(self) ?? self !! nqp::create(self);
        nqp::writeint($self,$offset,$value,
          nqp::bitor_i(nqp::const::BINARY_SIZE_64_BIT,$endian));
        $self
    }
    method write-int128(::?ROLE:
      int $offset, Int:D $value, Endian $endian = NativeEndian
    ) is raw {
        # These uints are intentional to keep the value within 64 bits
        my uint $first  = ($value +> 64) +& (1 +< 64 - 1);
        my uint $second = $value         +& (1 +< 64 - 1);
        my $be = $endian == BigEndian
          || ($endian == NativeEndian && Kernel.endian == BigEndian);

        my $self := nqp::isconcrete(self) ?? self !! nqp::create(self);
        $self.write-int64($offset,     $be ?? $first !! $second, $endian);
        $self.write-int64($offset + 8, $be ?? $second !! $first, $endian);
        $self
    }
    method write-uint8(::?ROLE:
      int $offset, uint8 $value, Endian $endian = NativeEndian
    ) is raw {
        my $self := nqp::isconcrete(self) ?? self !! nqp::create(self);
        nqp::writeuint($self,$offset,$value,
          nqp::bitor_i(nqp::const::BINARY_SIZE_8_BIT,$endian));
        $self
    }
    method write-uint16(::?ROLE:
      int $offset, uint16 $value, Endian $endian = NativeEndian
    ) is raw {
        my $self := nqp::isconcrete(self) ?? self !! nqp::create(self);
        nqp::writeuint($self,$offset,$value,
          nqp::bitor_i(nqp::const::BINARY_SIZE_16_BIT,$endian));
        $self
    }
    method write-uint32(::?ROLE:
      int $offset, uint32 $value, Endian $endian = NativeEndian
    ) is raw {
        my $self := nqp::isconcrete(self) ?? self !! nqp::create(self);
        nqp::writeuint($self,$offset,$value,
          nqp::bitor_i(nqp::const::BINARY_SIZE_32_BIT,$endian));
        $self
    }
    method write-uint64(::?ROLE:
      int $offset, UInt:D $value, Endian $endian = NativeEndian
    ) is raw {
        my $self := nqp::isconcrete(self) ?? self !! nqp::create(self);
        nqp::writeuint($self,$offset,$value,
          nqp::bitor_i(nqp::const::BINARY_SIZE_64_BIT,$endian));
        $self
    }
    method write-uint128(::?ROLE:
      int $offset, UInt:D $value, Endian $endian = NativeEndian
    ) is raw {
        my \first  := $value +> 64;
        my \second := $value +& ( 1 +< 64 - 1 );
        my $be = $endian == BigEndian
          || ($endian == NativeEndian && Kernel.endian == BigEndian);

        my $self := nqp::isconcrete(self) ?? self !! nqp::create(self);
        $self.write-uint64($offset,     $be ?? first !! second, $endian);
        $self.write-uint64($offset + 8, $be ?? second !! first, $endian);
        $self
    }
    method write-num32(::?ROLE:
      int $offset, num32 $value, Endian $endian = NativeEndian
    ) is raw {
        my $self := nqp::isconcrete(self) ?? self !! nqp::create(self);
        nqp::writenum($self,$offset,$value,
          nqp::bitor_i(nqp::const::BINARY_SIZE_32_BIT,$endian));
        $self
    }
    method write-num64(::?ROLE:
      int $offset, num64 $value, Endian $endian = NativeEndian
    ) is raw {
        my $self := nqp::isconcrete(self) ?? self !! nqp::create(self);
        nqp::writenum($self,$offset,$value,
          nqp::bitor_i(nqp::const::BINARY_SIZE_64_BIT,$endian));
        $self
    }
#?endif

    sub POS-OOR(\SELF, int $pos --> Nil) is hidden-from-backtrace {
        die "Can only write from position 0..* in buffer{
            " '" ~ SELF.VAR.name ~ "'" if nqp::iscont(SELF)
        }, you tried: $pos"
    }

    method write-bits(::?ROLE \SELF:
      int $pos, Int:D $bits, Int:D \value
    ) is raw {
        SELF.write-ubits($pos, $bits, value +& (1 +< $bits - 1))
    }

    method write-ubits(::?ROLE \SELF:
      int $pos, Int:D $bits, UInt:D \value
    ) is raw {

        # sanity check
        POS-OOR(SELF, $pos) if $pos < 0;
        my $self := nqp::isconcrete(self) ?? self !! nqp::create(self);

        # set up basic info
        my int $first-bit = $pos +& 7;
        my int $last-bit  = ($pos + $bits) +& 7;
        my int $first-byte = $pos +> 3;
        my int $last-byte  = ($pos + $bits - 1) +> 3;

        my $value := value +& (1 +< $bits - 1);            # mask valid part
        $value := $value +< (8 - $last-bit) if $last-bit;  # move into position

        my int $lmask = nqp::sub_i(1 +< $first-bit,1) +< (8 - $first-bit)
          if $first-bit;
        my int $rmask = 1 +< nqp::sub_i(8 - $last-bit,1)
          if $last-bit;

        # all done in a single byte
        if $first-byte == $last-byte {
            nqp::bindpos_i($self,$first-byte,
              $value +| (nqp::atpos_i($self,$first-byte) +& ($lmask +| $rmask))
            );
        }

        # spread over multiple bytes
        else {
            my int $i = $last-byte;

            # process last byte first if it is a partial
            if $last-bit {
                nqp::bindpos_i($self,$i,
                  ($value +& 255) +| (nqp::atpos_i($self,$i) +& $rmask)
                );
                $value := $value +> 8;
            }

            # not a partial, so make sure we process last byte later
            else {
                ++$i;
            }

            # walk from right to left, exclude left-most is partial
            my int $last = $first-byte + nqp::isgt_i($first-bit,0);
            nqp::while(
              nqp::isge_i(--$i,$last),
              nqp::stmts(
                nqp::bindpos_i($self,$i,($value +& 255)),
                ($value := $value +> 8)
              )
            );

            # process last byte if it was a partial
            nqp::bindpos_i($self,$i,($value +& 255)
              +| (nqp::atpos_i($self,$i) +& $lmask))
              if $first-bit;
        }

        $self
    }

    multi method list(Buf:D:) {
        my int $elems = nqp::elems(self);

        # presize memory, but keep it empty, so we can just push
        my $buffer := nqp::setelems(
          nqp::setelems(nqp::create(IterationBuffer),$elems),
          0
        );

        my int $i = -1;
        nqp::while(
          nqp::islt_i(++$i,$elems),
          nqp::push($buffer,nqp::atposref_i(self,$i))
        );
        $buffer.List
    }

    proto method pop(|) { * }
    multi method pop(Buf:D:) {
        nqp::elems(self)
          ?? nqp::pop_i(self)
          !! self.fail-cannot-be-empty('pop')
    }
    proto method shift(|) { * }
    multi method shift(Buf:D:) {
        nqp::elems(self)
          ?? nqp::shift_i(self)
          !! self.fail-cannot-be-empty('shift')
    }

    method reallocate(Buf:D: Int:D $elements) { nqp::setelems(self,$elements) }

    my $empty := nqp::list_i;
    proto method splice(|) { * }
    multi method splice(Buf:D \SELF:) { my $buf = SELF; SELF = Buf.new; $buf }
    multi method splice(Buf:D: Int:D $offset, $size = Whatever) {
        my int $remove = self!remove($offset,$size);
        my $result := $remove
          ?? self.subbuf($offset,$remove)  # until something smarter
          !! nqp::create(self);
        nqp::splice(self,$empty,$offset,$remove);
        $result
    }
    multi method splice(Buf:D: Int:D $offset, $size, int $got) {
        self!splice-native($offset,$size,$got)
    }
    multi method splice(Buf:D: Int:D $offset, $size, Int:D $got) {
        self!splice-native($offset,$size,$got)
    }
    multi method splice(Buf:D: Int:D $offset, $size, Mu:D $got) {
        self!fail-typecheck('splice',$got)
    }
    multi method splice(Buf:D: Int:D $offset, $size, Buf:D $buf) {
        self!splice-native($offset,$size,$buf)
    }
    multi method splice(Buf:D: Int:D $offset, $size, int @values) {
        self!splice-native($offset,$size,@values)
    }
    multi method splice(Buf:D: Int:D $offset, $size, @values) {
        self!splice-native($offset,$size,
          self!push-list("splic",nqp::create(self),@values))
    }

    method !remove(\offset,\size) {
        nqp::istype(size,Whatever)
          ?? nqp::elems(self) - offset
          !! nqp::istype(size,Int)
            ?? size
            !! size.Int
    }

    method !splice-native(Buf:D: Int:D $offset, $size, \x) {
        my int $remove = self!remove($offset,$size);
        my $result := $remove
          ?? self.subbuf($offset,$remove)  # until something smarter
          !! nqp::create(self);
        nqp::splice(
          self,nqp::islist(x) ?? x !! nqp::list_i(x),$offset,$remove);
        $result
    }

    proto method push(|) { * }
    multi method push(Buf:D: int $got) { nqp::push_i(self,$got); self }
    multi method push(Buf:D: Int:D $got) { nqp::push_i(self,$got); self }
    multi method push(Buf:D: Mu:D $got) { self!fail-typecheck('push',$got) }
    multi method push(Buf:D: Blob:D $buf) {
        nqp::splice(self,$buf,nqp::elems(self),0)
    }
    multi method push(Buf:D: **@values) { self!pend(@values,'push') }
    proto method append(|) { * }

    multi method append(Buf:D: int $got) { nqp::push_i(self,$got); self }
    multi method append(Buf:D: Int:D $got) { nqp::push_i(self,$got); self }
    multi method append(Buf:D: Mu:D $got) { self!fail-typecheck('append',$got) }
    multi method append(Buf:D: Blob:D $buf) {
        nqp::splice(self,$buf,nqp::elems(self),0)
    }
    multi method append(Buf:D: int @values) {
        nqp::splice(self,@values,nqp::elems(self),0)
    }
    multi method append(Buf:D:  @values) { self!pend(@values,'append') }
    multi method append(Buf:D: *@values) { self!pend(@values,'append') }
    proto method unshift(|) { * }

    multi method unshift(Buf:D: int $got) { nqp::unshift_i(self,$got); self }
    multi method unshift(Buf:D: Int:D $got) { nqp::unshift_i(self,$got); self }
    multi method unshift(Buf:D: Mu:D $got) { self!fail-typecheck('unshift',$got) }
    multi method unshift(Buf:D: Blob:D $buf) { nqp::splice(self,$buf,0,0) }
    multi method unshift(Buf:D: **@values) { self!pend(@values,'unshift') }

    proto method prepend(|) { * }
    multi method prepend(Buf:D: int $got) { nqp::unshift_i(self,$got); self }
    multi method prepend(Buf:D: Int:D $got) { nqp::unshift_i(self,$got); self }
    multi method prepend(Buf:D: Mu:D $got) { self!fail-typecheck('prepend',$got) }
    multi method prepend(Buf:D: Blob:D $buf)  { nqp::splice(self,$buf,0,0)    }
    multi method prepend(Buf:D: int @values) { nqp::splice(self,@values,0,0) }
    multi method prepend(Buf:D:  @values) { self!pend(@values,'prepend') }
    multi method prepend(Buf:D: *@values) { self!pend(@values,'prepend') }

    method !pend(Buf:D: @values, $action) {
        @values.is-lazy
          ?? self.fail-iterator-cannot-be-lazy($action)
          !! $action eq 'push' || $action eq 'append'
            ?? self!push-list($action,self,@values)
            !! self!unshift-list($action,self,@values)
    }

    method subbuf-rw($from = 0, $elems = self.elems - $from) is rw {
        my Blob $subbuf = self.subbuf($from, $elems);
        Proxy.new(
            FETCH   => sub ($) { $subbuf },
            STORE   => sub ($, Blob:D $new) {
                nqp::splice(self,nqp::decont($new),$from,$elems)
            }
        );
    }

}

constant buf8  = Buf[uint8];
constant buf16 = Buf[uint16];
constant buf32 = Buf[uint32];
constant buf64 = Buf[uint64];

multi sub prefix:<~>(Blob:D \a) {
    X::Buf::AsStr.new(object => a, method => '~' ).throw
}

multi sub infix:<~>(Blob:D \a) { a }
multi sub infix:<~>(Blob:D $a, Blob:D $b) {
    my $res := nqp::create(nqp::eqaddr($a.WHAT,$b.WHAT) ?? $a !! Buf.^pun);
    my $adc := nqp::decont($a);
    my $bdc := nqp::decont($b);
    my int $alen = nqp::elems($adc);
    my int $blen = nqp::elems($bdc);

    nqp::setelems($res, $alen + $blen);
    nqp::splice($res, $adc, 0, $alen);
    nqp::splice($res, $bdc, $alen, $blen);
}

multi sub prefix:<~^>(Blob:D \a) {
    my $a        := nqp::decont(a);
    my int $elems = nqp::elems($a);

    my $r := nqp::create($a);
    nqp::setelems($a,$elems);

    my int    $i    = -1;
    nqp::bindpos_i($r,$i,nqp::bitneg_i(nqp::atpos_i($a,$i)))
      while nqp::islt_i(++$i,$elems);

    $r
}

multi sub infix:<~&>(Blob:D \a, Blob:D \b) {
    my $a := nqp::decont(a);
    my $b := nqp::decont(b);
    my int $elemsa = nqp::elems($a);
    my int $elemsb = nqp::elems($b);
    my int $do  = $elemsa > $elemsb ?? $elemsb !! $elemsa;
    my int $max = $elemsa > $elemsb ?? $elemsa !! $elemsb;

    my $r := nqp::create($a);
    nqp::setelems($r,$max);

    my int $i = -1;
    nqp::bindpos_i($r,$i,
      nqp::bitand_i(nqp::atpos_i($a,$i),nqp::atpos_i($b,$i)))
      while nqp::islt_i(++$i,$do);

    --$i;    # went one too far
    nqp::bindpos_i($r,$i,0) while nqp::islt_i(++$i,$max);

    $r
}

multi sub infix:<~|>(Blob:D \a, Blob:D \b) {
    my $a := nqp::decont(a);
    my $b := nqp::decont(b);
    my int $elemsa = nqp::elems($a);
    my int $elemsb = nqp::elems($b);
    my int $do  = $elemsa > $elemsb ?? $elemsb !! $elemsa;
    my int $max = $elemsa > $elemsb ?? $elemsa !! $elemsb;
    my $from   := $elemsa > $elemsb ?? $a      !! $b;

    my $r := nqp::create($a);
    nqp::setelems($r,$max);

    my int $i = -1;
    nqp::bindpos_i($r,$i,
      nqp::bitor_i(nqp::atpos_i($a,$i),nqp::atpos_i($b,$i)))
      while nqp::islt_i(++$i,$do);

    $i = $i - 1;    # went one too far
    nqp::bindpos_i($r,$i,nqp::atpos_i($from,$i))
      while nqp::islt_i(++$i,$max);

    $r
}

multi sub infix:<~^>(Blob:D \a, Blob:D \b) {
    my $a := nqp::decont(a);
    my $b := nqp::decont(b);
    my int $elemsa = nqp::elems($a);
    my int $elemsb = nqp::elems($b);
    my int $do  = $elemsa > $elemsb ?? $elemsb !! $elemsa;
    my int $max = $elemsa > $elemsb ?? $elemsa !! $elemsb;
    my $from   := $elemsa > $elemsb ?? $a      !! $b;

    my $r := nqp::create($a);
    nqp::setelems($r,$max);

    my int $i = -1;
    nqp::bindpos_i($r,$i,
      nqp::bitxor_i(nqp::atpos_i($a,$i),nqp::atpos_i($b,$i)))
      while nqp::islt_i(++$i,$do);

    --$i;    # went one too far
    nqp::bindpos_i($r,$i,nqp::atpos_i($from,$i))
      while nqp::islt_i(++$i,$max);

    $r
}

multi sub infix:<eqv>(Blob:D \a, Blob:D \b --> Bool:D) {
    nqp::hllbool(
      nqp::eqaddr(nqp::decont(a),nqp::decont(b))
        || (nqp::eqaddr(a.WHAT,b.WHAT) && a.SAME(b))
    )
}

multi sub infix:<cmp>(Blob:D \a, Blob:D \b) { ORDER(a.COMPARE(b))     }
multi sub infix:<eq> (Blob:D \a, Blob:D \b --> Bool:D) {
    nqp::hllbool(
      nqp::eqaddr(nqp::decont(a),nqp::decont(b)) || a.SAME(b)
    )
}
multi sub infix:<ne> (Blob:D \a, Blob:D \b --> Bool:D) {
    nqp::hllbool(
      nqp::not_i(nqp::eqaddr(nqp::decont(a),nqp::decont(b)) || a.SAME(b))
    )
}
multi sub infix:<lt> (Blob:D \a, Blob:D \b) {
    nqp::hllbool(nqp::iseq_i(a.COMPARE(b),-1))
}
multi sub infix:<gt> (Blob:D \a, Blob:D \b) {
    nqp::hllbool(nqp::iseq_i(a.COMPARE(b),1))
}
multi sub infix:<le> (Blob:D \a, Blob:D \b) {
    nqp::hllbool(nqp::isne_i(a.COMPARE(b),1))
}
multi sub infix:<ge> (Blob:D \a, Blob:D \b) {
    nqp::hllbool(nqp::isne_i(a.COMPARE(b),-1))
}

proto sub subbuf-rw($, $?, $?, *%) {*}
multi sub subbuf-rw(Buf:D \b) is rw {
    b.subbuf-rw(0, b.elems);
}
multi sub subbuf-rw(Buf:D \b, Int() $from) is rw {
    b.subbuf-rw($from, b.elems - $from)
}
multi sub subbuf-rw(Buf:D \b, $from, $elems) is rw {
    b.subbuf-rw($from, $elems)
}

# vim: expandtab shiftwidth=4
