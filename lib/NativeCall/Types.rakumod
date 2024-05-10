use nqp;
unit module NativeCall::Types;

#- lookups ---------------------------------------------------------------------
# Quick lookup of REPRs allowed for parameterization
my constant $ok-REPRs = nqp::hash(
  'CArray', 1, 'CPointer', 1, 'CPPStruct', 1, 'CStruct', 1, 'CUnion', 1
);

#- helper subs -----------------------------------------------------------------
# Helper sub for type mapping, also used in NativeCall
my proto sub map_return_type(|) is export {*}
my multi sub map_return_type(Mu $type) { $type }
my multi sub map_return_type(Int     ) { Int   }
my multi sub map_return_type(Num     ) { Num   }

# This version of nativecast only used here
my sub nativecast($target-type, $source) {
    nqp::nativecallcast(
      nqp::decont($target-type),
      nqp::decont(map_return_type($target-type)),
      nqp::decont($source)
    )
}

# Handle parameterization issues
my sub wrong-type(str $what, \whatnot) is hidden-from-backtrace {
    my str $name = whatnot.^name;
    die qq:to/MESSAGE/.chomp.naive-word-wrapper;
A $what can only hold: (u)int8, (u)int16, (u)int32, (u)int64, (u)long,
(u)longlong, num16, num32, (s)size_t, bool, Str and types with representation:
CArray, CPointer, CStruct, CPPStruct and CUnion, not: $name
MESSAGE
}

#- representations -------------------------------------------------------------
our role ExplicitlyManagedString { has $.cstr is rw }

our class void is repr<Uninstantiable> { }

our native long      is Int is ctype<long>                 is repr<P6int> { }
our native longlong  is Int is ctype<longlong>             is repr<P6int> { }
our native ulong     is Int is ctype<long>     is unsigned is repr<P6int> { }
our native ulonglong is Int is ctype<longlong> is unsigned is repr<P6int> { }
our native size_t    is Int is ctype<size_t>   is unsigned is repr<P6int> { }
our native ssize_t   is Int is ctype<size_t>               is repr<P6int> { }
our native bool      is Int is ctype<bool>                 is repr<P6int> { }

#- Pointer----------------------------------------------------------------------
# Expose a Pointer class for working with raw pointers.
our class Pointer is repr<CPointer> {
    method of() { void }

    proto method new(|) {*}
    multi method new() { nqp::create(self) }
    multi method new(int $addr) { nqp::box_i($addr, ::?CLASS)
    }
    multi method new(Int:D $addr) {
        nqp::box_i(nqp::unbox_i(nqp::decont($addr)), ::?CLASS)
    }

    proto method Numeric(|) {*}
    multi method Numeric(::?CLASS:U: --> 0) { }
    multi method Numeric(::?CLASS:D:) { nqp::p6box_i(nqp::unbox_i(self)) }

    proto method Int(|) {*}
    multi method Int(::?CLASS:U: --> 0) { }
    multi method Int(::?CLASS:D:) { nqp::p6box_i(nqp::unbox_i(self)) }

    proto method Bool(|) {*}
    multi method Bool(::?CLASS:U: --> False) { }
    multi method Bool(::?CLASS:D:) { nqp::hllbool(nqp::unbox_i(self)) }

    method deref(::?CLASS:D \ptr:) {
        self
          ?? nativecast(void, ptr)
          !! "Can't dereference a Null Pointer".Failure
    }

    multi method gist(::?CLASS:U:) { '(' ~ self.^name ~ ')' }
    multi method gist(::?CLASS:D:) {
        if self.Int -> $addr {
            self.^name ~ '<' ~ $addr.fmt('%#x') ~ '>'
        }
        else {
            self.^name ~ '<NULL>'
        }
    }

    multi method raku(::?CLASS:U:) { self.^name                            }
    multi method raku(::?CLASS:D:) { self.^name ~ '.new(' ~ self.Int ~ ')' }

    my role TypedPointer[::TValue] {
        method of() { TValue }

        method deref(::?CLASS:D \ptr:) {
            self
              ?? nativecast(TValue, ptr)
              !! "Can't dereference a Null Pointer".Failure
        }

        method add(::?CLASS:D: Int:D $off --> Pointer:D) {
            TValue.isa(void)
              ?? die("Can't do arithmetic with a void pointer")
              !! nqp::box_i(
                   self.Int + nqp::nativecallsizeof(TValue) * $off,
                   self.WHAT
                 )
        }
        method succ(::?CLASS:D:) { self.add( 1) }
        method pred(::?CLASS:D:) { self.add(-1) }

        method AT-POS(::?CLASS:D: Int:D $pos) {
            nqp::nativecallcast(
              TValue,
              map_return_type(TValue),
              nqp::box_i(
                nqp::unbox_i(self) + nqp::nativecallsizeof(TValue) * $pos,
                Pointer
              )
            )
        }
    }

    method ^parameterize(Mu:U \p, Mu:U \t) {
        if   nqp::istype(t, Int)
          || nqp::istype(t, Num)
          || nqp::istype(t, Bool)
          || t === Str
          || t === void
          || nqp::existskey($ok-REPRs, t.REPR) {

            my $w := p.^mixin: TypedPointer[t.WHAT];
            $w.^set_name(p.^name ~ '[' ~ t.^name ~ ']');
            $w
        }

        else {
            wrong-type('typed pointer', t);
        }
    }
}

#- CArray ----------------------------------------------------------------------
# CArray class, used to represent C arrays.
our class CArray is repr('CArray') is array_type(Pointer) {

    method AT-POS(::?CLASS:D: $pos) {
        die "CArray cannot be used without a type"
    }

    # For parameterization to ints
    my role IntTypedCArray[::TValue]
      does Positional[TValue]
      is   array_type(TValue)
    {
        multi method AT-POS(::?CLASS:D: int $pos) is raw {
            nqp::atposref_i(self, $pos)
        }
        multi method AT-POS(::?CLASS:D: Int:D $pos) is raw {
            nqp::atposref_i(self, nqp::unbox_i($pos))
        }
        multi method AT-POS(::?CLASS:D: Any:D $pos) is raw {
            nqp::atposref_i(self, nqp::unbox_i($pos.Int))
        }

        multi method ASSIGN-POS(::?CLASS:D: int $pos, int $value) {
            nqp::bindpos_i(self, $pos, $value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D $pos, int $value) {
            nqp::bindpos_i(self, nqp::unbox_i($pos), $value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D $pos, Int:D $value) {
            nqp::bindpos_i(self, nqp::unbox_i($pos), nqp::unbox_i($value))
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D $pos, Any:D $value) {
            nqp::bindpos_i(self, nqp::unbox_i($pos), nqp::unbox_i($value.Int))
        }
        multi method ASSIGN-POS(::?CLASS:D: Any:D $pos, Any:D $value) {
            nqp::bindpos_i(self,nqp::unbox_i($pos.Int),nqp::unbox_i($value.Int))
        }

        method !allocate(::?CLASS:U: int $elems) {
            my $array := nqp::create(self);
            nqp::bindpos_i($array, $elems - 1, 0);
            $array
        }

        multi method allocate(::?CLASS:U: int $elems) {
            self!allocate($elems)
        }
        multi method allocate(::?CLASS:U: Int:D $elems) {
            self!allocate($elems)
        }
        multi method allocate(::?CLASS:U: Any:D $elems) {
            self!allocate($elems.Int)
        }
    }

    # For parameterization to unsigned ints
    my role UIntTypedCArray[::TValue]
      does Positional[TValue]
      is   array_type(TValue)
    {
        multi method AT-POS(::?CLASS:D: int $pos) is raw {
            nqp::atposref_u(self, $pos);
        }
        multi method AT-POS(::?CLASS:D: Int:D $pos) is raw {
            nqp::atposref_u(self, nqp::unbox_i($pos));
        }
        multi method AT-POS(::?CLASS:D: Any:D $pos) is raw {
            nqp::atposref_u(self, nqp::unbox_i($pos.Int));
        }

        multi method ASSIGN-POS(::?CLASS:D: int $pos, int $value) {
            nqp::bindpos_u(self, $pos, $value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D $pos, int $value) {
            nqp::bindpos_u(self, nqp::unbox_i($pos), $value)
        }
        multi method ASSIGN-POS(::?CLASS:D: int $pos, uint $value) {
            nqp::bindpos_u(self, $pos, $value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D $pos, uint $value) {
            nqp::bindpos_u(self, nqp::unbox_i($pos), $value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D $pos, Int:D $value) {
            nqp::bindpos_u(self, nqp::unbox_i($pos), nqp::unbox_u($value))
        }
        multi method ASSIGN-POS(::?CLASS:D: Any:D $pos, Int:D $value) {
            nqp::bindpos_u(self, nqp::unbox_i($pos.Int), nqp::unbox_u($value))
        }
        multi method ASSIGN-POS(::?CLASS:D: Any:D $pos, Any:D $value) {
            nqp::bindpos_u(self,nqp::unbox_i($pos.Int),nqp::unbox_u($value.Int))
        }

        method !allocate(::?CLASS:U: int $elems) {
            my $array := nqp::create(self);
            nqp::bindpos_u($array, $elems - 1, 0);
            $array
        }

        multi method allocate(::?CLASS:U: int $elems) {
            self!allocate($elems)
        }
        multi method allocate(::?CLASS:U: Int:D $elems) {
            self!allocate($elems)
        }
        multi method allocate(::?CLASS:U: Any:D $elems) {
            self!allocate($elems.Int)
        }
    }

    # For parameterization to nums
    my role NumTypedCArray[::TValue]
      does Positional[TValue]
      is   array_type(TValue)
    {
        multi method AT-POS(::?CLASS:D: int $pos) is raw {
            nqp::atposref_n(self, $pos);
        }
        multi method AT-POS(::?CLASS:D: Int:D $pos) is raw {
            nqp::atposref_n(self, nqp::unbox_i($pos))
        }
        multi method AT-POS(::?CLASS:D: Any:D $pos) is raw {
            nqp::atposref_n(self, nqp::unbox_i($pos.Int))
        }

        multi method ASSIGN-POS(::?CLASS:D: int $pos, num $value) {
            nqp::bindpos_n(self, $pos, $value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D $pos, num $value) {
            nqp::bindpos_n(self, nqp::unbox_i($pos), $value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D $pos, Num:D $value) {
            nqp::bindpos_n(self, nqp::unbox_i($pos), nqp::unbox_n($value))
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D $pos, Any:D $value) {
            nqp::bindpos_n(self, nqp::unbox_i($pos), nqp::unbox_n($value.Num))
        }

        method !allocate(::?CLASS:U: int $elems) {
            my $array := nqp::create(self);
            nqp::bindpos_n($array, $elems - 1, 0e0);
            $array
        }

        multi method allocate(::?CLASS:U: int $elems) {
            self!allocate($elems)
        }
        multi method allocate(::?CLASS:U: Int:D $elems) {
            self!allocate($elems)
        }
        multi method allocate(::?CLASS:U: Any:D $elems) {
            self!allocate($elems.Int)
        }
    }

    # For parameterization to all other allowed REPRs
    my role TypedCArray[::TValue]
      does Positional[TValue]
      is   array_type(TValue)
    {
        multi method AT-POS(::?CLASS:D: int $pos) is rw {
            Proxy.new:
              FETCH => -> $ {
                  nqp::atpos(self, $pos)
              },
              STORE => -> $invocant, $value {
                  nqp::bindpos(self, $pos, nqp::decont($value));
                  $invocant
              }
        }
        multi method AT-POS(::?CLASS:D: Int:D $pos) is rw {
            Proxy.new:
              FETCH => -> $ {
                  nqp::atpos(self, nqp::unbox_i($pos))
              },
              STORE => -> $invocant, $value {
                  nqp::bindpos(self, nqp::unbox_i($pos), nqp::decont($value));
                  $invocant
              }
        }
        multi method AT-POS(::?CLASS:D: Any:D $pos) is rw {
            Proxy.new:
              FETCH => -> $ {
                  nqp::atpos(self, nqp::unbox_i($pos.Int))
              },
              STORE => -> $self, $value {
                  nqp::bindpos(self,nqp::unbox_i($pos.Int),nqp::decont($value));
                  $self
              }
        }

        multi method ASSIGN-POS(::?CLASS:D: int $pos, \value) {
            nqp::bindpos(self, $pos, nqp::decont(value))
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D $pos, \value) {
            nqp::bindpos(self, nqp::unbox_i($pos), nqp::decont(value))
        }
        multi method ASSIGN-POS(::?CLASS:D: Any:D $pos, \value) {
            nqp::bindpos(self, nqp::unbox_i($pos.Int), nqp::decont(value))
        }

        method !allocate(int $elems) {
            my $array := nqp::create(self);
            my $type  := ::?CLASS.^array_type;

            my int $i;
            nqp::while(
              $i < $elems,
              nqp::bindpos($array, $i++, nqp::create($type))
            );
            $array
        }

        multi method allocate(::?CLASS:U: int $elems) {
            self!allocate($elems)
        }
        multi method allocate(::?CLASS:U: Int:D $elems) {
            self!allocate($elems)
        }
        multi method allocate(::?CLASS:U: Any:D $elems) {
            self!allocate($elems.Int)
        }
    }

    method ^parameterize(Mu:U \array, Mu:U \t) {
        my $WHAT  := t.WHAT;
        my $mixin := nqp::istype(t, Int)
          ?? t.^unsigned
            ?? UIntTypedCArray[$WHAT]
            !! IntTypedCArray[$WHAT]
          !! nqp::istype(t, Num)
            ?? NumTypedCArray[$WHAT]
            !! (t === Str || nqp::existskey($ok-REPRs, t.REPR))
              ?? TypedCArray[$WHAT]
              !! wrong-type('C array', t);

        my $what := array.^mixin: $mixin;
        $what.^set_name(array.^name ~ '[' ~ t.^name ~ ']');
        $what
    }

    method Str(::?CLASS:D:) { self.join(' ') }

    method elems(::?CLASS:D:) { nqp::elems(self) }

    method list(::?CLASS:D:) {
        my int $m   = nqp::elems(self);
        my $buffer := nqp::setelems(  # presize buffer
          nqp::setelems(nqp::create(IterationBuffer), $m),
          0
        );
        my int $i;

        nqp::while(
          $i < $m,
          nqp::push($buffer, self.AT-POS($i++))
        );

        $buffer.List
    }

    multi method new(::?CLASS:)          { nqp::create(self) }
    multi method new(::?CLASS: *@values) { self.new(@values) }
    multi method new(::?CLASS: @values) {
        my $result := nqp::create(self);
        my int $n   = @values.elems;  # reifies

        # By counting down, we're effectively doing a setelems for the
        # right size on the first iteration
        nqp::if(
          $n,
          nqp::while(
            --$n >= 0,
            $result.ASSIGN-POS($n, @values.AT-POS($n))
          )
        );

        $result
    }
}

# vim: expandtab shiftwidth=4
