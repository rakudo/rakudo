my class Rat { ... }
my class X::Cannot::Capture { ... }

my class Int { ... }
my subset UInt of Int where {
    nqp::not_i(nqp::isconcrete($_)) || nqp::isge_I(nqp::decont($_),0)
}
#?if moar
nqp::syscall('set-cur-hll-config-key', 'uint_box', UInt);
#?endif
#?if !moar
nqp::dispatch('boot-syscall', 'set-cur-hll-config-key', 'uint_box', UInt);
#?endif

my class Int does Real { # declared in BOOTSTRAP
    # class Int is Cool
    #     has bigint $!value is box_target;

    multi method WHICH(Int:D: --> ValueObjAt:D) {
        nqp::box_s(
          nqp::concat(
            nqp::if(
              nqp::eqaddr(self.WHAT,Int),
              'Int|',
              nqp::concat(nqp::unbox_s(self.^name), '|')
            ),
            nqp::tostr_I(self)
          ),
          ValueObjAt
        )
    }

    multi method ACCEPTS(Int:D: Int:D $other, --> Bool:D) {
        nqp::hllbool(nqp::iseq_I(self, $other))
    }

    proto method new(|) {*}
    multi method new(Any:U $type) {
        die "Cannot create an Int from a '$type.^name()' type object";
    }
    multi method new(Any:D \value --> Int:D) { self.new: value.Int }
    multi method new(int   \value --> Int:D) {
        # rebox the value, so we get rid of any potential mixins
        nqp::fromI_I(nqp::decont(value), self)
    }
    multi method new(Int:D \value = 0 --> Int:D) {
        # rebox the value, so we get rid of any potential mixins
        nqp::fromI_I(nqp::decont(value), self)
    }

    multi method raku(Int:D: --> Str:D) {
        self.Str;
    }
    multi method Bool(Int:D: --> Bool:D) {
        nqp::hllbool(nqp::bool_I(self));
    }

    method Capture() { X::Cannot::Capture.new( :what(self) ).throw }

    method Int() { self }

    method sign(Int:D: --> Int:D) {
        nqp::isgt_I(self,0) || nqp::neg_i(nqp::islt_I(self,0))
    }

    multi method Str(Int:D: --> Str:D) {
        nqp::p6box_s(nqp::tostr_I(self))
    }
    multi method Str(Int:D: :$superscript! --> Str:D) {
        $_ := self.Str;
        $superscript ?? .trans('-0123456789' => '⁻⁰¹²³⁴⁵⁶⁷⁸⁹') !! $_
    }
    multi method Str(Int:D: :$subscript! --> Str:D) {
        $_ := self.Str;
        $subscript ?? .trans('-0123456789' => '₋₀₁₂₃₄₅₆₇₈₉') !! $_
    }

    method Num(Int:D: --> Num:D) {
        nqp::p6box_n(nqp::tonum_I(self));
    }

    method Rat(Int:D: $? --> Rat:D) {
        nqp::p6bindattrinvres(
          nqp::p6bindattrinvres(nqp::create(Rat),Rat,'$!numerator',self),
          Rat,'$!denominator',1
        )
    }
    method FatRat(Int:D: $? --> FatRat:D) {
        nqp::p6bindattrinvres(
          nqp::p6bindattrinvres(nqp::create(FatRat),FatRat,'$!numerator',self),
          FatRat,'$!denominator',1
        )
    }

    method abs(Int:D: --> Int:D) {
        nqp::abs_I(self, Int)
    }

    method Bridge(Int: --> Num:D) {
        self.defined
            ?? nqp::p6box_n(nqp::tonum_I(self))
            !! self.Real::Bridge
    }

    multi method sqrt(Int:D: --> Num:D) {
        nqp::p6box_n(nqp::sqrt_n(nqp::tonum_I(self)))
    }

    sub BASE_OUT_OF_RANGE(int $got) {
        X::OutOfRange.new(
          :what('base argument to base'), :$got, :range<2..36>
        ).Failure
    }
    sub DIGITS_OUT_OF_RANGE(int $got) {
        X::OutOfRange.new(
          :what('digits argument to base'), :$got, :range<2..36>
        ).Failure
    }

    proto method base(|) {*}
    multi method base(Int:D: Int:D $base --> Str:D) {
        2 <= $base <= 36
          ?? nqp::base_I(self,nqp::unbox_i($base))
          !! BASE_OUT_OF_RANGE($base)
    }
    multi method base(Int:D: Int(Cool) $base, Whatever --> Str:D) {
        self.base($base)
    }
    multi method base(Int:D: Int(Cool) $base, Int:D $digits = 0 --> Str:D) {
        2 <= $base <= 36
          ?? $digits
            ?? $digits < 0
              ?? DIGITS_OUT_OF_RANGE($digits)
              !! nqp::base_I(self,nqp::unbox_i($base))
                   ~ '.'
                   ~ nqp::x('0',nqp::unbox_i($digits))
            !! nqp::base_I(self,nqp::unbox_i($base))
          !! BASE_OUT_OF_RANGE($base)
    }

    method !eggify($egg --> Str:D) { self.base(2).trans("01" => $egg) }
    multi method base(Int:D: "camel" --> Str:D) { self!eggify: "🐪🐫" }
    multi method base(Int:D: "beer"  --> Str:D) { self!eggify: "🍺🍻" }

    # If self is Int, we assume mods are Ints also.  (div fails otherwise.)
    # If do-not-want, user should cast invocant to proper domain.
    method polymod(Int:D: +@mods --> Seq:D) {
        fail X::OutOfRange.new(
          :what('invocant to polymod'), :got(self), :range<0..^Inf>
        ) if self < 0;

        gather {
            my $more = self;
            if @mods.is-lazy {
                for @mods -> $mod {
                    $more
                      ?? $mod
                        ?? take $more mod $mod
                        !! X::Numeric::DivideByZero.new(
                             using => 'polymod', numerator => $more
                           ).Failure
                      !! last;
                    $more = $more div $mod;
                }
                take $more if $more;
            }
            else {
                for @mods -> $mod {
                    $mod
                      ?? take $more mod $mod
                      !! X::Numeric::DivideByZero.new(
                           using => 'polymod', numerator => $more
                         ).Failure;
                    $more = $more div $mod;
                }
                take $more;
            }
        }
    }

    method expmod(Int:D: Int:D \base, Int:D \mod --> Int:D) {
        nqp::expmod_I(self, nqp::decont(base), nqp::decont(mod), Int);
    }
    method is-prime(--> Bool:D) { nqp::hllbool(nqp::isprime_I(self)) }

    method floor(Int:D:) { self }
    method ceiling(Int:D:) { self }
    proto method round(|) {*}
    multi method round(Int:D:) { self }
    multi method round(Int:D: Real(Cool) $scale --> Real:D) {
        (self / $scale + 1/2).floor * $scale
    }

    method lsb(Int:D: --> Int:D) {
        nqp::unless(
          self, # short-circuit `0`, as it doesn't have any bits set…
          Nil,  # … and the algo we'll use requires at least one that is.
          nqp::stmts(
            (my int $lsb),
            (my $x := nqp::abs_I(self, Int)),
            nqp::while( # "fast-forward": shift off by whole all-zero-bit bytes
              nqp::isfalse(nqp::bitand_I($x, 0xFF, Int)),
              nqp::stmts(
                ($lsb += 8),
                ($x := nqp::bitshiftr_I($x, 8, Int)))),
            nqp::while( # our lsb is in the current byte; shift off zero bits
              nqp::isfalse(nqp::bitand_I($x, 0x01, Int)),
              nqp::stmts(
                ++$lsb,
                ($x := nqp::bitshiftr_I($x, 1, Int)))),
            $lsb)) # we shifted enough to get to the first set bit
    }

    method msb(Int:D: --> Int:D) {
        nqp::unless(
          self,
          Nil,
          nqp::if(
            nqp::iseq_I(self, -1),
            0,
            nqp::stmts(
              (my int $msb),
              (my $x := self),
              nqp::islt_I($x, 0) # handle conversion of negatives
                && ($x := nqp::mul_I(-2,
                  nqp::add_I($x, 1, Int), Int)),
              nqp::while(
                nqp::isgt_I($x, 0xFF),
                nqp::stmts(
                  ($msb += 8),
                  ($x := nqp::bitshiftr_I($x, 8, Int)))),
              nqp::isgt_I($x, 0x0F)
                && ($msb += 4) && ($x := nqp::bitshiftr_I($x, 4, Int)),
                 nqp::bitand_I($x, 0x8, Int) && ($msb += 3)
              || nqp::bitand_I($x, 0x4, Int) && ($msb += 2)
              || nqp::bitand_I($x, 0x2, Int) && ($msb += 1),
              $msb)))
    }

    method narrow(Int:D:) { self }

    method Range(Int:U: --> Range:D) {
        given self {
            when int  { $?BITS == 64 ??  int64.Range !!  int32.Range }
            when uint { $?BITS == 64 ?? uint64.Range !! uint32.Range }

            when int64  { Range.new(-9223372036854775808, 9223372036854775807) }
            when int32  { Range.new(         -2147483648, 2147483647         ) }
            when int16  { Range.new(              -32768, 32767              ) }
            when int8   { Range.new(                -128, 127                ) }
            # Bring back in a future Raku version, or just put on the type object
            #when int4   { Range.new(                  -8, 7                  ) }
            #when int2   { Range.new(                  -2, 1                  ) }
            #when int1   { Range.new(                  -1, 0                  ) }

            when uint64 { Range.new( 0, 18446744073709551615 ) }
            when uint32 { Range.new( 0, 4294967295           ) }
            when uint16 { Range.new( 0, 65535                ) }
            when uint8  { Range.new( 0, 255                  ) }
            when byte   { Range.new( 0, 255                  ) }
            # Bring back in a future Raku version, or just put on the type object
            #when uint4  { Range.new( 0, 15                   ) }
            #when uint2  { Range.new( 0, 3                    ) }
            #when uint1  { Range.new( 0, 1                    ) }

            default {  # some other kind of Int
                my str $name = .^name;
                $name eq 'UInt'
                  ?? Range.new(    0, Inf, :excludes-max )
                  !! $name eq 'atomicint'
                    ?? ($?BITS == 64 ??  int64.Range !!  int32.Range)
                    !! Range.new( -Inf, Inf, :excludes-min, :excludes-max )
            }
        }
    }

    method !int() {
        $_ := nqp::bitand_I(self, 0xffffffffffffffff, Int);
        $_ >= 0x8000000000000000 ?? $_ - 0x10000000000000000 !! $_
    }

    method int  (--> int  ) is raw { my int   $a = self!int; $a }
    method int64(--> int64) is raw { my int64 $a = self!int; $a }
    method int32(--> int32) is raw { my int32 $a = self!int; $a }
    method int16(--> int16) is raw { my int16 $a = self!int; $a }
    method int8( --> int8 ) is raw { my int8  $a = self!int; $a }

    method uint  (--> uint  ) is raw {
        my uint   $a = nqp::bitand_I(self, 0xffffffffffffffff, Int);
        $a
    }
    method uint64(--> uint32) is raw {
        my uint64 $a = nqp::bitand_I(self, 0xffffffffffffffff, Int);
        $a
    }
    method uint32(--> uint32) is raw {
        my uint32 $a = nqp::bitand_I(self, 0x00000000ffffffff, Int);
        $a
    }
    method uint16(--> uint16) is raw {
        my uint16 $a = nqp::bitand_I(self, 0x000000000000ffff, Int);
        $a
    }
    method uint8( --> uint8 ) is raw {
        my uint8  $a = nqp::bitand_I(self, 0x00000000000000ff, Int);
        $a
    }
    method byte(  --> byte  ) is raw {
        my byte   $a = nqp::bitand_I(self, 0x00000000000000ff, Int);
        $a
    }
}

multi sub prefix:<++>(Int:D $a is rw --> Int:D) {
    $a = nqp::add_I(nqp::decont($a), 1, Int);
}
multi sub prefix:<++>( int $a is rw --> int)  { $a = nqp::add_i($a, 1) }
multi sub prefix:<++>(uint $a is rw --> uint) { $a = nqp::add_i($a, 1) }

multi sub prefix:<-->(Int:D $a is rw --> Int:D) {
    $a = nqp::sub_I(nqp::decont($a), 1, Int);
}
multi sub prefix:<-->( int $a is rw --> int)  { $a = nqp::sub_i($a, 1) }
multi sub prefix:<-->(uint $a is rw --> uint) { $a = nqp::sub_i($a, 1) }

multi sub postfix:<++>(Int:D $a is rw --> Int:D) {
    my \b := nqp::decont($a); $a = nqp::add_I(b, 1, Int); b
}
multi sub postfix:<++>(int $a is rw --> int) {
    my int $b = $a; $a = nqp::add_i($b, 1); $b
}
multi sub postfix:<++>(uint $a is rw --> uint) {
    my uint $b = $a; $a = my uint $ = nqp::add_i($b, 1); $b
}

multi sub postfix:<-->(Int:D $a is rw --> Int:D) {
    my \b := nqp::decont($a); $a = nqp::sub_I(b, 1, Int); b
}
multi sub postfix:<-->(int $a is rw --> int) {
    my int $b = $a; $a = nqp::sub_i($b, 1); $b
}
multi sub postfix:<-->(uint $a is rw --> uint) {
    my int $b = $a; $a = nqp::sub_i($b, 1); $b
}

multi sub prefix:<->(Int:D \a --> Int:D) { nqp::neg_I(nqp::decont(a), Int) }
multi sub prefix:<->( int $a --> int) { nqp::neg_i($a) }
multi sub prefix:<->(uint $a --> int) { nqp::neg_i($a) }

multi sub abs(Int:D \a --> Int:D) { nqp::abs_I(nqp::decont(a), Int) }
multi sub abs( int $a --> int)  { nqp::abs_i($a) }
multi sub abs(uint $a --> uint) { $a }

multi sub infix:<+>(Int:D $a, Int:D $b --> Int:D) { nqp::add_I($a,$b,Int) }
multi sub infix:<+>( int  $a,  int  $b --> int)   { nqp::add_i($a,$b)     }
multi sub infix:<+>(uint  $a, uint  $b --> uint)  { nqp::add_i($a,$b)     }

multi sub infix:<->(Int:D $a, Int:D $b --> Int:D) { nqp::sub_I($a,$b,Int) }
multi sub infix:<->( int  $a,  int  $b --> int)   { nqp::sub_i($a,$b)     }
multi sub infix:<->(uint  $a, uint  $b --> uint)  { nqp::sub_i($a,$b)     }

multi sub infix:<*>(Int:D $a, Int:D $b --> Int:D) { nqp::mul_I($a,$b,Int) }
multi sub infix:<*>( int  $a,  int  $b --> int)   { nqp::mul_i($a,$b)     }
multi sub infix:<*>(uint  $a, uint  $b --> uint)  { nqp::mul_i($a,$b)     }

multi sub infix:<eqv>(Int:D $a, Int:D $b --> Bool:D) {
    nqp::hllbool(  # need to check types as enums such as Bool wind up here
      nqp::eqaddr($a.WHAT,$b.WHAT) && nqp::iseq_I($a,$b)
    )
}
multi sub infix:<eqv>(int $a, int $b --> Bool:D) {
    nqp::hllbool(nqp::iseq_i($a,$b))
}
multi sub infix:<eqv>(uint $a, uint $b --> Bool:D) {
    nqp::hllbool(nqp::iseq_i($a,$b))
}

multi sub infix:<div>(Int:D $a, Int:D $b --> Int:D) {
    $b
      ?? nqp::div_I($a,$b,Int)
      !! X::Numeric::DivideByZero.new(:using<div>, :numerator($a)).Failure
}
# relies on opcode or hardware to detect division by 0
multi sub infix:<div>(int  $a, int  $b --> int)  { nqp::div_i($a, $b) }
multi sub infix:<div>(uint $a, uint $b --> uint) { nqp::div_i($a, $b) }

multi sub infix:<div>($a, $b --> Int:D) {
    $b.Int
      ?? nqp::div_I($a.Int,$b.Int,Int)
      !! X::Numeric::DivideByZero.new(:using<div>, :numerator($a)).Failure
}

multi sub infix:<%>(Int:D $a, Int:D $b --> Int:D) {
    nqp::isbig_I($a) || nqp::isbig_I($b)
      ?? $b
        ?? nqp::mod_I($a,$b,Int)
        !! X::Numeric::DivideByZero.new(:using<%>, :numerator($a)).Failure
      !! nqp::isne_i($b,0)
        # quick fix https://github.com/Raku/old-issue-tracker/issues/4999
        ?? nqp::mod_i(nqp::add_i(nqp::mod_i($a,$b),$b),$b)
        !! X::Numeric::DivideByZero.new(:using<%>, :numerator($a)).Failure
}
# relies on opcode or hardware to detect division by 0
# quick fix https://github.com/Raku/old-issue-tracker/issues/4999
multi sub infix:<%>(int $a, int $b --> int) {
    nqp::mod_i(nqp::add_i(nqp::mod_i($a,$b),$b),$b)
}
multi sub infix:<%>(uint $a, uint $b --> uint) {
    nqp::mod_i(nqp::add_i(nqp::mod_i($a,$b),$b),$b)
}

multi sub infix:<%%>(Int:D $a, Int:D $b) {
    nqp::isbig_I($a) || nqp::isbig_I($b)
      ?? $b
        ?? !nqp::mod_I($a,$b,Int)
        !! X::Numeric::DivideByZero.new(
             using => 'infix:<%%>', :numerator($a)
           ).Failure
      !! nqp::isne_i($b,0)
        ?? nqp::hllbool(nqp::not_i(nqp::mod_i($a,$b)))
        !! X::Numeric::DivideByZero.new(
             using => 'infix:<%%>', :numerator($a)
           ).Failure
}
multi sub infix:<%%>(int $a, int $b --> Bool:D) {
    nqp::hllbool(nqp::iseq_i(nqp::mod_i($a, $b), 0))
}
multi sub infix:<%%>(uint $a, uint $b --> Bool:D) {
    nqp::hllbool(nqp::iseq_i(nqp::mod_i($a, $b), 0))
}

multi sub infix:<**>(Int:D $a, Int:D $b --> Real:D) {
    nqp::isge_I($b,0)
      # when a**b is too big nqp::pow_I returns Inf
      ?? nqp::istype((my $power := nqp::pow_I($a,$b,Num,Int)),Int)
        ?? $power
        !! X::Numeric::Overflow.new.Failure
      # when a**b is too big nqp::pow_I returns Inf
      !! nqp::istype(($power := nqp::pow_I($a,nqp::neg_I($b,Int),Num,Int)),Num) ||
         (nqp::istype(($power := CREATE_RATIONAL_FROM_INTS(1, $power, Int, Int)),Num) && nqp::iseq_n($power,0e0) && nqp::isne_I($a,0))
        ?? X::Numeric::Underflow.new.Failure
        !! $power
}
multi sub infix:<**>( int $a,  int $b --> int)  { nqp::pow_i($a, $b) }
multi sub infix:<**>(uint $a, uint $b --> uint) { nqp::pow_i($a, $b) }

multi sub infix:<lcm>(--> 1) { }
multi sub infix:<lcm>(Int:D $x) { $x }
multi sub infix:<lcm>(Int:D $a, Int:D $b --> Int:D) { nqp::lcm_I($a,$b,Int) }
multi sub infix:<lcm>( int  $a,  int  $b --> int)   { nqp::lcm_i($a,$b)     }
multi sub infix:<lcm>(uint  $a, uint  $b --> uint)  { nqp::lcm_i($a,$b)     }

multi sub infix:<gcd>(Int:D $x) { $x }
multi sub infix:<gcd>(Int:D $a, Int:D $b --> Int:D) { nqp::gcd_I($a,$b,Int) }
multi sub infix:<gcd>( int  $a,  int  $b --> int)   { nqp::gcd_i($a,$b)     }
multi sub infix:<gcd>(uint  $a, uint  $b --> uint)  { nqp::gcd_i($a,$b)     }

multi sub infix:<===>(Int:D $a, Int:D $b --> Bool:D) {
    nqp::hllbool(
      nqp::eqaddr($a.WHAT,$b.WHAT) && nqp::iseq_I($a,$b)
    )
}
# hey, the optimizer is smart enough to figure these ones out for us, no?
multi sub infix:<===>( int $a,  int $b --> Bool:D) { $a == $b }
multi sub infix:<===>(uint $a, uint $b --> Bool:D) { $a == $b }

multi sub infix:<==>(Int:D $a, Int:D $b --> Bool:D) {
    nqp::hllbool(nqp::iseq_I($a,$b))
}
multi sub infix:<==>(int $a, int $b --> Bool:D) {
    nqp::hllbool(nqp::iseq_i($a,$b))
}
multi sub infix:<==>(uint $a, uint $b --> Bool:D) {
    nqp::hllbool(nqp::iseq_u($a,$b))
}
multi sub infix:<==>(int $a, uint $b --> Bool:D) {
    nqp::hllbool(nqp::isge_i($a,0) && nqp::iseq_u($a,$b))
}
multi sub infix:<==>(uint $a, int $b --> Bool:D) {
    nqp::hllbool(nqp::isge_i($b,0) && nqp::iseq_u($a,$b))
}

multi sub infix:<!=>(Int:D $a, Int:D $b --> Bool:D) {
    nqp::hllbool(nqp::isne_I($a,$b))
}
multi sub infix:<!=>(int $a, int $b --> Bool:D) {
    nqp::hllbool(nqp::isne_i($a,$b))
}
multi sub infix:<!=>(uint $a, uint $b --> Bool:D) {
    nqp::hllbool(nqp::isne_u($a,$b))
}
multi sub infix:<!=>(int $a, uint $b --> Bool:D) {
    nqp::hllbool(nqp::isge_i($a,0) && nqp::isne_u($a,$b))
}
multi sub infix:<!=>(uint $a, int $b --> Bool:D) {
    nqp::hllbool(nqp::isge_i($b,0) && nqp::isne_u($a,$b))
}

multi sub infix:«<»(Int:D $a, Int:D $b --> Bool:D) {
    nqp::hllbool(nqp::islt_I($a,$b))
}
multi sub infix:«<»(int $a, int $b --> Bool:D) {
    nqp::hllbool(nqp::islt_i($a,$b))
}
multi sub infix:«<»(uint $a, uint $b --> Bool:D) {
    nqp::hllbool(nqp::islt_u($a,$b))
}
multi sub infix:«<»(int $a, uint $b --> Bool:D) {
    nqp::hllbool(nqp::islt_i($a,0) || nqp::islt_u($a,$b))
}
multi sub infix:«<»(uint $a, int $b --> Bool:D) {
    nqp::hllbool(nqp::isge_i($b,0) && nqp::islt_u($a,$b))
}

multi sub infix:«<=»(Int:D $a, Int:D $b --> Bool:D) {
    nqp::hllbool(nqp::isle_I($a,$b))
}
multi sub infix:«<=»(int $a, int $b --> Bool:D) {
    nqp::hllbool(nqp::isle_i($a,$b))
}
multi sub infix:«<=»(uint $a, uint $b --> Bool:D) {
    nqp::hllbool(nqp::isle_u($a,$b))
}
multi sub infix:«<=»(int $a, uint $b --> Bool:D) {
    nqp::hllbool(nqp::isle_i($a,0) || nqp::isle_u($a,$b))
}
multi sub infix:«<=»(uint $a, int $b --> Bool:D) {
    nqp::hllbool(nqp::isge_i($b,0) && nqp::isle_u($a,$b))
}

multi sub infix:«>»(Int:D $a, Int:D $b --> Bool:D) {
    nqp::hllbool(nqp::isgt_I($a,$b))
}
multi sub infix:«>»(int $a, int $b --> Bool:D) {
    nqp::hllbool(nqp::isgt_i($a,$b))
}
multi sub infix:«>»(uint $a, uint $b --> Bool:D) {
    nqp::hllbool(nqp::isgt_u($a,$b))
}
multi sub infix:«>»(int $a, uint $b --> Bool:D) {
    nqp::hllbool(nqp::isgt_i($a,0) && nqp::isgt_u($a,$b))
}
multi sub infix:«>»(uint $a, int $b --> Bool:D) {
    nqp::hllbool(nqp::islt_i($b,0) || nqp::isgt_u($a,$b))
}

multi sub infix:«>=»(Int:D $a, Int:D $b --> Bool:D) {
    nqp::hllbool(nqp::isge_I($a,$b))
}
multi sub infix:«>=»(int $a, int $b --> Bool:D) {
    nqp::hllbool(nqp::isge_i($a,$b))
}
multi sub infix:«>=»(uint $a, uint $b --> Bool:D) {
    nqp::hllbool(nqp::isge_u($a,$b))
}
multi sub infix:«>=»(int $a, uint $b --> Bool:D) {
    nqp::hllbool(nqp::isge_i($a,0) && nqp::isge_u($a,$b))
}
multi sub infix:«>=»(uint $a, int $b --> Bool:D) {
    nqp::hllbool(nqp::isle_i($b,0) || nqp::isge_u($a,$b))
}

multi sub infix:<+|>(Int:D $a, Int:D $b --> Int:D) { nqp::bitor_I($a,$b,Int) }
multi sub infix:<+|>( int  $a,  int  $b --> int)   { nqp::bitor_i($a,$b)     }
multi sub infix:<+|>(uint  $a, uint  $b --> uint)  { nqp::bitor_i($a,$b)     }

multi sub infix:<+&>(Int:D $a, Int:D $b --> Int:D) { nqp::bitand_I($a,$b,Int) }
multi sub infix:<+&>( int  $a,  int  $b --> int)   { nqp::bitand_i($a,$b)     }
multi sub infix:<+&>(uint  $a, uint  $b --> uint)  { nqp::bitand_i($a,$b)     }

multi sub infix:<+^>(Int:D $a, Int:D $b --> Int:D) { nqp::bitxor_I($a,$b,Int) }
multi sub infix:<+^>( int  $a,  int  $b --> int)   { nqp::bitxor_i($a, $b)    }
multi sub infix:<+^>(uint  $a, uint  $b --> uint)  { nqp::bitxor_i($a, $b)    }

multi sub infix:«+<»(Int:D $a, Int:D $b --> Int:D) {
    nqp::bitshiftl_I($a,$b,Int)
}
multi sub infix:«+<»(int $a, int $b --> int) {
   nqp::bitshiftl_i($a,$b);
}
multi sub infix:«+<»(uint $a, uint $b --> int) {
   nqp::bitshiftl_i($a,$b);
}

multi sub infix:«+>»(Int:D $a, Int:D $b --> Int:D) {
    nqp::bitshiftr_I($a,$b,Int)
}
multi sub infix:«+>»(int $a, int $b --> int) {
   nqp::bitshiftr_i($a,$b)
}
multi sub infix:«+>»(uint $a, uint $b --> int) {
   nqp::bitshiftr_i($a,$b)
}

multi sub prefix:<+^>(Int:D $a --> Int:D) { nqp::bitneg_I($a,Int) }
multi sub prefix:<+^>( int  $a --> int)   { nqp::bitneg_i($a)     }
multi sub prefix:<+^>(uint  $a --> uint)  { nqp::bitneg_u($a)     }

proto sub is-prime($, *%) is pure {*}
multi sub is-prime(\x --> Int:D) { x.is-prime }

proto sub expmod($, $, $, *%) is pure  {*}
multi sub expmod(Int:D $base, Int:D $exp, Int:D $mod --> Int:D) {
    nqp::expmod_I($base,$exp,$mod,Int)
}
multi sub expmod(\base, \exp, \mod --> Int:D) {
    nqp::expmod_I(
      nqp::decont(base.Int),
      nqp::decont(exp.Int),
      nqp::decont(mod.Int),
      Int
    )
}

proto sub lsb($, *%) {*}
multi sub lsb(Int:D $a --> Int:D) { $a.lsb }

proto sub msb($, *%) {*}
multi sub msb(Int:D $a --> Int:D) { $a.msb }

# vim: expandtab shiftwidth=4
