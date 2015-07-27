my class Rat { ... }
my class X::Numeric::DivideByZero { ... }

my class Int { ... }
my subset UInt of Int where * >= 0;

my class Int does Real { # declared in BOOTSTRAP
    # class Int is Cool {
    #     has bigint $!value is box_target;

    multi method WHICH(Int:D:) {
        nqp::box_s(
            nqp::concat(
                nqp::concat(nqp::unbox_s(self.^name), '|'),
                nqp::tostr_I(self)
            ),
            ObjAt
        );
    }
    multi method new($value) {
        nqp::box_i($value, self.WHAT);
    }
    multi method perl(Int:D:) {
        self.Str;
    }
    multi method Bool(Int:D:) {
        nqp::p6bool(nqp::bool_I(self));
    }

    method Int() { self }
    multi method Pos(Int:D:) { nqp::box_i(nqp::unbox_i(self),Pos) }

    multi method Str(Int:D:) {
        nqp::p6box_s(nqp::tostr_I(self));
    }

    method Num(Int:D:) {
        nqp::p6box_n(nqp::tonum_I(self));
    }

    method Rat(Int:D: $?) {
        Rat.new(self, 1);
    }
    method FatRat(Int:D: $?) {
        FatRat.new(self, 1);
    }

    method abs(Int:D:) {
        nqp::abs_I(self, Int)
    }

    method Bridge(Int:D:) {
        nqp::p6box_n(nqp::tonum_I(self));
    }

    method chr(Int:D:) {
        nqp::p6box_s(nqp::chr(nqp::unbox_i(self)));
    }

    method sqrt(Int:D:) { nqp::p6box_n(nqp::sqrt_n(nqp::tonum_I(self))) }

    method base(Int:D: Cool $base, $digits = 0) {
        fail("base must be between 2 and 36, got $base") unless 2 <= $base <= 36;
        my int $b = nqp::unbox_i($base.Int);
        nqp::p6box_s(nqp::base_I(self, $b)) ~ ($digits ?? '.' ~ '0' x $digits !! '');
    }

    # If self is Int, we assume mods are Ints also.  (div fails otherwise.)
    # If do-not-want, user should cast invocant to proper domain.
    method polymod(Int:D: *@mods) {
        my $more = self;
        my $inf = @mods.elems == Inf;
        fail X::OutOfRange.new(
          what => 'invocant to polymod', got => $more, range => "0..*"
        ) if $more < 0;
        gather {
            for @mods -> $mod {
                last if $inf and not $more;
                fail X::Numeric::DivideByZero.new(
                  using => 'polymod', numerator => $more
                ) unless $mod;
                take $more mod $mod;
                $more div= $mod;
            }
            take $more unless $inf;
        }
    }

    method expmod(Int:D: Int:D \base, Int:D \mod) {
        nqp::expmod_I(self, nqp::decont(base), nqp::decont(mod), Int);
    }
    method is-prime(Int:D: Int:D $tries = 100) returns Bool:D {
        nqp::p6bool(nqp::isprime_I(self, nqp::unbox_i($tries)));
    }

    method floor(Int:D:) { self }
    method ceiling(Int:D:) { self }
    proto method round(|) {*}
    multi method round(Int:D:) { self }
    multi method round(Int:D: Real(Cool) $scale) { (self / $scale + 1/2).floor * $scale }

    method lsb(Int:D:) {
        return Nil if self == 0;
        my $lsb = 0;
        my $x = self.abs;
        while $x +& 0xff == 0 { $lsb += 8; $x +>= 8; }
        while $x +& 0x01 == 0 { $lsb++; $x +>= 1; }
        $lsb;
    }

    method msb(Int:D:) {
        return Nil if self == 0;
        return 0 if self == -1;
        my $msb = 0;
        my $x = self;
        $x = ($x + 1) * -2 if $x < 0;   # handle negative conversions
        while $x > 0xff   { $msb += 8; $x +>= 8; }
        if    $x > 0x0f   { $msb += 4; $x +>= 4; }
        if    $x +& 0x8   { $msb += 3; }
        elsif $x +& 0x4   { $msb += 2; }
        elsif $x +& 0x2   { $msb += 1; }
        $msb;
    }

    method narrow(Int:D:) { self }

    my constant $?BITS = do {
        my int $a = 0x1ffffffff;
        nqp::iseq_i($a,8589934591) ?? 64 !! 32;
    }

    method Range(Int:U:) {
        given self {
            when int  { $?BITS == 64 ??  int64.Range !!  int32.Range }
            when uint { $?BITS == 64 ?? uint64.Range !! uint32.Range }

            when int64  { Range.new(-9223372036854775808, 9223372036854775807) }
            when int32  { Range.new(         -2147483648, 2147483647         ) }
            when int16  { Range.new(              -32768, 32767              ) }
            when int8   { Range.new(                -128, 127                ) }
            when int4   { Range.new(                  -8, 7                  ) }
            when int2   { Range.new(                  -2, 1                  ) }
            when int1   { Range.new(                  -1, 0                  ) }

            when uint64 { Range.new( 0, 18446744073709551615 ) }
            when uint32 { Range.new( 0, 4294967295           ) }
            when uint16 { Range.new( 0, 65535                ) }
            when uint8  { Range.new( 0, 255                  ) }
            when uint4  { Range.new( 0, 15                   ) }
            when uint2  { Range.new( 0, 3                    ) }
            when uint1  { Range.new( 0, 1                    ) }

            when Int    {  # smartmatch matches both UInt and Int
                .^name eq 'UInt'
                  ?? Range.new(    0, Inf )
                  !! Range.new( -Inf, Inf )
                }

            default {
                fail "Unknown integer type: {self.^name}";
            }
        }
    }
}

my class Pos is Int {
    multi method Bool(Pos:U:) { False };
    multi method Bool(Pos:D:) { True };
    multi method Int(Pos:D:)  { nqp::box_i(nqp::unbox_i(self),Int) }
}

multi sub prefix:<++>(Int:D $a is rw) {
    $a = nqp::add_I(nqp::decont($a), 1, Int);
}
multi sub prefix:<++>(int $a is rw) {
    $a = nqp::add_i($a, 1);
}
multi sub prefix:<-->(Int:D $a is rw) {
    $a = nqp::sub_I(nqp::decont($a), 1, Int);
}
multi sub prefix:<-->(int $a is rw) {
    $a = nqp::sub_i($a, 1);
}
multi sub postfix:<++>(Int:D $a is rw) {
    my \b = nqp::decont($a);
    $a = nqp::add_I(b, 1, Int);
    b
}
multi sub postfix:<++>(int $a is rw) {
    my int $b = $a;
    $a = nqp::add_i($b, 1);
    $b
}
multi sub postfix:<-->(Int:D $a is rw) {
    my \b = nqp::decont($a);
    $a = nqp::sub_I(b, 1, Int);
    b
}
multi sub postfix:<-->(int $a is rw) {
    my int $b = $a;
    $a = nqp::sub_i($b, 1);
    $b
}

multi sub prefix:<->(Int:D \a) returns Int {
    nqp::neg_I(nqp::decont(a), Int);
}
multi sub prefix:<->(int $a) returns int {
    nqp::neg_i($a)
}

multi sub abs(Int:D \a) returns Int:D {
    nqp::abs_I(nqp::decont(a), Int);
}
multi sub abs(int $a) returns int {
    nqp::abs_i($a)
}

multi sub infix:<+>(Int:D \a, Int:D \b) returns Int:D {
    nqp::add_I(nqp::decont(a), nqp::decont(b), Int);
}
multi sub infix:<+>(int $a, int $b) returns int {
    nqp::add_i($a, $b)
}

multi sub infix:<->(Int:D \a, Int:D \b) returns Int:D {
    nqp::sub_I(nqp::decont(a), nqp::decont(b), Int);
}
multi sub infix:<->(int $a, int $b) returns int {
    nqp::sub_i($a, $b)
}

multi sub infix:<*>(Int:D \a, Int:D \b) returns Int {
    nqp::mul_I(nqp::decont(a), nqp::decont(b), Int);
}
multi sub infix:<*>(int $a, int $b) returns int {
    nqp::mul_i($a, $b)
}

multi sub infix:<div>(Int:D \a, Int:D \b) {
    fail X::Numeric::DivideByZero.new(
      using => 'div', numerator => a,
    ) unless b;
    nqp::div_I(nqp::decont(a), nqp::decont(b), Int)
}
multi sub infix:<div>(int $a, int $b) returns int {
    # relies on opcode or hardware to detect division by 0
    nqp::div_i($a, $b)
}

multi sub infix:<%>(Int:D \a, Int:D \b) returns Int {
    fail X::Numeric::DivideByZero.new(
      using => 'infix:<%>', numerator => a
    ) unless b;
    nqp::mod_I(nqp::decont(a), nqp::decont(b), Int);
}
multi sub infix:<%>(int $a, int $b) returns int {
    # relies on opcode or hardware to detect division by 0
    nqp::mod_i($a, $b)
}

multi sub infix:<**>(Int:D \a, Int:D \b) {
    b >= 0 ?? nqp::pow_I(nqp::decont(a), nqp::decont(b), Num, Int)
           !! 1 / nqp::pow_I(nqp::decont(a), nqp::decont(-b), Num, Int)
}

multi sub infix:<lcm>(Int:D \a, Int:D \b) returns Int {
    nqp::lcm_I(nqp::decont(a), nqp::decont(b), Int);
}
multi sub infix:<lcm>(int $a, int $b) returns int {
    nqp::lcm_i($a, $b)
}

multi sub infix:<gcd>(Int:D \a, Int:D \b) returns Int {
    nqp::gcd_I(nqp::decont(a), nqp::decont(b), Int);
}
multi sub infix:<gcd>(int $a, int $b) returns int {
    nqp::gcd_i($a, $b)
}

multi sub infix:<===>(Int:D \a, Int:D \b) {
    a.WHAT =:= b.WHAT && nqp::p6bool(nqp::iseq_I(nqp::decont(a), nqp::decont(b)))
}
multi sub infix:<===>(int $a, int $b) {
    # hey, the optimizer is smart enough to figure that one out for us, no?
    $a == $b
}

multi sub infix:<==>(Int:D \a, Int:D \b) {
    nqp::p6bool(nqp::iseq_I(nqp::decont(a), nqp::decont(b)))
}
multi sub infix:<==>(int $a, int $b) {
    nqp::p6bool(nqp::iseq_i($a, $b))
}

multi sub infix:<!=>(int $a, int $b) {
    nqp::p6bool(nqp::isne_i($a, $b))
}

multi sub infix:«<»(Int:D \a, Int:D \b) {
    nqp::p6bool(nqp::islt_I(nqp::decont(a), nqp::decont(b)))
}
multi sub infix:«<»(int $a, int $b) {
    nqp::p6bool(nqp::islt_i($a, $b))
}

multi sub infix:«<=»(Int:D \a, Int:D \b) {
    nqp::p6bool(nqp::isle_I(nqp::decont(a), nqp::decont(b)))
}
multi sub infix:«<=»(int $a, int $b) {
    nqp::p6bool(nqp::isle_i($a, $b))
}

multi sub infix:«>»(Int:D \a, Int:D \b) {
    nqp::p6bool(nqp::isgt_I(nqp::decont(a), nqp::decont(b)))
}
multi sub infix:«>»(int $a, int $b) {
    nqp::p6bool(nqp::isgt_i($a, $b))
}

multi sub infix:«>=»(Int:D \a, Int:D \b) {
    nqp::p6bool(nqp::isge_I(nqp::decont(a), nqp::decont(b)))
}
multi sub infix:«>=»(int $a, int $b) {
    nqp::p6bool(nqp::isge_i($a, $b))
}

multi sub infix:<+|>(Int:D \a, Int:D \b) {
    nqp::bitor_I(nqp::decont(a), nqp::decont(b), Int)
}
multi sub infix:<+|>(int $a, int $b) {
    nqp::bitor_i($a, $b)
}

multi sub infix:<+&>(Int:D \a, Int:D \b) {
    nqp::bitand_I(nqp::decont(a), nqp::decont(b), Int)
}
multi sub infix:<+&>(int $a, int $b) {
    nqp::bitand_i($a, $b)
}

multi sub infix:<+^>(Int:D \a, Int:D \b) {
    nqp::bitxor_I(nqp::decont(a), nqp::decont(b), Int)
}
multi sub infix:<+^>(int $a, int $b) {
    nqp::bitxor_i($a, $b);
}

multi sub infix:«+<»(Int:D \a, Int:D \b) returns Int:D {
    nqp::bitshiftl_I(nqp::decont(a), nqp::unbox_i(b), Int)
}
multi sub infix:«+<»(int $a, int $b) {
    nqp::bitshiftl_i($a, $b);
}

multi sub infix:«+>»(Int:D \a, Int:D \b) returns Int:D {
    nqp::bitshiftr_I(nqp::decont(a), nqp::unbox_i(b), Int)
}
multi sub infix:«+>»(int $a, int $b) {
    nqp::bitshiftr_i($a, $b)
}

multi sub prefix:<+^>(Int:D \a) {
    nqp::bitneg_I(nqp::decont(a), Int);
}
multi sub prefix:<+^>(int $a) {
    nqp::bitneg_i($a);
}

proto sub chr($) is pure  {*}
multi sub chr(Int:D  \x) returns Str:D { x.chr     }
multi sub chr(Cool \x) returns Str:D { x.Int.chr }
multi sub chr(int $x) returns str {
    nqp::chr($x);
}

proto sub is-prime($, $?) is pure  {*}
multi sub is-prime(Int:D \i, Int:D $tries = 100) {
    nqp::p6bool(nqp::isprime_I(nqp::decont(i), nqp::unbox_i($tries)));
}
multi sub is-prime(\i, $tries = 100) {
    nqp::p6bool(nqp::isprime_I(nqp::decont(i.Int), nqp::unbox_i($tries.Int)));
}

proto sub expmod($, $, $) is pure  {*}
multi sub expmod(Int:D \base, Int:D \exp, Int:D \mod) {
    nqp::expmod_I(nqp::decont(base), nqp::decont(exp), nqp::decont(mod), Int);
}
multi sub expmod(\base, \exp, \mod) {
    nqp::expmod_I(nqp::decont(base.Int), nqp::decont(exp.Int), nqp::decont(mod.Int), Int);
}

proto sub lsb($) {*}
multi sub lsb(Int:D \i) { i.lsb }

proto sub msb($) {*}
multi sub msb(Int:D \i) { i.msb }

# vim: ft=perl6 expandtab sw=4
