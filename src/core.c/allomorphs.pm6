# the uses of add_I in this class are a trick to make bigints work right
my class IntStr is Int is Str {
    method new(Int:D $i, Str:D $s) {
        my \SELF = nqp::add_I($i, 0, self);
        nqp::bindattr_s(SELF, Str, '$!value', $s);
        SELF;
    }
    multi method ACCEPTS(IntStr:D: Any:D \a) {
        nqp::if(
          nqp::istype(a, Numeric),
            self.Int.ACCEPTS(a),
          nqp::if(
            nqp::istype(a, Str),
            self.Str.ACCEPTS(a),
            self.Str.ACCEPTS(a) && self.Int.ACCEPTS(a)))
    }
    multi method Numeric(IntStr:D:) { self.Int }
    multi method Numeric(IntStr:U:) {
        self.Mu::Numeric; # issue warning;
        0
    }
    multi method Real(IntStr:D:) { self.Int }
    multi method Real(IntStr:U:) {
        self.Mu::Real; # issue warning;
        0
    }
    method Int(IntStr:D:) { nqp::add_I(self, 0, Int) }
    multi method Str(IntStr:D:) { nqp::getattr_s(self, Str, '$!value') }

    multi method perl(IntStr:D:) { self.^name ~ '.new(' ~ self.Int.raku ~ ', ' ~ self.Str.raku ~ ')' }
}

my class NumStr is Num is Str {
    method new(Num $n, Str $s) {
        my \SELF = nqp::create(self);
        nqp::bindattr_n(SELF, Num, '$!value', $n);
        nqp::bindattr_s(SELF, Str, '$!value', $s);
        SELF;
    }
    multi method ACCEPTS(NumStr:D: Any:D \a) {
        nqp::if(
          nqp::istype(a, Numeric),
          self.Num.ACCEPTS(a),
          nqp::if(
            nqp::istype(a, Str),
            self.Str.ACCEPTS(a),
            self.Str.ACCEPTS(a) && self.Num.ACCEPTS(a)))
    }
    multi method Numeric(NumStr:D:) { self.Num }
    multi method Numeric(NumStr:U:) {
        self.Mu::Numeric; # issue warning;
        0e0
    }
    multi method Real(NumStr:D:) { self.Num }
    multi method Real(NumStr:U:) {
        self.Mu::Real; # issue warning;
        0e0
    }
    method Num(NumStr:D:) { nqp::getattr_n(self, Num, '$!value') }
    multi method Str(NumStr:D:) { nqp::getattr_s(self, Str, '$!value') }

    multi method perl(NumStr:D:) { self.^name ~ '.new(' ~ self.Num.raku ~ ', ' ~ self.Str.raku ~ ')' }
}

my class RatStr is Rat is Str {
    method new(Rat $r, Str $s) {
        my \SELF = nqp::create(self);
        nqp::bindattr(SELF, Rat, '$!numerator', $r.numerator);
        nqp::bindattr(SELF, Rat, '$!denominator', $r.denominator);
        nqp::bindattr_s(SELF, Str, '$!value', $s);
        SELF;
    }
    multi method ACCEPTS(RatStr:D: Any:D \a) {
        nqp::if(
          nqp::istype(a, Numeric),
          self.Rat.ACCEPTS(a),
          nqp::if(
            nqp::istype(a, Str),
            self.Str.ACCEPTS(a),
            self.Str.ACCEPTS(a) && self.Rat.ACCEPTS(a)))
    }
    method succ(RatStr:D: --> Rat:D) {
        my \denominator := nqp::getattr(self,Rat,'$!denominator');
        nqp::p6bindattrinvres(
          nqp::p6bindattrinvres(nqp::create(Rat),Rat,'$!numerator',
            nqp::add_I(nqp::getattr(self,Rat,'$!numerator'),denominator,Int)
          ),
          Rat, '$!denominator', denominator
        )
    }
    method pred(RatStr:D: --> Rat:D) {
        my \denominator := nqp::getattr(self,Rat,'$!denominator');
        nqp::p6bindattrinvres(
          nqp::p6bindattrinvres(nqp::create(Rat), Rat, '$!numerator',
            nqp::sub_I(nqp::getattr(self,Rat,'$!numerator'),denominator,Int)
          ),
          Rat, '$!denominator', denominator
        )
    }
    method Capture(RatStr:D:) { self.Mu::Capture }
    multi method Numeric(RatStr:D:) { self.Rat }
    multi method Numeric(RatStr:U:) {
        self.Mu::Numeric; # issue warning;
        0.0
    }
    multi method Real(RatStr:D:) { self.Rat }
    multi method Real(RatStr:U:) {
        self.Mu::Real; # issue warning;
        0.0
    }
    method Rat(RatStr:D:) {
        Rat.new(
          nqp::getattr(self, Rat, '$!numerator'),
          nqp::getattr(self, Rat, '$!denominator')
        )
    }
    multi method Str(RatStr:D:) { nqp::getattr_s(self, Str, '$!value') }

    multi method perl(RatStr:D:) { self.^name ~ '.new(' ~ self.Rat.raku ~ ', ' ~ self.Str.raku ~ ')' }
}

my class ComplexStr is Complex is Str {
    method new(Complex $c, Str $s) {
        my \SELF = nqp::create(self);
        nqp::bindattr_n(SELF, Complex, '$!re', $c.re);
        nqp::bindattr_n(SELF, Complex, '$!im', $c.im);
        nqp::bindattr_s(SELF, Str, '$!value', $s);
        SELF;
    }
    multi method ACCEPTS(ComplexStr:D: Any:D \a) {
        nqp::if(
          nqp::istype(a, Numeric),
          self.Complex.ACCEPTS(a),
          nqp::if(
            nqp::istype(a, Str),
            self.Str.ACCEPTS(a),
            self.Str.ACCEPTS(a) && self.Complex.ACCEPTS(a)))
    }
    method Capture(ComplexStr:D:) { self.Mu::Capture }
    multi method Numeric(ComplexStr:D:) { self.Complex }
    multi method Numeric(ComplexStr:U:) {
        self.Mu::Numeric; # issue warning;
        <0+0i>
    }
    multi method Real(ComplexStr:D:) { self.Complex.Real }
    multi method Real(ComplexStr:U:) {
        self.Mu::Real; # issue warning;
        <0+0i>.Real
    }
    method Complex(ComplexStr:D:) { Complex.new(nqp::getattr_n(self, Complex, '$!re'), nqp::getattr_n(self, Complex, '$!im')) }
    multi method Str(ComplexStr:D:) { nqp::getattr_s(self, Str, '$!value') }

    multi method perl(ComplexStr:D:) { self.^name ~ '.new(' ~ self.Complex.raku ~ ', ' ~ self.Str.raku ~ ')' }
}

# we define cmp ops for these allomorphic types as numeric first, then Str. If
# you want just one half of the cmp, you'll need to coerce the args
multi sub infix:<cmp>(IntStr:D     $a, IntStr:D     $b) { $a.Int     cmp $b.Int     || $a.Str cmp $b.Str }
multi sub infix:<cmp>(IntStr:D     $a, RatStr:D     $b) { $a.Int     cmp $b.Rat     || $a.Str cmp $b.Str }
multi sub infix:<cmp>(IntStr:D     $a, NumStr:D     $b) { $a.Int     cmp $b.Num     || $a.Str cmp $b.Str }
multi sub infix:<cmp>(IntStr:D     $a, ComplexStr:D $b) { $a.Int     cmp $b.Complex || $a.Str cmp $b.Str }

multi sub infix:<cmp>(RatStr:D     $a, IntStr:D     $b) { $a.Rat     cmp $b.Int     || $a.Str cmp $b.Str }
multi sub infix:<cmp>(RatStr:D     $a, RatStr:D     $b) { $a.Rat     cmp $b.Rat     || $a.Str cmp $b.Str }
multi sub infix:<cmp>(RatStr:D     $a, NumStr:D     $b) { $a.Rat     cmp $b.Num     || $a.Str cmp $b.Str }
multi sub infix:<cmp>(RatStr:D     $a, ComplexStr:D $b) { $a.Rat     cmp $b.Complex || $a.Str cmp $b.Str }

multi sub infix:<cmp>(NumStr:D     $a, IntStr:D     $b) { $a.Num     cmp $b.Int     || $a.Str cmp $b.Str }
multi sub infix:<cmp>(NumStr:D     $a, RatStr:D     $b) { $a.Num     cmp $b.Rat     || $a.Str cmp $b.Str }
multi sub infix:<cmp>(NumStr:D     $a, NumStr:D     $b) { $a.Num     cmp $b.Num     || $a.Str cmp $b.Str }
multi sub infix:<cmp>(NumStr:D     $a, ComplexStr:D $b) { $a.Num     cmp $b.Complex || $a.Str cmp $b.Str }

multi sub infix:<cmp>(ComplexStr:D $a, IntStr:D     $b) { $a.Complex cmp $b.Int     || $a.Str cmp $b.Str }
multi sub infix:<cmp>(ComplexStr:D $a, RatStr:D     $b) { $a.Complex cmp $b.Rat     || $a.Str cmp $b.Str }
multi sub infix:<cmp>(ComplexStr:D $a, NumStr:D     $b) { $a.Complex cmp $b.Num     || $a.Str cmp $b.Str }
multi sub infix:<cmp>(ComplexStr:D $a, ComplexStr:D $b) { $a.Complex cmp $b.Complex || $a.Str cmp $b.Str }


multi sub infix:<eqv>(IntStr:D     $a, IntStr:D     $b) { $a.Int     eqv $b.Int     && $a.Str eqv $b.Str }
multi sub infix:<eqv>(IntStr:D     $a, RatStr:D     $b --> False) {}
multi sub infix:<eqv>(IntStr:D     $a, NumStr:D     $b --> False) {}
multi sub infix:<eqv>(IntStr:D     $a, ComplexStr:D $b --> False) {}

multi sub infix:<eqv>(RatStr:D     $a, IntStr:D     $b --> False) {}
multi sub infix:<eqv>(RatStr:D     $a, RatStr:D     $b) { $a.Rat     eqv $b.Rat     && $a.Str eqv $b.Str }
multi sub infix:<eqv>(RatStr:D     $a, NumStr:D     $b --> False) {}
multi sub infix:<eqv>(RatStr:D     $a, ComplexStr:D $b --> False) {}

multi sub infix:<eqv>(NumStr:D     $a, IntStr:D     $b --> False) {}
multi sub infix:<eqv>(NumStr:D     $a, RatStr:D     $b --> False) {}
multi sub infix:<eqv>(NumStr:D     $a, NumStr:D     $b) { $a.Num     eqv $b.Num     && $a.Str eqv $b.Str }
multi sub infix:<eqv>(NumStr:D     $a, ComplexStr:D $b --> False) {}

multi sub infix:<eqv>(ComplexStr:D $a, IntStr:D     $b --> False) {}
multi sub infix:<eqv>(ComplexStr:D $a, RatStr:D     $b --> False) {}
multi sub infix:<eqv>(ComplexStr:D $a, NumStr:D     $b --> False) {}
multi sub infix:<eqv>(ComplexStr:D $a, ComplexStr:D $b) { $a.Complex eqv $b.Complex && $a.Str eqv $b.Str }

multi sub infix:<===>(IntStr:D $a, IntStr:D $b) {
    $a.Int === $b.Int && $a.Str === $b.Str
}
multi sub infix:<===>(RatStr:D $a, RatStr:D $b) {
    $a.Rat === $b.Rat && $a.Str === $b.Str
}
multi sub infix:<===>(NumStr:D $a, NumStr:D $b) {
    $a.Num === $b.Num && $a.Str === $b.Str
}
multi sub infix:<===>(ComplexStr:D $a, ComplexStr:D $b) {
    $a.Complex === $b.Complex && $a.Str === $b.Str
}

multi sub val(*@maybevals) {
    @maybevals.list.map({ val($_) }).eager;
}

multi sub val(Mu \mu) {
    warn "{ mu.raku } uselessly passed to val()";
    mu
}

multi sub val(Slip:D \maybevals) { val(|maybevals).Slip }
multi sub val(List:D \maybevals) { val(|maybevals)      }

multi sub val(Pair:D \ww-thing) is raw {
    # this is a Pair object possible in «» constructs; just pass it through. We
    # capture this specially from the below sub to avoid emitting a warning
    # whenever an affected «» construct is being processed.
    ww-thing
}

multi sub val(\one-thing) is raw {
    warn "Value of type { one-thing.^name } uselessly passed to val()";
    one-thing
}

multi sub val(Str:D $expression) { $expression.val }

# vim: ft=perl6 expandtab sw=4
