my class X::Numeric::Real { ... };
my class Complex is Cool does Numeric {
    has num $.re;
    has num $.im;

    method !SET-SELF(Num() \re, Num() \im) {
        $!re = re;
        $!im = im;
        self
    }
    proto method new(|) {*}
    multi method new() { self.new: 0, 0 }
    multi method new(Real \re, Real \im) { nqp::create(self)!SET-SELF(re, im) }

    multi method WHICH(Complex:D: --> ValueObjAt:D) {
        nqp::box_s(
          nqp::concat(
            nqp::if(
              nqp::eqaddr(self.WHAT,Complex),
              'Complex|',
              nqp::concat(nqp::unbox_s(self.^name), '|')
            ),
            nqp::concat($!re, nqp::concat('|', $!im))
          ),
          ValueObjAt
        )
    }

    method reals(Complex:D:) {
        (self.re, self.im);
    }

    method isNaN(Complex:D:) {
        self.re.isNaN || self.im.isNaN;
    }

    method coerce-to-real(Complex:D: $exception-target) {
        $!im ≅ 0e0
          ?? $!re
          !! Failure.new(X::Numeric::Real.new(target => $exception-target, reason => "imaginary part not zero", source => self))
    }
    multi method Real(Complex:D:) { self.coerce-to-real(Real); }

    # should probably be eventually supplied by role Numeric
    method Num(Complex:D:) { self.coerce-to-real(Num).Num; }
    method Int(Complex:D:) { self.coerce-to-real(Int).Int; }
    method Rat(Complex:D: $epsilon?) {
        self.coerce-to-real(Rat).Rat( |($epsilon // Empty) );
    }
    method FatRat(Complex:D: $epsilon?) {
        self.coerce-to-real(FatRat).FatRat( |($epsilon // Empty) );
    }

    multi method Bool(Complex:D:) {
        $!re != 0e0 || $!im != 0e0;
    }

    method Complex() { self }
    multi method Str(Complex:D:) {
        nqp::concat(
          $!re,
          nqp::concat(
            nqp::if(nqp::iseq_i( # we could have negative zero, so stringify
              nqp::ord(my $im := nqp::p6box_s($!im)),45),'','+'),
            nqp::concat(
              $im,
              nqp::if(nqp::isnanorinf($!im),'\\i','i')
            )
          )
        )
    }

    multi method raku(Complex:D:) {
        '<' ~ self.Str ~ '>';
    }
    method conj(Complex:D:) {
        Complex.new($.re, -$.im);
    }

    method abs(Complex $x:) {
        nqp::p6box_n(nqp::sqrt_n(
            nqp::add_n(
                nqp::mul_n($!re, $!re),
                nqp::mul_n($!im, $!im),
            )
        ))
    }

    method polar() {
        $.abs, $!im.atan2($!re);
    }
    multi method log(Complex:D:) {
        my Num ($mag, $angle) = self.polar;
        Complex.new($mag.log, $angle);
    }
    method cis(Complex:D:) {
        self.cos + self.sin*Complex.new(0,1)
    }
    method sqrt(Complex:D:) {
        my Num $abs = self.abs;
        my Num $re = (($abs + self.re)/2).sqrt;
        my Num $im = (($abs - self.re)/2).sqrt;
        Complex.new($re, self.im < 0 ?? -$im !! $im);
    }

    multi method exp(Complex:D:) {
        my Num $mag = $!re.exp;
        Complex.new($mag * $!im.cos, $mag * $!im.sin);
    }

    method roots(Complex:D: Int() $n) {
        return NaN if $n < 1;
        return self if $n == 1;
        for $!re, $!im {
            return NaN if $_ eq 'Inf' || $_ eq '-Inf' || $_ eq 'NaN';
        }

        my ($mag, $angle) = self.polar;
        $mag **= 1e0 / $n;
        (^$n).map: { $mag.unpolar( ($angle + $_ * 2e0 * pi) / $n) };
    }

    method sin(Complex:D:) {
        $!re.sin * $!im.cosh + ($!re.cos * $!im.sinh)i;
    }

    method asin(Complex:D:) {
        (Complex.new(0e0, -1e0) * log((self)i + sqrt(1e0 - self * self)));
    }

    method cos(Complex:D:) {
        $!re.cos * $!im.cosh - ($!re.sin * $!im.sinh)i;
    }

    method acos(Complex:D:) {
        (pi / 2e0) - self.asin;
    }

    method tan(Complex:D:) {
        self.sin / self.cos;
    }

    method atan(Complex:D:) {
        ((log(1e0 - (self)i) - log(1e0 + (self)i))i / 2e0);
    }

    method sec(Complex:D:) {
        1e0 / self.cos;
    }

    method asec(Complex:D:) {
        (1e0 / self).acos;
    }

    method cosec(Complex:D:) {
        1e0 / self.sin;
    }

    method acosec(Complex:D:) {
        (1e0 / self).asin;
    }

    method cotan(Complex:D:) {
        self.cos / self.sin;
    }

    method acotan(Complex:D:) {
        (1e0 / self).atan;
    }

    method sinh(Complex:D:) {
        -((Complex.new(0e0, 1e0) * self).sin)i;
    }

    method asinh(Complex:D:) {
        (self + sqrt(1e0 + self * self)).log;
    }

    method cosh(Complex:D:) {
        (Complex.new(0e0, 1e0) * self).cos;
    }

    method acosh(Complex:D:) {
        (self + sqrt(self * self - 1e0)).log;
    }

    method tanh(Complex:D:) {
        -((Complex.new(0e0, 1e0) * self).tan)i;
    }

    method atanh(Complex:D:) {
        (((1e0 + self) / (1e0 - self)).log / 2e0);
    }

    method sech(Complex:D:) {
        1e0 / self.cosh;
    }

    method asech(Complex:D:) {
        (1e0 / self).acosh;
    }

    method cosech(Complex:D:) {
        1e0 / self.sinh;
    }

    method acosech(Complex:D:) {
        (1e0 / self).asinh;
    }

    method cotanh(Complex:D:) {
        1e0 / self.tanh;
    }

    method acotanh(Complex:D:) {
        (1e0 / self).atanh;
    }

    method floor(Complex:D:) {
        Complex.new( self.re.floor, self.im.floor );
    }

    method ceiling(Complex:D:) {
        Complex.new( self.re.ceiling, self.im.ceiling );
    }

    proto method round(|) {*}
    multi method round(Complex:D:) {
        Complex.new( self.re.round, self.im.round );
    }
    multi method round(Complex:D: Real() $scale) {
        Complex.new( self.re.round($scale), self.im.round($scale) );
    }

    method truncate(Complex:D:) {
        Complex.new( self.re.truncate, self.im.truncate );
    }

    method narrow(Complex:D:) {
        self == 0e0 ?? 0 !!
        $!re == 0e0 ?? self !!
        $!im / $!re ≅ 0e0
            ?? $!re.narrow
            !! self;
    }
}

multi sub prefix:<->(Complex:D \a --> Complex:D) {
    my $new := nqp::create(Complex);
    nqp::bindattr_n( $new, Complex, '$!re',
        nqp::neg_n(
            nqp::getattr_n(nqp::decont(a), Complex, '$!re')
        )
    );
    nqp::bindattr_n( $new, Complex, '$!im',
        nqp::neg_n(
            nqp::getattr_n(nqp::decont(a), Complex, '$!im')
        )
    );
    $new;
}

multi sub abs(Complex:D \a --> Num:D) {
    my num $re = nqp::getattr_n(nqp::decont(a), Complex, '$!re');
    my num $im = nqp::getattr_n(nqp::decont(a), Complex, '$!im');
    nqp::p6box_n(nqp::sqrt_n(nqp::add_n(nqp::mul_n($re, $re), nqp::mul_n($im, $im))));
}

multi sub infix:<+>(Complex:D \a, Complex:D \b --> Complex:D) {
    my $new := nqp::create(Complex);
    nqp::bindattr_n( $new, Complex, '$!re',
        nqp::add_n(
            nqp::getattr_n(nqp::decont(a), Complex, '$!re'),
            nqp::getattr_n(nqp::decont(b), Complex, '$!re'),
        )
    );
    nqp::bindattr_n( $new, Complex, '$!im',
        nqp::add_n(
            nqp::getattr_n(nqp::decont(a), Complex, '$!im'),
            nqp::getattr_n(nqp::decont(b), Complex, '$!im'),
        )
    );
    $new;
}

multi sub infix:<+>(Complex:D \a, Num(Real) \b --> Complex:D) {
    my $new := nqp::create(Complex);
    nqp::bindattr_n( $new, Complex, '$!re',
        nqp::add_n(
            nqp::getattr_n(nqp::decont(a), Complex, '$!re'),
            nqp::unbox_n(b)
        )
    );
    nqp::bindattr_n($new, Complex, '$!im',
        nqp::getattr_n(nqp::decont(a), Complex, '$!im'),
    );
    $new
}

multi sub infix:<+>(Num(Real) \a, Complex:D \b --> Complex:D) {
    my $new := nqp::create(Complex);
    nqp::bindattr_n($new, Complex, '$!re',
        nqp::add_n(
            nqp::unbox_n(a),
            nqp::getattr_n(nqp::decont(b), Complex, '$!re'),
        )
    );
    nqp::bindattr_n($new, Complex, '$!im',
        nqp::getattr_n(nqp::decont(b), Complex, '$!im'),
    );
    $new;
}

multi sub infix:<->(Complex:D \a, Complex:D \b --> Complex:D) {
    my $new := nqp::create(Complex);
    nqp::bindattr_n( $new, Complex, '$!re',
        nqp::sub_n(
            nqp::getattr_n(nqp::decont(a), Complex, '$!re'),
            nqp::getattr_n(nqp::decont(b), Complex, '$!re'),
        )
    );
    nqp::bindattr_n($new, Complex, '$!im',
        nqp::sub_n(
            nqp::getattr_n(nqp::decont(a), Complex, '$!im'),
            nqp::getattr_n(nqp::decont(b), Complex, '$!im'),
        )
    );
    $new
}

multi sub infix:<->(Complex:D \a, Num(Real) \b --> Complex:D) {
    my $new := nqp::create(Complex);
    nqp::bindattr_n( $new, Complex, '$!re',
        nqp::sub_n(
            nqp::getattr_n(nqp::decont(a), Complex, '$!re'),
            b,
        )
    );
    nqp::bindattr_n($new, Complex, '$!im',
        nqp::getattr_n(nqp::decont(a), Complex, '$!im')
    );
    $new
}

multi sub infix:<->(Num(Real) \a, Complex:D \b --> Complex:D) {
    my $new := nqp::create(Complex);
    nqp::bindattr_n( $new, Complex, '$!re',
        nqp::sub_n(
            a,
            nqp::getattr_n(nqp::decont(b), Complex, '$!re'),
        )
    );
    nqp::bindattr_n($new, Complex, '$!im',
        nqp::neg_n(
            nqp::getattr_n(nqp::decont(b), Complex, '$!im')
        )
    );
    $new
}

multi sub infix:<*>(Complex:D \a, Complex:D \b --> Complex:D) {
    my num $a_re = nqp::getattr_n(nqp::decont(a), Complex, '$!re');
    my num $a_im = nqp::getattr_n(nqp::decont(a), Complex, '$!im');
    my num $b_re = nqp::getattr_n(nqp::decont(b), Complex, '$!re');
    my num $b_im = nqp::getattr_n(nqp::decont(b), Complex, '$!im');
    my $new := nqp::create(Complex);
    nqp::bindattr_n($new, Complex, '$!re',
        nqp::sub_n(nqp::mul_n($a_re, $b_re), nqp::mul_n($a_im, $b_im)),
    );
    nqp::bindattr_n($new, Complex, '$!im',
        nqp::add_n(nqp::mul_n($a_re, $b_im), nqp::mul_n($a_im, $b_re)),
    );
    $new;
}

multi sub infix:<*>(Complex:D \a, Num(Real) \b --> Complex:D) {
    my $new := nqp::create(Complex);
    my num $b_num = b;
    nqp::bindattr_n($new, Complex, '$!re',
        nqp::mul_n(
            nqp::getattr_n(nqp::decont(a), Complex, '$!re'),
            $b_num,
        )
    );
    nqp::bindattr_n($new, Complex, '$!im',
        nqp::mul_n(
            nqp::getattr_n(nqp::decont(a), Complex, '$!im'),
            $b_num,
        )
    );
    $new
}

multi sub infix:<*>(Num(Real) \a, Complex:D \b --> Complex:D) {
    my $new := nqp::create(Complex);
    my num $a_num = a;
    nqp::bindattr_n($new, Complex, '$!re',
        nqp::mul_n(
            $a_num,
            nqp::getattr_n(nqp::decont(b), Complex, '$!re'),
        )
    );
    nqp::bindattr_n($new, Complex, '$!im',
        nqp::mul_n(
            $a_num,
            nqp::getattr_n(nqp::decont(b), Complex, '$!im'),
        )
    );
    $new
}

multi sub infix:</>(Complex:D \a, Complex:D \b --> Complex:D) {
    my num $a_re = nqp::getattr_n(nqp::decont(a), Complex, '$!re');
    my num $a_im = nqp::getattr_n(nqp::decont(a), Complex, '$!im');
    my num $b_re = nqp::getattr_n(nqp::decont(b), Complex, '$!re');
    my num $b_im = nqp::getattr_n(nqp::decont(b), Complex, '$!im');
    my num $d    = nqp::add_n(nqp::mul_n($b_re, $b_re), nqp::mul_n($b_im, $b_im));
    my $new := nqp::create(Complex);
    nqp::bindattr_n($new, Complex, '$!re',
        nqp::div_n(
            nqp::add_n(nqp::mul_n($a_re, $b_re), nqp::mul_n($a_im, $b_im)),
            $d,
        )
    );
    nqp::bindattr_n($new, Complex, '$!im',
        nqp::div_n(
            nqp::sub_n(nqp::mul_n($a_im, $b_re), nqp::mul_n($a_re, $b_im)),
            $d,
        )
    );
    $new;
}

multi sub infix:</>(Complex:D \a, Real \b --> Complex:D) {
    Complex.new(a.re / b, a.im / b);
}

multi sub infix:</>(Real \a, Complex:D \b --> Complex:D) {
    Complex.new(a, 0e0) / b;
}

multi sub infix:<**>(Complex:D \a, Complex:D \b --> Complex:D) {
    (a.re == 0e0 && a.im == 0e0)
        ?? ( b.re == 0e0 && b.im == 0e0
                ?? Complex.new(1e0, 0e0)
                !! Complex.new(0e0, 0e0)
           )
        !! (b * a.log).exp
}
multi sub infix:<**>(Num(Real) \a, Complex:D \b --> Complex:D) {
    a == 0e0
        ?? ( b.re == 0e0 && b.im == 0e0
                ?? Complex.new(1e0, 0e0)
                !! Complex.new(0e0, 0e0)
           )
        !! (b * a.log).exp
}
multi sub infix:<**>(Complex:D \a, Num(Real) \b --> Complex:D) {
    b.isNaN || b == Inf || b == -Inf
      ?? Complex.new(NaN, NaN)
      !! (my $ib := b.Int) == b
        ?? a ** $ib
        !! (my $fb2 := b - $ib * 2) == 1e0
          ?? a ** $ib * a.sqrt
          !! $fb2 == -1e0
            ?? a ** $ib / a.sqrt
            !! (b * a.log).exp
}
multi sub infix:<**>(Complex:D \a, Int:D \b --> Complex:D) {
    my $r := Complex.new(1e0, 0e0);
    nqp::if(
      b == 0,
      $r,
      nqp::if(
        a == $r || b == 1,
        a,
        nqp::stmts(
          (my $u := b.abs),
          (my $t := a),
          nqp::while(
            $u  > 0,
            nqp::stmts(
              nqp::if(
                $u +& 1 == 1,
                $r := $r * $t
              ),
              ($u := $u +> 1),
              ($t := $t * $t)
            )
          ),
          nqp::if(
            b < 0,
            1e0 / $r,
            $r
          )
        )
      )
    )
}

multi sub infix:<==>(Complex:D \a, Complex:D \b --> Bool:D) { a.re == b.re && a.im == b.im }
multi sub infix:<==>(Complex:D \a, Num(Real) \b --> Bool:D) { a.re == b    && a.im == 0e0  }
multi sub infix:<==>(Num(Real) \a, Complex:D \b --> Bool:D) { a    == b.re && 0e0  == b.im }
multi sub infix:<===>(Complex:D \a, Complex:D \b --> Bool:D) {
    a.WHAT =:= b.WHAT && a.re === b.re && a.im === b.im
}

multi sub infix:<≅>(Complex:D \a, Complex:D \b --> Bool:D) { a.re ≅ b.re && a.im ≅ b.im || a <=> b =:= Same }
multi sub infix:<≅>(Complex:D \a, Num(Real) \b --> Bool:D) { a ≅ b.Complex }
multi sub infix:<≅>(Num(Real) \a, Complex:D \b --> Bool:D) { a.Complex ≅ b }

# Meaningful only for sorting purposes, of course.
# We delegate to Real::cmp rather than <=> because parts might be NaN.
multi sub infix:<cmp>(Complex:D \a, Complex:D \b) {
    nqp::eqaddr((my $cmp := a.re cmp b.re),Order::Same)
      ?? a.im cmp b.im
      !! $cmp
}
multi sub infix:<cmp>(Num(Real) \a, Complex:D \b) {
    nqp::eqaddr((my $cmp := a cmp b.re),Order::Same)
      ?? 0 cmp b.im
      !! $cmp
}
multi sub infix:<cmp>(Complex:D \a, Num(Real) \b) {
    nqp::eqaddr((my $cmp := a.re cmp b),Order::Same)
      ?? a.im cmp 0
      !! $cmp
}

multi sub infix:«<=>»(Complex:D \a, Complex:D \b) {
    my $tolerance = a && b
        ?? (a.re.abs + b.re.abs) / 2 * $*TOLERANCE  # Scale slop to average real parts.
        !! $*TOLERANCE;                             # Don't want tolerance 0 if either arg is 0.
    # Fail unless imaginary parts are relatively negligible, compared to real parts.
    infix:<≅>(a.im, 0e0, :$tolerance) && infix:<≅>(b.im, 0e0, :$tolerance)
      ?? a.re <=> b.re
      !! Failure.new(X::Numeric::Real.new(target => Real, reason => "Complex is not numerically orderable", source => "Complex"))
}
multi sub infix:«<=>»(Num(Real) \a, Complex:D \b) { a.Complex <=> b }
multi sub infix:«<=>»(Complex:D \a, Num(Real) \b) { a <=> b.Complex }

proto sub postfix:<i>($, *%        --> Complex:D) is pure {*}
multi sub postfix:<i>(Real      \a --> Complex:D) { Complex.new(0e0, a);     }
multi sub postfix:<i>(Complex:D \a --> Complex:D) { Complex.new(-a.im, a.re) }
multi sub postfix:<i>(Numeric   \a --> Complex:D) { a * Complex.new(0e0, 1e0) }
multi sub postfix:<i>(Cool      \a --> Complex:D) { a.Numeric * Complex.new(0e0, 1e0) }

constant i = Complex.new(0e0, 1e0);

# vim: expandtab shiftwidth=4
