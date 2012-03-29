my class Complex is Cool does Numeric {
    has num $.re;
    has num $.im;

    proto method new(|$) { * }
    multi method new(Real \$re, Real \$im) {
        my $new = nqp::create(self);
        $new.BUILD($re.Num, $im.Num);
        $new;
    }
    method BUILD(Num \$re, Num \$im) {
        $!re = $re;
        $!im = $im;
    }
    method reals(Complex:D:) {
        (self.re, self.im);
    }

    method isNaN(Complex:D:) {
        self.re.isNaN || self.im.isNaN;
    }

    multi method Real(Complex:D:) {
        if $!im == 0 {
            $!re;
        } else {
            fail "You can only coerce a Complex to Real if the imaginary part is zero"
        }
    }

    # should probably be eventually supplied by role Numeric
    method Num(Complex:D:) { self.Real.Num }
    method Int(Complex:D:) { self.Real.Int }
    method Rat(Complex:D:) { self.Real.Rat }

    multi method Bool(Complex:D:) {
        $!re != 0e0 || $!im != 0e0;
    }

    method Complex() { self }
    multi method Str(Complex:D:) {
        my Str $i = nqp::isnanorinf($!im) ?? '\\i' !! 'i';
        $!im < 0e0
            ?? nqp::p6box_s($!re) ~ '-' ~ nqp::p6box_s(nqp::abs_n($!im)) ~ $i
            !! nqp::p6box_s($!re) ~ '+' ~ nqp::p6box_s($!im) ~ $i;
    }

    multi method perl(Complex:D:) {
        "Complex.new($.re, $.im)";
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

    method sqrt(Complex:D:) {
        my Num ($mag, $angle) = self.polar;
        $mag.sqrt.unpolar($angle/2);
    }

    multi method exp(Complex:D:) {
        my Num $mag = $!re.exp;
        Complex.new($mag * $!im.cos, $mag * $!im.sin);
    }

    method roots(Complex:D: $an) {
        my Int $n = $an.Int;
        return $NaN if $n < 1;
        return self if $n == 1;
        for $!re, $!im {
            return $NaN if $_ eq 'Inf' || $_ eq '-Inf' || $_ eq 'NaN';
        }

        my ($mag, $angle) = self.polar;
        $mag **= 1e0 / $n;
        (^$n).map: { $mag.unpolar( ($angle + $_ * 2e0 * pi) / $n) };
    }
    
    method sin(Complex:D:) {
        $!re.sin * $!im.cosh + ($!re.cos * $!im.sinh)i;
    }
    
    method asin(Complex:D:) {
        (Complex.new(0, -1) * log((self)i + sqrt(1 - self * self)));
    }

    method cos(Complex:D:) {
        $!re.cos * $!im.cosh - ($!re.sin * $!im.sinh)i;
    }
    
    method acos(Complex:D:) {
        (pi / 2) - self.asin;
    }
    
    method tan(Complex:D:) {
        self.sin / self.cos;
    }
    
    method atan(Complex:D:) {
        ((log(1 - (self)i) - log(1 + (self)i))i / 2);
    }

    method sec(Complex:D:) {
        1 / self.cos;
    }

    method asec(Complex:D:) {
        (1 / self).acos;
    }

    method cosec(Complex:D:) {
        1 / self.sin;
    }

    method acosec(Complex:D:) {
        (1 / self).asin;
    }

    method cotan(Complex:D:) {
        self.cos / self.sin;
    }

    method acotan(Complex:D:) {
        (1 / self).atan;
    }

    method sinh(Complex:D:) {
        -((Complex.new(0, 1) * self).sin)i;
    }
    
    method asinh(Complex:D:) {
        (self + sqrt(1 + self * self)).log;
    }
    
    method cosh(Complex:D:) {
        (Complex.new(0, 1) * self).cos;
    }
    
    method acosh(Complex:D:) {
        (self + sqrt(self * self - 1)).log;
    }
    
    method tanh(Complex:D:) {
        -((Complex.new(0, 1) * self).tan)i;
    }
    
    method atanh(Complex:D:) {
        (((1 + self) / (1 - self)).log / 2);
    }
    
    method sech(Complex:D:) {
        1 / self.cosh;
    }
    
    method asech(Complex:D:) {
        (1 / self).acosh;
    }
    
    method cosech(Complex:D:) {
        1 / self.sinh;
    }
    
    method acosech(Complex:D:) {
        (1 / self).asinh;
    }
    
    method cotanh(Complex:D:) {
        1 / self.tanh;
    }
    
    method acotanh(Complex:D:) {
        (1 / self).atanh;
    }
}

multi sub prefix:<->(Complex:D \$a) returns Complex:D {
    my $new := nqp::create(Complex);
    nqp::bindattr_n( $new, Complex, '$!re',
        nqp::neg_n(
            nqp::getattr_n(nqp::p6decont($a), Complex, '$!re')
        )
    );
    nqp::bindattr_n( $new, Complex, '$!im',
        nqp::neg_n(
            nqp::getattr_n(nqp::p6decont($a), Complex, '$!im')
        )
    );
    $new;
}

multi sub prefix:<abs>(Complex:D \$a) returns Num:D {
    my num $re = nqp::getattr_n(nqp::p6decont($a), Complex, '$!re');
    my num $im = nqp::getattr_n(nqp::p6decont($a), Complex, '$!im');
    nqp::p6box_n(pir::sqrt__NN(nqp::add_n(nqp::mul_n($re, $re), nqp::mul_n($im, $im))));
}

multi sub infix:<+>(Complex:D \$a, Complex:D \$b) returns Complex:D {
    my $new := nqp::create(Complex);
    nqp::bindattr_n( $new, Complex, '$!re',
        nqp::add_n(
            nqp::getattr_n(nqp::p6decont($a), Complex, '$!re'),
            nqp::getattr_n(nqp::p6decont($b), Complex, '$!re'),
        )
    );
    nqp::bindattr_n( $new, Complex, '$!im',
        nqp::add_n(
            nqp::getattr_n(nqp::p6decont($a), Complex, '$!im'),
            nqp::getattr_n(nqp::p6decont($b), Complex, '$!im'),
        )
    );
    $new;
}

multi sub infix:<+>(Complex:D \$a, Real \$b) returns Complex:D {
    my $new := nqp::create(Complex);
    nqp::bindattr_n( $new, Complex, '$!re',
        nqp::add_n(
            nqp::getattr_n(nqp::p6decont($a), Complex, '$!re'),
            nqp::unbox_n($b.Num)
        )
    );
    nqp::bindattr_n($new, Complex, '$!im',
        nqp::getattr_n(nqp::p6decont($a), Complex, '$!im'),
    );
    $new
}

multi sub infix:<+>(Real \$a, Complex:D \$b) returns Complex:D {
    my $new := nqp::create(Complex);
    nqp::bindattr_n($new, Complex, '$!re',
        nqp::add_n(
            nqp::unbox_n($a.Num),
            nqp::getattr_n(nqp::p6decont($b), Complex, '$!re'),
        )
    );
    nqp::bindattr_n($new, Complex, '$!im',
        nqp::getattr_n(nqp::p6decont($b), Complex, '$!im'),
    );
    $new;
}

multi sub infix:<->(Complex:D \$a, Complex:D \$b) returns Complex:D {
    my $new := nqp::create(Complex);
    nqp::bindattr_n( $new, Complex, '$!re',
        nqp::sub_n(
            nqp::getattr_n(nqp::p6decont($a), Complex, '$!re'),
            nqp::getattr_n(nqp::p6decont($b), Complex, '$!re'),
        )
    );
    nqp::bindattr_n($new, Complex, '$!im',
        nqp::sub_n(
            nqp::getattr_n(nqp::p6decont($a), Complex, '$!im'),
            nqp::getattr_n(nqp::p6decont($b), Complex, '$!im'),
        )
    );
    $new
}

multi sub infix:<->(Complex:D \$a, Real \$b) returns Complex:D {
    my $new := nqp::create(Complex);
    nqp::bindattr_n( $new, Complex, '$!re',
        nqp::sub_n(
            nqp::getattr_n(nqp::p6decont($a), Complex, '$!re'),
            $b.Num,
        )
    );
    nqp::bindattr_n($new, Complex, '$!im',
        nqp::getattr_n(nqp::p6decont($a), Complex, '$!im')
    );
    $new
}

multi sub infix:<->(Real \$a, Complex:D \$b) returns Complex:D {
    my $new := nqp::create(Complex);
    nqp::bindattr_n( $new, Complex, '$!re',
        nqp::sub_n(
            $a.Num,
            nqp::getattr_n(nqp::p6decont($b), Complex, '$!re'),
        )
    );
    nqp::bindattr_n($new, Complex, '$!im',
        nqp::neg_n(
            nqp::getattr_n(nqp::p6decont($b), Complex, '$!im')
        )
    );
    $new
}

multi sub infix:<*>(Complex:D \$a, Complex:D \$b) returns Complex:D {
    my num $a_re = nqp::getattr_n(nqp::p6decont($a), Complex, '$!re');
    my num $a_im = nqp::getattr_n(nqp::p6decont($a), Complex, '$!im');
    my num $b_re = nqp::getattr_n(nqp::p6decont($b), Complex, '$!re');
    my num $b_im = nqp::getattr_n(nqp::p6decont($b), Complex, '$!im');
    my $new := nqp::create(Complex);
    nqp::bindattr_n($new, Complex, '$!re',
        nqp::sub_n(nqp::mul_n($a_re, $b_re), nqp::mul_n($a_im, $b_im)),
    );
    nqp::bindattr_n($new, Complex, '$!im',
        nqp::add_n(nqp::mul_n($a_re, $b_im), nqp::mul_n($a_im, $b_re)),
    );
    $new;
}

multi sub infix:<*>(Complex:D \$a, Real \$b) returns Complex:D {
    my $new := nqp::create(Complex);
    my num $b_num = $b.Num;
    nqp::bindattr_n($new, Complex, '$!re',
        nqp::mul_n(
            nqp::getattr_n(nqp::p6decont($a), Complex, '$!re'),
            $b_num,
        )
    );
    nqp::bindattr_n($new, Complex, '$!im',
        nqp::mul_n(
            nqp::getattr_n(nqp::p6decont($a), Complex, '$!im'),
            $b_num,
        )
    );
    $new
}

multi sub infix:<*>(Real \$a, Complex:D \$b) returns Complex:D {
    my $new := nqp::create(Complex);
    my num $a_num = $a.Num;
    nqp::bindattr_n($new, Complex, '$!re',
        nqp::mul_n(
            $a_num,
            nqp::getattr_n(nqp::p6decont($b), Complex, '$!re'),
        )
    );
    nqp::bindattr_n($new, Complex, '$!im',
        nqp::mul_n(
            $a_num,
            nqp::getattr_n(nqp::p6decont($b), Complex, '$!im'),
        )
    );
    $new
}

multi sub infix:</>(Complex:D \$a, Complex:D \$b) returns Complex:D {
    my num $a_re = nqp::getattr_n(nqp::p6decont($a), Complex, '$!re');
    my num $a_im = nqp::getattr_n(nqp::p6decont($a), Complex, '$!im');
    my num $b_re = nqp::getattr_n(nqp::p6decont($b), Complex, '$!re');
    my num $b_im = nqp::getattr_n(nqp::p6decont($b), Complex, '$!im');
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

multi sub infix:</>(Complex:D \$a, Real \$b) returns Complex:D {
    Complex.new($a.re / $b, $a.im / $b);
}

multi sub infix:</>(Real \$a, Complex:D \$b) returns Complex:D {
    Complex.new($a, 0) / $b;
}

multi sub infix:<**>(Complex:D \$a, Complex:D \$b) returns Complex:D {
    ($a.re == 0e0 && $a.im == 0e0) ?? Complex.new(0e0, 0e0) !! ($b * $a.log).exp
}
multi sub infix:<**>(Real      \$a, Complex:D \$b) returns Complex:D {
    $a == 0 ?? Complex.new(0e0, 0e0) !! ($b * $a.log).exp 
}
multi sub infix:<**>(Complex:D \$a, Real      \$b) returns Complex:D {
    ($b * $a.log).exp
}

multi sub infix:<==>(Complex:D \$a, Complex:D \$b) returns Bool:D { $a.re == $b.re && $a.im == $b.im }
multi sub infix:<==>(Complex:D \$a, Real      \$b) returns Bool:D { $a.re == $b    && $a.im == 0e0   }
multi sub infix:<==>(Real      \$a, Complex:D \$b) returns Bool:D { $a    == $b.re && 0e0   == $b.im }

proto postfix:<i>(|$) returns Complex:D { * }
multi postfix:<i>(Real      \$a) returns Complex:D { Complex.new(0e0, $a);     }
multi postfix:<i>(Complex:D \$a) returns Complex:D { Complex.new(-$a.im, $a.re) }
multi postfix:<i>(Numeric   \$a) returns Complex:D { $a * Complex.new(0e0, 1e0) }
multi postfix:<i>(Cool      \$a) returns Complex:D { $a.Numeric * Complex.new(0e0, 1e0) }

constant i = Complex.new(0e0, 1e0);

# vim: ft=perl6
