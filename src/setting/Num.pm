class Num is also {
    multi method ACCEPTS($other) {
        if self eq 'NaN' {
            $other eq 'NaN';
        } else {
            $other == self;
        }
    }
    multi method ACCEPTS(Complex $other) {
        if self eq 'NaN' {
            $other.re eq 'NaN' || $other.im eq 'NaN';
        } else {
            $other.im == 0 && $other.re == self;
        }
    }
    multi method Complex() {
        Complex.new(self, 0);
    }

    our Num multi method exp() {
        my $r = Q:PIR {
            $N0 = self
            $N1 = exp $N0
            %r = box $N1
        };
    }

    our Num multi method acos($base = 'radians') {
        my $r = Q:PIR {
            $N0 = self
            $N1 = acos $N0
            %r = box $N1
        };
        $r!from-radians($base)
    }

    our Num multi method acosh($base = 'radians') {
        my $r = Q:PIR {
            $N0 = self
            $N1 = $N0 * $N0
            $N1 -= 1
            $N1 = sqrt $N1
            $N0 += $N1
            $N0 = ln $N0
            %r = box $N0
        };
        $r!from-radians($base)
    }

    our Num multi method acosec($base = 'radians') {
        my $r = Q:PIR {
            $N0 = self
            $N1 = 1 / $N0
            $N2 = asin $N1
            %r = box $N2
        };
        $r!from-radians($base)
   }

    our Num multi method acosech($base = 'radians') is export {
        # MUST: This is certainly wrong -- if nothing else,
        # asinh also calls from-radians on its result.
        # (Except it seems to be passing tests?)
        asinh(1/+self)!from-radians($base)
    }

    our Num multi method acotan($base = 'radians') {
        my $r = Q:PIR {
            $N0 = self
            $N1 = 1 / $N0
            $N2 = atan $N1
            %r = box $N2
        };
        $r!from-radians($base)
   }

    our Num multi method acotanh($base = 'radians') is export {
        my $r = Q:PIR {
            $N0 = self
            $N1 = 1 + $N0
            $N2 = $N0 - 1
            $N3 = $N1 / $N2
            $N4 = ln $N3
            $N4 = $N4 / 2
            %r = box $N4
        };
        $r!from-radians($base)
    }

    our Num multi method asec($base = 'radians') {
        my $r = Q:PIR {
            $N0 = self
            $N1 = asec $N0
            %r = box $N1
        };
        $r!from-radians($base)
   }

    our Num multi method asech($base = 'radians') {
        my $r = Q:PIR {
            $N0 = self
            $N1 = neg $N0
            $N1 *= $N0
            $N1 += 1
            $N1 = sqrt $N1
            $N1 += 1
            $N1 /= $N0
            $N1 = ln $N1
            %r = box $N1
        };
        $r!from-radians($base)
    }

    our Num multi method asin($base = 'radians') {
        my $r = Q:PIR {
            $N0 = self
            $N1 = asin $N0
            %r = box $N1
        };
        $r!from-radians($base)
    }

    our Num multi method asinh($base = 'radians') {
        my $r = Q:PIR {
            $N0 = self
            $N1 = $N0 * $N0
            $N1 += 1
            $N1 = sqrt $N1
            $N0 += $N1
            $N0 = ln $N0
            %r = box $N0
        };
        $r!from-radians($base)
    }

    our Num multi method atan($base = 'radians') {
        my $r = Q:PIR {
            $N0 = self
            $N1 = atan $N0
            %r = box $N1
        };
        $r!from-radians($base)
    }

    our Num multi method atan2(Num $x = 1, $base = 'radians') is export {
        my $r = Q:PIR {
            $N0 = self
            $P1 = find_lex "$x"
            $N1 = $P1
            $N2 = atan $N0, $N1
            %r = box $N2
        };
        $r!from-radians($base)
    }

    our Num multi method atanh($base = 'radians') {
        my $r = Q:PIR {
            $N0 = self
            $N1 = 1 - $N0
            $N0 += 1
            $N0 /= $N1
            $N0 = ln $N0
            $N0 /= 2
            %r = box $N0
        };
        $r!from-radians($base)
    }

    our Num multi method cos($base = 'radians') {
        my $x = self!to-radians($base);
        Q:PIR {
            $P0 = find_lex "$x"
            $N0 = $P0
            $N1 = cos $N0
            %r = box $N1
        };
    }

    our Num multi method cosh($base = 'radians') {
        my $x = self!to-radians($base);
        Q:PIR {
            $P0 = find_lex "$x"
            $N0 = $P0
            $N1 = cosh $N0
            %r = box $N1
        };
    }

    our Num multi method cosec($base = 'radians') {
        my $x = self!to-radians($base);
        Q:PIR {
            $P0 = find_lex "$x"
            $N0 = $P0
            $N1 = sin $N0
            $N1 = 1 / $N1
            %r = box $N1
        };
    }

    our Num multi method cosech($base = 'radians') is export {
        my $x = self!to-radians($base);
        Q:PIR {
            $P0 = find_lex "$x"
            $N0 = $P0
            $N1 = sinh $N0
            $N1 = 1 / $N1
            %r = box $N1
        };
   }

    our Num multi method cotan($base = 'radians') {
        my $x = self!to-radians($base);
        Q:PIR {
            $P0 = find_lex "$x"
            $N0 = $P0
            $N1 = tan $N0
            $N1 = 1 / $N1
            %r = box $N1
        }
    }

    our Num multi method cotanh($base = 'radians') is export {
        my $x = self!to-radians($base);
        Q:PIR {
            $P0 = find_lex "$x"
            $N0 = $P0
            $N1 = tanh $N0
            $N1 = 1 / $N1
            %r = box $N1
        }
   }

    multi method log() {
        Q:PIR {
            $N0 = self
            $N0 = ln $N0
            %r  = box $N0
        }
    }

    our method log10 {
        Q:PIR {
            $N0 = self
            $N0 = log10 $N0
            %r  = box $N0
        }
    }

    our Str multi method perl() {
        ~self
    }

    our Num multi method sec($base = 'radians') {
        my $x = self!to-radians($base);
        Q:PIR {
            $P0 = find_lex "$x"
            $N0 = $P0
            $N1 = sec $N0
            %r = box $N1
        }
    }

    our Num multi method sech($base = 'radians') {
        my $x = self!to-radians($base);
        Q:PIR {
            $P0 = find_lex "$x"
            $N0 = $P0
            $N1 = sech $N0
            %r = box $N1
        }
    }

    our Num multi method sin($base = 'radians') {
        my $x = self!to-radians($base);
        Q:PIR {
            $P0 = find_lex "$x"
            $N0 = $P0
            $N1 = sin $N0
            %r = box $N1
        }
    }

    our Num multi method sinh($base = 'radians') {
        my $x = self!to-radians($base);
        Q:PIR {
            $P0 = find_lex "$x"
            $N0 = $P0
            $N1 = sinh $N0
            %r = box $N1
        }
    }

    multi method sqrt() {
        Q:PIR {
            $N0 = self
            $N0 = sqrt $N0
            %r  = box $N0
        }
    }

    our Str multi method Str() {
        ~self
    }

    our Num multi method Num() {
        self;
    }

    our Num multi method tan($base = 'radians') {
        my $x = self!to-radians($base);
        Q:PIR {
            $P0 = find_lex "$x"
            $N0 = $P0
            $N1 = tan $N0
            %r = box $N1
        }
    }

    our Num multi method tanh($base = 'radians') {
        my $x = self!to-radians($base);
        Q:PIR {
            $P0 = find_lex "$x"
            $N0 = $P0
            $N1 = tanh $N0
            %r = box $N1
        }
    }

    our Complex multi method unpolar(Num $angle) is export {
        Complex.new(self * $angle.cos("radians"), self * $angle.sin("radians"));
    }
}

# vim: ft=perl6
