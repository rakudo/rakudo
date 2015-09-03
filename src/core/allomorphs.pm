my class IntStr is Int is Str {
    method new(Int $i, Str $s) {
        my \SELF = nqp::create(self);
        # XXX this bindattr_i fails for bigints
        nqp::bindattr_i(SELF, Int, '$!value', $i);
        nqp::bindattr_s(SELF, Str, '$!value', $s);
        SELF;
    }

    multi method Numeric(IntStr:D:) { self.Int }
    method Int(IntStr:D:) { nqp::getattr_i(self, Int, '$!value') }
    multi method Str(IntStr:D:) { nqp::getattr_s(self, Str, '$!value') }

    multi method gist(IntStr:D:) {
        "val({self.Str.perl})";
    }

    multi method perl(IntStr:D:) {
        "IntStr.new({self.Int.perl}, {self.Str.perl})";
    }
}

my class NumStr is Num is Str {
    method new(Num $n, Str $s) {
        my \SELF = nqp::create(self);
        nqp::bindattr_n(SELF, Num, '$!value', $n);
        nqp::bindattr_s(SELF, Str, '$!value', $s);
        SELF;
    }

    multi method Numeric(NumStr:D:) { self.Num }
    method Num(NumStr:D:) { nqp::getattr_n(self, Num, '$!value') }
    multi method Str(NumStr:D:) { nqp::getattr_s(self, Str, '$!value') }

    multi method gist(NumStr:D:) {
        "val({self.Str.perl})";
    }

    multi method perl(NumStr:D:) {
        "NumStr.new({self.Num.perl}, {self.Str.perl})";
    }
}

my class RatStr is Rat is Str {
    method new(Rat $r, Str $s) {
        my \SELF = nqp::create(self);
        nqp::bindattr(SELF, Rat, '$!numerator', $r.numerator);
        nqp::bindattr(SELF, Rat, '$!denominator', $r.denominator);
        nqp::bindattr_s(SELF, Str, '$!value', $s);
        SELF;
    }

    multi method Numeric(RatStr:D:) { self.Rat }
    method Rat(RatStr:D:) { Rat.new(nqp::getattr(self, Rat, '$!numerator'), nqp::getattr(self, Rat, '$!denominator')) }
    multi method Str(RatStr:D:) { nqp::getattr_s(self, Str, '$!value') }

    multi method gist(RatStr:D:) {
        "val({self.Str.perl})";
    }

    multi method perl(RatStr:D:) {
        "RatStr.new({self.Rat.perl}, {self.Str.perl})";
    }
}

my class ComplexStr is Complex is Str {
    method new(Complex $c, Str $s) {
        my \SELF = nqp::create(self);
        nqp::bindattr_n(SELF, Complex, '$!re', $c.re);
        nqp::bindattr_n(SELF, Complex, '$!im', $c.im);
        nqp::bindattr_s(SELF, Str, '$!value', $s);
        SELF;
    }

    multi method Numeric(ComplexStr:D:) { self.Complex }
    method Complex(ComplexStr:D:) { Complex.new(nqp::getattr_n(self, Complex, '$!re'), nqp::getattr_n(self, Complex, '$!im')) }
    multi method Str(ComplexStr:D:) { nqp::getattr_s(self, Str, '$!value') }

    multi method gist(ComplexStr:D:) {
        "val({self.Str.perl})";
    }

    multi method perl(ComplexStr:D:) {
        "ComplexStr.new({self.Complex.perl}, {self.Str.perl})";
    }
}

# we define cmp ops for these allomorphic types as numeric first, then Str. If
# you want just one half of the cmp, you'll need to coerce the args
multi sub infix:<cmp>(IntStr $a, IntStr $b) {
    given $a.Int cmp $b.Int {
        return $_ unless $_ === Order::Same;
        $a.Str cmp $b.Str
    }
}

multi sub infix:<cmp>(RatStr $a, RatStr $b) {
    given $a.Rat cmp $b.Rat {
        return $_ unless $_ === Order::Same;
        $a.Str cmp $b.Str
    }
}

multi sub infix:<cmp>(NumStr $a, NumStr $b) {
    given $a.Num cmp $b.Num {
        return $_ unless $_ === Order::Same;
        $a.Str cmp $b.Str
    }
}

multi sub infix:<cmp>(ComplexStr $a, ComplexStr $b) {
    given $a.Complex cmp $b.Complex {
        return $_ unless $_ === Order::Same;
        $a.Str cmp $b.Str
    }
}

# these allomorphic multis are needed to use their C<.gist>s properly, lest they
# pick the Str:D candidate.
multi sub say(IntStr:D \x) {
    my $out := $*OUT;
    $out.print: x.gist;
    $out.print-nl;
}
multi sub say(RatStr:D \x) {
    my $out := $*OUT;
    $out.print: x.gist;
    $out.print-nl;
}
multi sub say(NumStr:D \x) {
    my $out := $*OUT;
    $out.print: x.gist;
    $out.print-nl;
}
multi sub say(ComplexStr:D \x) {
    my $out := $*OUT;
    $out.print: x.gist;
    $out.print-nl;
}

multi sub note(IntStr:D \x) {
    my $err := $*ERR;
    $err.print: x.gist;
    $err.print-nl;
}
multi sub note(RatStr:D \x) {
    my $err := $*ERR;
    $err.print: x.gist;
    $err.print-nl;
}
multi sub note(NumStr:D \x) {
    my $err := $*ERR;
    $err.print: x.gist;
    $err.print-nl;
}
multi sub note(ComplexStr:D \x) {
    my $err := $*ERR;
    $err.print: x.gist;
    $err.print-nl;
}

multi sub val(*@maybevals) {
    # XXX .Parcel not needed on GLR (just .eager suffices)
    # XXX GLR would need a .List before the .map, so that the output is === compatible
    @maybevals.map({ val($_) }).eager.Parcel;
}

# XXX this multi not needed in GLR ?
multi sub val(@maybevals) {
    val(|@maybevals);
}

multi sub val(Pair $ww-thing) {
    # this is a Pair object possible in «» constructs; just pass it through. We
    # capture this specially from the below sub to avoid emitting a warning
    # whenever an affected «» construct is being processed.

    $ww-thing;
}

multi sub val(\one-thing) {
    warn "Value of type {one-thing.WHAT.perl} uselessly passed to val()";
    one-thing;
}

multi sub val(Str $MAYBEVAL, :$val-or-fail = False) {
    #
    #  docs/val.pod6  describes what's going on in this sub; take a look if you're lost!
    #

    # get trimmed string
    my $str-form = nqp::unbox_s($MAYBEVAL.trim);

    # cover all the cases without any numerals
    return NumStr.new(Inf, $MAYBEVAL) if $str-form eq "Inf" || $str-form eq "+Inf";
    return NumStr.new(NaN, $MAYBEVAL) if $str-form eq "NaN" || $str-form eq "+NaN";
    return NumStr.new(-Inf, $MAYBEVAL) if $str-form eq "-Inf";
    return NumStr.new(-NaN, $MAYBEVAL) if $str-form eq "-NaN";
#    return ComplexStr.new(i, $MAYBEVAL) if $str-form eq "i";

    # die if there's no digits, since now there's no way it could be a val.
    unless nqp::findcclass(nqp::const::CCLASS_NUMERIC, $str-form, 0, nqp::chars($str-form)) < nqp::chars($str-form) {
        if $val-or-fail {
            fail X::Str::Numeric.new(source => $MAYBEVAL,
                                     reason => "No digits in value, cannot possibly be a value",
                                     pos    => 0);
        }
        return $MAYBEVAL;
    }

    my $val-form;

    if nqp::eqat($str-form, ':', 0) || nqp::eqat($str-form, ':', 1) {
        $val-form = radix-adverb($str-form);
    } elsif nqp::eqat($str-form, 'i', nqp::chars($str-form) - 1) {
        $val-form = complex-num($str-form);
    } elsif nqp::index($str-form, '/') > -1 {
        $val-form = frac-rat($str-form);
    } elsif nqp::index($str-form, 'e') > -1 || nqp::index($str-form, 'E') > -1 {
        # can be a scientific num or an integer
        $val-form = science-num($str-form) // just-int($str-form);
    } elsif nqp::index($str-form, '.') > -1 {
        $val-form = point-rat($str-form);
    } else {
        $val-form = just-int($str-form);
    }

    with $val-form {
        given $val-form {
            when Int {
                return IntStr.new($_, $MAYBEVAL);
            }

            when Rat {
                return RatStr.new($_, $MAYBEVAL);
            }

            when Num {
                return NumStr.new($_, $MAYBEVAL);
            }

            when Complex {
                return ComplexStr.new($_, $MAYBEVAL);
            }

            default {
                die "Unknown type from val() processing: {$_.WHAT}";
            }
        }
    } else {
        if $val-or-fail {
            my $ws-off = nqp::findnotcclass(nqp::const::CCLASS_WHITESPACE, $MAYBEVAL, 0, $MAYBEVAL.chars);
            fail X::Str::Numeric.new(source => $MAYBEVAL,
                                     reason => $val-form.reason,
                                     pos    => ($val-form.offset + $ws-off));
        }

        return $MAYBEVAL;
    }

    #
    # Here's where the subs used are defined
    #

    ##| checks if number is to be negated, and chops off the sign
    sub is-negated($val) {
        nqp::eqat($val, '-', 0) ?? 1 !! 0;
    }

    sub has-sign($val) {
        nqp::eqat($val, '+', 0) || is-negated($val) ?? 1 !! 0;
    }

    ##| retrieve an "oh radix" (0x, 0o, etc.), if there
    sub get-ohradix($maybeint is copy, $radix = 10) { # $radix to limit valid ohradices
        if $radix < 34 && nqp::eqat($maybeint, '0x', 0) {
            16
        } elsif $radix < 14 && nqp::eqat($maybeint, '0d', 0) {
            10
        } elsif $radix < 25 && nqp::eqat($maybeint, '0o', 0) {
            8
        } elsif $radix < 12 && nqp::eqat($maybeint, '0b', 0) {
            2
        } else {
            parsing-fail("nohradix");
        }
    }

    # passing around a regular ol' Failure will cause Perl 6 to hang, for
    # whatever reason, so we use this "fake" class instead. Incidentally it lets
    # us also defer creating the actual exception (and subsequent failure) until
    # the end.
    my class ProtoFailure {
        has $.reason = "";
        has $.offset = 0;

        ##| Imitates Failure.defined
        method defined {
            Bool::False
        }

        ##| Called at the end of val() processing, when we aren't sure our error
        ##| will be meaningful (though the pointer will still be helpful)
        method doubt-reason {
            $!reason = $!reason.trim-trailing ~ " (but we're not sure what kind of value this wants to be)";
        }

        ##| Prefixes an additional bit of explanation to the reason (used when
        ##| subs carry failures from inner calls upwards)
        method stack-reason($newbit) {
            $!reason = $newbit ~ $!reason;
            self;
        }

        ##| Adjusts the offset, effectively setting it relative to a new string
        ##| (used when subs carry failures from inner calls upwards)
        method adjust-offset($off) {
            $!offset += $off;
            self;
        }
    }

    ##| A failure caught during parsing of the string
    sub parsing-fail(Str $reason, Int $offset = 0) {
        return ProtoFailure.new.stack-reason($reason).adjust-offset($offset);
    }

    ##| processes an integer, by default decimal
    sub just-int($maybeint, $radix is copy = 10, :$e = False, :$nosign = False) {
        my $negated = $nosign ?? 0 !! is-negated($maybeint);
        my $signed = $nosign ?? 0 !! has-sign($maybeint);
        my $ohradix = $e ?? parsing-fail("dummy-nohradix") !! get-ohradix(nqp::substr($maybeint, $signed), $radix);
        my $startpos = 0;

        $startpos++ if $signed;

        with $ohradix {
            $radix = $ohradix;
            $startpos += 2;

            # handle initial underscore, since radix_I won't
            if nqp::eqat($maybeint, '_', $startpos) {
                $startpos++;
            }
        }

        my $radresult := nqp::radix_I($radix, $maybeint, $startpos, $negated, Int);

        if nqp::atpos($radresult, 2) == -1 {
            return parsing-fail("Strange text where integer expected");
        }

        if nqp::atpos($radresult, 2) < nqp::chars($maybeint) {
            return parsing-fail("Trailing garbage after integer", nqp::atpos($radresult, 2));
        }

        nqp::atpos($radresult, 0);
    }

    ##| process a Rat in "radix point" notation
    sub point-rat($mayberat, $radix is copy = 10, :$nosign = False, :$adverb = False) {
        my $radixpoint = nqp::index($mayberat, '.');

        if $radixpoint == -1 {
            return parsing-fail("No point found for supposed radix point rational");
        }

        my $signed  = $nosign ?? 0 !! has-sign($mayberat);
        my $negated = $nosign ?? 0 !! is-negated($mayberat);
        my $ohradix = $adverb ?? get-ohradix(nqp::substr($mayberat, $signed), $radix) !! parsing-fail("dummy-nohradix");

        my $ipart = nqp::substr($mayberat, $signed + (2 with $ohradix), $radixpoint - $signed - (2 with $ohradix));
        my $fpart = nqp::substr($mayberat, $radixpoint + 1);

        $radix = $ohradix with $ohradix;

        if nqp::index($fpart, '.') > -1 {
            return parsing-fail("Extra point found in supposed radix point rational", $radixpoint + 1 + nqp::index($fpart, '.'));
        }

        my $irad := nqp::radix_I($radix, $ipart, 0, 0, Int);

        if nqp::atpos($irad, 2) < nqp::chars($ipart) {
            return parsing-fail("Unexpected text in integral part of radix point rational", nqp::atpos($irad, 2) max 0);
        }

        my $frad := nqp::radix_I($radix, $fpart, 0, 4, Int);

        if nqp::atpos($frad, 2) < nqp::chars($fpart) {
            return parsing-fail("Trailing garbage after supposed radix point rational", $radixpoint + 1 + (nqp::atpos($frad, 2) max 0));
        }

        my $numer = nqp::atpos($irad, 0) * nqp::atpos($frad, 1);
        $numer += nqp::atpos($frad, 0);
        $numer *= $negated ?? -1 !! 1;

        return Rat.new($numer, nqp::atpos($frad, 1));
    }

    ##| process a :#<> form number (:#[] NYI)
    sub radix-adverb($maybenum is copy, :$nofrac = False, :$nosign = False) {
        unless nqp::eqat($maybenum, ':', 0) || nqp::eqat($maybenum, ':', 1) {
            return parsing-fail("Not an adverbial number");
        }

        # get the sign, if there
        my $signed  = $nosign ?? 0 !! has-sign($maybenum);
        my $negated = $nosign ?? 0 !! is-negated($maybenum);

        # get the radix
        my $baseradix := nqp::radix_I(10, $maybenum, 1 + $signed, 0, Int);

        if nqp::atpos($baseradix, 2) == -1 {
            return parsing-fail("Strange text after colon", nqp::atpos($baseradix, 2));
        }

        if !(2 <= nqp::atpos($baseradix, 0) <= 36) {
            # wouldn't be so immediately failing when :#[] form is supported
            return parsing-fail("Invalid radix of {nqp::atpos($baseradix, 0)} in adverb (must be in range 2..36)", nqp::atpos($baseradix, 2));
        }

        # get start point

        my $numstart = nqp::atpos($baseradix, 2);
        my $defradix = nqp::atpos($baseradix, 0);

        if !nqp::eqat($maybenum, '<', $numstart) {
            if nqp::eqat($maybenum, '[', $numstart) {
                return parsing-fail(":#[] style adverbs NYI, sorry", $numstart);
            } elsif nqp::eqat($maybenum, '(', $numstart) {
                return parsing-fail(":#() style adverbs not supported by val(), please use EVAL($MAYBEVAL.perl()) instead", $numstart);
            }

            return parsing-fail("Unknown text after radix in supposed adverbial number", $numstart);
        }

        $numstart++;

        # get components (coeff, base, exp)

        my ($coeff, $coend, $base, $bend, $exp, $eend);

        $coend = nqp::index($maybenum, '*', $numstart);

        if $coend == -1 { # no base and exp, just coeff
            $coend = nqp::index($maybenum, '>', $numstart);

            if $coend < nqp::chars($maybenum) - 1 {
                return parsing-fail("Trailing garbage after supposed adverbial form", $coend + 1);
            }

            $coeff = nqp::substr($maybenum, $numstart, $coend - $numstart);

            my $res;
            if !$nofrac && nqp::index($coeff, '.') > -1 {
                $res = point-rat($coeff, $defradix, :nosign, :adverb);
            } else {
                $res = just-int($coeff, $defradix, :nosign);
            }

            with $res {
                $res = -$res if $negated;
            } else {
                $res = $res.stack-reason("Error parsing adverbial number: ").adjust-offset($numstart);
            }

            return $res;
        }

        # we know we have a Num at this point, so no fractionals allowed means
        # failure
        if $nofrac {
            return parsing-fail("Num adverbial detected where integer required (don't use * and ** here)", $coend);
        }

        # get a base and exponent

        $bend = nqp::index($maybenum, '**', $coend + 1);
        if $bend == -1 {
            return parsing-fail("Missing ** in Num adverbial", $coend + 1);
        }

        $eend = nqp::index($maybenum, '>', $bend + 2);
        if $eend == -1 {
            return parsing-fail("Missing > in Num adverbial", $bend + 2);
        }

        if $eend < (nqp::chars($maybenum) - 1) {
            return parsing-fail("Trailing garbage after supposed adverbial number", $eend + 1);
        }

        # get substrings

        $coeff = nqp::substr($maybenum, $numstart, $coend - $numstart);
        $base  = nqp::substr($maybenum, $coend + 1, $bend - $coend - 1);
        $exp   = nqp::substr($maybenum, $bend + 2, $eend - $bend - 2);

        # coefficient is at best a decimal number, base and exp can be adverbs themselves

        if nqp::index($coeff, '.') > -1 {
            $coeff = point-rat($coeff, $defradix, :nosign, :adverb);
        } else {
            $coeff = just-int($coeff, $defradix, :nosign);
        }

        if nqp::eqat($base, ':', 0) || !$nosign && nqp::eqat($base, ':', 1) { # the position 1 check is to account for signs
            $base = radix-adverb($base, :nofrac, :$nosign);
        } else {
            $base = just-int($base, 10, :$nosign);
        }

        if nqp::eqat($exp, ':', 0) || nqp::eqat($base, ':', 1) {
            $exp = radix-adverb($exp, :nofrac);
        } else {
            $exp = just-int($exp);
        }

        without $coeff {
            return $coeff.stack-reason("Problematic coefficient: ").adjust-offset($numstart);
        }

        without $base {
            return $base.stack-reason("Problematic base: ").adjust-offset($coend + 1);
        }

        without $exp {
            return $exp.stack-reason("Problematic exponent: ").adjust-offset($bend + 2);
        }

        ($negated ?? -1 !! 1) * $coeff.Num * $base.Num ** $exp.Num;
    }

    ##| Rationals in fraction form
    sub frac-rat($mayberat) {
        my $slash = nqp::index($mayberat, '/');

        if $slash == -1 {
            return parsing-fail("Required slash for fractional Rats not found");
        }

        my $nstr = nqp::substr($mayberat, 0, $slash);
        my $dstr = nqp::substr($mayberat, $slash + 1);

        # Rat literals only allow integral numerators/denominators
        my $numer;
        my $denom;

        if nqp::eqat($nstr, ':', 0) || nqp::eqat($nstr, ':', 1) {
            $numer = radix-adverb($nstr, :nofrac);
        } else {
            $numer = just-int($nstr);
        }

        if nqp::eqat($dstr, ':', 0) { # denom currently has to be unsigned, so only check pos 0
            $denom = radix-adverb($dstr, :nofrac, :nosign);
        } else {
            $denom = just-int($dstr, :nosign);
        }

        without $numer {
            return $numer.stack-reason("Problematic numerator: ");
        }

        without $denom {
            return $denom.stack-reason("Problematic denominator: ").adjust-offset($slash + 1);
        }

        return Rat.new($numer, $denom);
    }

    ##| Scientific notation Nums, or Inf/NaN
    sub science-num($maybenum) {
        my $e = nqp::index($maybenum, 'e');
        $e = nqp::index($maybenum, 'E') if $e == -1;

        if $e == -1 {
            # Inf and NaN handled by shortcuts above
            return parsing-fail("Supposed scientific Num doesn't have 'e' or 'E', nor is 'Inf' or 'NaN'");
        }

        my $cstr = nqp::substr($maybenum, 0, $e);
        my $estr = nqp::substr($maybenum, $e + 1);

        my $coeff;
        my $exp = just-int($estr, :e);

        if nqp::index($cstr, '.') > -1 {
            $coeff = point-rat($cstr);
        } else {
            $coeff = just-int($cstr, :e);
        }

        without $coeff {
            return $coeff.stack-reason("Bad coefficient: ");
        }

        without $exp {
            return $exp.stack-reason("Bad exponent: ").adjust-offset($e);
        }

        return $coeff.Num * 10 ** $exp.Num;
    }

    ##| Complex numbers
    sub complex-num($maybecmpx) {
        unless nqp::eqat($maybecmpx, 'i', nqp::chars($maybecmpx) - 1) {
            return parsing-fail("No 'i' found for supposed complex number");
        }

        my $escape-i = 0;
        $escape-i++ if nqp::eqat($maybecmpx, Q[\i], nqp::chars($maybecmpx) - 2);

        my $splitpos = 1;

        my $negated-im = 1;

        while $splitpos < nqp::chars($maybecmpx) {
            last if nqp::eqat($maybecmpx, '+', $splitpos);

            if nqp::eqat($maybecmpx, '-', $splitpos) {
                $negated-im = -1;
                last;
            }

            $splitpos++;
        }

        my $re; my $rstr;
        my $im; my $istr;

        if $splitpos == nqp::chars($maybecmpx) { # purely imaginary
            $istr = nqp::substr($maybecmpx, 0, nqp::chars($maybecmpx) - (1 + $escape-i));
            $re = 0;
        } else {
            $rstr = nqp::substr($maybecmpx, 0, $splitpos);
            $istr = nqp::substr($maybecmpx, $splitpos + 1, (nqp::chars($maybecmpx) - (1 + $escape-i)) - $splitpos - 1);

            if nqp::iseq_s($rstr, "NaN") || nqp::iseq_s($rstr, "+NaN") {
                $re = NaN;
            } elsif nqp::iseq_s($rstr, "Inf") || nqp::iseq_s($rstr, "+Inf") {
                $re = Inf;
            } elsif nqp::iseq_s($rstr, "-NaN") {
                $re = -NaN;
            } elsif nqp::iseq_s($rstr, "-Inf") {
                $re = -Inf;
            } elsif nqp::eqat($rstr, ':', 0) || nqp::eqat($rstr, ':', 1) {
                $re = radix-adverb($rstr);
            } elsif nqp::index($rstr, 'e') > -1 || nqp::index($rstr, 'E') > 1 {
                $re = science-num($rstr) // just-int($rstr);
            } elsif nqp::index($rstr, '.') > -1 {
                $re = point-rat($rstr);
            } else {
                $re = just-int($rstr);
            }

        }

        if nqp::iseq_s($istr, "NaN") || nqp::iseq_s($istr, "+NaN") {
            $im = NaN;
        } elsif nqp::iseq_s($istr, "Inf") || nqp::iseq_s($istr, "+Inf") {
            $im = Inf;
        } elsif nqp::iseq_s($istr, "-NaN") {
            $im = -NaN;
        } elsif nqp::iseq_s($istr, "-Inf") {
            $im = -Inf;
        } elsif nqp::eqat($istr, ':', 0) || nqp::eqat($istr, ':', 1) {
            $im = radix-adverb($istr);
        } elsif nqp::index($istr, 'e') > -1 || nqp::index($istr, 'E') > 1 {
            $im = science-num($istr) // just-int($istr);
        } elsif nqp::index($istr, '.') > -1 {
            $im = point-rat($istr);
        } else {
            $im = just-int($istr);
        }

        without $re {
            return $re.stack-reason("Problem with real part: ");
        }

        without $im {
            return $im.stack-reason("Problem with imaginary part: ").adjust-offset($splitpos + 1);
        }

        if $im === (NaN|Inf|-Inf) && $escape-i == 0 { # don't let NaNi or Infi work, must be NaN\i or Inf\i
            return parsing-fail("Imaginary unit must be escaped with an imaginary part of '$im', i.e. $im\\i", nqp::chars($maybecmpx) - 1);
        }

        return Complex.new($re, $im * $negated-im);
    }
}