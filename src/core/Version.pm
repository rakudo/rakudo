class Version {
    has @.parts;
    has Bool $.plus = False;

    multi method new(Whatever) { self.bless(:parts(*)) }
    multi method new(Str() $s) {
        my @parts = $s.comb(/:r '*' || \d+ || <.alpha>+/);
        for @parts {
            if $_ eq '*' {
                $_ = *;
            }
            else {
                my $numeric = .Numeric;
                $_ = $numeric if $numeric.defined;
            }
        }
        self.bless(
          :parts(@parts),
          :plus($s.ends-with("+")),
        );
    };

    multi method Str(Version:D:) {
        @!parts.map({ nqp::istype($_,Whatever) ?? '*' !! $_}).join('.')
          ~ ($!plus ?? '+' !! '');
    }
    multi method gist(Version:D:) { 'v' ~ self.Str }
    multi method perl(Version:D:) {
        self.^name ~ ".new('" ~ self.Str ~ "')";

    }
    multi method ACCEPTS(Version:D: Version:D $other) {
        for @!parts.kv -> $i, $v {
            next if nqp::istype($v,Whatever);
            my $o = $other.parts[$i];
            return True unless defined $o;
            next if nqp::istype($o,Whatever);
            return $.plus if $o after  $v;
            return False  if $o before $v;
        }
        True;
    }

    multi method WHICH(Version:D:) {
        my $s := join '|', self.^name, self.Str;
        nqp::box_s(nqp::unbox_s($s), ObjAt);
    }
}


multi sub infix:<eqv>(Version:D $a, Version:D $b) {
    $a.WHAT === $b.WHAT && $a.Str eq $b.Str
}


multi sub infix:<cmp>(Version:D $a, Version:D $b) {
    proto vnumcmp(|) { * }
    multi vnumcmp(Str, Int) { Order::Less }
    multi vnumcmp(Int, Str) { Order::More }
    multi vnumcmp($av, $bv) { $av cmp $bv }

    my @av = $a.parts.values;
    my @bv = $b.parts.values;
    while @av || @bv {
       my $cmp = vnumcmp(@av.shift // 0, @bv.shift // 0);
       return $cmp if $cmp != Order::Same;
    }
    $a.plus cmp $b.plus;
}

# vim: ft=perl6 expandtab sw=4
