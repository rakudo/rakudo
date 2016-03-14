class Version {
    has $!parts;
    has int $!plus;
    has str $!string;

    method !SET-SELF(\parts,\plus,\string) {
        $!parts := nqp::getattr(parts,List,'$!reified');
        $!plus   = plus;
        $!string = $!plus ?? nqp::concat(string,"+") !! string;
        self
    }

    multi method new(Version:) {
        # "v" sentinel
        once nqp::create(self)!SET-SELF(nqp::list,0,"")
    }
    multi method new(Version: Whatever) {
        # "v*" sentinel
        once nqp::create(self)!SET-SELF(nqp::list(*),0,"*")
    }
    multi method new(Version: Str() $s) {

        # sentinel most common
        if $s eq '6' {
            once nqp::create(self)!SET-SELF(nqp::list("6"),0,"6")
        }
        elsif $s eq '6.c' {
            once nqp::create(self)!SET-SELF(nqp::list("6","c"),0,"6.c")
        }

        # something sensible given
        elsif $s.comb(/:r '*' || \d+ || <.alpha>+/).eager -> @s {
            my $strings  := nqp::getattr(@s,List,'$!reified');
            my int $elems = nqp::elems($strings);
            my $parts    := nqp::setelems(nqp::list,$elems);

            my int $i = -1;
            while nqp::islt_i($i = nqp::add_i($i,1),$elems) {
                my str $s = nqp::atpos($strings,$i);
                nqp::bindpos($parts,$i, nqp::iseq_s($s,"*")
                  ?? *
                  !! (my $numeric = $s.Numeric).defined
                    ?? nqp::decont($numeric)
                    !! nqp::unbox_s($s)
                );
            }

            nqp::create(self)!SET-SELF($parts,$s.ends-with("+"),nqp::join(".", $strings))
        }

        # "v+" sentinel
        elsif $s.ends-with("+") {
            once nqp::create(self)!SET-SELF(nqp::list,1,"")
        }
        # get "v" sentinel
        else {
            self.new
        }
    }

    multi method Str(Version:D:)  { $!string }
    multi method gist(Version:D:) { nqp::concat("v",$!string) }
    multi method perl(Version:D:) {
        if nqp::chars($!string) {
            my int $first = nqp::ord($!string);
            nqp::isge_i($first,48) && nqp::isle_i($first,57) # "0" <= x <= "9"
              ?? nqp::concat("v",$!string)
              !! self.^name ~ ".new('$!string')"
        }
        else {
            self.^name ~ ".new"
        }
    }
    multi method ACCEPTS(Version:D: Version:D $other) {
        my $oparts := nqp::getattr($other,Version,'$!parts');
        my $oelems  = nqp::isnull($oparts) ?? 0 !! nqp::elems($oparts);

        my int $elems = nqp::elems($!parts);
        my int $i     = -1;
        while nqp::islt_i($i = nqp::add_i($i,1),$elems) {
            my $v := nqp::atpos($!parts,$i);

            # if whatever here, no more check this iteration
            unless nqp::istype($v,Whatever) {

                # nothing left to check, so ok
                return True if nqp::isge_i($i,$oelems);

                # if whatever there, no more to check this iteration
                my $o := nqp::atpos($oparts,$i);
                unless nqp::istype($o,Whatever) {
                    return nqp::p6bool($!plus) if $o after  $v;
                    return False               if $o before $v;
                }
            }
        }
        True;
    }

    multi method WHICH(Version:D:) {
        nqp::box_s(nqp::unbox_s(self.^name ~ '|' ~ $!string), ObjAt);
    }

    method parts() { $!parts }
    method plus()  { nqp::p6bool($!plus) }
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
