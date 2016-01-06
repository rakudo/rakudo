class Version {
    has $!parts;
    has int $!plus;
    has str $!string;

    method !SET-SELF(\parts,\plus,\string) {
        $!parts := nqp::getattr(parts,List,'$!reified');
        $!plus   = plus;
        $!string = string;
        self
    }

    multi method new(Version:) {
        # "v" highlander
        INIT nqp::create(Version)!SET-SELF(nqp::list,0,"")      # should be once
    }
    multi method new(Version: Whatever) {
        # "v*" highlander
        INIT nqp::create(Version)!SET-SELF(nqp::list(*),-1,"*") # should be once
    }
    multi method new(Version: @parts, Str:D $string, Int() $plus = 0) {
        nqp::create(self)!SET-SELF(@parts.eager,$plus,$string)
    }
    multi method new(Version: Str() $s) {

        # highlanderize most common
        if $s eq '6' {
            INIT nqp::create(Version)!SET-SELF(nqp::list(6),0,"6") # should be once
        }
        elsif $s eq '6.c' {
            INIT nqp::create(Version)!SET-SELF(nqp::list(6,"c"),0,"6.c") # should be once
        }

        # something sensible given
        elsif $s.comb(/:r '*' || \d+ || <.alpha>+/).eager -> @s {
            my $strings  := nqp::getattr(@s,List,'$!reified');
            my int $elems = nqp::elems($strings);
            my $parts    := nqp::setelems(nqp::list,$elems);

            my int $i = -1;
            while nqp::islt_i(++$i,$elems) {
                my str $s = nqp::atpos($strings,$i);
                nqp::bindpos($parts,$i, nqp::iseq_s($s,"*")
                  ?? *
                  !! (my $numeric = $s.Numeric).defined
                    ?? nqp::decont($numeric)
                    !! nqp::unbox_s($s)
                );
            }

            my str $string = nqp::join(".", $strings);
            my int $plus   = $s.ends-with("+");
            nqp::create(self)!SET-SELF($parts,$plus,$plus
              ?? nqp::concat($string,"+")
              !! $string
            )
        }

        # "v+" highlander
        elsif $s.ends-with("+") {
            INIT nqp::create(Version)!SET-SELF(nqp::list,1,"") # should be once
        }
        # get "v" highlander
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
        my $oparts := nqp::getattr(nqp::decont($other),Version,'$!parts');
        my $oelems  = nqp::isnull($oparts) ?? 0 !! nqp::elems($oparts);

        my int $elems = nqp::elems($!parts);
        my int $i     = -1;
        while nqp::islt_i(++$i,$elems) {
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

    method parts() { nqp::hllize($!parts) }
    method plus()  { nqp::p6bool($!plus) }
}


multi sub infix:<eqv>(Version:D \a, Version:D \b) {
    nqp::p6bool(
      nqp::eqaddr(a,b)
        || (nqp::eqaddr(a.WHAT,b.WHAT)
             && nqp::iseq_s(
               nqp::getattr_s(a,Version,'$!string'),
               nqp::getattr_s(b,Version,'$!string')
             ))
    )
}

multi sub infix:<cmp>(Version:D \a, Version:D \b) {
    proto vnumcmp(|) { * }
    multi vnumcmp(Str, Int) { Order::Less }
    multi vnumcmp(Int, Str) { Order::More }
    multi vnumcmp($av, $bv) { $av cmp $bv }

    # we're us
    if a =:= b {
        Same
    }

    # need to check
    else {
        my \ia := nqp::iterator(nqp::getattr(nqp::decont(a),Version,'$!parts'));
        my \ib := nqp::iterator(nqp::getattr(nqp::decont(b),Version,'$!parts'));

        # check from left
        while ia {
            if vnumcmp(nqp::shift(ia), ib ?? nqp::shift(ib) !! 0) -> $cmp {
                return $cmp;
            }
        }

        # check from right
        while ib {
            if vnumcmp(0, nqp::shift(ib)) -> $cmp {
                return $cmp;
            }
        }

        a.plus cmp b.plus
    }
}

multi sub infix:«<=>»(Version:D \a, Version:D \b) { a cmp b }
multi sub infix:«<»  (Version:D \a, Version:D \b) { a cmp b == Less }
multi sub infix:«<=» (Version:D \a, Version:D \b) { a cmp b != More }
multi sub infix:«==» (Version:D \a, Version:D \b) { a cmp b == Same }
multi sub infix:«!=» (Version:D \a, Version:D \b) { a cmp b != Same }
multi sub infix:«>=» (Version:D \a, Version:D \b) { a cmp b != Less }
multi sub infix:«>»  (Version:D \a, Version:D \b) { a cmp b == More }

# vim: ft=perl6 expandtab sw=4
