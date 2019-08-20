class Version {
    has $!parts;
    has int $!plus;
    has int $!whatever;
    has str $!string;

    method !SET-SELF(\parts,\plus,\whatever,\string) {
        $!parts := nqp::getattr(parts,List,'$!reified');
        $!plus   = plus;
        $!whatever = whatever;
        $!string = string;
        self
    }

    multi method new(Version:) {
        # "v" highlander
        INIT nqp::create(Version)!SET-SELF(nqp::list,0,0,"")      # should be once
    }
    multi method new(Version: Whatever) {
        # "v*" highlander
        INIT nqp::create(Version)!SET-SELF(nqp::list(*),-1,1,"*") # should be once
    }
    multi method new(Version: @parts, Str:D $string, Int() $plus = 0, Int() $whatever = 0) {
        nqp::create(self)!SET-SELF(@parts.eager,$plus,$whatever,$string)
    }

    method !SLOW-NEW(str $s) {
        # we comb the version string for /:r '*' || \d+ || <.alpha>+/, which
        # will become our parts. The `*` becomes a Whatever, numbers Numeric,
        # and the rest of the parts remain as strings
        nqp::stmts(
          (my int $pos),
          (my int $chars = nqp::chars($s)),
          (my int $mark),
          (my int $whatever = 0),
          (my $strings := nqp::list_s),
          (my $parts   := nqp::list),
          nqp::while(
            nqp::islt_i($pos, $chars),
            nqp::if(
              nqp::eqat($s, '*', $pos),
              nqp::stmts( # Whatever portion
                nqp::push_s($strings, '*'),
                nqp::push($parts,      * ),
                ($whatever = 1),
                ($pos = nqp::add_i($pos, 1))),
              nqp::if(
                nqp::iscclass(nqp::const::CCLASS_NUMERIC, $s, $pos),
                nqp::stmts( # we're at the start of a numeric portion
                  ($mark = $pos),
                  ($pos = nqp::add_i($pos, 1)),
                  nqp::while( # seek the end of numeric portion
                    nqp::islt_i($pos, $chars)
                    && nqp::iscclass(nqp::const::CCLASS_NUMERIC, $s, $pos),
                    ($pos = nqp::add_i($pos, 1))),
                  nqp::push($parts, # grab numeric portion
                    nqp::atpos(
                      nqp::radix(
                        10,
                        nqp::push_s($strings, nqp::substr($s, $mark,
                          nqp::sub_i($pos, $mark))),
                        0, 0),
                      0))),
                nqp::if( # same idea as for numerics, except for <.alpha> class
                  nqp::iscclass(nqp::const::CCLASS_ALPHABETIC, $s, $pos)
                  || nqp::iseq_i(nqp::ord($s, $pos), 95),
                  nqp::stmts( # we're at the start of a alpha portion
                    ($mark = $pos),
                    ($pos = nqp::add_i($pos, 1)),
                    nqp::while( # seek the end of alpha portion
                      nqp::islt_i($pos, $chars)
                      && (nqp::iscclass(nqp::const::CCLASS_ALPHABETIC, $s, $pos)
                        || nqp::iseq_i(nqp::ord($s, $pos), 95)),
                      ($pos = nqp::add_i($pos, 1))),
                    nqp::push($parts, # grab alpha portion
                      nqp::push_s($strings, nqp::substr($s, $mark,
                        nqp::sub_i($pos, $mark))))),
                  ($pos = nqp::add_i($pos, 1)))))),
          nqp::if(
            nqp::elems($strings), # return false if we didn't get any parts
            nqp::stmts(
              (my int $plus = nqp::eqat($s, '+',
                nqp::sub_i(nqp::chars($s), 1))),
              nqp::create(self)!SET-SELF($parts, $plus, $whatever,
                nqp::concat(nqp::join('.', $strings), $plus ?? '+' !! '')))))
    }
    # highlander cache
    my $v6; my $v6c; my $vplus;
    multi method new(Version: Str() $s) {
        nqp::if(
          nqp::iseq_s($s, '6'), # highlanderize most common
          ($v6 //= nqp::create(Version)!SET-SELF(nqp::list(6),0,0,"6")),
          nqp::if(
            nqp::iseq_s($s, '6.c'),
            ($v6c //= nqp::create(Version)!SET-SELF(nqp::list(6,"c"),0,0,"6.c")),
            nqp::unless(
              self!SLOW-NEW($s),
              nqp::if(
                nqp::eqat($s, '+', nqp::sub_i(nqp::chars($s),1)),
                ($vplus //= nqp::create(Version)!SET-SELF(nqp::list,1,0,"")),
                self.new))))
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
        my \oparts       := nqp::getattr(nqp::decont($other),Version,'$!parts');
        my int $oelems    = nqp::isnull(oparts) ?? 0 !! nqp::elems(oparts);
        my int $elems     = nqp::elems($!parts);
        my int $max-elems = nqp::if(nqp::isge_i($oelems,$elems), $oelems, $elems);

        my int $i = -1;
        while nqp::islt_i(++$i,$max-elems) {
            my $v := nqp::if(nqp::isge_i($i,$elems), Whatever, nqp::atpos($!parts,$i));

            # if whatever here, no more check this iteration
            unless nqp::istype($v,Whatever) {
                my $o := nqp::if(nqp::isge_i($i,$oelems), 0, nqp::atpos(oparts,$i));

                # if whatever there, no more to check this iteration
                unless nqp::istype($o,Whatever) {
                    return nqp::hllbool($!plus) if $o after  $v;
                    return False               if $o before $v;
                }
            }
        }
        True;
    }

    method Capture() { X::Cannot::Capture.new( :what(self) ).throw }

    multi method WHICH(Version:D: --> ValueObjAt:D) {
        nqp::box_s(
          nqp::concat(
            nqp::if(
              nqp::eqaddr(self.WHAT,Version),
              'Version|',
              nqp::concat(nqp::unbox_s(self.^name), '|')
            ),
            $!string
          ),
          ValueObjAt
        )
    }

    method parts() { nqp::hllize($!parts) }
    method plus()  { nqp::hllbool($!plus) }
    method whatever() { nqp::hllbool($!whatever) }
}


multi sub infix:<eqv>(Version:D \a, Version:D \b) {
    nqp::hllbool(
      nqp::eqaddr(nqp::decont(a),nqp::decont(b))
        || (nqp::eqaddr(a.WHAT,b.WHAT)
             && nqp::iseq_s(
               nqp::getattr_s(nqp::decont(a),Version,'$!string'),
               nqp::getattr_s(nqp::decont(b),Version,'$!string')
             ))
    )
}

multi sub infix:<cmp>(Version:D \a, Version:D \b) {
    nqp::if(
      nqp::eqaddr(nqp::decont(a),nqp::decont(b)), # we're us
      Same,
      nqp::stmts(
        (my \ia := nqp::iterator(nqp::getattr(nqp::decont(a),Version,'$!parts'))),
        (my \ib := nqp::iterator(nqp::getattr(nqp::decont(b),Version,'$!parts'))),
        (my ($ret, $a-part, $b-part)),
        nqp::while(
          ia, # check from left
          nqp::stmts(
            ($a-part := nqp::shift(ia)),
            ($b-part := ib ?? nqp::shift(ib) !! 0),
            nqp::if(
              ($ret := nqp::if(
                nqp::istype($a-part,Str) && nqp::istype($b-part,Int),
                Less,
                nqp::if(
                  nqp::istype($a-part,Int) && nqp::istype($b-part,Str),
                  More,
                  ($a-part cmp $b-part)))),
              return $ret))),
        nqp::while(
          ib, # check from right
          nqp::stmts(
            ($a-part := 0),
            ($b-part := nqp::shift(ib)),
            nqp::if(
              ($ret := nqp::if(
                nqp::istype($a-part,Str) && nqp::istype($b-part,Int),
                Less,
                nqp::if(
                  nqp::istype($a-part,Int) && nqp::istype($b-part,Str),
                  More,
                  ($a-part cmp $b-part)))),
              return $ret))),
        (     nqp::getattr_i(nqp::decont(a),Version,'$!plus')
          cmp nqp::getattr_i(nqp::decont(b),Version,'$!plus'))))
}

multi sub infix:«<=>»(Version:D \a, Version:D \b) { a cmp b }
multi sub infix:«<»  (Version:D \a, Version:D \b) { a cmp b == Less }
multi sub infix:«<=» (Version:D \a, Version:D \b) { a cmp b != More }
multi sub infix:«==» (Version:D \a, Version:D \b) { a cmp b == Same }
multi sub infix:«!=» (Version:D \a, Version:D \b) { a cmp b != Same }
multi sub infix:«>=» (Version:D \a, Version:D \b) { a cmp b != Less }
multi sub infix:«>»  (Version:D \a, Version:D \b) { a cmp b == More }

# vim: ft=perl6 expandtab sw=4
