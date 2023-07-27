my class Version {
    # class Version {
    #     has $!parts;
    #     has int $!plus;
    #     has str $!string;
    # }

    # Define a constant string for Whatever so that we can use
    # nqp::eqaddr to see whether a part of the version is a Whatever
    # without actually using the Whatever type object.
    my constant star = '*';

    my constant $v  = do {
        my $version := nqp::create(Version);
        nqp::bindattr(  $version,Version,'$!parts', nqp::list());
        nqp::bindattr_s($version,Version,'$!string',"");
        $version
    }
    my constant $vstar = do {
        my $version := nqp::create(Version);
        nqp::bindattr(  $version,Version,'$!parts',  nqp::list(star));
        nqp::bindattr_i($version,Version,'$!plus',   -1);
        nqp::bindattr_s($version,Version,'$!string', '*');
        $version
    }
    my constant $v6  = do {
        my $version := nqp::create(Version);
        nqp::bindattr(  $version,Version,'$!parts', nqp::list(6));
        nqp::bindattr_s($version,Version,'$!string',"6");
        $version
    }
    my constant $v6c = do {
        my $version := nqp::create(Version);
        nqp::bindattr(  $version,Version,'$!parts', nqp::list(6,"c"));
        nqp::bindattr_s($version,Version,'$!string',"6.c");
        $version
    }
    my constant $v6d = do {
        my $version := nqp::create(Version);
        nqp::bindattr(  $version,Version,'$!parts', nqp::list(6,"d"));
        nqp::bindattr_s($version,Version,'$!string',"6.d");
        $version
    }
    my constant $v6e = do {
        my $version := nqp::create(Version);
        nqp::bindattr(  $version,Version,'$!parts', nqp::list(6,"e","PREVIEW"));
        nqp::bindattr_s($version,Version,'$!string',"6.e.PREVIEW");
        $version
    }
    my constant $v6star = do {
        my $version := nqp::create(Version);
        nqp::bindattr(  $version,Version,'$!parts', nqp::list(6,star));
        nqp::bindattr_s($version,Version,'$!string',"6.*");
        $version
    }
    my constant $vplus = do {
        my $version := nqp::create(Version);
        nqp::bindattr(  $version,Version,'$!parts', nqp::list);
        nqp::bindattr_i($version,Version,'$!plus',  1);
        nqp::bindattr_s($version,Version,'$!string',"");
        $version
    }
    my constant $vminus = do {
        my $version := nqp::create(Version);
        nqp::bindattr(  $version,Version,'$!parts', nqp::list);
        nqp::bindattr_i($version,Version,'$!plus',  -1);
        nqp::bindattr_s($version,Version,'$!string',"");
        $version
    }

    method !SET-SELF(\parts,\plus,\string) {
        $!parts := nqp::getattr(parts,List,'$!reified');
        $!plus   = plus;
        $!string = string;
        self
    }

    multi method new(Version:)               { $v      }
    multi method new(Version: '6')           { $v6     }
    multi method new(Version: '6.c')         { $v6c    }
    multi method new(Version: '6.d')         { $v6d    }
    multi method new(Version: '6.e.PREVIEW') { $v6e    }  # update on language
    multi method new(Version: '6.*')         { $v6star }  # level bump
    multi method new(Version: '*')           { $vstar  }
    multi method new(Version: Whatever)      { $vstar  }

    multi method new(Version: @parts, Str:D $string, Int() $plus = 0, $?) {
        nqp::create(self)!SET-SELF(@parts.List, $plus, $string)
    }

    method !SLOW-NEW(str $s) {
        # we comb the version string for /:r '*' || \d+ || <.alpha>+/, which
        # will become our parts. Decimal numbers are converted to Ints, and
        # the rest of the parts remain as strings
        my int $pos;
        my int $chars = nqp::chars($s);
        my int $mark;
        my $strings := nqp::list_s;
        my $parts   := nqp::list;

        nqp::while(
          nqp::islt_i($pos, $chars),
          nqp::if(
            nqp::eqat($s, '*', $pos),
            nqp::stmts( # Whatever portion
              nqp::push_s($strings, '*'),
              nqp::push($parts,star),
              ++$pos
            ),
            nqp::if(
              nqp::iscclass(nqp::const::CCLASS_NUMERIC, $s, $pos),
              nqp::stmts( # we're at the start of a numeric portion
                ($mark = $pos++),
                nqp::while( # seek the end of numeric portion
                  nqp::islt_i($pos, $chars)
                  && nqp::iscclass(nqp::const::CCLASS_NUMERIC, $s, $pos),
                  ++$pos
                ),
                nqp::push($parts, # grab numeric portion
                  nqp::atpos(
                    nqp::radix(
                      10,
                      nqp::push_s(
                        $strings,
                        nqp::substr($s, $mark, nqp::sub_i($pos, $mark))
                      ),
                      0,
                      0
                    ),
                    0
                  )
                )
              ),
              nqp::if( # same idea as for numerics, except for <.alpha> class
                nqp::iscclass(nqp::const::CCLASS_ALPHABETIC, $s, $pos)
                  || nqp::iseq_i(nqp::ord($s, $pos), 95),
                nqp::stmts( # we're at the start of a alpha portion
                  ($mark = $pos++),
                  nqp::while( # seek the end of alpha portion
                    nqp::islt_i($pos, $chars)
                    && (nqp::iscclass(nqp::const::CCLASS_ALPHABETIC, $s, $pos)
                      || nqp::iseq_i(nqp::ord($s, $pos), 95)),
                    ++$pos
                  ),
                  nqp::push($parts, # grab alpha portion
                    nqp::push_s(
                      $strings, nqp::substr($s, $mark, nqp::sub_i($pos, $mark))
                    )
                  )
                ),
                ++$pos
              )
            )
          )
        );

        nqp::if(
          nqp::elems($strings),
          nqp::stmts(
            (my str $last = nqp::substr($s,nqp::sub_i(nqp::chars($s),1))),
            (my int $plus = nqp::iseq_s($last,'+') - nqp::iseq_s($last,'-')),
            nqp::create(self)!SET-SELF($parts, $plus,
              nqp::concat(nqp::join('.',$strings),nqp::if($plus,$last,''))
            )
          ),
          Nil  # no parts found
        )
    }

    multi method new(Version: Str() $s) {
        self!SLOW-NEW($s)
          // ($s.ends-with('+') ?? $vplus !! $s.ends-with('-') ?? $vminus !! $v)
    }

    multi method Str(Version:D:)  { nqp::p6box_s($!string) }
    multi method gist(Version:D:) { nqp::concat("v",$!string) }
    multi method raku(Version:D:) {
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
        my \oparts       := nqp::getattr($other,Version,'$!parts');
        my int $oelems    = nqp::isnull(oparts) ?? 0 !! nqp::elems(oparts);
        my int $elems     = nqp::elems($!parts);
        my int $max-elems = nqp::isge_i($oelems,$elems) ?? $oelems !! $elems;

        my int $i = -1;
        nqp::while(
          nqp::islt_i(++$i,$max-elems),
          nqp::stmts(
            (my $v := nqp::if(
              nqp::isge_i($i,$elems),
              star,
              nqp::atpos($!parts,$i)
            )),

            # if whatever here, no more check this iteration
            nqp::unless(
              nqp::eqaddr($v,star),
              nqp::stmts(
                (my $o := nqp::if(
                  nqp::isge_i($i,$oelems),
                  0,
                  nqp::atpos(oparts,$i)
                )),

                # if whatever there, no more to check this iteration
                nqp::unless(
                  nqp::eqaddr($o,star),
                  nqp::if(
                    nqp::eqaddr((my $order := $o cmp $v),Order::More),
                    (return nqp::hllbool(nqp::isgt_i($!plus,0))),
                    nqp::if(
                      nqp::eqaddr($order,Order::Less),
                      (return nqp::hllbool(nqp::islt_i($!plus,0)))
                    )
                  )
                )
              )
            )
          )
        );

        True
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
    method plus()  { nqp::hllbool(nqp::iseq_i($!plus, 1)) }
    method minus() { nqp::hllbool(nqp::iseq_i($!plus,-1)) }
    method whatever() {
        my int $i     = -1;
        my int $elems = nqp::elems($!parts);
        nqp::until(
          nqp::iseq_i(++$i,$elems)
            || nqp::eqaddr(nqp::atpos($!parts,$i),star),
          nqp::null
        );
        nqp::hllbool(nqp::isne_i($i,$elems))
    }

    method Version() { self }
}


multi sub infix:<eqv>(Version:D $a, Version:D $b --> Bool:D) {
    nqp::hllbool(
      nqp::eqaddr($a,$b)
        || (nqp::eqaddr($a.WHAT,$b.WHAT)
             && (nqp::iseq_s(
                  nqp::getattr_s($a,Version,'$!string'),
                  nqp::getattr_s($b,Version,'$!string')
                ) || nqp::eqaddr(($a cmp $b),Order::Same))
           )
    )
}

multi sub infix:<cmp>(Version:D $a, Version:D $b) {
    nqp::if(
      nqp::eqaddr($a,$b), # we're us
      Same,
      nqp::stmts(
        (my \ia := nqp::clone(nqp::getattr($a,Version,'$!parts'))),
        (my \ib := nqp::clone(nqp::getattr($b,Version,'$!parts'))),
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
                  ($a-part cmp $b-part)
                )
              )),
              return $ret
            )
          )
        ),
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
                  ($a-part cmp $b-part)
                )
              )),
              return $ret
            )
          )
        ),
        (     nqp::getattr_i($a,Version,'$!plus')
          cmp nqp::getattr_i($b,Version,'$!plus')
        )
      )
    )
}

multi sub infix:«<=>»(Version:D $a, Version:D $b) { $a cmp $b         }
multi sub infix:«<»  (Version:D $a, Version:D $b) { $a cmp $b == Less }
multi sub infix:«<=» (Version:D $a, Version:D $b) { $a cmp $b != More }
multi sub infix:«==» (Version:D $a, Version:D $b) { $a cmp $b == Same }
multi sub infix:«!=» (Version:D $a, Version:D $b) { $a cmp $b != Same }
multi sub infix:«>=» (Version:D $a, Version:D $b) { $a cmp $b != Less }
multi sub infix:«>»  (Version:D $a, Version:D $b) { $a cmp $b == More }

# vim: expandtab shiftwidth=4
