class Version {
    has $!parts;
    has int $!plus;
    has int $!whatever;
    has str $!string;
}

augment class Version {

    constant $v  = do {
        my $version := nqp::create(Version);
        nqp::bindattr(  $version,Version,'$!parts', nqp::list());
        nqp::bindattr_s($version,Version,'$!string',"");
        $version
    }
    constant $vw = do {
        my $version := nqp::create(Version);
        nqp::bindattr(  $version,Version,'$!parts',   nqp::list(*));
        nqp::bindattr_i($version,Version,'$!plus',   -1);
        nqp::bindattr_i($version,Version,'$!whatever',1);
        nqp::bindattr_s($version,Version,'$!string', "*");
        $version
    }
    constant $v6  = do {
        my $version := nqp::create(Version);
        nqp::bindattr(  $version,Version,'$!parts', nqp::list(6));
        nqp::bindattr_s($version,Version,'$!string',"6");
        $version
    }
    constant $v6c = do {
        my $version := nqp::create(Version);
        nqp::bindattr(  $version,Version,'$!parts', nqp::list(6,"c"));
        nqp::bindattr_s($version,Version,'$!string',"6.c");
        $version
    }
    constant $v6d = do {
        my $version := nqp::create(Version);
        nqp::bindattr(  $version,Version,'$!parts', nqp::list(6,"d"));
        nqp::bindattr_s($version,Version,'$!string',"6.d");
        $version
    }
    constant $v6e = do {
        my $version := nqp::create(Version);
        nqp::bindattr(  $version,Version,'$!parts', nqp::list(6,"e","PREVIEW"));
        nqp::bindattr_s($version,Version,'$!string',"6.e.PREVIEW");
        $version
    }
    constant $vplus = do {
        my $version := nqp::create(Version);
        nqp::bindattr(  $version,Version,'$!parts', nqp::list);
        nqp::bindattr_i($version,Version,'$!plus',  1);
        nqp::bindattr_s($version,Version,'$!string',"");
        $version
    }

    method !SET-SELF(\parts,\plus,\whatever,\string) {
        $!parts := nqp::getattr(parts,List,'$!reified');
        $!plus   = plus;
        $!whatever = whatever;
        $!string = string;
        self
    }

    multi method new(Version:) { $v }
    multi method new(Version: Whatever) { $vw }
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

    multi method new(Version: Str() $s) {
        nqp::if(
          nqp::iseq_s($s,'6.d'),
          $v6d,
          nqp::if(
            nqp::iseq_s($s,'6.c'),
            $v6c,
            nqp::if(
              nqp::iseq_s($s,'6'),
              $v6,
              nqp::if(
                nqp::iseq_s($s,'6.e.PREVIEW'),
                $v6e,
                nqp::unless(
                  self!SLOW-NEW($s),
                  nqp::if(
                    nqp::eqat($s, '+', nqp::sub_i(nqp::chars($s),1)),
                    $vplus,
                    self.new
                  )
                )
              )
            )
          )
        )
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
        my \oparts       := nqp::getattr(nqp::decont($other),Version,'$!parts');
        my int $oelems    = nqp::isnull(oparts) ?? 0 !! nqp::elems(oparts);
        my int $elems     = nqp::elems($!parts);
        my int $max-elems = nqp::isge_i($oelems,$elems) ?? $oelems !! $elems;

        my int $i = -1;
        while nqp::islt_i(++$i,$max-elems) {
            my $v := nqp::isge_i($i,$elems)
              ?? Whatever
              !! nqp::atpos($!parts,$i);

            # if whatever here, no more check this iteration
            unless nqp::istype($v,Whatever) {
                my $o := nqp::isge_i($i,$oelems)
                  ?? 0
                  !! nqp::atpos(oparts,$i);

                # if whatever there, no more to check this iteration
                unless nqp::istype($o,Whatever) {
                    return nqp::hllbool($!plus) if $o after  $v;
                    return False                if $o before $v;
                }
            }
        }
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
    method plus()  { nqp::hllbool($!plus) }
    method whatever() { nqp::hllbool($!whatever) }

    method Version() { self }
}


multi sub infix:<eqv>(Version:D \a, Version:D \b --> Bool:D) {
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
        (my \ia := nqp::clone(nqp::getattr(nqp::decont(a),Version,'$!parts'))),
        (my \ib := nqp::clone(nqp::getattr(nqp::decont(b),Version,'$!parts'))),
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

# vim: expandtab shiftwidth=4
