my class Capture { # declared in BOOTSTRAP
    # class Capture is Any
    #     has @!list;   # positional parameters
    #     has %!hash;   # named parameters

    method from-args(|c) { c }

    submethod BUILD(:@list, :%hash --> Nil) {
        my Int:D $elems = @list.elems; # force reification of all
        nqp::bindattr(self, Capture, '@!list',
          nqp::getattr(nqp::decont(@list.list), List, '$!reified'))
            if $elems;
        my Mu $source-hash := nqp::getattr(nqp::decont(%hash), Map, '$!storage');
        nqp::bindattr(self,Capture,'%!hash', $source-hash) if nqp::ishash($source-hash);
    }

    multi method WHICH (Capture:D: --> ValueObjAt:D) {
        my Mu $WHICH := nqp::list_s(nqp::eqaddr(self.WHAT,Capture) ?? 'Capture' !! nqp::unbox_s(self.^name));
        if nqp::isconcrete(@!list) && nqp::elems(@!list) {
            nqp::push_s($WHICH, '|');
            my Mu $list := nqp::clone(@!list);
            nqp::while(
              nqp::elems($list),
              nqp::stmts(
                (my Mu \value = nqp::shift($list)),
                nqp::push_s($WHICH, '('),
                nqp::push_s($WHICH, nqp::unbox_s(value.VAR.WHICH)),
                nqp::push_s($WHICH, ')')
              )
            );
        }
        if nqp::isconcrete(%!hash) && nqp::elems(%!hash) {
            nqp::push_s($WHICH, '|');
            for nqp::hllize(%!hash).keys.sort -> str \key {
                nqp::push_s($WHICH, key);
                nqp::push_s($WHICH, '(');
                nqp::push_s($WHICH, nqp::unbox_s(nqp::atkey(%!hash,key).WHICH));
                nqp::push_s($WHICH, ')');
            }
        }
        nqp::box_s(nqp::join('',$WHICH),ValueObjAt)
    }

    multi method AT-KEY(Capture:D: Str() $key) is raw {
        nqp::isconcrete(%!hash)
          ?? nqp::ifnull(nqp::atkey(%!hash,$key), Nil)
          !! Nil
    }

    sub OUT_OF_RANGE(int $got) {
        X::OutOfRange.new(
          :what($*INDEX // 'Index'), :$got, :range<0..^Inf>
        ).Failure
    }

    multi method AT-POS(Capture:D: uint $pos) is raw {
        nqp::ifnull(nqp::atpos(@!list,$pos),Nil)
    }
    multi method AT-POS(Capture:D: Int() $pos) is raw {
        nqp::islt_i($pos,0)
          ?? OUT_OF_RANGE($pos)
          !! nqp::ifnull(nqp::atpos(@!list,$pos),Nil)
    }

    method hash(Capture:D:) {
        (nqp::isconcrete(%!hash) && nqp::elems(%!hash))
          ?? nqp::p6bindattrinvres(nqp::create(Map),Map,'$!storage',%!hash)
          !! nqp::create(Map)
    }

    multi method EXISTS-KEY(Capture:D: Str() $key) {
        nqp::hllbool(
          nqp::isconcrete(%!hash) && nqp::existskey(%!hash, $key)
        )
    }

    multi method EXISTS-POS(Capture:D: uint $pos) {
        nqp::hllbool(
          nqp::isconcrete(@!list) && nqp::existspos(@!list, $pos)
        )
    }
    multi method EXISTS-POS(Capture:D: Int() $pos) {
        nqp::islt_i($pos,0)
          ?? OUT_OF_RANGE($pos)
          !! nqp::hllbool(
               nqp::isconcrete(@!list) && nqp::existspos(@!list, $pos)
             )
    }

    method list(Capture:D:) {
        nqp::isconcrete(@!list) && nqp::elems(@!list)
          ?? nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',@!list)
          !! nqp::create(List)
    }

    multi method elems(Capture:D:) {
        nqp::isconcrete(@!list) && nqp::elems(@!list)
    }

    multi method Str(Capture:D:) {
        my Mu $str := nqp::list_s();
        if @!list {
            my Mu $iter := nqp::iterator(@!list);
            nqp::push_s($str, nqp::unbox_s(nqp::shift($iter).Str)) while $iter;
        }
        if %!hash {
            my Mu $iter := nqp::iterator(%!hash);
            while $iter {
                my $kv := nqp::shift($iter);
                nqp::push_s($str, nqp::unbox_s((nqp::p6box_s(nqp::iterkey_s($kv)) => nqp::iterval($kv).Str).Str));
            }
        }
        nqp::p6box_s(nqp::join(' ', $str))
    }

    multi method gist(Capture:D:) { self.Capture::raku }

    multi method raku(Capture:D:) {
        my int $has-list  = nqp::isconcrete(@!list) && nqp::elems(@!list);
        my int $has-hash  = nqp::isconcrete(%!hash) && nqp::elems(%!hash);
        my Mu  $raku     := nqp::list_s();
        if nqp::eqaddr(self.WHAT, Capture) {
            nqp::push_s($raku, '\(');
            if $has-list {
                my $positionals := nqp::clone(@!list);
                nqp::push_s(
                  $raku,
                  nqp::unbox_s(nqp::shift($positionals).raku(:arglist))
                );
                nqp::while(
                  nqp::elems($positionals),
                  nqp::push_s(
                    $raku,
                    nqp::concat(
                      ', ',
                      nqp::unbox_s(nqp::shift($positionals).raku(:arglist))
                    )
                  )
                );
                nqp::push_s($raku, ', ') if $has-hash;
            }
            if $has-hash {
                nqp::push_s($raku,
                    nqp::unbox_s(self.Capture::hash.sort.map(*.raku).join(', ')));
            }
            nqp::push_s($raku, ')');
        }
        else {
            nqp::push_s($raku, nqp::concat(nqp::unbox_s(self.^name), '.new'));
            if $has-list || $has-hash {
                nqp::push_s($raku, '(');
                if $has-list {
                    my $positionals := nqp::clone(@!list);
                    nqp::push_s($raku, 'list => (');
                    nqp::push_s(
                      $raku,
                      nqp::unbox_s(nqp::shift($positionals).raku(:arglist))
                    );
                    nqp::while(
                      nqp::elems($positionals),
                      nqp::push_s(
                        $raku,
                        nqp::concat(
                          ', ',
                          nqp::unbox_s(nqp::shift($positionals).raku(:arglist))
                        )
                      )
                    );
                    nqp::push_s($raku, ')');
                    nqp::push_s($raku, ', ') if $has-hash;
                }
                if $has-hash {
                    nqp::push_s($raku, 'hash => {');
                    nqp::push_s($raku, nqp::unbox_s(self.Capture::hash.sort.map(*.raku).join(', ')));
                    nqp::push_s($raku, '}');
                }
                nqp::push_s($raku, ')');
            }
        }
        nqp::p6box_s(nqp::join('', $raku))
    }

    multi method Bool(Capture:D:) {
        nqp::hllbool(
          (nqp::isconcrete(@!list) && nqp::elems(@!list))
            || (nqp::isconcrete(%!hash) && nqp::elems(%!hash))
        )
    }

    method Capture(Capture:D:) {
        self
    }

    multi method Numeric(Capture:D:) {
        self.Capture::elems
    }

    method FLATTENABLE_LIST() is raw is implementation-detail {
        nqp::isconcrete(@!list) ?? @!list !! nqp::list
    }
    method FLATTENABLE_HASH() is raw is implementation-detail {
        nqp::isconcrete(%!hash) ?? %!hash !! nqp::hash
    }

    multi method keys(Capture:D:) {
        (self.Capture::list.keys, self.Capture::hash.keys).flat;
    }
    multi method kv(Capture:D:) {
        (self.Capture::list.kv, self.Capture::hash.kv).flat;
    }
    multi method values(Capture:D:) {
        (self.Capture::list.values, self.Capture::hash.values).flat;
    }
    multi method pairs(Capture:D:) {
        (self.Capture::list.pairs, self.Capture::hash.pairs).flat;
    }
    multi method antipairs(Capture:D:) {
        (self.Capture::list.antipairs, self.Capture::hash.antipairs).flat;
    }
}

multi sub infix:<eqv>(Capture:D $a, Capture:D $b --> Bool:D) {
    nqp::hllbool(
      nqp::eqaddr($a,$b)
        || (nqp::eqaddr($a.WHAT,$b.WHAT)
             && $a.Capture::list eqv $b.Capture::list
             && $a.Capture::hash eqv $b.Capture::hash)
    )
}

# vim: expandtab shiftwidth=4
