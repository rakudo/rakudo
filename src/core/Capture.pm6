my class Capture { # declared in BOOTSTRAP
    # class Capture is Any
    #     has @!list;   # positional parameters
    #     has %!hash;   # named parameters

    method from-args(|c) { c }

    submethod BUILD(:@list, :%hash --> Nil) {
        @list.elems; # force reification of all
        nqp::bindattr(self, Capture, '@!list',
            nqp::getattr(nqp::decont(@list.list), List, '$!reified')
        );
        nqp::bindattr(self,Capture,'%!hash',
          nqp::getattr(nqp::decont(%hash),Map,'$!storage'))
            if nqp::attrinited(nqp::decont(%hash),Map,'$!storage')
    }

    multi method WHICH (Capture:D: --> ValueObjAt:D) {
        my str $WHICH = nqp::istype(self.WHAT,Capture)
          ?? 'Capture'
          !! self.^name;
        if !nqp::isnull(@!list) && @!list {
            $WHICH ~= '|';
            for nqp::hllize(@!list) -> \elem {
                $WHICH ~= ( '(' ~ elem.VAR.WHICH ~ ')' )
            }
        }
        if !nqp::isnull(%!hash) && %!hash {
            $WHICH ~= '|';
            $WHICH ~= ( $_ ~ '(' ~ nqp::atkey(%!hash, nqp::unbox_s($_)).WHICH ~ ')' )
              for nqp::hllize(%!hash).keys.sort;
        }
        nqp::box_s($WHICH,ValueObjAt)
    }

    multi method AT-KEY(Capture:D: Str:D \key) is raw {
        nqp::if(
          (nqp::isnull(%!hash) || !nqp::defined(%!hash)),
          Nil,
          nqp::ifnull(nqp::atkey(%!hash,nqp::unbox_s(key)), Nil)
        )
    }
    multi method AT-KEY(Capture:D: \key) is raw {
        nqp::if(
          (nqp::isnull(%!hash) || !nqp::defined(%!hash)),
          Nil,
          nqp::ifnull(nqp::atkey(%!hash,nqp::unbox_s(key.Str)), Nil)
        )
    }

    multi method AT-POS(Capture:D: int \pos) is raw {
        nqp::islt_i(pos,0)
          ?? Failure.new(X::OutOfRange.new(
               :what($*INDEX // 'Index'),:got(pos),:range<0..^Inf>))
          !! nqp::ifnull(nqp::atpos(@!list,pos),Nil)
    }
    multi method AT-POS(Capture:D: Int:D \pos) is raw {
        my int $pos = nqp::unbox_i(pos);
        nqp::islt_i($pos,0)
          ?? Failure.new(X::OutOfRange.new(
               :what($*INDEX // 'Index'),:got(pos),:range<0..^Inf>))
          !! nqp::ifnull(nqp::atpos(@!list,$pos),Nil)
    }

    method hash(Capture:D:) {
        nqp::if(
          (nqp::defined(%!hash) && nqp::elems(%!hash)),
          nqp::p6bindattrinvres(nqp::create(Map),Map,'$!storage',%!hash),
          nqp::create(Map)
        )
    }

    multi method EXISTS-KEY(Capture:D: Str:D \key ) {
        nqp::hllbool(nqp::existskey(%!hash, nqp::unbox_s(key)));
    }
    multi method EXISTS-KEY(Capture:D: \key ) {
        nqp::hllbool(nqp::existskey(%!hash, nqp::unbox_s(key.Str)));
    }

    method list(Capture:D:) {
        nqp::if(
          (nqp::defined(@!list) && nqp::elems(@!list)),
          nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',@!list),
          nqp::create(List)
        )
    }

    method elems(Capture:D:) {
        nqp::isnull(@!list) ?? 0 !! nqp::p6box_i(nqp::elems(@!list))
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
    multi method gist(Capture:D:) { self.Capture::perl }
    multi method perl(Capture:D:) {
        my %hash := self.Capture::hash;
        if self.^name eq 'Capture' {
            "\\({
                join ', ',
                    ((nqp::atpos(@!list, $_).perl for ^nqp::elems(@!list)) if @!list),
                    %hash.sort.map( *.perl )
            })";
        } else {
            self.^name
              ~ '.new('
              ~ ( 'list => (' ~ (nqp::atpos(@!list, $_).perl for ^nqp::elems(@!list)).join(', ') ~ ',)' if @!list)
              ~ (', ' if +@!list and +%hash)
              ~ ( 'hash => {' ~ %hash.sort.map( *.perl ).join(', ') ~ '}' if +%hash)
              ~ ')';
        }
    }
    multi method Bool(Capture:D:) {
        nqp::hllbool(
          nqp::elems(@!list) || nqp::elems(%!hash)
        )
    }

    method Capture(Capture:D:) {
        self
    }

    multi method Numeric(Capture:D:) {
        self.Capture::elems
    }

    method FLATTENABLE_LIST() is raw {
        nqp::if(nqp::isconcrete(@!list),@!list,nqp::list)
    }
    method FLATTENABLE_HASH() is raw {
        nqp::if(nqp::isconcrete(%!hash),%!hash,nqp::hash)
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

multi sub infix:<eqv>(Capture:D \a, Capture:D \b) {
    nqp::hllbool(
      nqp::eqaddr(a,b)
        || (nqp::eqaddr(a.WHAT,b.WHAT)
             && a.Capture::list eqv b.Capture::list && a.Capture::hash eqv b.Capture::hash)
    )
}

# vim: ft=perl6 expandtab sw=4
