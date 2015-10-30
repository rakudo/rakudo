my class Capture { # declared in BOOTSTRAP
    # class Capture is Any {
    #     has Mu $!list;   # positional parameters
    #     has Mu $!hash;   # named parameters

    method new(:@list,:%hash) { self.bless(:@list,:%hash) }

    method from-args(|c) { c }

    submethod BUILD(:@list, :%hash) {
        @list.elems; # force reification of all
        nqp::bindattr(self, Capture, '$!list',
            nqp::getattr(nqp::decont(@list.list), List, '$!reified')
        );
        my Mu $hs := nqp::getattr(nqp::decont(%hash), Map, '$!storage');
        nqp::bindattr(self, Capture, '$!hash', nqp::ishash($hs) ?? $hs !! nqp::hash());
    }

    multi method WHICH (Capture:D:) {
        my $WHICH = self.^name;
        if !nqp::isnull($!list) && $!list {
            $WHICH ~= '|';
            $WHICH ~= ( '(' ~ $_.WHICH ~ ')' )
              for nqp::hllize($!list);
        }
        if !nqp::isnull($!hash) && $!hash {
            $WHICH ~= '|';
            $WHICH ~= ( $_ ~ '(' ~ nqp::atkey($!hash, nqp::unbox_s($_)).WHICH ~ ')' )
              for nqp::hllize($!hash).keys.sort;
        }
        $WHICH;
    }

    multi method AT-KEY(Capture:D: \key) is raw {
        my str $skey = nqp::unbox_s(key.Str);
        nqp::existskey($!hash,$skey) ?? nqp::atkey($!hash, $skey) !! Nil;
    }
    multi method AT-KEY(Capture:D: Str:D \key) is raw {
        my str $skey = nqp::unbox_s(key);
        nqp::existskey($!hash,$skey) ?? nqp::atkey($!hash, $skey) !! Nil;
    }

    multi method AT-POS(Capture:D: int \pos) is raw {
        fail X::OutOfRange.new(
          :what($*INDEX // 'Index'),:got(pos),:range<0..Inf>)
            if nqp::islt_i(pos,0);
        nqp::existspos($!list,pos) ?? nqp::atpos($!list,pos) !! Nil;
    }
    multi method AT-POS(Capture:D: Int:D \pos) is raw {
        my int $pos = nqp::unbox_i(pos);
        fail X::OutOfRange.new(
          :what($*INDEX // 'Index'),:got(pos),:range<0..Inf>)
          if nqp::islt_i($pos,0);
        nqp::existspos($!list,$pos) ?? nqp::atpos($!list,$pos) !! Nil;
    }

    method hash(Capture:D:) {
        my $enum := nqp::create(Map);
        nqp::bindattr($enum, Map, '$!storage', $!hash);
        $enum;
    }

    multi method EXISTS-KEY(Capture:D: Str:D \key ) {
        nqp::p6bool(nqp::existskey($!hash, nqp::unbox_s(key)));
    }
    multi method EXISTS-KEY(Capture:D: \key ) {
        nqp::p6bool(nqp::existskey($!hash, nqp::unbox_s(key.Str)));
    }

    method list(Capture:D:) {
        nqp::isnull($!list)
            ?? ()
            !! nqp::p6bindattrinvres(nqp::create(List), List, '$!reified', $!list)
    }

    method elems(Capture:D:) {
        nqp::isnull($!list) ?? 0 !! nqp::p6box_i(nqp::elems($!list))
    }

    multi method Str(Capture:D:) {
        my Mu $str := nqp::list_s();
        if $!list {
            my Mu $iter := nqp::iterator($!list);
            nqp::push_s($str, nqp::unbox_s(nqp::shift($iter).Str)) while $iter;
        }
        if $!hash {
            my Mu $iter := nqp::iterator($!hash);
            while $iter {
                my $kv := nqp::shift($iter);
                nqp::push_s($str, nqp::unbox_s((nqp::p6box_s($kv) => $kv.value).Str));
            }
        }
        nqp::p6box_s(nqp::join(' ', $str))
    }
    multi method gist(Capture:D:) { self.perl }
    multi method perl(Capture:D:) {
        my %hash := self.hash;
        if self.^name eq 'Capture' {
            "\\({
                join ', ',
                    ((nqp::atpos($!list, $_).perl for ^nqp::elems($!list)) if $!list),
                    %hash.sort.map( *.perl )
            })";
        } else {
            self.^name
              ~ '.new('
              ~ ( 'list => (' ~ (nqp::atpos($!list, $_).perl for ^nqp::elems($!list)).join(', ') ~ ',)' if $!list)
              ~ (', ' if +$!list and +%hash)
              ~ ( 'hash => {' ~ %hash.sort.map( *.perl ).join(', ') ~ '}' if +%hash)
              ~ ')';
        }
    }
    multi method Bool(Capture:D:) {
        $!list || $!hash ?? True !! False
    }

    method Capture(Capture:D:) {
        self
    }

    multi method Numeric(Capture:D:) {
        self.elems
    }

    method FLATTENABLE_LIST() { $!list ?? $!list !! nqp::list() }
    method FLATTENABLE_HASH() { $!hash ?? $!hash !! nqp::hash() }

    multi method keys(Capture:D:) {
        (self.list.keys, self.hash.keys).flat;
    }
    multi method kv(Capture:D:) {
        (self.list.kv, self.hash.kv).flat;
    }
    multi method values(Capture:D:) {
        (self.list.values, self.hash.values).flat;
    }
    multi method pairs(Capture:D:) {
        (self.list.pairs, self.hash.pairs).flat;
    }
    multi method antipairs(Capture:D:) {
        (self.list.antipairs, self.hash.antipairs).flat;
    }
}

multi sub infix:<eqv>(Capture $a, Capture $b) {
    $a.WHAT === $b.WHAT && $a.list eqv $b.list && $a.hash eqv $b.hash
}

# vim: ft=perl6 expandtab sw=4
