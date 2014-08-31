my class Capture { # declared in BOOTSTRAP
    # class Capture is Any {
    #     has Mu $!list;   # positional parameters
    #     has Mu $!hash;   # named parameters

    submethod BUILD(:@list, :%hash) {
        nqp::bindattr(self, Capture, '$!list',
            nqp::getattr(nqp::decont(@list.Parcel), Parcel, '$!storage')
        );
        my Mu $hs := nqp::getattr(nqp::decont(%hash), EnumMap, '$!storage');
        nqp::bindattr(self, Capture, '$!hash', nqp::ishash($hs) ?? $hs !! nqp::hash());
        1;
    }
    submethod WHICH {
        my $WHICH = self.^name;
        if self.defined {
            if $!list {
                $WHICH ~= '|';
                $WHICH ~= ( '(' ~ $_.WHICH ~ ')' )
                  for $!list;
            }
            if $!hash {
                $WHICH ~= '|';
                $WHICH ~= ( $_ ~ '(' ~ $!hash{nqp::unbox_s($_)}.WHICH ~ ')' )
                  for self.hash.keys.sort;
            }
        }
        $WHICH;
    }

    method at_key(Capture:D: $key is copy) {
        $key = $key.Str;
        nqp::existskey($!hash, nqp::unbox_s($key))
          ?? nqp::atkey($!hash, nqp::unbox_s($key))
          !! Any
    }

    method at_pos(Capture:D: $pos is copy) {
        $pos = $pos.Int;
        nqp::existspos($!list, nqp::unbox_i($pos))
          ?? nqp::atpos($!list, nqp::unbox_i($pos))
          !! Any
    }

    method hash(Capture:D:) {
        my $enum := nqp::create(EnumMap);
        nqp::bindattr($enum, EnumMap, '$!storage', $!hash);
        $enum;
    }

    method exists (Capture:D: $key ) {  # is DEPRECATED doesn't work in settings
        DEPRECATED("the :exists adverb");
        self.exists_key($key);
    }
    method exists_key(Capture:D: $key ) {
        nqp::p6bool(nqp::existskey($!hash, nqp::unbox_s($key.Str)));
    }

    method list(Capture:D:) {
        nqp::p6list(nqp::clone($!list), List, Mu);
    }
    
    method elems(Capture:D:) {
        nqp::p6box_i(nqp::elems($!list))
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
    multi method gist(Capture:D:) {
        my @list := self.list;
        my %hash := self.hash;
        '\('
          ~ (@list.map( {.gist} ).join: ', ' if +@list)
          ~ (', ' if +@list and +%hash)
          ~ (%hash.keys.sort.map( { $_.gist ~ ' => ' ~ %hash{$_}.gist } ).join: ', ' if +%hash)
          ~ ')';
    }
    multi method perl(Capture:D:) {
        my @list := self.list;
        my %hash := self.hash;
        self.^name
          ~ '.new('
          ~ ( 'list => (' ~ @list.map( {.perl} ).join(', ') ~ ',)' if +@list)
          ~ (', ' if +@list and +%hash)
          ~ ( 'hash => {' ~ %hash.keys.pick(*).map( { $_.perl ~ ' => ' ~ %hash{$_}.perl } ).join(', ') ~ '}' if +%hash)
          ~ ')';
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

    method pairs(Capture:D:) {
        (self.list.pairs, self.hash.pairs).flat
    }
    method values(Capture:D:) {
        (self.list.values, self.hash.values).flat
    }
    method keys(Capture:D:) {
        (self.list.keys, self.hash.keys).flat
    }
    method kv(Capture:D:) {
        (self.list.kv, self.hash.kv).flat
    }
}

multi sub infix:<eqv>(Capture $a, Capture $b) {
    $a.WHAT === $b.WHAT && $a.list eqv $b.list && $a.hash eqv $b.hash
}

# vim: ft=perl6 expandtab sw=4
