my class KeyBag is Bag {

    method delete($k) {
        nqp::getattr(self, Bag, '%!elems').delete($k.WHICH).value;
    }
    method Bag { Bag.new-fp(nqp::getattr(self, Bag, '%!elems').values) }
    method KeyBag { self }

    method at_key($k) {
        Proxy.new(
          FETCH => {
              my $key   := $k.WHICH;
              my $elems := nqp::getattr(self, Bag, '%!elems');
              nqp::existskey($elems, nqp::unbox_s($key))
                ?? $elems{$key}.value
                !! 0;
          },
          STORE => -> $, $value {
              if $value > 0 {
                  (nqp::getattr(self, Bag, '%!elems'){$k.WHICH}
                    //= ($k => 0)).value = $value;
              }
              elsif $value == 0 {
                  my $key   := $k.WHICH;
                  my $elems := nqp::getattr(self, Bag, '%!elems');
                  if nqp::existskey($elems, nqp::unbox_s($key)) {
                      my $value = $elems{$key}.value;
                      $elems.delete($key);
                      $value;
                  }
                  else {
                      0;
                  }
              }
              else {
                  fail "Cannot put negative value $value for $k in {self.^name}";
              }
              $value;
          }
        );
    }

#    method pick($count = 1) {
#        return self.roll if $count ~~ Num && $count == 1;
#
#        my $temp-bag = KeyBag.new-fp(self.hash);
#        my $lc = $count ~~ Whatever ?? Inf !! $count;
#        gather while $temp-bag && $lc-- {
#            my $choice = $temp-bag.roll;
#            take $choice;
#            $temp-bag{$choice}--;
#        }
#    }
#    method roll($count = 1) {
#        my @inverse-mapping;
#        my $a = 0;
#        for %!elems.pairs -> $pair {
#            my $b = $a + $pair.value;
#            @inverse-mapping.push(($a..^$b) => $pair.key);
#            $a = $b;
#        }
#
#        sub choose {
#            my $choice = $a.rand;
#            my $i = 0;
#            for @inverse-mapping -> $im {
#                if $choice ~~ $im.key {
#                    return $im.value;
#                }
#            }
#            fail "Problem with KeyBag.roll";
#        }
#
#        return choose() xx * if $count ~~ Whatever;
#        return choose() if $count == 1;
#        return choose() xx $count;
#    }

    proto method classify-list(|) { * }
    multi method classify-list( &test, *@list ) {
        fail 'Cannot .classify an infinite list' if @list.infinite;
        if @list {

            # multi-level classify
            if test(@list[0]) ~~ List {
                for @list -> $l {
                    my @keys  = test($l);
                    my $last := @keys.pop;
                    my $bag   = self;
                    $bag = $bag{$_} //= self.new for @keys;
                    $bag{$last}++;
                }
            }

            # just a simple classify
            else {
                self{test $_}++ for @list;
            }
        }
        self;
    }
    multi method classify-list( %test, *@list ) {
        samewith( { %test{$^a} }, @list );
    }
    multi method classify-list( @test, *@list ) {
        samewith( { @test[$^a] }, @list );
    }

    proto method categorize-list(|) { * }
    multi method categorize-list( &test, *@list ) {
        fail 'Cannot .categorize an infinite list' if @list.infinite;
        if @list {

            # multi-level categorize
            if test(@list[0])[0] ~~ List {
                for @list -> $l {
                    for test($l) -> $k {
                        my @keys  = @($k);
                        my $last := @keys.pop;
                        my $bag   = self;
                        $bag = $bag{$_} //= self.new for @keys;
                        $bag{$last}++;
                    }
                }
            }

            # just a simple categorize
            else {
                for @list -> $l {
                    self{$_}++ for test($l);
                }
            }
        }
        self;
    }
    multi method categorize-list( %test, *@list ) {
        samewith( { %test{$^a} }, @list );
    }
    multi method categorize-list( @test, *@list ) {
        samewith( { @test[$^a] }, @list );
    }
}
