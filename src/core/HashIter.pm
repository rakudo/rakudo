my class HashIter is Iterator {
    has $!reified;             # Parcel we return after reifying
    has Mu $!hashiter;         # the VM level hash iterator
    has Mu $!keystore;         # key store, if it's a typed hash
    has int $!mode;            # pair = 0, kv = 1, k = 2, v = 3, invert = 4
    
    method new($hash, :$keystore, :$pairs, :$kv, :$k, :$v, :$invert) { 
        my $new := nqp::create(self);
        $new.BUILD($hash, $keystore,
            $pairs  ?? 0 !!
            $kv     ?? 1 !!
            $k      ?? 2 !!
            $v      ?? 3 !!
            $invert ?? 4 !!
                       0);
        $new;
    }

    method BUILD($hash, $keystore, Int $mode) { 
        $!hashiter := nqp::iterator(nqp::getattr(nqp::decont($hash), EnumMap, '$!storage'));
        $!mode      = $mode;
        $!keystore := nqp::getattr(nqp::decont($keystore), EnumMap, '$!storage')
            if $keystore.DEFINITE;
        self 
    }
    
    method reify($n = 1000, :$sink) {
        unless nqp::isconcrete($!reified) {
            my int $count =  nqp::istype($n, Whatever) ?? 1000 !! $n.Int;
            my int $i     =  0;
            my int $mode  =  $!mode;
            my Mu $rpa    := nqp::list();
            my $it        := $!hashiter;
            
            my Mu $pairish;
            if $mode == 0 {
                if nqp::defined($!keystore) {
                    while $it && $i < $count {
                        $pairish := nqp::shift($it);
                        nqp::push($rpa, Pair.new(
                            :key(nqp::atkey($!keystore, nqp::iterkey_s($pairish))),
                            :value(nqp::hllize(nqp::iterval($pairish)))));
                        $i = $i + 1;
                    }
                }
                else {
                    while $it && $i < $count {
                        $pairish := nqp::shift($it);
                        nqp::push($rpa, Pair.new(
                            :key(nqp::p6box_s(nqp::iterkey_s($pairish))),
                            :value(nqp::hllize(nqp::iterval($pairish)))));
                        $i = $i + 1;
                    }
                }
            }
            elsif $mode == 1 {
                if nqp::defined($!keystore) {
                    while $it && $i < $count {
                        $pairish := nqp::shift($it);
                        nqp::push($rpa, nqp::atkey($!keystore, nqp::iterkey_s($pairish)).item);
                        nqp::push($rpa, nqp::hllize(nqp::iterval($pairish)).item);
                        $i = $i + 1;
                    }
                }
                else {
                    while $it && $i < $count {
                        $pairish := nqp::shift($it);
                        nqp::push($rpa, nqp::p6box_s(nqp::iterkey_s($pairish)));
                        nqp::push($rpa, nqp::hllize(nqp::iterval($pairish)).item);
                        $i = $i + 1;
                    }
                }
            }
            elsif $mode == 2 {
                if nqp::defined($!keystore) {
                    while $it && $i < $count {
                        $pairish := nqp::shift($it);
                        nqp::push($rpa, nqp::atkey($!keystore, nqp::iterkey_s($pairish)).item);
                        $i = $i + 1;
                    }
                }
                else {
                    while $it && $i < $count {
                        $pairish := nqp::shift($it);
                        nqp::push($rpa, nqp::p6box_s(nqp::iterkey_s($pairish)));
                        $i = $i + 1;
                    }
                }
            }
            elsif $mode == 3 {
                while $it && $i < $count {
                    $pairish := nqp::shift($it);
                    nqp::push($rpa, nqp::hllize(nqp::iterval($pairish)).item);
                    $i = $i + 1;
                }
            }
            elsif $mode == 4 {
                if nqp::defined($!keystore) {
                    while $it && $i < $count {
                        $pairish := nqp::shift($it);
                        nqp::push($rpa, Pair.new(
                            :value(nqp::atkey($!keystore, nqp::iterkey_s($pairish))),
                            :key(nqp::hllize(nqp::iterval($pairish)))));
                        $i = $i + 1;
                    }
                }
                else {
                    while $it && $i < $count {
                        $pairish := nqp::shift($it);
                        nqp::push($rpa, Pair.new(
                            :value(nqp::p6box_s(nqp::iterkey_s($pairish))),
                            :key(nqp::hllize(nqp::iterval($pairish)))));
                        $i = $i + 1;
                    }
                }
            }
            else {
                die "Unknown hash iteration mode";
            }

            if $it {
                my $nextiter := nqp::create(self);
                nqp::bindattr($nextiter, HashIter, '$!hashiter', $it);
                nqp::bindattr($nextiter, HashIter, '$!keystore', $!keystore);
                nqp::bindattr_i($nextiter, HashIter, '$!mode', $mode);
                nqp::push($rpa, $nextiter);
            }

            $!reified := nqp::p6parcel($rpa, nqp::null());
            # release references to objects we no longer need/own
            $!hashiter := Any;
        }
        $!reified;
    }

    method infinite() { False }

    multi method DUMP(HashIter:D: :$indent-step = 4, :%ctx?) {
        return DUMP(self, :$indent-step) unless %ctx;

        my Mu $attrs := nqp::list();
        nqp::push($attrs, '$!reified'  );
        nqp::push($attrs,  $!reified   );
        nqp::push($attrs, '$!hashiter' );
        nqp::push($attrs,  $!hashiter  );
        nqp::push($attrs, '$!keystore' );
        nqp::push($attrs,  $!keystore  );
        nqp::push($attrs, '$!mode'     );
        nqp::push($attrs,  $!mode      );
        self.DUMP-OBJECT-ATTRS($attrs, :$indent-step, :%ctx);
    }
}
