my class Junction { # declared in BOOTSTRAP
    # class Junction is Mu {
    #     has Mu $!storage;              # elements of Junction
    #     has str $!type;                # type of Junction

    method !SET-SELF(\type,\values) {
        $!type = type;
        fail "Junction Can only have 'any', 'all', 'none', 'one' type"
          unless nqp::iseq_s($!type,"any")
              || nqp::iseq_s($!type,"all") 
              || nqp::iseq_s($!type,"none") 
              || nqp::iseq_s($!type,"one");
        $!storage := values.eager.list;
        self
    }

    multi method new(Junction: \values, Str :$type!) {
        nqp::create(Junction)!SET-SELF($type,values)
    }
    multi method new(Junction: Str:D \type, \values) {
        nqp::create(Junction)!SET-SELF(type,values)
    }

    multi method Bool(Junction:D:) {
        if nqp::attrinited($!storage,List,'$!reified') {
            my $states   := nqp::getattr($!storage,List,'$!reified');
            my int $elems = nqp::elems($states);
            my int $i     = -1;

            if nqp::iseq_s($!type,'any') {
                return True if nqp::atpos($states,$i)
                  while nqp::islt_i(++$i,$elems);
                False
            }
            elsif nqp::iseq_s($!type,'all') {
                return False unless nqp::atpos($states,$i)
                  while nqp::islt_i(++$i,$elems);
                True
            }
            elsif nqp::iseq_s($!type,'none') {
                return False if nqp::atpos($states,$i)
                  while nqp::islt_i(++$i,$elems);
                True
            }
            else {
                my int $seen;
                return False if nqp::atpos($states,$i) && nqp::isgt_i(++$seen,1)
                  while nqp::islt_i(++$i,$elems);
                nqp::p6bool(nqp::iseq_i($seen,1))
            }
        }

        # all/none are True, any/one are False
        else {
            nqp::p6bool(
              nqp::iseq_s($!type,'all') || nqp::iseq_s($!type,'none')
            )
        }
    }

    multi method Str(Junction:D:) {
        self.perl
    }

    multi method ACCEPTS(Junction:U: Mu:D \topic) {
        nqp::p6bool(nqp::istype(topic, Junction));
    }
    multi method ACCEPTS(Junction:U: Any \topic) {
        nqp::p6bool(nqp::istype(topic, Junction));
    }
    multi method ACCEPTS(Junction:D: Mu \topic) {
        if nqp::attrinited($!storage,List,'$!reified') {
            my $states   := nqp::getattr($!storage,List,'$!reified');
            my int $elems = nqp::elems($states);
            my int $i     = -1;

            if nqp::iseq_s($!type,'any') {
                return True if nqp::atpos($states,$i).ACCEPTS(topic)
                  while nqp::islt_i(++$i,$elems);
                False
            }
            elsif nqp::iseq_s($!type,'all') {
                return False unless nqp::atpos($states,$i).ACCEPTS(topic)
                  while nqp::islt_i(++$i,$elems);
                True
            }
            elsif nqp::iseq_s($!type,'none') {
                return False if nqp::atpos($states,$i).ACCEPTS(topic)
                  while nqp::islt_i(++$i,$elems);
                True
            }
            else {
                my int $seen;
                return False if nqp::atpos($states,$i).ACCEPTS(topic)
                  && nqp::isgt_i(++$seen,1)
                  while nqp::islt_i(++$i,$elems);
                nqp::p6bool(nqp::iseq_i($seen,1))
            }
        }

        # all/none are True, any/one are False
        else {
            nqp::p6bool(
              nqp::iseq_s($!type,'all') || nqp::iseq_s($!type,'none')
            )
        }
    }

    multi method gist(Junction:D:) {
        if nqp::attrinited($!storage,List,'$!reified') {
            my $states   := nqp::getattr($!storage,List,'$!reified');
            my int $elems = nqp::elems($states);
            my int $i     = -1;
            my $gists    := nqp::setelems(nqp::list_s,$elems);
            nqp::bindpos_s($gists,$i,nqp::atpos($states,$i).gist)
              while nqp::islt_i(++$i,$elems);
            $!type ~ '(' ~ nqp::join(', ',$gists) ~ ')'
        }
        else {
            $!type ~ '()'
        }
    }

    multi method perl(Junction:D:) {
        if nqp::attrinited($!storage,List,'$!reified') {
            my $states   := nqp::getattr($!storage,List,'$!reified');
            my int $elems = nqp::elems($states);
            my int $i     = -1;
            my $perls    := nqp::setelems(nqp::list_s,$elems);
            nqp::bindpos_s($perls,$i,nqp::atpos($states,$i).perl)
              while nqp::islt_i(++$i,$elems);
            $!type ~ '(' ~ nqp::join(', ',$perls) ~ ')'
        }
        else {
            $!type ~ '()'
        }
    }

    method CALL-ME(|c) {
        self.AUTOTHREAD(
            -> $obj, |c { $obj(|c) },
            self, |c);
    }

    method sink(Junction:D: --> Nil) {
        if nqp::attrinited($!storage,List,'$!reified') {
            my $states   := nqp::getattr($!storage,List,'$!reified');
            my int $elems = nqp::elems($states);
            my int $i     = -1;
            nqp::atpos($states,$i).?sink while nqp::islt_i(++$i,$elems);
        }
    }

    method AUTOTHREAD(&call, |args) {
        my Mu $positionals := nqp::getattr(nqp::decont(args),Capture,'$!list');

        sub thread_junction(int $pos) {
            my $junction := nqp::decont(nqp::atpos($positionals, $pos));
            my $threaded := nqp::clone($junction);

            my $storage := nqp::getattr($junction,Junction,'$!storage');
            if nqp::attrinited($storage,List,'$!reified') {
                my $states   := nqp::getattr($storage,List,'$!reified');
                my int $elems = nqp::elems($states);
                my $result   := nqp::setelems(nqp::list,$elems);
                my int $i     = -1;
                while nqp::islt_i(++$i,$elems) {
                    # Next line is Officially Naughty, since captures are
                    # meant to be immutable. But hey, it's our capture to
                    # be naughty with...
                    nqp::bindpos($positionals,$pos,nqp::atpos($states,$i));
                    nqp::bindpos($result,$i,call(|args));
                }
                nqp::bindattr($threaded,Junction,'$!storage',
                  nqp::p6bindattrinvres(
                    nqp::create(List),List,'$!reified',$result));
            }

            $threaded
        }

        # Look for a junctional arg in the positionals.
        # we have to autothread the first all or none junction before
        # doing any one or any junctions.
        my int $first_any_one = -1;
        my int $elems = nqp::elems($positionals);
        my int $i     = -1;
        while nqp::islt_i(++$i,$elems) {

            # Junctional positional argument?
            my Mu $arg := nqp::atpos($positionals, $i);
            if nqp::istype($arg,Junction) {
                my str $type = nqp::getattr(nqp::decont($arg),Junction,'$!type');
                nqp::iseq_s($type,'any') || nqp::iseq_s($type,'one')
                  ?? $first_any_one == -1
                    ?? ($first_any_one = $i)
                    !! Nil
                  !! return thread_junction($i);
            }
        }
        return thread_junction($first_any_one) if $first_any_one >= 0;

        # Otherwise, look for one in the nameds.
        my Mu $nameds := nqp::getattr(nqp::decont(args), Capture, '$!hash');
        my $iter := nqp::iterator($nameds);
        while $iter {
            my \tmp = nqp::shift($iter);
            if nqp::istype(nqp::iterval(tmp),Junction) {
                my $junction := nqp::decont(nqp::iterval(tmp));
                my $states   := nqp::getattr(
                  nqp::getattr($junction,Junction,'$!storage'),
                  List,
                  '$!reified');
                my int $elems = nqp::elems($states);
                my $result   := nqp::setelems(nqp::list,$elems);
                my int $i     = -1;
                
                while nqp::islt_i(++$i,$elems) {
                    # also naughty, like above
                    nqp::bindkey($nameds,nqp::iterkey_s(tmp),nqp::atpos($states,$i));
                    nqp::bindpos($result,$i,call(|args));
                }

                my $threaded := nqp::clone(nqp::decont($junction));
                nqp::bindattr($threaded,Junction,'$!storage',
                  nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',$result));
                return $threaded;
            }
        }

        # If we get here, wasn't actually anything to autothread.
        call(|args);
    }
}

sub any (+values) is pure { values.any }
sub all (+values) is pure { values.all }
sub one (+values) is pure { values.one }
sub none(+values) is pure { values.none }

sub infix:<|>(+values) is pure { values.any }
sub infix:<&>(+values) is pure { values.all }
sub infix:<^>(+values) is pure { values.one }

sub AUTOTHREAD(|c) {
    Junction.AUTOTHREAD(|c)
}

sub AUTOTHREAD_METHOD($name, |c) {
    Junction.AUTOTHREAD(
        -> $obj, |c { $obj."$name"(|c) },
        |c);
}

nqp::p6setautothreader(&AUTOTHREAD);
Mu.HOW.setup_junction_fallback(Junction, &AUTOTHREAD_METHOD);

# vim: ft=perl6 expandtab sw=4
