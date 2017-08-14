my class Junction { # declared in BOOTSTRAP
    # class Junction is Mu
    #     has Mu $!storage;              # elements of Junction
    #     has str $!type;                # type of Junction

    method !SET-SELF(\type,\values) {
        nqp::stmts(
          ($!type = type),
          nqp::if(
            nqp::iseq_s($!type,"any")
              || nqp::iseq_s($!type,"all")
              || nqp::iseq_s($!type,"none")
              || nqp::iseq_s($!type,"one"),
            nqp::stmts(
              ($!storage := nqp::if(
                nqp::isconcrete(
                  $_ := nqp::getattr(values.eager.list,List,'$!reified')),
                $_,
                nqp::create(IterationBuffer))),
              self
            ),
            Failure.new("Junction can only have 'any', 'all', 'none', 'one' type")
          )
        )
    }

    proto method new(|) { * }
    multi method new(Junction: \values, Str :$type!) {
        nqp::create(Junction)!SET-SELF($type,values)
    }
    multi method new(Junction: Str:D \type, \values) {
        nqp::create(Junction)!SET-SELF(type,values)
    }

    method defined(Junction:D:) {
        nqp::stmts(
          (my int $elems = nqp::elems($!storage)),
          (my int $i = 0),
          nqp::if(
            nqp::iseq_s($!type,'any'),
            nqp::stmts(
              nqp::while(
                (nqp::islt_i($i,$elems)
                  && nqp::if(nqp::atpos($!storage,$i).defined,0,1)),
                ($i = nqp::add_i($i,1))
              ),
              nqp::p6bool(nqp::islt_i($i,$elems))
            ),
            nqp::if(
              nqp::iseq_s($!type,'all'),
              nqp::stmts(
                nqp::while(
                  (nqp::islt_i($i,$elems)
                    && nqp::atpos($!storage,$i).defined),
                  ($i = nqp::add_i($i,1))
                ),
                nqp::p6bool(nqp::iseq_i($i,$elems))
              ),
              nqp::if(
                nqp::iseq_s($!type,'none'),
                nqp::stmts(
                  nqp::while(
                    (nqp::islt_i($i,$elems)
                      && nqp::if(nqp::atpos($!storage,$i).defined,0,1)),
                    ($i = nqp::add_i($i,1))
                  ),
                  nqp::p6bool(nqp::iseq_i($i,$elems))
                ),
                nqp::stmts(    # $!type eq 'one'
                  (my int $seen = 0),
                  ($i = nqp::sub_i($i,1)),  # increment in condition
                  nqp::while(
                    (nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
                      && nqp::isle_i($seen,1)),
                    nqp::if(
                      nqp::atpos($!storage,$i).defined,
                      ($seen = nqp::add_i($seen,1))
                    )
                  ),
                  nqp::p6bool(nqp::iseq_i($seen,1))
                )
              )
            )
          )
        )
    }

    multi method Bool(Junction:D:) {
        nqp::stmts(
          (my int $elems = nqp::elems($!storage)),
          (my int $i = 0),
          nqp::if(
            nqp::iseq_s($!type,'any'),
            nqp::stmts(
              nqp::while(
                (nqp::islt_i($i,$elems)
                  && nqp::if(nqp::atpos($!storage,$i),0,1)),
                ($i = nqp::add_i($i,1))
              ),
              nqp::p6bool(nqp::islt_i($i,$elems))
            ),
            nqp::if(
              nqp::iseq_s($!type,'all'),
              nqp::stmts(
                nqp::while(
                  (nqp::islt_i($i,$elems)
                    && nqp::atpos($!storage,$i)),
                  ($i = nqp::add_i($i,1))
                ),
                nqp::p6bool(nqp::iseq_i($i,$elems))
              ),
              nqp::if(
                nqp::iseq_s($!type,'none'),
                nqp::stmts(
                  nqp::while(
                    (nqp::islt_i($i,$elems)
                      && nqp::if(nqp::atpos($!storage,$i),0,1)),
                    ($i = nqp::add_i($i,1))
                  ),
                  nqp::p6bool(nqp::iseq_i($i,$elems))
                ),
                nqp::stmts(    # $!type eq 'one'
                  (my int $seen = 0),
                  ($i = nqp::sub_i($i,1)),  # increment in condition
                  nqp::while(
                    (nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
                      && nqp::isle_i($seen,1)),
                    nqp::if(
                      nqp::atpos($!storage,$i),
                      ($seen = nqp::add_i($seen,1))
                    )
                  ),
                  nqp::p6bool(nqp::iseq_i($seen,1))
                )
              )
            )
          )
        )
    }

    multi method ACCEPTS(Junction:U: Mu:D \topic) {
        nqp::p6bool(nqp::istype(topic, Junction));
    }
    multi method ACCEPTS(Junction:U: Any \topic) {
        nqp::p6bool(nqp::istype(topic, Junction));
    }
    multi method ACCEPTS(Junction:D: Mu \topic) {
        nqp::stmts(
          (my int $elems = nqp::elems($!storage)),
          (my int $i = 0),
          nqp::if(
            nqp::iseq_s($!type,'any'),
            nqp::stmts(
              nqp::while(
                (nqp::islt_i($i,$elems)
                  && nqp::if(nqp::atpos($!storage,$i).ACCEPTS(topic),0,1)),
                ($i = nqp::add_i($i,1))
              ),
              nqp::p6bool(nqp::islt_i($i,$elems))
            ),
            nqp::if(
              nqp::iseq_s($!type,'all'),
              nqp::stmts(
                nqp::while(
                  (nqp::islt_i($i,$elems)
                    && nqp::atpos($!storage,$i).ACCEPTS(topic)),
                  ($i = nqp::add_i($i,1))
                ),
                nqp::p6bool(nqp::iseq_i($i,$elems))
              ),
              nqp::if(
                nqp::iseq_s($!type,'none'),
                nqp::stmts(
                  nqp::while(
                    (nqp::islt_i($i,$elems)
                      && nqp::if(nqp::atpos($!storage,$i).ACCEPTS(topic),0,1)),
                    ($i = nqp::add_i($i,1))
                  ),
                  nqp::p6bool(nqp::iseq_i($i,$elems))
                ),
                nqp::stmts(    # $!type eq 'one'
                  (my int $seen = 0),
                  ($i = nqp::sub_i($i,1)),  # increment in condition
                  nqp::while(
                    (nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
                      && nqp::isle_i($seen,1)),
                    nqp::if(
                      nqp::atpos($!storage,$i).ACCEPTS(topic),
                      ($seen = nqp::add_i($seen,1))
                    )
                  ),
                  nqp::p6bool(nqp::iseq_i($seen,1))
                )
              )
            )
          )
        )
    }

    multi method Str(Junction:D:) {
        nqp::stmts(
          (my $storage := nqp::bindattr(
            (my $junction := nqp::clone(self)),
            Junction,
            '$!storage',
            nqp::clone(nqp::getattr(self,Junction,'$!storage'))
          )),
          (my int $elems = nqp::elems($storage)),
          (my int $i = -1),
          nqp::while(
            nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
            nqp::unless(
              nqp::istype(nqp::atpos($storage,$i),Str),
              nqp::bindpos($storage,$i,nqp::atpos($storage,$i).Str)
            )
          ),
          $junction
        )
    }

    multi method gist(Junction:D:) {
        my int $elems = nqp::elems($!storage);
        my int $i     = -1;
        my $gists    := nqp::setelems(nqp::list_s,$elems);
        nqp::bindpos_s($gists,$i,nqp::atpos($!storage,$i).gist)
          while nqp::islt_i(++$i,$elems);
        $!type ~ '(' ~ nqp::join(', ',$gists) ~ ')'
    }

    multi method perl(Junction:D:) {
        my int $elems = nqp::elems($!storage);
        my int $i     = -1;
        my $perls    := nqp::setelems(nqp::list_s,$elems);
        nqp::bindpos_s($perls,$i,nqp::atpos($!storage,$i).perl)
          while nqp::islt_i(++$i,$elems);
        $!type ~ '(' ~ nqp::join(', ',$perls) ~ ')'
    }

    method CALL-ME(|c) {
        self.AUTOTHREAD(
            -> $obj, |c { $obj(|c) },
            self, |c);
    }

    method sink(Junction:D: --> Nil) {
        my int $elems = nqp::elems($!storage);
        my int $i     = -1;
        nqp::atpos($!storage,$i).sink while nqp::islt_i(++$i,$elems);
    }

    method AUTOTHREAD(&call, |args) {
        my Mu $positionals := nqp::getattr(nqp::decont(args),Capture,'@!list');

        sub thread_junction(int $pos) {
            my $junction := nqp::decont(nqp::atpos($positionals, $pos));
            my $storage := nqp::getattr($junction,Junction,'$!storage');
            my int $elems = nqp::elems($storage);
            my $result   := nqp::setelems(nqp::list,$elems);
            my int $i     = -1;
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              # Next line is Officially Naughty, since captures are
              # meant to be immutable. But hey, it's our capture to
              # be naughty with...
              nqp::stmts(
                nqp::bindpos($positionals,$pos,nqp::atpos($storage,$i)),
                nqp::bindpos($result,$i,call(|args))
              )
            );
            nqp::p6bindattrinvres(
              nqp::clone($junction),Junction,'$!storage',$result)
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
                my str $type = nqp::getattr_s(nqp::decont($arg),Junction,'$!type');
                nqp::iseq_s($type,'any') || nqp::iseq_s($type,'one')
                  ?? $first_any_one == -1
                    ?? ($first_any_one = $i)
                    !! Nil
                  !! return thread_junction($i);
            }
        }
        return thread_junction($first_any_one) if $first_any_one >= 0;

        # Otherwise, look for one in the nameds.
        my Mu $nameds := nqp::getattr(nqp::decont(args), Capture, '%!hash');
        my $iter := nqp::iterator($nameds);
        while $iter {
            if nqp::istype(nqp::iterval(nqp::shift($iter)),Junction) {
                my $junction := nqp::decont(nqp::iterval($iter));
                my $storage  := nqp::getattr($junction,Junction,'$!storage');
                my int $elems = nqp::elems($storage);
                my $result   := nqp::setelems(nqp::list,$elems);
                my int $i     = -1;

                while nqp::islt_i(++$i,$elems) {
                    # also naughty, like above
                    nqp::bindkey($nameds,nqp::iterkey_s($iter),nqp::atpos($storage,$i));
                    nqp::bindpos($result,$i,call(|args));
                }

                my $threaded := nqp::clone(nqp::decont($junction));
                nqp::bindattr($threaded,Junction,'$!storage',$result);
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

multi sub infix:<~>(Str:D $a, Junction:D $b) {
    nqp::if(
      $a,
      nqp::stmts(                                # something to concat with
        (my $storage := nqp::bindattr(
          (my $junction := nqp::clone($b)),
          Junction,
          '$!storage',
          nqp::clone(nqp::getattr($b,Junction,'$!storage'))
        )),
        (my int $elems = nqp::elems($storage)),
        (my int $i = -1),
        nqp::while(
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
          nqp::bindpos($storage,$i,
            nqp::concat(
              $a,
              nqp::if(
                nqp::istype((my $val := nqp::atpos($storage,$i)),Str),
                $val,
                $val.Str
              )
            )
          )
        ),
        $junction
      ),
      $b.Str                                     # nothing to concat with
    )
}

multi sub infix:<~>(Junction:D $a, Str:D $b) {
    nqp::if(
      $b,
      nqp::stmts(                                # something to concat with
        (my $storage := nqp::bindattr(
          (my $junction := nqp::clone($a)),
          Junction,
          '$!storage',
          nqp::clone(nqp::getattr($a,Junction,'$!storage'))
        )),
        (my int $elems = nqp::elems($storage)),
        (my int $i = -1),
        nqp::while(
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
          nqp::bindpos($storage,$i,
            nqp::concat(
              nqp::if(
                nqp::istype((my $val := nqp::atpos($storage,$i)),Str),
                $val,
                $val.Str
              ),
              $b
            )
          )
        ),
        $junction
      ),
      $a.Str                                     # nothing to concat with
    )
}

sub AUTOTHREAD(|c) {
    Junction.AUTOTHREAD(|c)
}

sub AUTOTHREAD_METHOD($name, |c) {
    Junction.AUTOTHREAD(
        -> \obj, |c { obj."$name"(|c) },
        |c);
}

nqp::p6setautothreader(&AUTOTHREAD);
Mu.HOW.setup_junction_fallback(Junction, &AUTOTHREAD_METHOD);

# vim: ft=perl6 expandtab sw=4
