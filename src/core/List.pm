# A List is a (potentially infite) immutable list. The immutability is not
# deep; a List may contain Scalar containers that can be assigned to. However,
# it is not possible to shift/unshift/push/pop/splice/bind. A List is also
# Positional, and so may be indexed.
my class List does Iterable does Positional { # declared in BOOTSTRAP
    # class List is Cool
    #   The reified elements in the list so far (that is, those that we already
    #   have produced the values for).
    #   has List $!reified;
    #
    #   Object that reifies the rest of the list. We don't just inline it into
    #   the List class itself, because a STORE on Array can clear things and
    #   upset an ongoing iteration. (An easy way to create such a case is to
    #   assign an array with lazy parts into itself.)
    #   has $!todo;

    # The object that goes into $!todo.
    class Reifier {
        # Our copy of the reified elements in the list so far.
        has $!reified;

        # The current iterator, if any, that we're working our way through in
        # order to lazily reify values. Must be depleted before $!future is
        # considered.
        has Iterator $!current-iter;

        # The (possibly lazy) values we've not yet incorporated into the list. The
        # only thing we can't simply copy from $!future into $!reified is a Slip
        # (and so the only reason to have a $!future is that there is at least one
        # Slip).
        has $!future;

        # The reification target (what .reify-* will .push to). Exists so we can
        # share the reification code between List/Array. List just uses its own
        # $!reified buffer; the Array one shoves stuff into Scalar containers
        # first.
        has $!reification-target;

        method reify-at-least(int $elems) {
            nqp::stmts(
              nqp::if(
                ($!current-iter.DEFINITE
                  && nqp::eqaddr(
                       $!current-iter.push-at-least(
                         $!reification-target,
                         nqp::sub_i($elems,nqp::elems($!reified))
                       ),
                       IterationEnd
                     )),
                $!current-iter := Iterator
              ),

              # there is a future
              nqp::if(
                $!future.DEFINITE,

                # still need and can get something from the future
                nqp::stmts(
                  nqp::while(
                    (nqp::islt_i(nqp::elems($!reified),$elems)
                      && nqp::elems($!future)),
                    nqp::if(
                      (nqp::istype((my $current := nqp::shift($!future)),Slip)
                        && nqp::isconcrete($current)),
                      nqp::stmts(
                        (my $iter := $current.iterator),
                        nqp::unless(
                          nqp::eqaddr(
                            $iter.push-at-least(
                              $!reification-target,
                              nqp::sub_i($elems,nqp::elems($!reified))
                            ),
                            IterationEnd
                          ),
                          # The iterator produced enough values to fill the need,
                          # but did not reach its end. We save it for next time.
                          # We know we'll exit the loop, since the < $elems check
                          # must be False (unless the iterator broke contract).
                          ($!current-iter := $iter)
                        )
                      ),
                      $!reification-target.push($current)
                    )
                  ),

                  # that was the future
                  nqp::unless(
                    nqp::elems($!future),
                    ($!future := Mu)
                  )
                )
              ),

              nqp::elems($!reified)
            )
        }

        method reify-until-lazy() {
            nqp::stmts(
              nqp::if(
                ($!current-iter.DEFINITE
                  && nqp::eqaddr(
                       $!current-iter.push-until-lazy($!reification-target),
                       IterationEnd
                     )
                ),
                $!current-iter := Iterator
              ),

              nqp::if(
                ($!future.DEFINITE && nqp::not_i($!current-iter.DEFINITE)),
                nqp::stmts(
                  nqp::while(
                    nqp::elems($!future),
                    nqp::if(
                      (nqp::istype((my $current := nqp::shift($!future)),Slip)
                        && nqp::isconcrete($current)),
                      nqp::unless(
                        nqp::eqaddr(
                          (my $iter := $current.iterator).push-until-lazy(
                            $!reification-target),
                          IterationEnd
                        ),
                        nqp::stmts(
                          ($!current-iter := $iter),
                          last
                        )
                      ),
                      $!reification-target.push($current)
                    )
                  ),
                  nqp::unless(
                    nqp::elems($!future),
                    $!future := Mu
                  )
                )
              ),
              nqp::elems($!reified)
            )
        }

        method reify-all() {
            nqp::stmts(
              nqp::if(
                $!current-iter.DEFINITE,
                nqp::stmts(
                  $!current-iter.push-all($!reification-target),
                  $!current-iter := Iterator
                )
              ),
              nqp::if(
                $!future.DEFINITE,
                nqp::stmts(
                  nqp::while(
                    nqp::elems($!future),
                    nqp::if(
                      (nqp::istype((my $current := nqp::shift($!future)),Slip)
                        && nqp::isconcrete($current)),
                      $current.iterator.push-all($!reification-target),
                      $!reification-target.push($current)
                    )
                  ),
                  ($!future := Mu)
                )
              ),
              nqp::elems($!reified)
            )
        }

        method fully-reified() {
            !($!current-iter.DEFINITE || $!future.DEFINITE)
        }

        method is-lazy() {
            nqp::if(
              $!current-iter.DEFINITE,
              $!current-iter.is-lazy
            )
        }
    }

    method from-iterator(List:U: Iterator $iter) {
        nqp::stmts(
          (my \buffer := nqp::create(IterationBuffer)),
          nqp::bindattr(
            (my \result := nqp::create(self)),List,'$!reified',buffer),
          nqp::bindattr(
            (my \todo := nqp::create(Reifier)),Reifier,'$!reified',buffer),
          nqp::bindattr(todo,Reifier,'$!current-iter',$iter),
          # since Array has its own from-iterator, we don't need to
          # call reification-target, because it is the same as buffer
          nqp::bindattr(todo,Reifier,'$!reification-target',buffer),
          nqp::p6bindattrinvres(result,List,'$!todo',todo)
        )
    }

    method from-slurpy(|) {
        my \result      := nqp::create(self);
        my Mu \vm-tuple := nqp::captureposarg(nqp::usecapture,1);

        nqp::if(
          nqp::isgt_i(nqp::elems(vm-tuple),0),
          nqp::stmts(
            nqp::bindattr(result,List,'$!reified',
              my \buffer := nqp::create(IterationBuffer)),
            nqp::bindattr(result,List,'$!todo',
              my \todo   := nqp::create(List::Reifier)),
            nqp::bindattr(todo,List::Reifier,'$!reified',
              buffer),
            nqp::bindattr(todo,List::Reifier,'$!reification-target',
              result.reification-target),
            nqp::bindattr(todo,List::Reifier,'$!future',vm-tuple)
          )
        );

        result
    }

    method from-slurpy-onearg(|) {
        my Mu \vm-tuple := nqp::captureposarg(nqp::usecapture, 1);
        my $result;
        my $buffer;
        my $todo;
        my $consider;

        nqp::if(
          nqp::isgt_i(nqp::elems(vm-tuple),1),
          nqp::stmts(  # handle as slurpy
            nqp::bindattr(($result := nqp::create(self)),List,'$!reified',
              $buffer := nqp::create(IterationBuffer)),
            nqp::bindattr($result,List,'$!todo',
              $todo   := nqp::create(List::Reifier)),
            nqp::bindattr($todo,List::Reifier,'$!reified',
              $buffer),
            nqp::bindattr($todo,List::Reifier,'$!reification-target',
              $result.reification-target),
            nqp::bindattr($todo,List::Reifier,'$!future',vm-tuple),
            $result
          ),
          nqp::if(
            nqp::iseq_i(nqp::elems(vm-tuple),1),
            nqp::if(  # single arg semantics active
              nqp::istype(($consider := nqp::atpos(vm-tuple,0)),Seq),
              nqp::if(  # a single Seq
                nqp::istype(self,Array),
                $consider.cache,
                $consider
              ),
              nqp::stmts( # something else
                nqp::bindattr(($result := nqp::create(self)),List,'$!reified',
                  $buffer := nqp::create(IterationBuffer)),
                nqp::bindattr($result,List,'$!todo',
                  $todo   := nqp::create(List::Reifier)),
                nqp::bindattr($todo,List::Reifier,'$!reified',
                  $buffer),
                nqp::bindattr($todo,List::Reifier,'$!reification-target',
                  $result.reification-target),
                nqp::if(
                  nqp::iscont($consider)
                    || nqp::not_i(nqp::istype($consider,Iterable))
                    || nqp::not_i(nqp::p6definite($consider)),
                  nqp::bindattr($todo,List::Reifier,'$!future',
                    vm-tuple),
                  nqp::bindattr($todo,List::Reifier,'$!future',
                    nqp::list($consider.list.Slip))
                ),
                $result
              )
            ),
            nqp::create(self)  # no args, so just a bare object
          )
        )
    }

    method from-slurpy-flat(|) {

        nqp::if(
          (my int $elems = nqp::elems(
            (my Mu $vm-tuple := nqp::captureposarg(nqp::usecapture,1))
          )),
          nqp::stmts(
            (my $future := nqp::setelems(nqp::create(IterationBuffer),$elems)),
            (my int $i   = -1),
            (my int $b   = 0),
            nqp::while(
              nqp::islt_i($i = nqp::add_i($i,1),$elems),
              nqp::if(
                nqp::iscont(my $consider := nqp::atpos($vm-tuple,$i)),
                nqp::bindpos($future,$i,$consider),
                nqp::if(
                  (nqp::istype($consider,Iterable) && $consider.DEFINITE),
                  nqp::if(
                    nqp::istype($consider,PositionalBindFailover),
                    nqp::bindpos($future,$i,$consider.cache.flat.Slip),
                    nqp::bindpos($future,$i,$consider.flat.Slip)
                  ),
                  nqp::stmts(
                    nqp::bindpos($future,$i,$consider),
                    ($b = nqp::add_i($b,1))
                  )
                )
              )
            ),
            nqp::if(
              nqp::iseq_i($b,$elems),

              # we already reified everything
              nqp::p6bindattrinvres(nqp::create(self),List,'$!reified',$future),

              # need full fledged List with a $todo
              nqp::stmts(
                (my $result :=
                  nqp::p6bindattrinvres(nqp::create(self),List,'$!reified',
                    (my $buffer := nqp::create(IterationBuffer))
                  )
                ),
                nqp::bindattr($result,List,'$!todo',
                  (my $todo := nqp::create(List::Reifier))
                ),
                nqp::bindattr($todo,List::Reifier,'$!reified',
                  $buffer
                ),
                nqp::bindattr($todo,List::Reifier,'$!reification-target',
                  $result.reification-target
                ),
                nqp::bindattr($todo,List::Reifier,'$!future',
                  $future
                ),
                $result
              )
            )
          ),

          # no args, an empty list suffices
          nqp::create(self)
        )
    }

    method new(**@things) {
        my \list = nqp::create(self);
        my \iterbuffer = nqp::create(IterationBuffer);
        nqp::bindattr(list, List, '$!reified', iterbuffer);

        my int $elems = +@things;  # reify
        my int $i     = -1;
        my $reified  := nqp::getattr(@things,List,'$!reified');

        nqp::while(  # doesn't sink
          nqp::islt_i($i = nqp::add_i($i,1),$elems),
          nqp::bindpos(iterbuffer,$i,(nqp::atpos($reified,$i)))
        );
        list
    }

    multi method Bool(List:D:) {
        nqp::p6bool(
          nqp::unless(
            ($!reified.DEFINITE && nqp::elems($!reified)),
            ($!todo.DEFINITE && $!todo.reify-at-least(1))
          )
        )
    }
    multi method Int(List:D:)     { self.elems }
    multi method end(List:D:)     { self.elems - 1 }
    multi method Numeric(List:D:) { self.elems }
    multi method Str(List:D:)     { self.join(' ') }

    # Pretend we're a Match assuming we're a list of Matches
    method to()      { self.elems ?? self[self.end].to !! Nil }
    method from()    { self.elems ?? self[0].from !! Nil }

    method sum() is nodal {
        nqp::if(
          self.is-lazy,
          Failure.new(X::Cannot::Lazy.new(:action('.sum'))),
          nqp::if(
            nqp::attrinited(self,List,'$!reified')
              && (my int $elems = self.elems),      # reifies
            nqp::stmts(
              (my $list := $!reified),
              (my $sum = nqp::ifnull(nqp::atpos($list,0),0)),
              (my int $i),
              nqp::while(
                nqp::islt_i($i = nqp::add_i($i,1),$elems),
                ($sum = $sum + nqp::ifnull(nqp::atpos($list,$i),0))
              ),
              $sum
            ),
            0
          )
        )
    }

    proto method fmt(|) { * }
    multi method fmt() {
        nqp::if(
          (my int $elems = self.elems),             # reifies
          nqp::stmts(
            (my $list    := $!reified),
            (my $strings := nqp::setelems(nqp::list_s,$elems)),
            (my int $i = -1),
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              nqp::bindpos_s($strings,$i,nqp::atpos($list,$i).Str)
            ),
            nqp::p6box_s(nqp::join(' ',$strings))
          ),
          ''
        )
    }
    multi method fmt(Str(Cool) $format) {
        nqp::if(
          nqp::iseq_s($format,'%s'),
          self.fmt,
          self.fmt($format,' ')
        )
    }
    multi method fmt(Str(Cool) $format, $separator) {
        nqp::if(
          nqp::iseq_s($format,'%s') && nqp::iseq_s($separator,' '),
          self.fmt,
          nqp::if(
            (my int $elems = self.elems),             # reifies
            nqp::stmts(
              (my $list    := $!reified),
              (my $strings := nqp::setelems(nqp::list_s,$elems)),
              (my int $i = -1),
              nqp::if(
                nqp::iseq_i(                          # only one % in format?
                  nqp::elems(nqp::split('%',$format)),
                  2
                ) && nqp::iseq_i(                     # only one %s in format
                       nqp::elems(my $parts := nqp::split('%s',$format)),
                       2
                     ),
                nqp::while(                           # only a single %s
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::bindpos_s($strings,$i,
                    nqp::join(nqp::atpos($list,$i).Str,$parts)
                  )
                ),
                nqp::while(                           # something else
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::bindpos_s($strings,$i,
                    nqp::atpos($list,$i).fmt($format)
                  )
                )
              ),
              nqp::p6box_s(nqp::join($separator,$strings))
            ),
            ''
          )
        )
    }

    multi method elems(List:D:) is nodal {
        nqp::if(
          $!todo.DEFINITE,
          nqp::stmts(
            $!todo.reify-until-lazy,
            nqp::if(
              $!todo.fully-reified,
              nqp::stmts(
                ($!todo := nqp::null),
                nqp::elems($!reified)
              ),
              Failure.new(X::Cannot::Lazy.new(:action('.elems')))
            )
          ),
          nqp::if(
            $!reified.DEFINITE,
            nqp::elems($!reified),
            0
          )
        )
    }

    multi method AT-POS(List:D: Int:D $Ipos) is raw {
        nqp::if(
          nqp::islt_i((my int $pos = nqp::unbox_i($Ipos)),0),
          Failure.new(X::OutOfRange.new(
            :what($*INDEX // 'Index'), :got($pos), :range<0..^Inf>)),
          nqp::if(
            $!reified.DEFINITE,
            nqp::if(
              nqp::islt_i($pos,nqp::elems($!reified)),
              nqp::atpos($!reified,$pos),
              nqp::if(
                ($!todo.DEFINITE && $!todo.reify-at-least(nqp::add_i($pos,1))),
                nqp::ifnull(nqp::atpos($!reified,$pos),Nil),
                Nil
              )
            ),
            Nil
          )
        )
    }

    multi method AT-POS(List:D: int $pos) is raw {
        nqp::if(
          nqp::islt_i($pos,0),
          Failure.new(X::OutOfRange.new(
            :what($*INDEX // 'Index'), :got($pos), :range<0..^Inf>)),
          nqp::if(
            $!reified.DEFINITE,
            nqp::if(
              nqp::islt_i($pos,nqp::elems($!reified)),
              nqp::atpos($!reified,$pos),
              nqp::if(
                ($!todo.DEFINITE && $!todo.reify-at-least(nqp::add_i($pos,1))),
                nqp::ifnull(nqp::atpos($!reified,$pos),Nil),
                Nil
              )
            ),
            Nil
          )
        )
    }

    method BIND-POS(List:D: Int:D \pos, \what) is raw {
        nqp::iscont(self.AT-POS(pos))
          ?? nqp::bindpos($!reified,nqp::unbox_i(pos),what)
          !! X::Bind.new.throw
    }

    multi method EXISTS-POS(List:D: int $pos) {
        nqp::p6bool(
          nqp::if(
            nqp::isge_i($pos,0),
            nqp::if(
              $!reified.DEFINITE && nqp::islt_i($pos,nqp::elems($!reified)),
              nqp::existspos($!reified,$pos),
              nqp::if(
                $!todo.DEFINITE,
                nqp::stmts(
                  $!todo.reify-at-least(nqp::add_i($pos,1)),
                  nqp::existspos($!reified,$pos)
                )
              )
            )
          )
        )
    }
    multi method EXISTS-POS(List:D: Int:D $pos) {
        nqp::p6bool(
          nqp::if(
            nqp::isge_i($pos,0),
            nqp::if(
              $!reified.DEFINITE && nqp::islt_i($pos,nqp::elems($!reified)),
              nqp::existspos($!reified,$pos),
              nqp::if(
                $!todo.DEFINITE,
                nqp::stmts(
                  $!todo.reify-at-least(nqp::add_i($pos,1)),
                  nqp::existspos($!reified,$pos)
                )
              )
            )
          )
        )
    }

    method reification-target(List:D:) {
        nqp::ifnull(
          $!reified,
          $!reified := nqp::create(IterationBuffer)
        )
    }

    method iterator(List:D:) {

        # something to iterate over in the future
        nqp::if(
          $!todo.DEFINITE,
            class :: does Iterator {
                has int $!i;
                has $!list;
                has $!reified;
                has $!todo;

                method !SET-SELF(\list) {
                    $!i        = -1;
                    $!list    := list;
                    $!reified := nqp::attrinited(list, List,'$!reified')
                      # we already have a place to put values in
                      ?? nqp::getattr(list,List,'$!reified')
                      # create a place here and there to put values in
                      !! nqp::bindattr(list,List,'$!reified',nqp::list);
                    $!todo    := nqp::getattr(list, List, '$!todo');
                    self
                }
                method new(\list) { nqp::create(self)!SET-SELF(list) }

                method pull-one() is raw {
                    nqp::ifnull(
                      nqp::atpos($!reified,$!i = nqp::add_i($!i,1)),
                      $!todo.DEFINITE
                        ?? nqp::islt_i($!i,$!todo.reify-at-least(nqp::add_i($!i,1)))
                          ?? nqp::atpos($!reified,$!i)
                          !! self!done
                        !! IterationEnd
                    )
                }
                method !done() is raw {
                    $!todo := nqp::bindattr($!list,List,'$!todo',nqp::null);
                    IterationEnd
                }

                method push-until-lazy($target) {
                    if $!todo.DEFINITE {
                        my int $elems = $!todo.reify-until-lazy;
                        nqp::while(  # doesn't sink
                          nqp::islt_i($!i = nqp::add_i($!i,1),$elems),
                          $target.push(nqp::atpos($!reified,$!i))
                        );
                        nqp::if(
                          $!todo.fully-reified,
                          self!done,
                          nqp::stmts(
                            ($!i = $elems - 1),
                            Mu
                          )
                        )
                    }
                    else {
                        my int $elems = nqp::elems($!reified);
                        nqp::while(  # doesn't sink
                          nqp::islt_i($!i = nqp::add_i($!i,1),$elems),
                          $target.push(nqp::atpos($!reified,$!i))
                        );
                        IterationEnd
                    }
                }

                method is-lazy() { $!todo.DEFINITE && $!todo.is-lazy }
            }.new(self),

          # everything we need is already there
          nqp::if(
            $!reified.DEFINITE,
            Rakudo::Iterator.ReifiedList(self),
            Rakudo::Iterator.Empty
          )
        )
    }

    multi method ACCEPTS(List:D: $topic) {
        unless nqp::istype($topic, Iterable) {
            return self unless self.elems;
            return self if nqp::istype(self[0], Match);
            return False;
        }
        my $sseq = self;
        my $tseq = $topic;

        sub tailmatch($s,$t) {
            my int $spos = $s;
            my int $tpos = $t;
            while $spos < $sseq {
                # if the next element is Whatever
                if nqp::istype($sseq[$spos], HyperWhatever) {
                    # skip over all of the Whatevers
                    $spos = $spos + 1
                        while $spos <= $sseq && nqp::istype($sseq[$spos], HyperWhatever);
                    # if nothing left, we're done
                    return True if $spos == $sseq;
                    # find a target matching our new target
                    while $tpos < $tseq {
                        my $result = tailmatch($spos,$tpos);
                        return True if $result;
                        $tpos = $tpos + 1
                    }
                    # return false if we ran out
                    return False;
                }
                elsif $tpos == $tseq or not $sseq[$spos].ACCEPTS($tseq[$tpos] ) {
                    return False;
                }
                # skip matching elements
                $spos = $spos + 1;
                $tpos = $tpos + 1;
            }
            # If nothing left to match, we're successful.
            $tpos >= $tseq;
        }

        tailmatch(0,0);
    }

    multi method list(List:D:) { self }

    proto method Seq(|) is nodal { * }
    multi method Seq(List:D:) { Seq.new(self.iterator) }

    method sink(--> Nil) { }

    multi method values(List:D:) {
        Seq.new(self.iterator)
    }
    multi method keys(List:D:) {
        Seq.new(nqp::if(
          self.is-lazy,
          nqp::stmts(
            (my int $i = -1),
            Rakudo::Iterator.Callable( { $i = nqp::add_i($i,1) }, True )
          ),
          Rakudo::Iterator.IntRange(0, self.elems - 1)
        ))
    }
    multi method kv(List:D:) {
        Seq.new(Rakudo::Iterator.KeyValue(self.iterator))
    }
    multi method pairs(List:D:) {
        Seq.new(Rakudo::Iterator.Pair(self.iterator))
    }
    multi method antipairs(List:D:) {
        Seq.new(Rakudo::Iterator.AntiPair(self.iterator))
    }
    multi method invert(List:D:) {
        my $laze = self.is-lazy;
        self.map(-> Pair \listelem {
            my \result = nqp::decont(listelem.value) »=>» listelem.key;
            result ~~ Pair ?? result !! |result  # Don't make slip where we don't need it.
        }).lazy-if($laze)
    }

    # Store in List targets containers with in the list. This handles list
    # assignments, like ($a, $b) = foo().
    proto method STORE(|) { * }
    multi method STORE(List:D: Iterable:D \iterable) {
        # First pass -- scan lhs containers and pick out scalar versus list
        # assignment. This also reifies the RHS values we need, and deconts
        # them. The decont is needed so that we can do ($a, $b) = ($b, $a).
        my \cv = nqp::list();
        my \lhs-iter = self.iterator;
        my \rhs-iter = iterable.iterator;
        my int $rhs-done;
        my Mu $v;
        my Mu $c;
        my Mu $sub-iter;
        my Mu $sc;

        nqp::until(
          nqp::eqaddr(($c := lhs-iter.pull-one),IterationEnd),
          nqp::if(          # Container: scalar assignment
            nqp::iscont($c),
            nqp::stmts(
              nqp::push(cv,$c),
              nqp::if(
                ($rhs-done || ($rhs-done =
                  nqp::eqaddr(($v := rhs-iter.pull-one),IterationEnd))),
                nqp::push(cv,Nil),
                nqp::push(cv,nqp::decont($v)),
              )
            ),
            nqp::if(        # Whatever: skip assigning value
              nqp::istype($c,Whatever),
              nqp::if(
                (nqp::not_i($rhs-done)
                  && nqp::eqaddr(rhs-iter.pull-one,IterationEnd)),
                ($rhs-done = 1)
              ),
              nqp::if(      # List splice into current lhs
                (nqp::istype($c,List) && nqp::not_i(nqp::istype($c,Array))),
                nqp::stmts(
                  ($sub-iter := $c.iterator),
                  nqp::until(
                    nqp::eqaddr(($sc := $sub-iter.pull-one),IterationEnd),
                    nqp::stmts(
                      nqp::push(cv,$sc);
                      nqp::if(
                        ($rhs-done = nqp::eqaddr(
                          ($v := rhs-iter.pull-one),IterationEnd
                        )),
                        nqp::push(cv,Nil),
                        nqp::push(cv,nqp::decont($v))
                      )
                    )
                  )
                ),
                nqp::stmts( # Non-container: store entire remaining rhs
                  nqp::push(cv,$c),
                  nqp::push(cv,List.from-iterator(rhs-iter)),
                  ($rhs-done = 1)
                )
              )
            )
          )
        );

        # Second pass, perform the assignments.
        nqp::shift(cv) = nqp::shift(cv) while nqp::elems(cv);

        self
    }
    multi method STORE(List:D: Mu \item) {
        self.STORE((item,));
    }

    multi method gist(List:D:) {
        self.gistseen('List', {
            '(' ~ self.map( -> $elem {
                given ++$ {
                    when 101 { '...' }
                    when 102 { last }
                    default  { $elem.gist }
                }
            }).join(' ') ~ ')'
        })
    }

    multi method perl(List:D \SELF:) {
        SELF.perlseen('List', {
            '$' x nqp::iscont(SELF) ~ '('
            ~ (self.elems == 1 ?? self[0].perl ~ ',' !! self.map({.perl}).join(', '))
            ~ ' ' x nqp::istrue(self.not && nqp::iscont(SELF)) # add space to avoid `$()`
            ~ ')'
        })
    }

    multi method List(List:D:) { self }

    multi method Slip(List:D:) {
        nqp::if(
          $!todo.DEFINITE,
          # We're not fully reified, and so have internal mutability still.
          # The safe thing to do is to take an iterator of ourself and build
          # the Slip out of that.
          Slip.from-iterator(self.iterator),
          # We're fully reified - and so immutable inside and out! Just make
          # a Slip that shares our reified buffer.
          nqp::p6bindattrinvres(nqp::create(Slip),List,'$!reified',$!reified)
        )
    }

    multi method Array(List:D:) {
        # We need to populate the Array slots with Scalar containers
        nqp::if(
          $!todo.DEFINITE,
          Array.from-iterator(self.iterator),
          nqp::if(
            $!reified.DEFINITE,
            nqp::stmts(
              (my int $elems = nqp::elems($!reified)),
              (my $array := nqp::setelems(nqp::create(IterationBuffer),$elems)),
              (my int $i = -1),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                nqp::bindpos($array, $i,
                  nqp::assign(
                    nqp::p6scalarfromdesc(nqp::null),
                    nqp::atpos($!reified,$i)
                  )
                )
              ),
              nqp::p6bindattrinvres(nqp::create(Array),List,'$!reified',$array)
            ),
            nqp::create(Array)
          )
        )
    }

    method eager {
        nqp::stmts(
          nqp::if(
            $!todo.DEFINITE,
            nqp::stmts(
              $!todo.reify-all,
              ($!todo := nqp::null)
            )
          ),
          self
        )
    }

    method Capture() {
        fail X::Cannot::Lazy.new(:action('create a Capture from'))
            if self.is-lazy;

        # we have something to work with
        if $!reified.DEFINITE && nqp::elems($!reified) -> int $elems {
            my $capture := nqp::create(Capture);
            my $list := nqp::list;
            my $hash := nqp::hash;
            my int $i = -1;
            my $v;
            nqp::istype(($v := nqp::atpos($!reified, $i)),Pair)
              ?? nqp::bindkey($hash, nqp::unbox_s($v.key), $v.value)
              !! nqp::push($list,$v)
              while nqp::islt_i($i = nqp::add_i($i,1),$elems);
            nqp::bindattr($capture,Capture,'@!list',$list) if nqp::elems($list);
            nqp::bindattr($capture,Capture,'%!hash',$hash) if nqp::elems($hash);
            $capture
        }

        # nothing to work with
        else {
            nqp::create(Capture)
        }
    }
    method FLATTENABLE_LIST() {
        nqp::if(
          $!todo.DEFINITE,
          nqp::stmts(
            $!todo.reify-all,
            $!reified
          ),
          nqp::if(
            $!reified.DEFINITE,
            $!reified,
            nqp::bindattr(self,List,'$!reified',nqp::create(IterationBuffer))
          )
        )
    }
    method FLATTENABLE_HASH() { nqp::hash() }

    method Supply(List:D:) { Supply.from-list(self) }

    method CALL-ME(List:U: |c) {
        self.new(|c);
    }

    multi method is-lazy(List:D:) {
        nqp::if(
          $!todo.DEFINITE,
          nqp::stmts(
            $!todo.reify-until-lazy,
            nqp::if(
              $!todo.fully-reified,
              nqp::p6bool($!todo := nqp::null),
              True
            )
          )
        )
    }

    proto method pick(|) is nodal { * }
    multi method pick(List:D:) {
        self.is-lazy
         ?? Failure.new(X::Cannot::Lazy.new(:action('.pick from')))
         !! (my Int $elems = self.elems)
           ?? nqp::atpos($!reified, $elems.rand.floor)
           !! Nil
    }
    multi method pick(List:D: Callable:D $calculate) {
        self.is-lazy
         ?? Failure.new(X::Cannot::Lazy.new(:action('.pick from')))
         !! self.pick( $calculate(self.elems) )
    }
    multi method pick(List:D: $number is copy) {
        fail X::Cannot::Lazy.new(:action('.pick from')) if self.is-lazy;
        my Int $elems = self.elems;
        return () unless $elems;

        $number = nqp::istype($number,Whatever) || $number == Inf
          ?? $elems
          !! $number.UInt min $elems;
        Seq.new(class :: does Iterator {
            has $!list;
            has Int $!elems;
            has int $!number;

            method !SET-SELF(\list,$!elems,\number) {
                $!list  := nqp::clone(nqp::getattr(list,List,'$!reified'));
                $!number = number + 1;
                self
            }
            method new(\list,\elems,\number) {
                nqp::create(self)!SET-SELF(list,elems,number)
            }
            method pull-one() {
                if ($!number = nqp::sub_i($!number,1)) {
                    my int $i;
                    my \tmp = nqp::atpos($!list,$i = $!elems.rand.floor);
                    nqp::bindpos($!list,$i,
                      nqp::atpos($!list,nqp::unbox_i(--$!elems))
                    );
                    tmp
                }
                else {
                    IterationEnd
                }
            }
            method push-all($target --> IterationEnd) {
                my int $i;
                nqp::while(
                  ($!number = nqp::sub_i($!number,1)),
                  nqp::stmts(  # doesn't sink
                    ($target.push(nqp::atpos($!list,$i = $!elems.rand.floor))),
                    (nqp::bindpos($!list,$i,
                      nqp::atpos($!list,nqp::unbox_i(--$!elems))))
                  )
                )
            }
        }.new(self,$elems,$number))
    }

    proto method roll(|) is nodal { * }
    multi method roll() {
        self.is-lazy
          ?? Failure.new(X::Cannot::Lazy.new(:action('.roll from')))
          !! (my Int $elems = self.elems)
            ?? nqp::atpos($!reified, $elems.rand.floor)
            !! Nil
    }
    multi method roll(Whatever) {
        nqp::if(
          self.is-lazy,
          X::Cannot::Lazy.new(:action('.roll from')).throw,
          Seq.new(nqp::if(
            self.elems,
            Rakudo::Iterator.Roller(self),
            Rakudo::Iterator.Empty
          ))
        )
    }
    multi method roll(\number) {
        number == Inf
          ?? self.roll(*)
          !! self.is-lazy
            ?? X::Cannot::Lazy.new(:action('.roll from')).throw
            !! self.elems   # this allocates/reifies
              ?? Seq.new(class :: does Iterator {
                     has $!list;
                     has Int $!elems;
                     has int $!todo;
                     method !SET-SELF(\list,\todo) {
                         $!list := nqp::getattr(list,List,'$!reified');
                         $!elems = nqp::elems($!list);
                         $!todo  = todo;
                         self
                     }
                     method new(\list,\todo) {
                         nqp::create(self)!SET-SELF(list,todo)
                     }
                     method pull-one() is raw {
                         if $!todo {
                             $!todo = $!todo - 1;
                             nqp::atpos($!list,$!elems.rand.floor)
                         }
                         else {
                             IterationEnd
                         }
                     }
                 }.new(self,number.Int))
              !! Seq.new(Rakudo::Iterator.Empty)
    }

    method reverse() is nodal {
        nqp::if(
          self.is-lazy,    # reifies
          Failure.new(X::Cannot::Lazy.new(:action<reverse>)),
          nqp::if(
            $!reified,
            Rakudo::Internals.ReverseListToList(
              self,
              nqp::p6bindattrinvres(nqp::create(self),List,'$!reified',
                nqp::setelems(
                  nqp::create(IterationBuffer),nqp::elems($!reified)
                )
              )
            ),
            nqp::create(self)
          )
        )
    }

    method rotate(Int(Cool) $rotate = 1) is nodal {
        nqp::if(
          self.is-lazy,    # reifies
          Failure.new(X::Cannot::Lazy.new(:action<rotate>)),
          nqp::if(
            $!reified,
            Rakudo::Internals.RotateListToList(
              self, $rotate,
              nqp::p6bindattrinvres(nqp::create(self),List,'$!reified',
                nqp::setelems(
                  nqp::create(IterationBuffer),nqp::elems($!reified)
                )
              )
            ),
            nqp::create(self)
          )
        )
    }

    proto method combinations(|) is nodal {*}
    multi method combinations() {
        nqp::stmts(
          (my int $elems = self.elems),           # reifies
          (my int $i = -1),
          Seq.new(
            Rakudo::Iterator.SequentialIterators(
              Rakudo::Iterator.Callable( {
                  nqp::if(
                    nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                    Rakudo::Iterator.ListIndexes( # basically .combinations($i)
                      self,
                      Rakudo::Iterator.Combinations($elems, $i, 1)
                    ),
                    nqp::if(
                      nqp::iseq_i($i,$elems),
                      Rakudo::Iterator.OneValue(  # last one is self
                        nqp::p6bindattrinvres(    # but must be a (new) List
                          nqp::create(List),      # so transplant innards
                          List,
                          '$!reified',
                          nqp::getattr(self,List,'$!reified')
                        )
                      ),
                      IterationEnd
                    )
                  )
              } )
            )
          )
        )
    }

    multi method combinations(Int() $of) {
        Seq.new(
          Rakudo::Iterator.ListIndexes(
            self, Rakudo::Iterator.Combinations( self.elems, $of, 1)
          )
        )
    }
    multi method combinations(Range:D $ofrange) {
        nqp::stmts(
          (my int $elems = self.elems),      # reifies
          $ofrange.int-bounds(my int $i, my int $to),
          ($i = nqp::if(nqp::islt_i($i,0),-1,nqp::sub_i($i,1))),
          nqp::if(nqp::isgt_i($to,$elems),($to = $elems)),
          Seq.new(
            Rakudo::Iterator.SequentialIterators(
              Rakudo::Iterator.Callable( {
                  nqp::if(
                    nqp::isle_i(($i = nqp::add_i($i,1)),$to),
                    Rakudo::Iterator.ListIndexes( # basically .combinations($i)
                      self,
                      Rakudo::Iterator.Combinations($elems, $i, 1)
                    ),
                    IterationEnd
                  )
              } )
            )
          )
        )
    }

    proto method permutations(|) is nodal {*}
    multi method permutations() {
        Seq.new(
          Rakudo::Iterator.ListIndexes(
            self, Rakudo::Iterator.Permutations( self.elems, 1)
          )
        )
    }

    method join(List:D: Str(Cool) $separator = '') is nodal {
        nqp::stmts(
          nqp::if(
            $!todo.DEFINITE,
            nqp::stmts(
              $!todo.reify-until-lazy,
              nqp::if(
                $!todo.fully-reified,
                ($!todo := nqp::null),
                (my int $infinite = 1)
              )
            )
          ),
          nqp::if(
            $!reified.DEFINITE
              && (my int $elems = nqp::elems($!reified)),
            nqp::stmts(                       # something to join
              (my $strings :=
                nqp::setelems(nqp::list_s,nqp::add_i($elems,$infinite))),
              (my int $i = -1),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                nqp::bindpos_s($strings,$i,nqp::if(
                  nqp::isnull(my $tmp := nqp::atpos($!reified,$i)),
                  '',
                  nqp::if(
                    nqp::isconcrete($tmp) && nqp::istype($tmp,Str),
                    $tmp,
                    nqp::if(
                      nqp::can($tmp,'Str'),
                      $tmp.Str,
                      nqp::box_s($tmp,Str)
                    )
                  )
                ))
              ),
              nqp::if($infinite,nqp::bindpos_s($strings,$i,'...')),
              nqp::p6box_s(nqp::join($separator,$strings))
            ),
            nqp::if($infinite,'...','')
          )
        )
    }

    # https://en.wikipedia.org/wiki/Merge_sort#Bottom-up_implementation
    multi method sort(List:D:) {
        nqp::stmts(
          nqp::if(
            $!todo.DEFINITE,
            nqp::stmts(
              $!todo.reify-until-lazy,
              nqp::if(
                $!todo.fully-reified,
                ($!todo := nqp::null),
                X::Cannot::Lazy.new(:action('.sort')).throw
              )
            )
          ),
          Seq.new(
            nqp::if(
              nqp::attrinited(self,List,'$!reified'),
              Rakudo::Iterator.ReifiedList(
                Rakudo::Internals.MERGESORT-REIFIED-LIST(
                  nqp::p6bindattrinvres(
                    nqp::create(List),List,'$!reified',
                    nqp::clone(nqp::getattr(self,List,'$!reified'))
                  )
                )
              ),
              Rakudo::Iterator.Empty
            )
          )
        )
    }
    multi method sort(List:D: &by) {
        nqp::stmts(
          nqp::if(
            $!todo.DEFINITE,
            nqp::stmts(
              $!todo.reify-until-lazy,
              nqp::if(
                $!todo.fully-reified,
                ($!todo := nqp::null),
                X::Cannot::Lazy.new(:action('.sort')).throw
              )
            )
          ),
          Seq.new(
            nqp::if(
              nqp::attrinited(self,List,'$!reified'),
              Rakudo::Iterator.ReifiedList(
                nqp::if(
                  nqp::eqaddr(&by,&infix:<cmp>),
                  Rakudo::Internals.MERGESORT-REIFIED-LIST(
                    nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',
                      nqp::clone(nqp::getattr(self,List,'$!reified')))
                  ),
                  nqp::if(
                    &by.count < 2,
                    Rakudo::Internals.MERGESORT-REIFIED-LIST-AS(
                      nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',
                        nqp::getattr(self,List,'$!reified')),
                      &by
                    ),
                    Rakudo::Internals.MERGESORT-REIFIED-LIST-WITH(
                      nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',
                        nqp::clone(nqp::getattr(self,List,'$!reified'))),
                      &by
                    )
                  )
                )
              ),
              Rakudo::Iterator.Empty
            )
          )
        )
    }
    method collate {
        self.sort(&[coll]);
    }
    multi method tail(List:D:) is raw {
        nqp::if(
          $!todo.DEFINITE,
          self.Any::tail,
          nqp::if(
            $!reified.DEFINITE && nqp::elems($!reified),
            nqp::atpos($!reified,nqp::sub_i(nqp::elems($!reified),1)),
            Nil
          )
        )
    }
    multi method tail(List:D: $n) {
        nqp::if(
          $!todo.DEFINITE,
          self.Any::tail($n),
          Seq.new(
            nqp::if(
              $!reified.DEFINITE && nqp::elems($!reified),
              nqp::stmts(
                (my $iterator :=
                  Rakudo::Iterator.ReifiedList(self))
                  .skip-at-least(nqp::elems($!reified) - $n),
                $iterator
              ),
              Rakudo::Iterator.Empty
            )
          )
        )
    }

    method push(|) is nodal {
        X::Immutable.new(:typename<List>,:method<push>).throw
    }
    method append(|) is nodal {
        X::Immutable.new(:typename<List>,:method<append>).throw
    }
    method unshift(|) is nodal {
        X::Immutable.new(:typename<List>,:method<unshift>).throw
    }
    method prepend(|) is nodal {
        X::Immutable.new(:typename<List>,:method<prepend>).throw
    }
    method shift(|) is nodal {
        X::Immutable.new(:typename<List>,:method<shift>).throw
    }
    method pop(|) is nodal {
        X::Immutable.new(:typename<List>, :method<pop>).throw
    }
}

# The , operator produces a List.
proto sub infix:<,>(|) is pure {*}
multi sub infix:<,>() { nqp::create(List) }
multi sub infix:<,>(|) {

    # look for a Slip in the parameters
    my \in := nqp::p6argvmarray();
    my int $i     = -1;
    my int $elems = nqp::elems(in);
    nqp::while(
      (nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
        && nqp::not_i(nqp::istype(nqp::atpos(in,$i),Slip))),
      nqp::null
    );

    nqp::if(
      nqp::iseq_i($i,$elems),  # no Slip seen, so just alias input params
      nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',in),
      nqp::stmts(  # Slip seen, first copy non-slippy things
        ($elems = $i),
        ($i     = -1),
        (my $reified := nqp::setelems(nqp::create(IterationBuffer),$elems)),
        nqp::while(
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
          nqp::bindpos($reified,$i,nqp::shift(in))
        ),
        # now set up the List with a future
        (my $list :=
          nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',$reified)),
        nqp::bindattr($list,List,'$!todo',
          my $todo:= nqp::create(List::Reifier)),
        nqp::bindattr($todo,List::Reifier,'$!reified',$reified),
        nqp::bindattr($todo,List::Reifier,'$!future',in),
        nqp::bindattr($todo,List::Reifier,'$!reification-target',$reified),
        $list
      )
    )
}

sub combinations(Int() $n, Int() $k) {
    Seq.new(Rakudo::Iterator.Combinations($n,$k,0))
}

sub permutations(Int() $n) {
    Seq.new(Rakudo::Iterator.Permutations($n,0))
}

sub list(+l) { l }

# Use **@list and then .flat it, otherwise we'll end up remembering all the
# things we flatten, which would be different semantics to .flat which gives
# back a Seq.
sub flat(**@list is raw) {
    @list.flat
}

sub cache(+@l) { @l }

role XX-Whatever does Iterator {
    has Mu $!x;
    method new(\x) { nqp::p6bindattrinvres(nqp::create(self),self,'$!x',x) }
    method is-lazy() { True }
}

proto sub infix:<xx>(|) { * }
multi sub infix:<xx>() { Failure.new("No zero-arg meaning for infix:<xx>") }
multi sub infix:<xx>(Mu \x) { x }
multi sub infix:<xx>(&x, Num() $n) {
    infix:<xx>(&x, $n == Inf ?? Whatever !! $n.Int);
}
multi sub infix:<xx>(&x, Whatever) {
    Seq.new(class :: does XX-Whatever {
        has @!slipped;
        method pull-one() {
            nqp::if(
              @!slipped,
              @!slipped.shift,
              nqp::if(
                nqp::istype((my $pulled := $!x.()),Slip),
                ( (@!slipped = $pulled) ?? @!slipped.shift !! IterationEnd ),
                nqp::if(
                  nqp::istype($pulled,Seq),
                  $pulled.cache,
                  $pulled
                )
              )
            )
        }
    }.new(&x))
}
multi sub infix:<xx>(&x, Int $n) {
    my int $todo = $n + 1;
    my Mu $pulled;
    my Mu $list := nqp::list();
    nqp::while(
      nqp::isgt_i($todo = nqp::sub_i($todo,1),0),
      nqp::if(
        nqp::istype(($pulled := &x.()),Slip),
        (nqp::push($list,$_) for $pulled),
        nqp::if(
          nqp::istype($pulled,Seq),
          nqp::push($list,$pulled.cache),
          nqp::push($list,$pulled)
        )
      )
    );
    nqp::p6bindattrinvres(nqp::create(List), List, '$!reified', $list)
}
multi sub infix:<xx>(Mu \x, Num() $n) {
    infix:<xx>(x, $n == Inf ?? Whatever !! $n.Int);
}
multi sub infix:<xx>(Mu \x, Whatever) {
    Seq.new(Rakudo::Iterator.UnendingValue(x))
}
multi sub infix:<xx>(Mu \x, Int $n) is pure {
    if nqp::isgt_i((my int $elems = $n),0) {
        my $list := nqp::setelems(nqp::list,$elems);
        my int $i = -1;
        nqp::while(
          nqp::islt_i($i = nqp::add_i($i,1),$elems),
          nqp::bindpos($list, $i, x),
        );
        nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',$list)
    }
    else {
        nqp::create(List)
    }
}

proto sub reverse(|)   { * }
multi sub reverse(@a)  { @a.reverse }
multi sub reverse(+@a) { @a.reverse }

sub rotate(@a, Int $n = 1)  { @a.rotate($n) }

sub prefix:<|>(\x) { x.Slip }

multi sub infix:<cmp>(@a, @b) {
    (@a Zcmp @b).first(&prefix:<?>) || @a <=> @b
}

proto sub infix:<X>(|) is pure {*}
multi sub infix:<X>(+lol, :&with!) {
    Seq.new(Rakudo::Iterator.CrossIterablesOp(lol,&with))
}
multi sub infix:<X>(+lol) {
    Seq.new(Rakudo::Iterator.CrossIterablesOp(lol,&infix:<,>))
}
my &cross := &infix:<X>;

proto sub infix:<Z>(|) is pure {*}
multi sub infix:<Z>(+lol, :&with!) {
    Seq.new(Rakudo::Iterator.ZipIterablesOp(lol,&with))
}
multi sub infix:<Z>(+lol) {
    Seq.new(Rakudo::Iterator.ZipIterables(lol))
}
my &zip := &infix:<Z>;

sub roundrobin(+lol) {
    Seq.new(Rakudo::Iterator.RoundrobinIterables(lol))
}

# vim: ft=perl6 expandtab sw=4
