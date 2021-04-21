# A List is a (potentially infinite) immutable list. The immutability is not
# deep; a List may contain Scalar containers that can be assigned to. However,
# it is not possible to shift/unshift/push/pop/splice/bind. A List is also
# Positional, and so may be indexed.
my class Array { ... }
my class List does Iterable does Positional { # declared in BOOTSTRAP
    # class List is Cool
    #   The reified elements in the list so far (that is, those that we already
    #   have produced the values for).
    #   has $!reified;
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
            nqp::if(
              (nqp::isconcrete($!current-iter)
                && nqp::eqaddr(
                     $!current-iter.push-at-least(
                       $!reification-target,
                       nqp::sub_i($elems,nqp::elems($!reified))
                     ),
                     IterationEnd
                   )),
              $!current-iter := Iterator
            );

            # there is a future
            nqp::if(
              nqp::isconcrete($!future),

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
            );
            nqp::elems($!reified)
        }

        method reify-until-lazy() {
            nqp::if(
              (nqp::isconcrete($!current-iter)
                && nqp::eqaddr(
                     $!current-iter.push-until-lazy($!reification-target),
                     IterationEnd
                   )
              ),
              $!current-iter := Iterator
            );

            nqp::if(
              nqp::isconcrete($!future)
                && nqp::not_i(nqp::isconcrete($!current-iter)),
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
            );
            nqp::elems($!reified)
        }

        method reify-all() {
            nqp::if(
              nqp::isconcrete($!current-iter),
              nqp::stmts(
                $!current-iter.push-all($!reification-target),
                $!current-iter := Iterator
              )
            );

            nqp::if(
              nqp::isconcrete($!future),
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
              ),
              nqp::elems($!reified)
            )
        }

        method fully-reified() {
            nqp::hllbool(nqp::not_i(
              nqp::isconcrete($!current-iter) || nqp::isconcrete($!future)
            ))
        }

        method is-lazy() {
            nqp::isconcrete($!current-iter)
              ?? $!current-iter.is-lazy
              !! False
        }
    }

    method from-iterator(List:U: Iterator $iter --> List:D) {
        my \buffer := nqp::create(IterationBuffer);
        nqp::bindattr(
          (my \result := nqp::create(self)),List,'$!reified',buffer);
        nqp::bindattr(
          (my \todo := nqp::create(Reifier)),Reifier,'$!reified',buffer);
        nqp::bindattr(todo,Reifier,'$!current-iter',$iter);

        # since Array has its own from-iterator, we don't need to
        # call reification-target, because it is the same as buffer
        nqp::bindattr(todo,Reifier,'$!reification-target',buffer);
        nqp::p6bindattrinvres(result,List,'$!todo',todo)
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
                  nqp::isconcrete($consider) && nqp::istype($consider,Iterable),
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
                (my $todo := nqp::create(List::Reifier)),
                nqp::bindattr($todo,List::Reifier,'$!reified',
                  $buffer
                ),
                nqp::bindattr($todo,List::Reifier,'$!reification-target',
                  $result.reification-target
                ),
                nqp::bindattr($todo,List::Reifier,'$!future',
                  $future
                ),
                $todo.reify-until-lazy,
                nqp::unless(
                  $todo.fully-reified,
                  nqp::bindattr($result,List,'$!todo', $todo),
                ),
                $result
              )
            )
          ),

          # no args, an empty list suffices
          nqp::create(self)
        )
    }

    method new(List: **@things is raw --> List:D) {
        nqp::p6bindattrinvres(
          nqp::create(self),
          List,
          '$!reified',
          nqp::if(
            @things.elems,  # reifies
            nqp::splice(
              nqp::create(IterationBuffer),
              nqp::getattr(@things,List,'$!reified'),
              0,
              0
            ),
            nqp::create(IterationBuffer),
          )
        )
    }

    multi method Bool(List:D: --> Bool:D) {
        nqp::hllbool(
          nqp::unless(
            nqp::isconcrete($!reified) && nqp::elems($!reified),
            nqp::isconcrete($!todo) && $!todo.reify-at-least(1)
          )
        )
    }
    multi method Int(List:D:     --> Int:D) { self.elems }
    multi method end(List:D:     --> Int:D) { self.elems - 1 }
    multi method Numeric(List:D: --> Int:D) { self.elems }
    multi method Str(List:D: --> Str:D)     { self.join(' ') }

    # Pretend we're a Match assuming we're a list of Matches
    method to(List:D:)      { self.elems ?? self[self.end].to !! Nil }
    method from(List:D:)    { self.elems ?? self[0].from !! Nil }

    multi method sum(List:D:) {
        nqp::if(
          self.is-lazy,
          self.fail-iterator-cannot-be-lazy('.sum'),
          nqp::if(
            (my int $elems = self.elems), # reify
            nqp::stmts(
              (my $sum  := 0),
              (my int $i = -1),
              nqp::while(
                nqp::islt_i($i = nqp::add_i($i,1),$elems),
                ($sum := $sum + nqp::ifnull(nqp::atpos($!reified,$i),0))
              ),
              $sum
            )
          )
        )
    }

    proto method fmt(|) {*}
    multi method fmt(List:D: --> Str:D) {
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
    multi method fmt(List:D: Str(Cool) $format --> Str:D) {
        nqp::iseq_s($format,'%s')
          ?? self.fmt
          !! self.fmt($format,' ')
    }
    multi method fmt(List:D: Str(Cool) $format, $separator --> Str:D) {
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

    multi method elems(List:D:) {
        nqp::if(
          nqp::isconcrete($!todo),
          nqp::stmts(
            $!todo.reify-until-lazy,
            nqp::if(
              $!todo.fully-reified,
              nqp::stmts(
                ($!todo := nqp::null),
                nqp::elems($!reified)
              ),
              self.fail-iterator-cannot-be-lazy('.elems')
            )
          ),
          nqp::isconcrete($!reified) && nqp::elems($!reified),
        )
    }

    multi method AT-POS(List:D: Int:D $pos) is raw {
        nqp::isge_i($pos,0) && nqp::isconcrete($!reified)
          ?? nqp::ifnull(
               nqp::atpos($!reified,$pos),
               self!AT_POS_SLOW($pos)
             )
          !! self!AT_POS_SLOW($pos)
    }

    method !AT_POS_SLOW(\pos) is raw {
        nqp::if(
          nqp::islt_i(pos,0),
          Failure.new(X::OutOfRange.new(
            :what($*INDEX // 'Index'), :got(pos), :range<0..^Inf>)),
          nqp::if(
            nqp::isconcrete($!reified),
            nqp::ifnull(
              nqp::atpos($!reified,pos),
              nqp::if(
                nqp::isconcrete($!todo)
                  && $!todo.reify-at-least(nqp::add_i(pos,1)),
                nqp::ifnull(nqp::atpos($!reified,pos),Nil),
                Nil
              )
            ),
            Nil
          )
        )
    }

    method ASSIGN-POS(List:D: Int:D \pos, \what) is raw {
        nqp::iscont(self.AT-POS(pos))
          ?? (nqp::atpos($!reified,nqp::unbox_i(pos)) = what)
          !! X::Assignment::RO.new(value => self).throw
    }

    method BIND-POS(List:D: Int:D \pos, \what) {
        X::Bind.new.throw
    }

    multi method EXISTS-POS(List:D: Int:D $pos --> Bool:D) {
        nqp::hllbool(
          nqp::if(
            nqp::isge_i($pos,0),
            nqp::if(
              nqp::isconcrete($!reified)
                && nqp::islt_i($pos,nqp::elems($!reified)),
              nqp::existspos($!reified,$pos),
              nqp::if(
                nqp::isconcrete($!todo),
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

    my class Todo does Iterator {
        has int $!i;
        has $!list;
        has $!reified;
        has $!todo;

        method !SET-SELF(\list) {
            nqp::stmts(
              ($!i = -1),
              ($!list := list),
              ($!reified := nqp::if(
                nqp::isconcrete(nqp::getattr(list,List,'$!reified')),
                # we already have a place to put values in
                nqp::getattr(list,List,'$!reified'),
                # create a place here and there to put values in
                nqp::bindattr(list,List,'$!reified',
                   nqp::create(IterationBuffer))
              )),
              ($!todo := nqp::getattr(list,List,'$!todo')),
              self
            )
        }
        method new(\list) { nqp::create(self)!SET-SELF(list) }

        method pull-one() is raw {
            nqp::ifnull(
              nqp::atpos($!reified,$!i = nqp::add_i($!i,1)),
              nqp::if(
                nqp::isconcrete($!todo),
                nqp::if(
                  nqp::islt_i(
                    $!i,
                    $!todo.reify-at-least(nqp::add_i($!i,1))
                  ),
                  nqp::atpos($!reified,$!i),
                  self!done
                ),
                IterationEnd
              )
            )
        }
        method !done() is raw {
            $!todo := nqp::bindattr($!list,List,'$!todo',nqp::null);
            IterationEnd
        }

        method push-until-lazy(\target) {
            nqp::if(
              nqp::isconcrete($!todo),
              nqp::stmts(                # something to reify still
                (my int $elems = $!todo.reify-until-lazy),
                nqp::while(  # doesn't sink
                  nqp::islt_i($!i = nqp::add_i($!i,1),$elems),
                  target.push(nqp::atpos($!reified,$!i))
                ),
                nqp::if(
                  $!todo.fully-reified,
                  self!done,
                  nqp::stmts(
                    ($!i = nqp::sub_i($elems,1)),
                    Mu
                  )
                )
              ),
              nqp::stmts(                # already fully reified
                ($elems = nqp::elems($!reified)),
                nqp::while(  # doesn't sink
                  nqp::islt_i($!i = nqp::add_i($!i,1),$elems),
                  target.push(nqp::atpos($!reified,$!i))
                ),
                IterationEnd
              )
            )
        }

        method is-lazy() { $!todo.DEFINITE && $!todo.is-lazy }
    }
    method iterator(List:D: --> Iterator:D) {
        nqp::isconcrete($!todo)
          ?? Todo.new(self)               # something to iterate in the future
          !! nqp::isconcrete($!reified)   # everything we need is already there
            ?? Rakudo::Iterator.ReifiedList(self)
            !! Rakudo::Iterator.Empty
    }

    multi method ACCEPTS(List:D: Iterable:U --> False) { }
    multi method ACCEPTS(List:D: Iterable:D $topic --> Bool:D) {
        CATCH { default { return False } } # .elems on lazies throws
        return True if nqp::eqaddr(self, nqp::decont($topic));

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

        tailmatch(0,0)
    }

    # Note that ACCEPTS not always returning Bool here is what powers m:g/ /
    # smartmatch behaviour
    multi method ACCEPTS(List:D: $topic) {
        self.elems
          ?? nqp::istype(self[0], Match)
            ?? self
            !! False
          !! self
    }

    multi method list(List:D:) { self }

    # We don't sink contents by design https://github.com/rakudo/rakudo/issues/1393
    method sink(--> Nil) { }

    multi method values(List:D: --> Seq:D) {
        Seq.new(self.iterator)
    }
    multi method keys(List:D: --> Seq:D) {
        Seq.new(nqp::if(
          self.is-lazy,
          nqp::stmts(
            (my int $i = -1),
            Rakudo::Iterator.Callable( { $i = nqp::add_i($i,1) }, True )
          ),
          Rakudo::Iterator.IntRange(0, self.elems - 1)
        ))
    }
    multi method kv(List:D: --> Seq:D) {
        Seq.new(Rakudo::Iterator.KeyValue(self.iterator))
    }
    multi method pairs(List:D: --> Seq:D) {
        Seq.new(Rakudo::Iterator.Pairs(self.iterator))
    }
    multi method antipairs(List:D: --> Seq:D) {
        Seq.new(Rakudo::Iterator.AntiPair(self.iterator))
    }
    multi method invert(List:D: --> Seq:D) {
        Seq.new(Rakudo::Iterator.Invert(self.iterator))
    }

    # Store in List targets containers with in the list. This handles list
    # assignments, like ($a, $b) = foo().
    proto method STORE(List:D: |) {*}
    multi method STORE(List:D: Iterable:D \iterable, :INITIALIZE($)! --> List:D) {
        my \buffer := nqp::create(IterationBuffer);
        iterable.iterator.push-all(buffer);
        nqp::p6bindattrinvres(self,List,'$!reified',buffer)
    }
    multi method STORE(List:D: Mu \item, :INITIALIZE($)! --> List:D) {
        self.STORE((item,), :INITIALIZE);
    }

    multi method STORE(List:D: Iterable:D \iterable --> List:D) {
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
    multi method STORE(List:D: Mu \item --> List:D) {
        self.STORE((item,));
    }

    multi method gist(List:D: --> Str:D) {
        self.gistseen(self.^name, {
            (nqp::istype(self,Array) ?? '[' !! '(')
            ~ self.map( -> $elem {
                given ++$ {
                    when 101 { '...' }
                    when 102 { last }
                    default  { $elem.gist }
                }
            }).join(' ')
            ~ (nqp::istype(self,Array) ?? ']' !! ')')
        })
    }

    multi method raku(List:D \SELF: --> Str:D) {
        SELF.rakuseen('List', {
            my $prefix := nqp::iscont(SELF) ?? '$(' !! '(';
            if self.is-lazy {
                my @elements = self.head(101);
                if @elements > 100 {
                    @elements.pop;
                    $prefix ~ @elements.map({.raku}).join(', ') ~ '...).lazy';
                }
                else {
                    $prefix ~ @elements.map({.raku}).join(', ') ~ ').lazy';
                }
            }
            elsif self.elems -> $elems {
                $prefix ~ (
                  $elems == 1
                    ?? self[0].raku ~ ',)'
                    !! self.map( {.raku} ).join(', ') ~ ')'
                )
            }
            else {
                $prefix ~ (nqp::iscont(SELF) ?? ' )' !! ')')
            }
        })
    }

    multi method List(List:D:) { self }

    multi method Slip(List:D: --> Slip:D) {
        nqp::isconcrete($!todo)
          # We're not fully reified, and so have internal mutability still.
          # The safe thing to do is to take an iterator of ourself and build
          # the Slip out of that.
          ?? Slip.from-iterator(self.iterator)
          # We're fully reified - and so immutable inside and out! Just make
          # a Slip that shares our reified buffer.
          !! nqp::p6bindattrinvres(nqp::create(Slip),List,'$!reified',$!reified)
    }

    multi method Array(List:D: --> Array:D) {
        # We need to populate the Array slots with Scalar containers
        nqp::isconcrete($!todo)
          ?? Array.from-iterator(self.iterator)
          !! Array.from-list(self)
    }

    method eager(List:D: --> List:D) {
        nqp::stmts(
          nqp::if(
            nqp::isconcrete($!todo),
            nqp::stmts(
              $!todo.reify-all,
              ($!todo := nqp::null)
            )
          ),
          self
        )
    }

    method Capture(List:D: --> Capture:D) {

        # too lazy
        if self.is-lazy {
            self.fail-iterator-cannot-be-lazy('create a Capture from');
        }

        # we have something to work with
        elsif nqp::isconcrete($!reified) && nqp::elems($!reified) -> int $elems {
            my $capture := nqp::create(Capture);
            my $list := nqp::create(IterationBuffer);
            my $hash := nqp::hash;
            my int $i = -1;
            my $v;
            nqp::istype(($v := nqp::atpos($!reified, $i)),Pair)
              ?? nqp::bindkey($hash, $v.key.Str, $v.value)
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
    method FLATTENABLE_LIST() is implementation-detail {
        nqp::if(
          nqp::isconcrete($!todo),
          nqp::stmts(
            $!todo.reify-all,
            $!reified
          ),
          nqp::if(
            nqp::isconcrete($!reified),
            $!reified,
            nqp::bindattr(self,List,'$!reified',nqp::create(IterationBuffer))
          )
        )
    }
    method FLATTENABLE_HASH() is implementation-detail { nqp::hash() }

    multi method Supply(List:D: --> Supply:D) { Supply.from-list(self) }

    method CALL-ME(List:U: |c) {
        self.new(|c);
    }

    multi method is-lazy(List:D: --> Bool:D) {
        nqp::if(
          nqp::isconcrete($!todo),
          nqp::stmts(
            $!todo.reify-until-lazy,
            nqp::if(
              $!todo.fully-reified,
              nqp::hllbool(nqp::istrue($!todo := nqp::null)),
              True
            )
          ),
          False
        )
    }

    proto method pick(|) is nodal {*}
    multi method pick(List:D:) {
        self.is-lazy
         ?? self.fail-iterator-cannot-be-lazy('.pick from')
         !! (my int $elems = self.elems)   # reifies
           ?? nqp::atpos($!reified,nqp::floor_n(nqp::rand_n($elems)))
           !! Nil
    }
    multi method pick(List:D: Callable:D $calculate) {
        self.is-lazy
         ?? self.fail-iterator-cannot-be-lazy('.pick from')
         !! self.pick( $calculate(self.elems) )
    }

    my class PickN does Iterator {
        has $!list;
        has int $!elems;
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
            nqp::if(
              ($!number = nqp::sub_i($!number,1)),
              nqp::stmts(
                (my \tmp = nqp::atpos(
                  $!list,
                  my int $i = nqp::floor_n(nqp::rand_n($!elems))
                )),
                nqp::bindpos(
                  $!list,
                  $i,
                  nqp::atpos($!list,($!elems = nqp::sub_i($!elems,1)))
                ),
                tmp
              ),
              IterationEnd
            )
        }
        method push-all(\target --> IterationEnd) {
            nqp::stmts(
              (my $list := $!list),
              (my int $number = $!number),
              (my int $elems  = $!elems),
              nqp::while(
                ($number = nqp::sub_i($number,1)),
                nqp::stmts(  # doesn't sink
                  target.push(nqp::atpos(
                    $list,
                    (my int $i = nqp::floor_n(nqp::rand_n($elems)))
                  )),
                  nqp::bindpos(
                    $list,
                    $i,
                    nqp::atpos($list,($elems = nqp::sub_i($elems,1)))
                  )
                )
              ),
              ($!number = $number),
              ($!elems  = $elems)
            )
        }
        method is-deterministic(--> False) { }
    }
    multi method pick(List:D: $number is copy) {
        return self.fail-iterator-cannot-be-lazy('.pick from') if self.is-lazy;
        my Int $elems = self.elems;
        return () unless $elems;

        $number = nqp::istype($number,Whatever) || $number == Inf
          ?? $elems
          !! $number.UInt min $elems;
        Seq.new(PickN.new(self,$elems,$number))
    }

    proto method roll(|) is nodal {*}
    multi method roll(List:D:) {
        self.is-lazy
          ?? self.fail-iterator-cannot-be-lazy('.roll from')
          !! (my int $elems = self.elems)    # reify
            ?? nqp::atpos($!reified,nqp::floor_n(nqp::rand_n($elems)))
            !! Nil
    }
    multi method roll(List:D: Whatever) {
        nqp::if(
          self.is-lazy,
          self.fail-iterator-cannot-be-lazy('.roll from'),
          Seq.new(nqp::if(
            (my int $elems = self.elems),
            nqp::stmts(
              (my $reified := $!reified),
              Rakudo::Iterator.Callable( {
                  nqp::atpos($reified,nqp::floor_n(nqp::rand_n($elems)))
              }, True )
            ),
            Rakudo::Iterator.Empty
          ))
        )
    }

    my class RollN does Iterator {
        has $!list;
        has int $!elems;
        has int $!todo;
        method !SET-SELF(\list,\todo) {
            $!list := nqp::getattr(list,List,'$!reified');
            $!elems = nqp::elems($!list);
            $!todo  = todo + 1;
            self
        }
        method new(\list,\todo) {
            nqp::create(self)!SET-SELF(list,todo)
        }
        method pull-one() is raw {
            ($!todo = nqp::sub_i($!todo,1))
              ?? nqp::atpos($!list,nqp::floor_n(nqp::rand_n($!elems)))
              !! IterationEnd
        }
        method push-all(\target --> IterationEnd) {
            nqp::stmts(
              (my int $todo  = $!todo),
              (my int $elems = $!elems),
              nqp::while(
                ($todo = nqp::sub_i($todo,1)),
                target.push(nqp::atpos(
                  $!list,
                 nqp::floor_n(nqp::rand_n($elems))
                ))
              ),
              ($!todo = $todo)
            )
        }
        method is-deterministic(--> False) { }
    }
    multi method roll(List:D: \number) {
        number == Inf
          ?? self.roll(*)
          !! self.is-lazy
            ?? self.fail-iterator-cannot-be-lazy('.roll from').throw
            !! Seq.new(self.elems   # this allocates/reifies
                 ?? RollN.new(self,number.Int)
                 !! Rakudo::Iterator.Empty
               )
    }

    method reverse(List:D: --> Seq:D) is nodal {
        self.is-lazy    # reifies
          ?? self.fail-iterator-cannot-be-lazy('reverse')
          !! Seq.new: $!reified
            ?? Rakudo::Iterator.ReifiedReverse(self, Mu)
            !! Rakudo::Iterator.Empty
    }

    method rotate(List:D: Int(Cool) $rotate = 1 --> Seq:D) is nodal {
        self.is-lazy    # reifies
          ?? self.fail-iterator-cannot-be-lazy('rotate')
          !! Seq.new: $!reified
            ?? Rakudo::Iterator.ReifiedRotate($rotate, self, Mu)
            !! Rakudo::Iterator.Empty
    }

    proto method combinations(|) is nodal {*}
    multi method combinations(--> Seq:D) {
        my int $elems = self.elems;           # reifies
        my int $i = -1;

        Seq.new(
          Rakudo::Iterator.SequentialIterators(
            Rakudo::Iterator.Callable( {
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
                  ?? Rakudo::Iterator.ListIndexes( #  .combinations($i)
                       self,
                       Rakudo::Iterator.Combinations($elems, $i, 1)
                     )
                  !! nqp::iseq_i($i,$elems)
                    ?? Rakudo::Iterator.OneValue(  # last one is self
                         nqp::p6bindattrinvres(    # but must be a (new) List
                           nqp::create(List),      # so transplant innards
                           List,
                           '$!reified',
                           nqp::getattr(self,List,'$!reified')
                         )
                       )
                    !! IterationEnd
            } )
          )
        )
    }

    multi method combinations(Int() $of --> Seq:D) {
        Seq.new(
          Rakudo::Iterator.ListIndexes(
            self, Rakudo::Iterator.Combinations( self.elems, $of, 1)
          )
        )
    }
    multi method combinations(Range:D $ofrange --> Seq:D) {
        nqp::stmts(
          (my int $elems = self.elems),      # reifies
          nqp::if(
            $ofrange.is-int,
            $ofrange.int-bounds(my int $i, my int $to),
            nqp::stmts(
              nqp::unless(
                $ofrange.min < 0,   # $i already 0 if not
                ($i = ($ofrange.min + $ofrange.excludes-min).Int)
              ),
              nqp::if(
                $ofrange.max > $elems,
                ($to = $elems),
                ($to = ($ofrange.max - $ofrange.excludes-max).Int)
              )
            )
          ),
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
    multi method permutations(--> Seq:D) {
        Seq.new:
          Rakudo::Iterator.ListIndexes:
            self, Rakudo::Iterator.Permutations: self.elems, 1
    }

    method join(List:D: Str(Cool) $separator = '') is nodal {
        nqp::stmts(
          nqp::if(
            nqp::isconcrete($!todo),
            nqp::stmts(                          # need to reify first
              $!todo.reify-until-lazy,
              nqp::if(
                $!todo.fully-reified,
                ($!todo := nqp::null),           # all reified
                (my int $infinite = 1)           # still stuff left to do
              )
            )
          ),
          nqp::if(
            nqp::isconcrete($!reified)
              && (my int $elems = nqp::elems($!reified)),
            nqp::stmts(                          # something to join
              (my $strings := nqp::list_s),
              (my int $i = -1),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                nqp::stmts(                      # something left to check
                  (my $tmp := nqp::ifnull(
                    nqp::atpos($!reified,$i),
                    nqp::if(
                      nqp::isconcrete(my $default),
                      $default,                  # seen before
                      ($default := nqp::if(      # first time we see null
                        nqp::can(self,'default'),
                        self.default.Str,
                        ''
                      ))
                    )
                  )),
                  nqp::if(
                    nqp::isconcrete($tmp),
                    nqp::if(                     # not a type object
                      nqp::istype($tmp,Junction),
                      (return self!JUNCTIONIZE(  # follow Junction path
                        $separator, $strings, $i, $elems, $tmp
                      )),
                      nqp::push_s(               # no special action needed
                        $strings,
                        nqp::if(
                          nqp::istype($tmp,Str),
                          $tmp,
                          nqp::if(
                            nqp::can($tmp,'Str'),
                            $tmp.Str,
                            nqp::box_s($tmp,Str)
                          )
                        )
                      )
                    ),
                    nqp::push_s($strings,$tmp.Str)   # type object
                  )
                )
              ),
              nqp::if($infinite,nqp::push_s($strings,'...')),
              nqp::box_s(                            # done
                nqp::join($separator,$strings),
                Str
              )
            ),
            nqp::if($infinite,'...','')          # nothing to join
          )
        )
    }

    # When we find a Junction in the list, start handling the rest
    # of the list as junctions, and stringify the parts between Junctions
    # normally, for performance.
    method !JUNCTIONIZE(\sep, Mu \strings, \i, \elems, Mu \initial) {
        nqp::stmts(
          nqp::if(
            nqp::elems(strings),
            nqp::stmts(                          # some strings on left
              (my $junction := infix:<~>(
                nqp::concat(nqp::join(sep,strings),sep),
                initial
              )),
              nqp::setelems(strings,0)
            ),
            ($junction := initial)               # just start with this one
          ),
          nqp::while(
            nqp::islt_i((i = nqp::add_i(i,1)),elems),
            nqp::stmts(                          # something left in list
              (my $tmp := nqp::ifnull(
                nqp::atpos($!reified,i),
                nqp::if(
                  nqp::isconcrete(my $default),
                  $default,                      # seen before
                  ($default := nqp::if(          # first time we have a null
                    nqp::can(self,'default'),
                    self.default.Str,
                    ''
                  ))
                )
              )),
              nqp::if(
                nqp::isconcrete($tmp),
                nqp::if(                         # not a type object
                  nqp::istype($tmp,Junction),
                  nqp::stmts(                    # found another Junction
                    nqp::if(
                      nqp::elems(strings),
                      nqp::stmts(                # process string on left
                        ($junction := infix:<~>(
                          $junction,
                          nqp::concat(sep,nqp::join(sep,strings))
                        )),
                        nqp::setelems(strings,0)
                      )
                    ),
                    ($junction := infix:<~>($junction, $tmp))
                  ),
                  nqp::push_s(strings,nqp::if(   # not a Junction
                    nqp::istype($tmp,Str),
                    $tmp,
                    nqp::if(
                      nqp::can($tmp,'Str'),
                      $tmp.Str,
                      nqp::box_s($tmp,Str)
                    )
                  ))
                ),
                nqp::push_s(strings,$tmp.Str)    # type object
              )
            )
          ),
          nqp::if(
            nqp::elems(strings),
            infix:<~>(                           # need to concat right
              $junction,
              nqp::concat(sep,nqp::join(sep,strings))
            ),
            $junction                            # nothing left to concat
          )
        )
    }

    # https://en.wikipedia.org/wiki/Merge_sort#Bottom-up_implementation
    multi method sort(List:D: --> Seq:D) {
        nqp::stmts(
          nqp::if(
            nqp::isconcrete($!todo),
            nqp::stmts(
              $!todo.reify-until-lazy,
              nqp::if(
                $!todo.fully-reified,
                ($!todo := nqp::null),
                self.throw-iterator-cannot-be-lazy('.sort')
              )
            )
          ),
          Seq.new(
            nqp::if(
              nqp::isconcrete($!reified),
              Rakudo::Iterator.ReifiedList(
                Rakudo::Sorting.MERGESORT-REIFIED-LIST(
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
    multi method sort(List:D: &by --> Seq:D) {
        nqp::stmts(
          nqp::if(
            nqp::isconcrete($!todo),
            nqp::stmts(
              $!todo.reify-until-lazy,
              nqp::if(
                $!todo.fully-reified,
                ($!todo := nqp::null),
                self.throw-iterator-cannot-be-lazy('.sort')
              )
            )
          ),
          Seq.new(
            nqp::if(
              nqp::isconcrete($!reified),
              Rakudo::Iterator.ReifiedList(
                nqp::if(
                  nqp::eqaddr(&by,&infix:<cmp>),
                  Rakudo::Sorting.MERGESORT-REIFIED-LIST(
                    nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',
                      nqp::clone(nqp::getattr(self,List,'$!reified')))
                  ),
                  nqp::if(
                    &by.count < 2,
                    Rakudo::Sorting.MERGESORT-REIFIED-LIST-AS(
                      nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',
                        nqp::getattr(self,List,'$!reified')),
                      &by
                    ),
                    Rakudo::Sorting.MERGESORT-REIFIED-LIST-WITH(
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
    multi method tail(List:D:) is raw {
        nqp::isconcrete($!todo)
          ?? self.Any::tail
          !! nqp::isconcrete($!reified) && nqp::elems($!reified)
            ?? nqp::atpos($!reified,nqp::sub_i(nqp::elems($!reified),1))
            !! Nil
    }
    multi method tail(List:D: $n --> Seq:D) {
        nqp::if(
          nqp::isconcrete($!todo),
          self.Any::tail($n),
          Seq.new(
            nqp::if(
              nqp::isconcrete($!reified) && nqp::elems($!reified),
              nqp::stmts(
                (my $iterator := Rakudo::Iterator.ReifiedList(self)),
                nqp::if(
                  nqp::istype($n,Callable),
                  nqp::if(
                    nqp::isgt_i((my $skip := -($n(0).Int)),0),
                    $iterator.skip-at-least($skip)
                  ),
                  nqp::unless(
                    nqp::istype($n,Whatever) || $n == Inf,
                    $iterator.skip-at-least(nqp::elems($!reified) - $n.Int)
                  )
                ),
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
multi sub infix:<,>(--> List:D) { nqp::create(List) }
multi sub infix:<,>(Slip:D \a, Slip:D \b --> List:D) {
    # now set up the List with a future
    Rakudo::Internals.INFIX_COMMA_SLIP_HELPER(nqp::create(IterationBuffer), nqp::list(a,b))
}
multi sub infix:<,>(Any \a, Slip:D \b --> List:D) {
    nqp::stmts(  # Slip seen, first copy non-slippy thing
      (my $reified := nqp::create(IterationBuffer)),
      nqp::bindpos($reified,0,a),
      # now set up the List with a future
      Rakudo::Internals.INFIX_COMMA_SLIP_HELPER($reified, nqp::list(b))
    )
}
multi sub infix:<,>(Slip:D \a, Any \b --> List:D) {
    # now set up the List with a future
    Rakudo::Internals.INFIX_COMMA_SLIP_HELPER(nqp::create(IterationBuffer), nqp::list(a,b))
}
multi sub infix:<,>(Any \a, Any \b --> List:D) {
    nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',nqp::list(a,b))
}
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
        Rakudo::Internals.INFIX_COMMA_SLIP_HELPER($reified, in)
      )
    )
}

proto sub combinations($, $?, *%) {*}
multi sub combinations(Int() \n, Int() \k --> Seq:D) {
    Seq.new(Rakudo::Iterator.Combinations(n,k,0))
}
multi sub combinations(Int() \n, Range:D \k --> Seq:D) {
    ^n .combinations: k
}
multi sub combinations(Iterable \n, \k --> Seq:D) is default {
    n.combinations: k
}
multi sub combinations(\n  --> Seq:D) {
    combinations n, 0..*
}

proto sub permutations($, *%) {*}
multi sub permutations(Int() \n --> Seq:D) {
    Seq.new(Rakudo::Iterator.Permutations(n,0))
}
multi sub permutations(Iterable \n --> Seq:D) {
    n.permutations
}

proto sub list(|) {*}
multi sub list(+l) { l }

proto sub cache(|) {*}
multi sub cache(+@l) { @l }

# Use **@list and then .flat it, otherwise we'll end up remembering all the
# things we flatten, which would be different semantics to .flat which gives
# back a Seq. We also add an Iterable candidate, to preserve .is-lazy
# of an Iterable whenever we can.
proto sub flat(|) {*}
multi flat(**@list is raw) { @list.flat }
multi flat(Iterable \a)    {     a.flat }

proto sub infix:<xx>(Mu $?, $?, *%) {*}
multi sub infix:<xx>() { Failure.new("No zero-arg meaning for infix:<xx>") }
multi sub infix:<xx>(Mu \x) { x }
multi sub infix:<xx>(&x, Num:D() $n) {
    infix:<xx>(&x, $n == Inf ?? Whatever !! $n.Int);
}
multi sub infix:<xx>(&x, Whatever) {
    Seq.new(Rakudo::Iterator.Callable-xx-Whatever(&x))
}
multi sub infix:<xx>(&x, Bool:D $b) {
    $b ?? infix:<xx>(&x, 1) !! Seq.new(Rakudo::Iterator.Empty)
}
multi sub infix:<xx>(&x, Int:D $n) {
    my int $todo = $n;
    my Mu $list := nqp::create(IterationBuffer);
    nqp::while(
      nqp::isgt_i($todo = nqp::sub_i($todo,1),-1),
      nqp::if(
        nqp::istype((my $pulled := x()),Slip),
        $pulled.iterator.push-all($list),
        nqp::if(
          nqp::istype($pulled,Seq),
          nqp::push($list,$pulled.cache),
          nqp::push($list,nqp::decont($pulled))
        )
      )
    );
    Seq.new(Rakudo::Iterator.ReifiedList($list))
}
multi sub infix:<xx>(Mu \x, Num:D() $n) {
    Seq.new(
      $n == Inf
        ?? Rakudo::Iterator.UnendingValue(x)
        !! Rakudo::Iterator.OneValueTimes(x,$n.Int)
    )
}
multi sub infix:<xx>(Mu \x, Whatever) {
    Seq.new(Rakudo::Iterator.UnendingValue(x))
}
multi sub infix:<xx>(Mu \x, Bool:D $b) {
    Seq.new( $b ?? Rakudo::Iterator.OneValue(x) !! Rakudo::Iterator.Empty )
}
multi sub infix:<xx>(Mu \x, Int:D $n) is pure {
    Seq.new(Rakudo::Iterator.OneValueTimes(x,$n))
}

proto sub reverse(|)   {*}
multi sub reverse(@a)  { @a.reverse }
multi sub reverse(+@a) { @a.reverse }

proto sub rotate($, $?, *%) {*}
multi sub rotate(@a)           { @a.rotate     }
multi sub rotate(@a, Int:D $n) { @a.rotate($n) }

proto sub prefix:<|>($, *%) {*}
multi sub prefix:<|>(\x --> Slip:D) { x.Slip }

multi sub infix:<cmp>(@a, @b) {

    sub CMP-SLOW(@a, @b) {
        (@a Zcmp @b).first(&prefix:<?>) || &infix:<cmp>( |do .is-lazy for @a, @b ) || @a <=> @b
    }

    nqp::if(
        @a.is-lazy || @b.is-lazy,
        CMP-SLOW(@a, @b),
        nqp::stmts(
            ( my $a_r := nqp::getattr(@a, List, '$!reified') ),
            ( my $b_r := nqp::getattr(@b, List, '$!reified') ),
            ( my int $ord = nqp::cmp_i(
                ( my int $n_a = nqp::elems($a_r) ), ( my int $n_b = nqp::elems($b_r) )
            )),
            ( my int $res = ( my int $i = 0) ),
            ( my int $total_iters = nqp::islt_i($ord, 0) ?? $n_a !! $n_b ),
            nqp::while(
                nqp::not_i($res) && nqp::islt_i($i, $total_iters),
                nqp::stmts(
                    $res = &infix:<cmp>(nqp::atpos($a_r, $i), nqp::atpos($b_r, $i)),
                    $i   = nqp::add_i($i, 1)
                )
            ),
            ORDER( $res ?? $res !! $ord )
        )
    )
}

proto sub infix:<X>(|) is pure {*}
multi sub infix:<X>(+lol, :$with! --> Seq:D) {
    Seq.new(Rakudo::Iterator.CrossIterablesOp(lol,$with))
}
multi sub infix:<X>(+lol --> Seq:D) {
    Seq.new(Rakudo::Iterator.CrossIterablesOp(lol,&infix:<,>))
}
my constant &cross := &infix:<X>;

proto sub infix:<Z>(|) is pure {*}
multi sub infix:<Z>(+lol, :&with! --> Seq:D) {
    Seq.new(Rakudo::Iterator.ZipIterablesOp(lol,&with))
}
multi sub infix:<Z>(+lol --> Seq:D) {
    Seq.new(Rakudo::Iterator.ZipIterables(lol))
}
my constant &zip := &infix:<Z>;

proto sub roundrobin(|) {*}
multi sub roundrobin(+lol --> Seq:D) {
    Seq.new(Rakudo::Iterator.RoundrobinIterables(lol))
}

# vim: expandtab shiftwidth=4
