# for our tantrums
my class X::TypeCheck::Splice { ... }
my class Supply { ... }
my class Supplier { ... }

my sub combinations(Int() $n, Int() $k) {
    return () if $k < 0;
    return ((),) if $n < 1 || $k < 1;

    fail X::OutOfRange.new(
      :what("First parameter"),
      :got($n),
      :range("1..2147483647"),
    ) if nqp::isbig_I(nqp::decont($n));
    fail X::OutOfRange.new(
      :what("Second parameter"),
      :got($k),
      :range("1..2147483647"),
    ) if nqp::isbig_I(nqp::decont($k));

    Seq.new(class :: does Iterator {
        has int $!n;
        has int $!k;
        has Mu $!stack;
        has Mu $!combination;
        method !SET-SELF(\n,\k) {
            $!n = n;
            $!k = k;
            $!stack       := nqp::list_i(0);
            $!combination := nqp::list();
            self
        }
        method new(\n,\k) { nqp::create(self)!SET-SELF(n,k) }

        method pull-one() {
            my int $n = $!n;
            my int $k = $!k;

            while (my int $elems = nqp::elems($!stack)) {
                my int $index = $elems - 1;
                my int $value = nqp::pop_i($!stack);

                while nqp::islt_i($value, $n) && nqp::islt_i($index, $k) {
                    nqp::bindpos($!combination, $index, +$value);
#?if jvm
# temporary fix for RT #128123
                    $index++;
                    $value++;
#?endif
#?if !jvm
                    ++$index;
                    ++$value;
#?endif
                    nqp::push_i($!stack, $value);
                }
                return nqp::clone($!combination) if nqp::iseq_i($index, $k);
            }
            IterationEnd
        }
        method count-only { ([*] ($!n ... 0) Z/ 1 .. min($!n - $!k, $!k)).Int }
        method bool-only(--> True) { }
    }.new($n, $k))
}

sub find-reducer-for-op($op) {
    try my %prec := $op.prec;
    return &METAOP_REDUCE_LEFT if (nqp::isnull(%prec) or ! %prec);
    my $reducer = %prec<prec> eq 'f='
        ?? 'listinfix'
        !! %prec<assoc> // 'left';
    ::('&METAOP_REDUCE_' ~ $reducer.uc);
}

# A List is a (potentially infite) immutable list. The immutability is not
# deep; a List may contain Scalar containers that can be assigned to. However,
# it is not possible to shift/unshift/push/pop/splice/bind. A List is also
# Positional, and so may be indexed.
my class List does Iterable does Positional { # declared in BOOTSTRAP
    # class List is Cool {
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
            if $!current-iter.DEFINITE {
                $!current-iter.push-all($!reification-target);
                $!current-iter := Iterator;
            }
            if $!future.DEFINITE {
                while nqp::elems($!future) {
                    my \current = nqp::shift($!future);
                    nqp::istype(current, Slip) && nqp::isconcrete(current)
                        ?? current.iterator.push-all($!reification-target)
                        !! my $ = $!reification-target.push(current);
                }
                $!future := Mu;
            }
            nqp::elems($!reified);
        }

        method fully-reified() {
            !($!current-iter.DEFINITE || $!future.DEFINITE)
        }

        method is-lazy() {
            $!current-iter.DEFINITE ?? $!current-iter.is-lazy !! False
        }
    }

    method from-iterator(List:U: Iterator $iter) {
        my \result := nqp::create(self);
        my \buffer := nqp::create(IterationBuffer);
        my \todo := nqp::create(Reifier);
        nqp::bindattr(result, List, '$!reified', buffer);
        nqp::bindattr(result, List, '$!todo', todo);
        nqp::bindattr(todo, Reifier, '$!reified', buffer);
        nqp::bindattr(todo, Reifier, '$!current-iter', $iter);
        nqp::bindattr(todo, Reifier, '$!reification-target',
            result.reification-target());
        result
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
        my \result := nqp::create(self);
        my Mu \vm-tuple = nqp::captureposarg(nqp::usecapture(), 1);
        my int $elems = nqp::elems(vm-tuple);
        my int $i     = -1;
        my $consider;

        nqp::if(
          nqp::isgt_i($elems,0),
          nqp::stmts(
            nqp::bindattr(result,List,'$!reified',
              my \buffer := nqp::create(IterationBuffer)),
            nqp::bindattr(result,List,'$!todo',
              my \todo   := nqp::create(List::Reifier)),
            nqp::bindattr(todo,List::Reifier,'$!reified',
              buffer),
            nqp::bindattr(todo,List::Reifier,'$!reification-target',
              result.reification-target),
            nqp::bindattr(todo,List::Reifier,'$!future',
              my \future := nqp::setelems(nqp::create(IterationBuffer),$elems)),
            nqp::while(
              nqp::islt_i($i = nqp::add_i($i,1),$elems),
              nqp::stmts(
                ($consider := nqp::atpos(vm-tuple,$i)),
                nqp::if(
                  nqp::iscont($consider),
                  nqp::bindpos(future,$i,$consider),
                  nqp::if(
                    (nqp::istype($consider, Iterable) && $consider.DEFINITE),
                    nqp::if(
                      nqp::istype($consider, PositionalBindFailover),
                      nqp::bindpos(future,$i,$consider.cache.flat.Slip),
                      nqp::bindpos(future,$i,$consider.flat.Slip)
                    ),
                    nqp::bindpos(future,$i,$consider)
                  )
                )
              )
            )
          )
        );

        result
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

    method sum(--> Numeric) {
        fail X::Cannot::Lazy.new(:action('.sum')) if self.is-lazy;

        if nqp::attrinited(self,List,'$!reified') {
            my int $elems   = self.elems;  # reifies
            my $list       := nqp::getattr(self,List,'$!reified');
            my Numeric $sum = 0;
            my int $i       = -1;
            nqp::while(
              nqp::islt_i($i = nqp::add_i($i,1),$elems),
              ($sum = $sum + nqp::ifnull(nqp::atpos($list,$i),0))
            );
            $sum
        }
        else {
            0
        }
    }

    method fmt($format = '%s', $separator = ' ') {
        self.map({ .fmt($format) }).join($separator);
    }

    multi method elems(List:D:) is nodal {
        nqp::if(
          $!todo.DEFINITE,
          nqp::stmts(
            $!todo.reify-until-lazy,
            nqp::if(
              $!todo.fully-reified,
              nqp::stmts(
                ($!todo := Mu),
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
            :what($*INDEX // 'Index'), :got($pos), :range<0..Inf>)),
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
            )
          )
        )
    }

    multi method AT-POS(List:D: int $pos) is raw {
        nqp::if(
          nqp::islt_i($pos,0),
          Failure.new(X::OutOfRange.new(
            :what($*INDEX // 'Index'), :got($pos), :range<0..Inf>)),
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
            )
          )
        )
    }

    method BIND-POS(List:D: Int:D \pos, \what) is raw {
        nqp::iscont(self.AT-POS(pos))
          ?? nqp::bindpos($!reified,nqp::unbox_i(pos),what)
          !! Failure.new(X::Bind.new)
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
                    $!todo := nqp::bindattr($!list,List,'$!todo',Mu);
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
            class :: does Iterator {
                has $!reified;
                has int $!i;

                method !SET-SELF(\list) {
                    $!reified := nqp::getattr(list,List,'$!reified');
                    $!i        = -1;
                    self
                }
                method new(\list) { nqp::create(self)!SET-SELF(list) }

                method pull-one() is raw {
                    # lists cannot have holes, so null indicates the end
                    nqp::ifnull(
                      nqp::atpos($!reified,$!i = nqp::add_i($!i,1)),
                      IterationEnd
                    )
                }
                method push-all($target) {
                    my int $elems = nqp::elems($!reified);
                    nqp::while(  # doesn't sink
                      nqp::islt_i($!i = nqp::add_i($!i,1),$elems),
                      $target.push(nqp::atpos($!reified,$!i))
                    );
                    IterationEnd
                }
            }.new(self),

            # nothing now or in the future to iterate over
            Rakudo::Internals.EmptyIterator
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
        self.is-lazy
          ?? self.values.map: { (state $)++ }
          !! Range.new( 0, self.elems - 1 )
    }
    multi method kv(List:D:) {
        Seq.new(class :: does Iterator {
            has Mu $!iter;
            has Mu $!pulled;
            has int $!on-key;
            has int $!key;

            method !SET-SELF(\iter) {
                $!iter := iter;
                $!on-key = 1;
                $!key    = 0;
                self
            }
            method new(\iter) { nqp::create(self)!SET-SELF(iter) }

            method pull-one() is raw {
                nqp::if(
                  $!on-key,
                  nqp::if(
                    nqp::eqaddr(
                      ($!pulled := $!iter.pull-one),IterationEnd
                    ),
                    IterationEnd,
                    nqp::stmts(
                      ($!on-key = 0),
                      +$!key  # need a right value
                    )
                  ),
                  nqp::stmts(
                    ($!on-key = 1),
                    ($!key = nqp::add_i($!key,1)),
                    $!pulled
                  )
                )
            }
            method push-all($target) {
                my $pulled;
                my int $key;
                nqp::until(
                  nqp::eqaddr(
                    ($pulled := $!iter.pull-one),
                    IterationEnd
                  ),
                  nqp::stmts(
                    $target.push(nqp::p6box_i($key)),
                    $target.push($pulled),
                    ($key = nqp::add_i($key,1))
                  )
                );
                IterationEnd
            }
        }.new(self.iterator))
    }
    multi method pairs(List:D:) {
        Seq.new(class :: does Iterator {
            has Mu $!iter;
            has int $!key;

            method !SET-SELF(\iter) { $!iter := iter; $!key = -1; self }
            method new(\iter) { nqp::create(self)!SET-SELF(iter) }

            method pull-one() is raw {
                nqp::if(
                  nqp::eqaddr((my $pulled := $!iter.pull-one),IterationEnd),
                  IterationEnd,
                  Pair.new(($!key = nqp::add_i($!key,1)),$pulled)
                )
            }
            method push-all($target) {
                my $pulled;
                my int $key = -1;
                nqp::until(
                  nqp::eqaddr(($pulled := $!iter.pull-one),IterationEnd),
                  $target.push(Pair.new(($key = nqp::add_i($key,1)),$pulled))
                );
                IterationEnd
            }
        }.new(self.iterator))
    }
    multi method antipairs(List:D:) {
        self.values.map: { $_ => (state $)++ }
    }
    multi method invert(List:D:) {
        self.map({ nqp::decont(.value) »=>» .key }).flat
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
        # We need to populate the Array slots with Scalar containers, so no
        # shortcuts (and no special casing is likely worth it; iterators can
        # batch up the work too).
        Array.from-iterator(self.iterator)
    }
    method eager {
        $!todo.reify-all() if $!todo.DEFINITE;
        self;
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
            nqp::bindattr($capture,Capture,'$!list',$list) if nqp::elems($list);
            nqp::bindattr($capture,Capture,'$!hash',$hash) if nqp::elems($hash);
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

    method is-lazy() {
        nqp::if(
          $!todo.DEFINITE,
          nqp::stmts(
            $!todo.reify-until-lazy,
            nqp::if(
              $!todo.fully-reified,
              nqp::stmts(
                ($!todo := Mu),
                False
              ),
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
    multi method pick(List:D: $number is copy) {
        fail X::Cannot::Lazy.new(:action('.pick from')) if self.is-lazy;
        my Int $elems = self.elems;
        return () unless $elems;

        $number = nqp::istype($number,Whatever) || $number == Inf
          ?? $elems
          !! $number.Int min $elems;
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
            method push-all($target) {
                my int $i;
                nqp::while(
                  ($!number = nqp::sub_i($!number,1)),
                  nqp::stmts(  # doesn't sink
                    ($target.push(nqp::atpos($!list,$i = $!elems.rand.floor))),
                    (nqp::bindpos($!list,$i,
                      nqp::atpos($!list,nqp::unbox_i(--$!elems))))
                  )
                );
                IterationEnd
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
        self.is-lazy
          ?? Failure.new(X::Cannot::Lazy.new(:action('.roll from')))
          !! (my Int $elems = self.elems)
            ?? Seq.from-loop({nqp::atpos($!reified, $elems.rand.floor)})
            !! ()
    }
    multi method roll(\number) {
        number == Inf
          ?? self.roll(*)
          !! self.is-lazy
            ?? Failure.new(X::Cannot::Lazy.new(:action('.roll from')))
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
              !! ()
    }

    method reverse() is nodal {
        fail X::Cannot::Lazy.new(:action<reverse>) if self.is-lazy;
        my $rlist   := nqp::create(self);
        my $reified := $!reified;
        if $reified {
            my int $i     = -1;
            my int $elems = nqp::elems($reified);
            my int $last  = $elems - 1;
            my $reversed := nqp::list;
            nqp::setelems($reversed,$elems);
            nqp::bindpos($reversed, $last - $i, nqp::atpos($reified, $i))
                while ($i = $i + 1) < $elems;
            nqp::bindattr($rlist, List, '$!reified', $reversed);
        }
        $rlist
    }

    method rotate(Int(Cool) $rotate = 1) is nodal {
        fail X::Cannot::Lazy.new(:action<rotate>) if self.is-lazy;
        my int $elems = self.elems;  # this allocates/reifies
        my $rotated := nqp::create(self);
        if $elems {
            my int $n = $rotate % $elems;
            my $list := nqp::clone($!reified);
            if $n > 0 {
                $n = $n + 1;
                nqp::push($list, nqp::shift($list)) while $n = $n - 1;
            }
            elsif $n < 0 {
                $n = $n - 1;
                nqp::unshift($list, nqp::pop($list)) while $n = $n + 1;
            }
            nqp::bindattr($rotated,List,'$!reified',$list);
        }
        $rotated;
    }

    method rotor(List:D: *@cycle, :$partial) is nodal {
        die "Must specify *how* to rotor a List"
          unless @cycle.is-lazy || @cycle;

        # done if there's nothing to rotor on
        return Rakudo::Internals.EmptyIterator
          unless nqp::getattr(self,List,'$!reified').DEFINITE
                   || nqp::getattr(self,List,'$!todo').DEFINITE;

        my $finished = 0;
        # (Note, the xx should be harmless if the cycle is already infinite by accident.)
        my @c := @cycle.is-lazy ?? @cycle !! (@cycle xx *).cache;
        gather for flat @c -> $s {
            my $elems;
            my $gap;
            if $s ~~ Pair {
                $elems = +$s.key;
                $gap   = +$s.value;
            }
            elsif $s < 1 {
                die "Cannot have elems < 1, did you mean to specify a Pair with => $s?";
            }
            else {
                $elems = +$s;
                $gap   = 0;
            }

            $!todo.reify-at-least($finished + $elems) if $!todo.DEFINITE;
            if $finished + $elems <= nqp::elems($!reified) {
                take self[$finished ..^ $finished + $elems];
                $finished += $elems + $gap;
            }
            else {
                take self[$finished .. *]
                  if $partial and $finished < self.elems;
                last;
            }
        }
    }

    proto method combinations($?) is nodal {*}
    multi method combinations( Int $of ) {
        combinations(self.elems, $of).map: { self[@$_] }
    }
    multi method combinations( Range $ofrange = 0 .. * ) {
        my $over := ($ofrange.first max 0)
                 .. (($ofrange.first(:end) // -1) min self.elems);

        $over.map: { |combinations(self.elems, $_).map: { self[@$_] } }
    }

    proto method permutations(|) is nodal {*}
    multi method permutations() is nodal {
        permutations(self.elems).map: { self[@$_] }
    }

    method join(List:D: $separator = '') is nodal {
        my int $infinite;
        if $!todo.DEFINITE {
            $!todo.reify-until-lazy;
            $!todo.fully-reified
              ?? ($!todo := Mu)
              !! ($infinite = 1);
        }

        # something to join
        if $!reified.DEFINITE && nqp::elems($!reified) -> int $elems {
            my Mu $strings := nqp::setelems(nqp::list_s,$elems + $infinite);
            my int $i = -1;

            my $tmp;
            nqp::bindpos_s($strings,$i,
              nqp::isnull($tmp := nqp::atpos($!reified,$i))
              ?? ''
              !! nqp::unbox_s(nqp::isconcrete($tmp) && nqp::istype($tmp,Str)
                  ?? $tmp
                  !! nqp::can($tmp,'Str')
                    ?? $tmp.Str
                    !! nqp::box_s($tmp,Str)
                 )
            ) while nqp::islt_i(++$i,$elems);

            nqp::bindpos_s($strings,$i,'...') if $infinite;
            nqp::join(nqp::unbox_s($separator.Str),$strings)
        }

        # nothing to join
        else {
            $infinite ?? '...' !! ''
        }
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
      Nil
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
    method !SET-SELF($!x) { self }
    method new(\x) { nqp::create(self)!SET-SELF(x) }
    method is-lazy() { True }
}

proto sub infix:<xx>(Mu $, $, *%) { * }
multi sub infix:<xx>() { Failure.new("No zero-arg meaning for infix:<xx>") }
multi sub infix:<xx>(Mu \x) { x }
multi sub infix:<xx>(&x, Num $n) {
    infix:<xx>(&x, $n == Inf ?? Whatever !! $n.Int);
}
multi sub infix:<xx>(&x, Whatever) {
    Seq.new(class :: does XX-Whatever {
        has @!slipped;
        method pull-one() {
            my $pulled;
            nqp::if(
              @!slipped,
              @!slipped.shift,
              nqp::if(
                nqp::istype(($pulled := $!x.()),Slip),
                nqp::stmts(
                  (@!slipped = $pulled),
                  @!slipped.shift
                ),
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
multi sub infix:<xx>(&x, Int() $n) {
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
multi sub infix:<xx>(Mu \x, Num $n) {
    infix:<xx>(x, $n == Inf ?? Whatever !! $n.Int);
}
multi sub infix:<xx>(Mu \x, Whatever) {
    Seq.new(class :: does XX-Whatever {
        method pull-one() { $!x }
    }.new(x))
}
multi sub infix:<xx>(Mu \x, Int() $n) is pure {
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
multi sub infix:<X>(+lol, :$with!) {
    METAOP_CROSS($with, find-reducer-for-op($with))(|lol.list);
}
multi sub infix:<X>(+lol) {
    my int $n = lol.elems - 1;
    my $laze = False;
    my @l = do for 0..$n -> $i {
        my \elem = lol[$i];
        if nqp::iscont(elem) {
            (elem,)
        }
        else {
            $laze = True if $i and elem.is-lazy;
            elem.list
        }
    }

    my Mu $v := nqp::list();
    my int $i = 0;

    if $laze {  # general case treats all lists as potentially lazy
        return gather {
            my @i = @l.map: *.iterator;
            while $i >= 0 {
                my \e = @i[$i].pull-one();
                if !(e =:= IterationEnd) {
                    nqp::bindpos($v, $i, e);
                    if $i >= $n { take nqp::clone($v) }
                    else {
                        $i = $i + 1;
                        my \elem = lol[$i];
                        @l[$i] = nqp::istype(elem, Iterable) ?? elem !! elem.list;
                    }
                }
                else { $i = $i - 1 }
            }
        }.lazy;
    }

    # eagerize 2nd and subsequent lists if finite
    my Mu $end := nqp::list_i();
    for 1 .. $n -> $i {
        nqp::bindpos_i($end,$i,@l[$i].elems);
    }
    $laze = True if @l[0].is-lazy;  # check pass-thru on the 1st one too

    # optimize for 2D and 3D crosses
    if $n == 1 { # 2-dimensional
        gather {
            my int $e = nqp::atpos_i($end,1);
            my $l0 = @l[0];
            my $l1 = @l[1];
            my \source = $l0.iterator;
            until (my \value = source.pull-one) =:= IterationEnd {
                nqp::bindpos($v, 0, value);
                loop (my int $j = 0; $j < $e; $j = $j + 1) {
                    nqp::bindpos($v, 1, $l1[$j]);
                    take nqp::clone($v);
                }
            }
        }.lazy-if($laze);
    }
    elsif $n == 2 { # 3-dimensional
        gather {
            my int $e1 = nqp::atpos_i($end,1);
            my int $e2 = nqp::atpos_i($end,2);
            my $l0 = @l[0];
            my $l1 = @l[1];
            my $l2 = @l[2];
            my \source = $l0.iterator;
            until (my \value = source.pull-one) =:= IterationEnd {
                nqp::bindpos($v, 0, value);
                loop (my int $j = 0; $j < $e1; $j = $j + 1) {
                    nqp::bindpos($v, 1, $l1[$j]);
                    loop (my int $k = 0; $k < $e2; $k = $k + 1) {
                        nqp::bindpos($v, 2, $l2[$k]);
                        take nqp::clone($v);
                    }
                }
            }
        }.lazy-if($laze);
    }
    else { # more than 3 dimensions
        my Mu $jsave := nqp::list_i();
        my \source = @l[0].iterator;
        gather {
            while $i == 0 {
                my \e = source.pull-one;
                if !(e =:= IterationEnd) {
                    nqp::bindpos($v, $i, e);

                    if $i >= $n { take nqp::clone($v) }
                    else { $i = $i + 1; }

                    my int $j = 0;
                    while $i >= 1 {
                        if $j < nqp::atpos_i($end,$i) {
                            nqp::bindpos($v, $i, @l[$i][$j]);
                            $j = $j + 1;

                            if $i >= $n { take nqp::clone($v) }
                            else {
                                nqp::bindpos_i($jsave, $i, $j);
                                $i = $i + 1;
                                $j = 0;
                            }
                        }
                        else {
                            $i = $i - 1;
                            $j = nqp::atpos_i($jsave,$i);
                        }
                    }
                }
                else { $i = $i - 1 }
            }
        }.lazy-if($laze);
    }
}

my &cross = &infix:<X>;

proto sub infix:<Z>(|) is pure {*}
multi sub infix:<Z>(+lol, :$with!) {
    METAOP_ZIP($with, find-reducer-for-op($with))(|lol.list);
}
multi sub infix:<Z>(+lol) {
    my $arity = lol.elems;
    my $laze = True;
    return () if $arity == 0;
    eager my @l = (^$arity).map: -> $i {
        my \elem = lol[$i];
        if nqp::iscont(elem) {
            $laze = False;
            Rakudo::Internals::WhateverIterator.new((elem,).iterator)
        }
        else {
            $laze = False unless elem.is-lazy;
            Rakudo::Internals::WhateverIterator.new(elem.iterator)
        }
    };

    gather {
        loop {
            my \p = @l.map: {
                my \val = .pull-one;
                last if val =:= IterationEnd;
                val
            }
            my \l = p.list;
            last if l.elems < $arity;
            take-rw l;
        }
    }.lazy-if($laze);
}

my &zip := &infix:<Z>;

sub roundrobin(**@lol is raw) {
    my $laze = False;
    my @iters = do for @lol -> \elem {
        if nqp::iscont(elem) {
            (elem,).iterator
        }
        else {
            $laze = True if elem.is-lazy;
            elem.iterator
        }
    }
    gather {
        while @iters {
            my @new-iters;
            my @values;
            for @iters -> $i {
                my \v = $i.pull-one;
                unless v =:= IterationEnd {
                    @values.push: v;
                    @new-iters.push: $i;
                }
            }
            take @values.List if @values;
            @iters = @new-iters;
        }
    }.lazy-if($laze);
}

# vim: ft=perl6 expandtab sw=4
