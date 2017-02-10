# Now that Iterable is defined, we add extra methods into Any for the list
# operations. (They can't go into Any right away since we need Attribute to
# define the various roles, and Attribute inherits from Any. We will do a
# re-compose of Attribute to make sure it gets the list methods at the end
# of this file. Note the general pattern for these list-y methods is that
# they check if they have an Iterable already, and if not obtain one to
# work on by doing a .list coercion.
use MONKEY-TYPING;
augment class Any {

    proto method map(|) is nodal { * }
    multi method map(Hash \h) {
        die "Cannot map a {self.^name} to a {h.^name}.
Did you mean to add a stub (\{...\}) or did you mean to .classify?"
    }

    multi method map(\SELF: &block;; :$label, :$item) {
        sequential-map(($item ?? (SELF,) !! SELF).iterator, &block, $label);
    }

    multi method map(HyperIterable:D: &block;; :$label) {
        # For now we only know how to parallelize when we've only one input
        # value needed per block. For the rest, fall back to sequential.
        if &block.count != 1 {
            sequential-map(self.iterator, &block, $label)
        }
        else {
            HyperSeq.new(class :: does HyperIterator {
                has $!source;
                has &!block;

                method new(\source, &block) {
                    my \iter = nqp::create(self);
                    nqp::bindattr(iter, self, '$!source', source);
                    nqp::bindattr(iter, self, '&!block', &block);
                    iter
                }

                method fill-buffer(HyperWorkBuffer:D $work, int $items) {
                    $!source.fill-buffer($work, $items);
                }

                method process-buffer(HyperWorkBuffer:D $work) {
                    unless $!source.process-buffer($work) =:= Nil {
                        $work.swap();
                    }
                    my \buffer-mapper = sequential-map($work.input-iterator, &!block, $label);
                    buffer-mapper.iterator.push-all($work.output);
                    $work
                }

                method configuration() {
                    $!source.configuration
                }
            }.new(self.hyper-iterator, &block))
        }
    }

    my class IterateOneWithPhasers does SlippyIterator {
        has &!block;
        has $!source;
        has $!label;
        has Int $!NEXT;         # SHOULD BE int, but has Int performs better
        has Int $!did-init;     # SHOULD BE int, but has Int performs better
        has Int $!did-iterate;  # SHOULD BE int, but has Int performs better

        method !SET-SELF(\block,\source,\label) {
            nqp::stmts(
              (&!block  := block),
              ($!source := source),
              ($!label  := label),
              ($!NEXT = block.has-phaser('NEXT')),
              self
            )
        }
        method new(\bl,\sou,\la) { nqp::create(self)!SET-SELF(bl,sou,la) }

        method is-lazy() { $!source.is-lazy }

        method pull-one() is raw {
            my int $stopped;
            my $value;
            my $result;

            nqp::unless(
              $!did-init,
              nqp::stmts(
                ($!did-init = 1),
                nqp::if(
                  &!block.has-phaser('FIRST'),
                  nqp::p6setfirstflag(&!block)
                )
              )
            );

            if $!slipping && nqp::not_i(nqp::eqaddr(($result := self.slip-one),IterationEnd)) {
                # $result will be returned at the end
            }
            elsif nqp::eqaddr(($value := $!source.pull-one),IterationEnd) {
                $result := IterationEnd
            }
            else {
                nqp::until(
                  $stopped,
                  nqp::handle(
                    nqp::stmts(
                      ($stopped = 1),
                      ($result := &!block($value)),
                      ($!did-iterate = 1),
                      nqp::if(
                        nqp::istype($result, Slip),
                        nqp::if(
                          nqp::eqaddr(($result := self.start-slip($result)), IterationEnd),
                          nqp::if(
                            nqp::not_i(nqp::eqaddr(($value := $!source.pull-one),IterationEnd)),
                            ($stopped = 0)
                          ),
                        )
                      ),
                      nqp::if($!NEXT, &!block.fire_phasers('NEXT')),
                    ),
                    'LABELED', $!label,
                    'NEXT', nqp::stmts(
                       ($!did-iterate = 1),
                       nqp::if($!NEXT, &!block.fire_phasers('NEXT')),
                       nqp::eqaddr(($value := $!source.pull-one), IterationEnd)
                         ?? ($result := IterationEnd)
                         !! ($stopped = 0)
                    ),
                    'REDO', ($stopped = 0),
                    'LAST', nqp::stmts(
                      ($!did-iterate = 1),
                      ($result := IterationEnd)
                    )
                  ),
                  :nohandler
                )
            }
            nqp::if(
              $!did-iterate && nqp::eqaddr($result,IterationEnd),
              &!block.fire_phasers('LAST')
            );
            $result
        }

        method push-all($target --> IterationEnd) {
            nqp::unless(
              $!did-init,
              nqp::stmts(
                ($!did-init = 1),
                nqp::if(
                  &!block.has-phaser('FIRST'),
                  nqp::p6setfirstflag(&!block)
                )
              )
            );

            my int $stopped;
            my int $done;
            my $pulled;
            my $value;
            until $done
                || nqp::eqaddr(($value := $!source.pull-one),IterationEnd) {
                nqp::stmts(
                  ($stopped = 0),
                  nqp::until(
                    $stopped,
                    nqp::stmts(
                      ($stopped = 1),
                      nqp::handle(
                        nqp::stmts(  # doesn't sink
                          ($pulled := &!block($value)),
                          ($!did-iterate = 1),
                          nqp::if($!NEXT, &!block.fire_phasers('NEXT')),
                          nqp::if(
                            nqp::istype($pulled,Slip),
                            self.slip-all($pulled,$target),
                            $target.push($pulled)
                          )
                        ),
                        'LABELED', $!label,
                        'NEXT', nqp::stmts(
                          ($!did-iterate = 1),
                          nqp::if($!NEXT, &!block.fire_phasers('NEXT')),
                          nqp::eqaddr(
                            ($value := $!source.pull-one),
                            IterationEnd
                          )
                            ?? ($done = 1)
                            !! ($stopped = 0)),
                        'REDO', ($stopped = 0),
                        'LAST', ($done = $!did-iterate = 1)
                      )
                    ),
                    :nohandler
                  )
                )
            }
            nqp::if($!did-iterate,&!block.fire_phasers('LAST'))
        }

        method sink-all(--> IterationEnd) {
            nqp::unless(
              $!did-init,
              nqp::stmts(
                ($!did-init = 1),
                nqp::if(
                  &!block.has-phaser('FIRST'),
                  nqp::p6setfirstflag(&!block)
                )
              )
            );

            my int $stopped;
            my int $done;
            my $value;
            until $done
                || nqp::eqaddr(($value := $!source.pull-one()),IterationEnd) {
                nqp::stmts(
                  ($stopped = 0),
                  nqp::until(
                    $stopped,
                    nqp::stmts(
                      ($stopped = 1),
                      nqp::handle(
                        nqp::stmts(  # doesn't sink
                          (&!block($value)),
                          ($!did-iterate = 1),
                          nqp::if($!NEXT, &!block.fire_phasers('NEXT')),
                        ),
                        'LABELED', $!label,
                        'NEXT', nqp::stmts(
                          ($!did-iterate = 1),
                          nqp::if($!NEXT, &!block.fire_phasers('NEXT')),
                          nqp::eqaddr(
                            ($value := $!source.pull-one),
                            IterationEnd
                          )
                            ?? ($done = 1)
                            !! ($stopped = 0)),
                        'REDO', ($stopped = 0),
                        'LAST', ($done = $!did-iterate = 1)
                      )
                    ),
                    :nohandler
                  )
                )
            }
            nqp::if($!did-iterate,&!block.fire_phasers('LAST'))
        }
    }

    my class IterateOneNotSlippingWithoutPhasers does Iterator {
        has &!block;
        has $!source;
        has $!label;

        method new(&block,$source,$label) {
            my $iter := nqp::create(self);
            nqp::bindattr($iter, self, '&!block', &block);
            nqp::bindattr($iter, self, '$!source', $source);
            nqp::bindattr($iter, self, '$!label', nqp::decont($label));
            $iter
        }

        method is-lazy() { $!source.is-lazy }

        method pull-one() is raw {
            if nqp::eqaddr((my $pulled := $!source.pull-one),IterationEnd) {
                IterationEnd
            }
            else {
                my $result;
                nqp::stmts(
                  nqp::until(
                    (my int $stopped),
                    nqp::stmts(
                      ($stopped = 1),
                      nqp::handle(
                        ($result := &!block($pulled)),
                        'LABELED', $!label,
                        'NEXT', nqp::if(
                          nqp::eqaddr(
                            ($pulled := $!source.pull-one),
                            IterationEnd
                          ),
                          ($result := IterationEnd),
                          ($stopped = 0)
                        ),
                        'REDO', ($stopped = 0),
                        'LAST', ($result := IterationEnd)
                      ),
                    ),
                    :nohandler
                  ),
                  $result
                )
            }
        }

        method push-all($target --> IterationEnd) {
            my $pulled;
            nqp::until(
              nqp::eqaddr(($pulled := $!source.pull-one),IterationEnd),
              nqp::until(
                (my int $stopped),
                nqp::stmts(
                  ($stopped = 1),
                  nqp::handle(
                    $target.push(&!block($pulled)),
                    'LABELED', $!label,
                    'REDO', ($stopped = 0),
                    'LAST', return
                  )
                ),
                :nohandler
              )
            )
        }

        method sink-all(--> IterationEnd) {
            my $pulled;
            nqp::until(
              nqp::eqaddr(($pulled := $!source.pull-one),IterationEnd),
              nqp::until(
                (my int $stopped),
                nqp::stmts(
                  ($stopped = 1),
                  nqp::handle(
                    &!block($pulled),
                    'LABELED', $!label,
                    'REDO', ($stopped = 0),
                    'LAST', return
                  )
                ),
                :nohandler
              )
            )
        }
    }

    my class IterateOneWithoutPhasers does SlippyIterator {
        has &!block;
        has $!source;
        has $!label;

        method new(&block,$source,$label) {
            my $iter := nqp::create(self);
            nqp::bindattr($iter, self, '&!block', &block);
            nqp::bindattr($iter, self, '$!source', $source);
            nqp::bindattr($iter, self, '$!label', nqp::decont($label));
            $iter
        }

        method is-lazy() { $!source.is-lazy }

        method pull-one() is raw {
            my int $redo = 1;
            my $value;
            my $result;

            if $!slipping && nqp::not_i(nqp::eqaddr(
              ($result := self.slip-one),
              IterationEnd
            )) {
                # $result will be returned at the end
            }
            elsif nqp::eqaddr(
              ($value := $!source.pull-one),
              IterationEnd
            ) {
                $result := $value
            }
            else {
              nqp::while(
                $redo,
                nqp::stmts(
                  $redo = 0,
                  nqp::handle(
                    nqp::if(
                      nqp::istype(($result := &!block($value)),Slip),
                      nqp::if(
                        nqp::eqaddr(
                          ($result := self.start-slip($result)), IterationEnd),
                        nqp::if(
                          nqp::not_i(nqp::eqaddr(
                            ($value := $!source.pull-one),
                            IterationEnd
                          )),
                          $redo = 1
                        )
                      )
                    ),
                    'LABELED',
                    $!label,
                    'NEXT',
                    nqp::if(
                      nqp::eqaddr(
                        ($value := $!source.pull-one),IterationEnd
                      ),
                      ($result := IterationEnd),
                      ($redo = 1)
                    ),
                    'REDO',
                    ($redo = 1),
                    'LAST',
                    ($result := IterationEnd)
                  ),
                ),
              :nohandler);
            }
            $result
        }

        method push-all($target --> IterationEnd) {

# This extra scope serves no other purpose than to make this method JIT
# and OSR faster.
{
            my int $redo;
            my $value;
            my $result;

            nqp::until(
              nqp::eqaddr(($value := $!source.pull-one),IterationEnd),
              nqp::stmts(
                ($redo = 1),
                nqp::while(
                  $redo,
                  nqp::stmts(
                    ($redo = 0),
                    nqp::handle(
                      nqp::if(
                        nqp::istype(($result := &!block($value)),Slip),
                        self.slip-all($result,$target),
                        $target.push($result)
                      ),
                      'LABELED', $!label,
                      'REDO', ($redo = 1),
                      'LAST', (return IterationEnd),
                    )
                  ),
                  :nohandler
                )
              )
            )
} # needed for faster JITting and OSRing
        }

        method sink-all(--> IterationEnd) {

# This extra scope serves no other purpose than to make this method JIT
# and OSR faster.
{
            my int $redo;
            my $value;

            nqp::until(
              nqp::eqaddr(($value := $!source.pull-one()),IterationEnd),
              nqp::stmts(
                ($redo = 1),
                nqp::while(
                  $redo,
                  nqp::stmts(
                    ($redo = 0),
                    nqp::handle(  # doesn't sink
                      &!block($value),
                      'LABELED', $!label,
                      'NEXT', nqp::null,  # need NEXT for next LABEL support
                      'REDO', ($redo = 1),
                      'LAST', (return IterationEnd)
                    ),
                  :nohandler
                  )
                )
              )
            )
} # needed for faster JITting and OSRing
        }
    }

    my class IterateTwoWithoutPhasers does SlippyIterator {
        has &!block;
        has $!source;
        has $!label;

        method new(&block,$source,$label) {
            my $iter := nqp::create(self);
            nqp::bindattr($iter, self, '&!block', &block);
            nqp::bindattr($iter, self, '$!source', $source);
            nqp::bindattr($iter, self, '$!label', nqp::decont($label));
            $iter
        }

        method is-lazy() { $!source.is-lazy }

        method pull-one() is raw {
            my int $redo = 1;
            my $value;
            my $value2;
            my $result;

            if $!slipping && nqp::not_i(nqp::eqaddr(
              ($result := self.slip-one),
              IterationEnd
            )) {
                # $result will be returned at the end
            }
            elsif nqp::eqaddr(
              ($value := $!source.pull-one),
              IterationEnd
            ) {
                $result := IterationEnd;
            }
            else {
              nqp::while(
                $redo,
                nqp::stmts(
                  $redo = 0,
                  nqp::handle(
                    nqp::stmts(
                      nqp::if(
                        nqp::eqaddr(($value2 := $!source.pull-one),IterationEnd),
                        nqp::if(                                 # don't have 2 params
                          nqp::istype(($result := &!block($value)),Slip),
                          ($result := self.start-slip($result))  # don't care if empty
                        ),
                        nqp::if(
                          nqp::istype(($result := &!block($value,$value2)),Slip),
                          nqp::if(
                            nqp::eqaddr(($result := self.start-slip($result)),IterationEnd),
                            nqp::unless(
                              nqp::eqaddr(($value := $!source.pull-one),IterationEnd),
                              ($redo = 1)
                            )
                          )
                        )
                      )
                    ),
                    'LABELED',
                    $!label,
                    'NEXT',
                    nqp::if(
                      nqp::eqaddr(
                        ($value := $!source.pull-one),IterationEnd
                      ),
                      ($result := IterationEnd),
                      ($redo = 1)
                    ),
                    'REDO',
                    ($redo = 1),
                    'LAST',
                    ($result := IterationEnd)
                  ),
                ),
              :nohandler);
            }
            $result
        }

        method push-all($target --> IterationEnd) {

# This extra scope serves no other purpose than to make this method JIT
# and OSR faster.
{
            my int $redo;
            my $value;
            my $value2;
            my $result;

            nqp::until(
              nqp::eqaddr(($value := $!source.pull-one),IterationEnd),
              nqp::stmts(
                ($redo = 1),
                nqp::while(
                  $redo,
                  nqp::stmts(
                    ($redo = 0),
                    nqp::handle(
                      nqp::if(
                        nqp::eqaddr(
                          ($value2 := $!source.pull-one),
                          IterationEnd
                        ),
                        nqp::stmts(
                          ($result := &!block($value)),
                          nqp::if(
                            nqp::istype($result,Slip),
                            self.slip-all($result,$target),
                            $target.push($result)
                          ),
                          (return IterationEnd)
                        ),
                        nqp::if(
                          nqp::istype(
                            ($result := &!block($value,$value2)),
                            Slip
                          ),
                          self.slip-all($result,$target),
                          $target.push($result)
                        )
                      ),
                      'LABELED', $!label,
                      'REDO', ($redo = 1),
                      'LAST', (return IterationEnd)
                    )
                  ),
                  :nohandler
                )
              )
            )
} # needed for faster JITting and OSRing
        }

        method sink-all(--> IterationEnd) {

# This extra scope serves no other purpose than to make this method JIT
# and OSR faster.
{
            my int $redo;
            my $value;
            my $value2;

            nqp::until(
              nqp::eqaddr(($value := $!source.pull-one()),IterationEnd),
              nqp::stmts(
                ($redo = 1),
                nqp::while(
                  $redo,
                  nqp::stmts(
                    ($redo = 0),
                    nqp::handle(  # doesn't sink
                      nqp::if(
                        nqp::eqaddr(
                          ($value2 := $!source.pull-one),
                          IterationEnd
                        ),
                        nqp::stmts(
                          (&!block($value)),
                          (return IterationEnd)
                        ),
                        (&!block($value,$value2))
                      ),
                      'LABELED', $!label,
                      'NEXT', nqp::null,  # need NEXT for next LABEL support
                      'REDO', ($redo = 1),
                      'LAST', (return IterationEnd)
                    )
                  ),
                :nohandler
                )
              )
            )
} # needed for faster JITting and OSRing
        }
    }

    my class IterateMoreWithPhasers does SlippyIterator {
        has &!block;
        has $!source;
        has $!count;
        has $!label;
        has $!value-buffer;
        has $!did-init;
        has $!did-iterate;
        has $!NEXT;
        has $!CAN_FIRE_PHASERS;

        method new(&block, $source, $count, $label) {
            my $iter := nqp::create(self);
            nqp::bindattr($iter, self, '&!block', &block);
            nqp::bindattr($iter, self, '$!source', $source);
            nqp::bindattr($iter, self, '$!count', $count);
            nqp::bindattr($iter, self, '$!label', nqp::decont($label));
            $iter
        }

        method is-lazy() { $!source.is-lazy }

        method pull-one() is raw {
            $!value-buffer.DEFINITE
                ?? nqp::setelems($!value-buffer, 0)
                !! ($!value-buffer := IterationBuffer.new);
            my int $redo = 1;
            my $result;

            if !$!did-init && nqp::can(&!block, 'fire_phasers') {
                $!did-init         = 1;
                $!CAN_FIRE_PHASERS = 1;
                $!NEXT             = &!block.has-phaser('NEXT');
                nqp::p6setfirstflag(&!block)
                  if &!block.has-phaser('FIRST');
            }

            if $!slipping && !(($result := self.slip-one()) =:= IterationEnd) {
                # $result will be returned at the end
            }
            elsif $!source.push-exactly($!value-buffer, $!count) =:= IterationEnd
                    && nqp::elems($!value-buffer) == 0 {
                $result := IterationEnd
            }
            else {
                nqp::while(
                  $redo,
                  nqp::stmts(
                    $redo = 0,
                    nqp::handle(
                      nqp::stmts(
                        ($result := nqp::p6invokeflat(&!block, $!value-buffer)),
                        ($!did-iterate = 1),
                        nqp::if(
                          nqp::istype($result, Slip),
                          nqp::stmts(
                            ($result := self.start-slip($result)),
                            nqp::if(
                              nqp::eqaddr($result, IterationEnd),
                              nqp::stmts(
                                (nqp::setelems($!value-buffer, 0)),
                                ($redo = 1
                                  unless nqp::eqaddr(
                                    $!source.push-exactly($!value-buffer, $!count),
                                    IterationEnd)
                                  && nqp::elems($!value-buffer) == 0)
                              )
                            )
                          )
                        ),
                        nqp::if($!NEXT, &!block.fire_phasers('NEXT')),
                      ),
                      'LABELED', $!label,
                      'NEXT', nqp::stmts(
                        ($!did-iterate = 1),
                        nqp::if($!NEXT, &!block.fire_phasers('NEXT')),
                          (nqp::setelems($!value-buffer, 0)),
                          nqp::eqaddr($!source.push-exactly($!value-buffer, $!count), IterationEnd)
                          && nqp::elems($!value-buffer) == 0
                            ?? ($result := IterationEnd)
                            !! ($redo = 1)),
                      'REDO', $redo = 1,
                      'LAST', nqp::stmts(
                        ($!did-iterate = 1),
                        ($result := IterationEnd)
                      )
                    )
                  ),
                :nohandler);
            }
            &!block.fire_phasers('LAST')
              if $!CAN_FIRE_PHASERS
              && $!did-iterate
              && nqp::eqaddr($result, IterationEnd);
            $result
        }
    }

    sub sequential-map(\source, &block, $label) {
        # We want map to be fast, so we go to some effort to build special
        # case iterators that can ignore various interesting cases.
        my $count = &block.count;

        Seq.new(
          nqp::istype(&block,Block) && &block.has-phasers
            ?? $count < 2 || $count === Inf
              ?? IterateOneWithPhasers.new(&block,source,$label)
              !! IterateMoreWithPhasers.new(&block,source,$count,$label)
            !! $count < 2 || $count === Inf
              ?? nqp::istype(Slip,&block.returns)
                ?? IterateOneWithoutPhasers.new(&block,source,$label)
                !! IterateOneNotSlippingWithoutPhasers.new(&block,source,$label)
              !! $count == 2
                ?? IterateTwoWithoutPhasers.new(&block,source,$label)
                !! IterateMoreWithPhasers.new(&block,source,$count,$label)
        )
    }

    proto method flatmap (|) is nodal { * }
    multi method flatmap(&block, :$label) {
        self.map(&block, :$label).flat
    }

    method !grep-k(Callable:D $test) {
        Seq.new(class :: does Iterator {
            has  Mu $!iter;
            has  Mu $!test;
            has int $!index;
            method !SET-SELF(\list,Mu \test) {
                $!iter  = list.iterator;
                $!test := test;
                $!index = -1;
                self
            }
            method new(\list,Mu \test) { nqp::create(self)!SET-SELF(list,test) }
            method pull-one() is raw {
                $!index = $!index + 1
                  until ($_ := $!iter.pull-one) =:= IterationEnd || $!test($_);
                $_ =:= IterationEnd
                  ?? IterationEnd
                  !! nqp::p6box_i($!index = $!index + 1)
            }
            method push-all($target) {
                until ($_ := $!iter.pull-one) =:= IterationEnd {
                    $!index = $!index + 1;
                    $target.push(nqp::p6box_i($!index)) if $!test($_);
                }
                IterationEnd
            }
        }.new(self, $test))
    }
    method !grep-kv(Callable:D $test) {
        Seq.new(class :: does Iterator {
            has  Mu $!iter;
            has  Mu $!test;
            has int $!index;
            has Mu $!value;
            method !SET-SELF(\list,Mu \test) {
                $!iter  = list.iterator;
                $!test := test;
                $!index = -1;
                self
            }
            method new(\list,Mu \test) { nqp::create(self)!SET-SELF(list,test) }
            method pull-one() is raw {
                if $!value.DEFINITE {
                    my \tmp  = $!value;
                    $!value := nqp::null;
                    tmp
                }
                else {
                    $!index = $!index + 1
                      until ($_ := $!iter.pull-one) =:= IterationEnd
                        || $!test($_);
                    if $_ =:= IterationEnd {
                        IterationEnd;
                    }
                    else {
                        $!value := $_;
                        nqp::p6box_i($!index = $!index + 1)
                    }
                }
            }
            method push-all($target) {
                nqp::until(
                  nqp::eqaddr(($_ := $!iter.pull-one),IterationEnd),
                  nqp::stmts(
                    $!index = nqp::add_i($!index,1);
                    nqp::if(
                      $!test($_),
                      nqp::stmts(  # doesn't sink
                        $target.push(nqp::p6box_i($!index));
                        $target.push($_);
                      )
                    )
                  )
                );
                IterationEnd
            }
        }.new(self, $test))
    }
    method !grep-p(Callable:D $test) {
        Seq.new(class :: does Iterator {
            has  Mu $!iter;
            has  Mu $!test;
            has int $!index;
            method !SET-SELF(\list,Mu \test) {
                $!iter  = list.iterator;
                $!test := test;
                $!index = -1;
                self
            }
            method new(\list,Mu \test) { nqp::create(self)!SET-SELF(list,test) }
            method pull-one() is raw {
                $!index = $!index + 1
                  until ($_ := $!iter.pull-one) =:= IterationEnd || $!test($_);
                $_ =:= IterationEnd
                  ?? IterationEnd
                  !! Pair.new($!index = $!index + 1,$_)
            }
            method push-all($target) {
                until ($_ := $!iter.pull-one) =:= IterationEnd {
                    $!index = $!index + 1;
                    $target.push(Pair.new($!index,$_)) if $!test($_);
                }
                IterationEnd
            }
        }.new(self, $test))
    }

    role Grepper does Iterator {
        has Mu $!iter;
        has Mu $!test;
        method SET-SELF(\list,Mu \test) {
            $!iter  = list.iterator;
            $!test := test;
            self
        }
        method new(\list,Mu \test) { nqp::create(self).SET-SELF(list,test) }
        method is-lazy() { $!iter.is-lazy }
    }
    method !grep-callable(Callable:D $test) {
        nqp::if(
          $test.count == 1,
          sequential-map(
            self.iterator,
            { nqp::if($test($_),$_,Empty) },
            Any)
          ,
          nqp::stmts(
            (my role CheatArity {
                has $!arity;
                has $!count;

                method set-cheat($new-arity, $new-count) {
                    $!arity = $new-arity;
                    $!count = $new-count;
                }

                method arity(Code:D:) { $!arity }
                method count(Code:D:) { $!count }
            }),
            (my &tester = -> |c {
                #note "*cough* {c.perl} -> {$test(|c).perl}";
                next unless $test(|c);
                c.list
            } but CheatArity),
            &tester.set-cheat($test.arity, $test.count),
            self.map(&tester)
          )
        )
    }
    method !grep-accepts(Mu $test) {
        Seq.new(class :: does Grepper {
            method pull-one() is raw {
                nqp::until(
                  nqp::eqaddr(($_ := $!iter.pull-one),IterationEnd)
                    || $!test.ACCEPTS($_),
                  nqp::null
                );
                $_
            }
            method push-all($target) {
                nqp::until(
                  nqp::eqaddr(($_ := $!iter.pull-one),IterationEnd),
                  nqp::if(  # doesn't sink
                    $!test.ACCEPTS($_),
                    $target.push($_)
                  )
                );
                IterationEnd
            }
        }.new(self, $test))
    }

    method !first-result(\index,\value,$what,%a) is raw {
        nqp::stmts(
          (my $storage := nqp::getattr(%a,Map,'$!storage')),
          nqp::if(
            nqp::elems($storage),                       # some adverb
            nqp::if(
              nqp::iseq_i(nqp::elems($storage),1),      # one adverb
              nqp::if(
                nqp::atkey($storage,"k"),               # :k
                nqp::p6box_i(index),
                nqp::if(
                  nqp::atkey($storage,"p"),             # :p
                  Pair.new(index,value),
                  nqp::if(
                    nqp::atkey($storage,"v"),           # :v
                    value,
                    nqp::if(
                      nqp::atkey($storage,"kv"),        # :kv
                      (index,value),
                      nqp::stmts(                       # no truthy or different
                        (my str $key =
                          nqp::iterkey_s(nqp::shift(nqp::iterator($storage)))),
                        nqp::if(
                          (nqp::iseq_s($key,"k")        # :!k || :!p || :!kv
                            || nqp::iseq_s($key,"p")
                            || nqp::iseq_s($key,"kv")),
                          value,
                          nqp::if(
                            nqp::iseq_s($key,"v"),      # :!v
                            Failure.new("Specified a negated :v adverb"),
                            Failure.new(X::Adverb.new(  # :foo ??
                              :$what,
                              :source(try { self.VAR.name } // self.WHAT.perl),
                              :unexpected(%a.keys)))
                          )
                        )
                      )
                    )
                  )
                )
              ),
              Failure.new(X::Adverb.new(                # multiple adverbs ??
                :$what,
                :source(try { self.VAR.name } // self.WHAT.perl),
                :nogo(%a.keys.grep: /k|v|p/)
                :unexpected(%a.keys.grep: { !.match(/k|v|p/) } )))
            ),
            value                                       # no adverb
          )
        )
    }

    proto method grep(|) is nodal { * }
    multi method grep(Bool:D $t) {
        X::Match::Bool.new( type => '.grep').throw
    }
    multi method grep(Mu $t) {
        my $storage := nqp::getattr(%_,Map,'$!storage');
        if nqp::iseq_i(nqp::elems($storage),0) {
            nqp::istype($t,Regex:D)
              ?? self!grep-accepts: $t
              !! nqp::istype($t,Callable:D)
                   ?? self!grep-callable: $t
                   !! self!grep-accepts: $t
        }
        elsif nqp::iseq_i(nqp::elems($storage),1) {
            if nqp::atkey($storage,"k") {
                nqp::istype($t,Regex:D)
                  ?? self!grep-k: { $t.ACCEPTS($_) }
                  !! nqp::istype($t,Callable:D)
                       ?? self!grep-k: $t
                       !! self!grep-k: { $t.ACCEPTS($_) }
            }
            elsif nqp::atkey($storage,"kv") {
                nqp::istype($t,Regex:D)
                  ?? self!grep-kv: { $t.ACCEPTS($_) }
                  !! nqp::istype($t,Callable:D)
                       ?? self!grep-kv: $t
                       !! self!grep-kv: { $t.ACCEPTS($_) }
            }
            elsif nqp::atkey($storage,"p") {
                nqp::istype($t,Regex:D)
                  ?? self!grep-p: { $t.ACCEPTS($_) }
                  !! nqp::istype($t,Callable:D)
                       ?? self!grep-p: $t
                       !! self!grep-p: { $t.ACCEPTS($_) }
            }
            elsif nqp::atkey($storage,"v") {
                nqp::istype($t,Regex:D)
                  ?? self!grep-accepts: $t
                  !! nqp::istype($t,Callable:D)
                       ?? self!grep-callable: $t
                       !! self!grep-accepts: $t
            }
            else {
                my str $key =
                  nqp::iterkey_s(nqp::shift(nqp::iterator($storage)));
                if nqp::iseq_s($key,"k") || nqp::iseq_s($key,"kv") || nqp::iseq_s($key,"p") {
                    nqp::istype($t,Regex:D)
                      ?? self!grep-accepts: $t
                      !! nqp::istype($t,Callable:D)
                           ?? self!grep-callable: $t
                           !! self!grep-accepts: $t
                }
                else {
                    nqp::iseq_s($key,"k")
                      ?? die "Specified a negated :v adverb"
                      !! X::Adverb.new(
                           :what<grep>,
                           :source(try { self.VAR.name } // self.WHAT.perl),
                           :unexpected($key)
                         ).throw
                }
            }
        }
        else {
            X::Adverb.new(
              :what<grep>,
              :source(try { self.VAR.name } // self.WHAT.perl),
              :nogo(%_.keys.grep: /k|v|kv|p/)
              :unexpected(%_.keys.grep: { !.match(/k|v|kv|p/) } )
            ).throw
        }
    }

    proto method first(|) is nodal { * }
    multi method first(Bool:D $t) {
        Failure.new(X::Match::Bool.new( type => '.first' ))
    }
    # need to handle Regex differently, since it is also Callable
    multi method first(Regex:D $test, :$end, *%a) is raw {
        $end
          ?? self!first-accepts-end($test,%a)
          !! self!first-accepts($test,%a)
    }
    multi method first(Callable:D $test, :$end, *%a is copy) is raw {
        if $end {
            nqp::stmts(
              (my $elems = self.elems),
              nqp::if(
                ($elems && nqp::not_i($elems == Inf)),
                nqp::stmts(
                  (my int $index = $elems),
                  nqp::while(
                    nqp::isge_i(($index = nqp::sub_i($index,1)),0),
                    nqp::if(
                      $test(self.AT-POS($index)),
                      return self!first-result(
                        $index,self.AT-POS($index),'first :end',%a)
                    )
                  ),
                  Nil
                ),
                Nil
              )
            )
        }
        else {
            nqp::stmts(
              (my $iter := self.iterator),
              (my int $index),
              nqp::until(
                (nqp::eqaddr(($_ := $iter.pull-one),IterationEnd)
                  || $test($_)),
                ($index = nqp::add_i($index,1))
              ),
              nqp::if(
                nqp::eqaddr($_,IterationEnd),
                Nil,
                self!first-result($index,$_,'first',%a)
              )
            )
        }
    }
    multi method first(Mu $test = True, :$end, *%a) is raw {
        $end
          ?? self!first-accepts-end($test,%a)
          !! self!first-accepts($test,%a)
    }
    method !first-accepts(Mu $test,%a) is raw {
        nqp::stmts(
          (my $iter := self.iterator),
          (my int $index),
          nqp::until(
            (nqp::eqaddr(($_ := $iter.pull-one),IterationEnd)
              || $test.ACCEPTS($_)),
            ($index = nqp::add_i($index,1))
          ),
          nqp::if(
            nqp::eqaddr($_,IterationEnd),
            Nil,
            self!first-result($index,$_,'first',%a)
          )
        )
    }
    method !first-accepts-end(Mu $test,%a) is raw {
        nqp::stmts(
          (my $elems = self.elems),
          nqp::if(
            ($elems && nqp::not_i($elems == Inf)),
            nqp::stmts(
              (my int $index = $elems),
              nqp::while(
                nqp::isge_i(($index = nqp::sub_i($index,1)),0),
                nqp::if(
                  $test.ACCEPTS(self.AT-POS($index)),
                  return self!first-result(
                    $index,self.AT-POS($index),'first :end',%a)
                )
              ),
              Nil
            ),
            Nil
          )
        )
    }
    method !iterator-and-first($action,\first) is raw {
        nqp::if(
          self.is-lazy,
          X::Cannot::Lazy.new(:$action).throw,
          nqp::stmts(
            (my $iterator := self.iterator),
            nqp::until(
              nqp::eqaddr((my $pulled := $iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::isconcrete($pulled),
                nqp::stmts(
                  (first = $pulled),
                  (return $iterator)
                )
              )
            ),
            Mu
          )
        )
    }

    proto method min (|) is nodal { * }
    multi method min() {
        nqp::stmts(
          nqp::if(
            (my $iter := self!iterator-and-first(".min",my $min)),
            nqp::until(
              nqp::eqaddr((my $pulled := $iter.pull-one),IterationEnd),
              nqp::if(
                (nqp::isconcrete($pulled) && $pulled cmp $min < 0),
                $min = $pulled
              )
            )
          ),
          nqp::if(nqp::defined($min),$min,Inf)
        )
    }
    multi method min(&by) {
        nqp::stmts(
          (my $cmp := nqp::if(
            nqp::iseq_i(&by.arity,2),&by,{ &by($^a) cmp &by($^b) })),
          nqp::if(
            (my $iter := self!iterator-and-first(".min",my $min)),
            nqp::until(
              nqp::eqaddr((my $pulled := $iter.pull-one),IterationEnd),
              nqp::if(
                (nqp::isconcrete($pulled) && $cmp($pulled,$min) < 0),
                $min = $pulled
              )
            )
          ),
          nqp::if(nqp::defined($min),$min,Inf)
        )
    }

    proto method max (|) is nodal { * }
    multi method max() {
        nqp::stmts(
          nqp::if(
            (my $iter := self!iterator-and-first(".max",my $max)),
            nqp::until(
              nqp::eqaddr((my $pulled := $iter.pull-one),IterationEnd),
              nqp::if(
                (nqp::isconcrete($pulled) && $pulled cmp $max > 0),
                $max = $pulled
              )
            )
          ),
          nqp::if(nqp::defined($max),$max,-Inf)
        )
    }
    multi method max(&by) {
        nqp::stmts(
          (my $cmp := nqp::if(
            nqp::iseq_i(&by.arity,2),&by,{ &by($^a) cmp &by($^b) })),
          nqp::if(
            (my $iter := self!iterator-and-first(".max",my $max)),
            nqp::until(
              nqp::eqaddr((my $pulled := $iter.pull-one),IterationEnd),
              nqp::if(
                (nqp::isconcrete($pulled) && $cmp($pulled,$max) > 0),
                $max = $pulled
              )
            )
          ),
          nqp::if(nqp::defined($max),$max,-Inf)
        )
    }

    method !minmax-range-init(\value,\mi,\exmi,\ma,\exma --> Nil) {
        mi   = value.min;
        exmi = value.excludes-min;
        ma   = value.max;
        exma = value.excludes-max;
    }
    method !minmax-range-check(\value,\mi,\exmi,\ma,\exma --> Nil) {
        nqp::stmts(
          nqp::if(
            ((value.min cmp mi) < 0),
            nqp::stmts(
              (mi   = value.min),
              (exmi = value.excludes-min)
            )
          ),
          nqp::if(
            ((value.max cmp ma) > 0),
            nqp::stmts(
              (ma   = value.max),
              (exma = value.excludes-max)
            )
          )
        )
    }
    method !cmp-minmax-range-check(\value,$cmp,\mi,\exmi,\ma,\exma --> Nil) {
        nqp::stmts(                     # $cmp sigillless confuses the optimizer
          nqp::if(
            ($cmp(value.min,mi) < 0),
            nqp::stmts(
              (mi   = value.min),
              (exmi = value.excludes-min)
            )
          ),
          nqp::if(
            ($cmp(value.max,ma) > 0),
            nqp::stmts(
              (ma   = value.max),
              (exma = value.excludes-max)
            )
          )
        )
    }

    proto method minmax (|) is nodal { * }
    multi method minmax() {
        nqp::stmts(
          nqp::if(
            (my $iter := self!iterator-and-first(".minmax",my $pulled)),
            nqp::stmts(
              nqp::if(
                nqp::istype($pulled,Range),
                self!minmax-range-init($pulled,
                  my $min,my int $excludes-min,my $max,my int $excludes-max),
                nqp::if(
                  nqp::istype($pulled,Positional),
                  self!minmax-range-init($pulled.minmax, # recurse for min/max
                    $min,$excludes-min,$max,$excludes-max),
                  ($min = $max = $pulled)
                )
              ),
              nqp::until(
                nqp::eqaddr(($pulled := $iter.pull-one),IterationEnd),
                nqp::if(
                  nqp::isconcrete($pulled),
                  nqp::if(
                    nqp::istype($pulled,Range),
                    self!minmax-range-check($pulled,
                       $min,$excludes-min,$max,$excludes-max),
                    nqp::if(
                      nqp::istype($pulled,Positional),
                      self!minmax-range-check($pulled.minmax,
                         $min,$excludes-min,$max,$excludes-max),
                      nqp::if(
                        (($pulled cmp $min) < 0),
                        ($min = $pulled),
                        nqp::if(
                          (($pulled cmp $max) > 0),
                          ($max = $pulled)
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          nqp::if(
            nqp::defined($min),
            Range.new($min,$max,:$excludes-min,:$excludes-max),
            Range.new(Inf,-Inf)
          )
        )
    }
    multi method minmax(&by) {
        nqp::stmts(
          nqp::if(
            (my $iter := self!iterator-and-first(".minmax",my $pulled)),
            nqp::stmts(
              (my $cmp = nqp::if(
                nqp::iseq_i(&by.arity,2),&by,{ &by($^a) cmp &by($^b) })
              ),
              nqp::if(
                nqp::istype($pulled,Range),
                self!minmax-range-init($pulled,
                  my $min,my int $excludes-min,my $max,my int $excludes-max),
                nqp::if(
                  nqp::istype($pulled,Positional),
                  self!minmax-range-init($pulled.minmax(&by), # recurse min/max
                    $min,$excludes-min,$max,$excludes-max),
                  ($min = $max = $pulled)
                )
              ),
              nqp::until(
                nqp::eqaddr(($pulled := $iter.pull-one),IterationEnd),
                nqp::if(
                  nqp::isconcrete($pulled),
                  nqp::if(
                    nqp::istype($pulled,Range),
                    self!cmp-minmax-range-check($pulled,
                       $cmp,$min,$excludes-min,$max,$excludes-max),
                    nqp::if(
                      nqp::istype($pulled,Positional),
                      self!cmp-minmax-range-check($pulled.minmax(&by),
                         $cmp,$min,$excludes-min,$max,$excludes-max),
                      nqp::if(
                        ($cmp($pulled,$min) < 0),
                        ($min = $pulled),
                        nqp::if(
                          ($cmp($pulled,$max) > 0),
                          ($max = $pulled)
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          nqp::if(
            nqp::defined($min),
            Range.new($min,$max,:$excludes-min,:$excludes-max),
            Range.new(Inf,-Inf)
          )
        )
    }

    proto method sort(|) is nodal { * }
    multi method sort() {
        nqp::if(
          nqp::eqaddr(
            self.iterator.push-until-lazy(my $list := IterationBuffer.new),
            IterationEnd
          ),
          Seq.new(
            Rakudo::Iterator.ReifiedList(
              Rakudo::Internals.MERGESORT-REIFIED-LIST(
                nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',$list)
              )
            )
          ),
          X::Cannot::Lazy.new(:action<sort>).throw
        )
    }
    multi method sort(&by) {
        nqp::stmts(
          nqp::unless(
            nqp::eqaddr(
              self.iterator.push-until-lazy(my $list := IterationBuffer.new),
              IterationEnd
            ),
            X::Cannot::Lazy.new(:action<sort>).throw
          ),
          Seq.new(
            Rakudo::Iterator.ReifiedList(
              nqp::if(
                nqp::eqaddr(&by,&infix:<cmp>),
                Rakudo::Internals.MERGESORT-REIFIED-LIST(
                  nqp::p6bindattrinvres(
                    nqp::create(List),List,'$!reified',$list)
                ),
                nqp::if(
                  &by.count < 2,
                  Rakudo::Internals.MERGESORT-REIFIED-LIST-AS(
                    nqp::p6bindattrinvres(
                      nqp::create(List),List,'$!reified',$list),
                    &by
                  ),
                  Rakudo::Internals.MERGESORT-REIFIED-LIST-WITH(
                    nqp::p6bindattrinvres(
                      nqp::create(List),List,'$!reified',$list),
                    &by
                  )
                )
              )
            )
          )
        )
    }

    method collate {
        self.sort(&[coll]);
    }
    sub find-reducer-for-op(&op) {
        nqp::if(
          nqp::iseq_s(&op.prec("prec"),"f="),
          &METAOP_REDUCE_LISTINFIX,
          nqp::if(
            nqp::iseq_i(nqp::chars(my str $assoc = &op.prec("assoc")),0),
            &METAOP_REDUCE_LEFT,
            ::(nqp::concat('&METAOP_REDUCE_',nqp::uc($assoc)))
          )
        )
    }

    proto method reduce(|) { * }
    multi method reduce(&with) is nodal {
        return unless self.DEFINITE;
        my $reducer := find-reducer-for-op(&with);
        $reducer(&with)(self) if $reducer;
    }

    proto method produce(|) { * }
    multi method produce(&with) is nodal {
        return unless self.DEFINITE;
        my $reducer := find-reducer-for-op(&with);
        $reducer(&with,1)(self) if $reducer;
    }

    proto method unique(|) is nodal {*}
    multi method unique() {
        Seq.new(class :: does Iterator {
            has Mu $!iter;
            has $!seen;
            method !SET-SELF(\list) {
                $!iter = list.iterator;
                $!seen := nqp::hash();
                self
            }
            method new(\list) { nqp::create(self)!SET-SELF(list) }
            method pull-one() {
                my Mu $value;
                my str $needle;
                nqp::until(
                  nqp::eqaddr(($value := $!iter.pull-one),IterationEnd),
                  nqp::unless(
                    nqp::existskey($!seen,$needle = nqp::unbox_s($value.WHICH)),
                    nqp::stmts(
                      nqp::bindkey($!seen, $needle, 1),
                      return $value
                    )
                  )
                );
                IterationEnd
            }
            method push-all($target) {
                my Mu $value;
                my str $needle;
                nqp::until(
                  nqp::eqaddr(($value := $!iter.pull-one),IterationEnd),
                  nqp::unless(
                    nqp::existskey($!seen,$needle = nqp::unbox_s($value.WHICH)),
                    nqp::stmts(  # doesn't sink
                      nqp::bindkey($!seen, $needle, 1),
                      $target.push($value)
                    )
                  )
                );
                IterationEnd
            }
            method is-lazy() { $!iter.is-lazy }
        }.new(self))
    }
    multi method unique( :&as!, :&with! ) {
        my @seen;  # should be Mu, but doesn't work in settings :-(
        my Mu $target;
        gather self.map: {
            $target = &as($_);
            if first( { with($target,$_) }, @seen ) =:= Nil {
                @seen.push($target);
                take $_;
            }
        };
    }
    multi method unique( :&as! ) {
        Seq.new(class :: does Iterator {
            has Mu $!iter;
            has &!as;
            has $!seen;
            method !SET-SELF(\list, &!as) {
                $!iter  = list.iterator;
                $!seen := nqp::hash();
                self
            }
            method new(\list, &as) { nqp::create(self)!SET-SELF(list, &as) }
            method pull-one() {
                my Mu $value;
                my str $needle;
                nqp::until(
                  nqp::eqaddr(($value := $!iter.pull-one),IterationEnd),
                  nqp::unless(
                    nqp::existskey($!seen,$needle = nqp::unbox_s(&!as($value).WHICH)),
                    nqp::stmts(
                      nqp::bindkey($!seen, $needle, 1),
                      return $value
                    )
                  )
                );
                IterationEnd
            }
            method push-all($target) {
                my Mu $value;
                my str $needle;
                nqp::until(
                  nqp::eqaddr(($value := $!iter.pull-one),IterationEnd),
                  nqp::unless(
                    nqp::existskey($!seen,$needle = nqp::unbox_s(&!as($value).WHICH)),
                    nqp::stmts(  # doesn't sink
                      nqp::bindkey($!seen, $needle, 1),
                      $target.push($value)
                    )
                  )
                );
                IterationEnd
            }
        }.new(self, &as))
    }
    multi method unique( :&with! ) {
        nextwith() if &with === &[===]; # use optimized version

        my @seen;  # should be Mu, but doesn't work in settings :-(
        my Mu $target;
        gather self.map: {
            $target := $_;
            if first( { with($target,$_) }, @seen ) =:= Nil {
                @seen.push($target);
                take $_;
            }
        }
    }

    proto method repeated(|) is nodal {*}
    multi method repeated() {
        Seq.new(class :: does Iterator {
            has Mu $!iter;
            has $!seen;
            method !SET-SELF(\list) {
                $!iter = list.iterator;
                $!seen := nqp::hash();
                self
            }
            method new(\list) { nqp::create(self)!SET-SELF(list) }
            method pull-one() {
                my Mu $value;
                my str $needle;
                nqp::until(
                  nqp::eqaddr(($value := $!iter.pull-one),IterationEnd),
                  nqp::existskey($!seen,$needle = nqp::unbox_s($value.WHICH))
                    ?? return $value
                    !! nqp::bindkey($!seen, $needle, 1)
                );
                IterationEnd
            }
            method push-all($target) {
                my Mu $value;
                my str $needle;
                nqp::until( # doesn't sink
                  nqp::eqaddr(($value := $!iter.pull-one),IterationEnd),
                  nqp::existskey($!seen,$needle = nqp::unbox_s($value.WHICH))
                    ?? $target.push($value)
                    !! nqp::bindkey($!seen, $needle, 1)
                );
                IterationEnd
            }
            method is-lazy() { $!iter.is-lazy }
        }.new(self))
    }
    multi method repeated( :&as!, :&with! ) {
        my @seen;  # should be Mu, but doesn't work in settings :-(
        my Mu $target;
        gather self.map: {
            $target = &as($_);
            first( { with($target,$_) }, @seen ) =:= Nil
              ?? @seen.push($target)
              !! take $_;
        };
    }
    multi method repeated( :&as! ) {
        Seq.new(class :: does Iterator {
            has Mu $!iter;
            has &!as;
            has $!seen;
            method !SET-SELF(\list, &!as) {
                $!iter  = list.iterator;
                $!seen := nqp::hash();
                self
            }
            method new(\list, &as) { nqp::create(self)!SET-SELF(list, &as) }
            method pull-one() {
                my Mu $value;
                my str $needle;
                nqp::until(
                  nqp::eqaddr(($value := $!iter.pull-one),IterationEnd),
                  nqp::existskey($!seen,$needle = nqp::unbox_s(&!as($value).WHICH))
                    ?? return $value
                    !! nqp::bindkey($!seen, $needle, 1)
                );
                IterationEnd
            }
            method push-all($target) {
                my Mu $value;
                my str $needle;
                nqp::until(  # doesn't sink
                  nqp::eqaddr(($value := $!iter.pull-one),IterationEnd),
                  nqp::existskey($!seen,$needle = nqp::unbox_s(&!as($value).WHICH))
                    ?? $target.push($value)
                    !! nqp::bindkey($!seen, $needle, 1)
                );
                IterationEnd
            }
            method is-lazy() { $!iter.is-lazy }
        }.new(self, &as))
    }
    multi method repeated( :&with! ) {
        nextwith() if &with === &[===]; # use optimized version

        my @seen;  # should be Mu, but doesn't work in settings :-(
        my Mu $target;
        gather self.map: {
            $target := $_;
            first( { with($target,$_) }, @seen ) =:= Nil
              ?? @seen.push($target)
              !! take $_;
        }
    }

    proto method squish(|) is nodal {*}
    multi method squish( :&as!, :&with = &[===] ) {
        Seq.new(class :: does Iterator {
            has Mu $!iter;
            has &!as;
            has &!with;
            has $!last_as;
            has int $!first;
            method !SET-SELF(\list, &!as, &!with) {
                $!iter  = list.iterator;
                $!first = 1;
                self
            }
            method new(\list, &as, &with) {
                nqp::create(self)!SET-SELF(list, &as, &with)
            }
            method pull-one() {
                my Mu $value := $!iter.pull-one;
                unless nqp::eqaddr($value,IterationEnd) {
                    my $which := &!as($value);
                    if $!first {
                        $!first = 0;
                    }
                    else {
                        until !with($!last_as, $which) or ($value := $!iter.pull-one) =:= IterationEnd {
                            $!last_as = $which;
                            $which := &!as($value);
                        }
                    }
                    $!last_as = $which;
                }
                $value;
            }
            method push-all($target) {
                my Mu $value := $!iter.pull-one;
                unless nqp::eqaddr($value,IterationEnd) {
                    my $which;
                    my $last_as := $!last_as;
                    nqp::if(
                      $!first,
                      nqp::stmts(  # doesn't sink
                        ($target.push($value)),
                        ($which := &!as($value)),
                        ($last_as := $which),
                        ($value := $!iter.pull-one)
                      )
                    );
                    nqp::until(
                      nqp::eqaddr($value,IterationEnd),
                      nqp::stmts(
                        nqp::unless(  # doesn't sink
                          with($last_as,$which := &!as($value)),
                          $target.push($value)
                        ),
                        ($last_as := $which),
                        ($value := $!iter.pull-one)
                      )
                    );
                }
                IterationEnd
            }
            method is-lazy() { $!iter.is-lazy }
        }.new(self, &as, &with))
    }
    multi method squish( :&with = &[===] ) {
        Seq.new(class :: does Iterator {
            has Mu $!iter;
            has &!with;
            has Mu $!last;
            has int $!first;
            method !SET-SELF(\list, &!with) {
                $!iter  = list.iterator;
                $!first = 1;
                self
            }
            method new(\list, &with) { nqp::create(self)!SET-SELF(list, &with) }
            method pull-one() {
                my Mu $value := $!iter.pull-one;
                unless nqp::eqaddr($value,IterationEnd) {
                    if $!first {
                        $!first = 0;
                    }
                    else {
                        my $ov = $value;
                        until !with($!last, $value)
                           or ($value := $!iter.pull-one) =:= IterationEnd {
                            $!last = $ov;
                            $ov = $value;
                        }
                    }
                    $!last = $value
                }
                $value;
            }
            method push-all($target) {
                my Mu $value := $!iter.pull-one;
                unless nqp::eqaddr($value,IterationEnd) {
                    my $last_val = $!last;
                    nqp::if(
                      $!first,
                      nqp::stmts(  # doesn't sink
                        ($target.push($value)),
                        ($last_val := $value),
                        ($value := $!iter.pull-one)
                      )
                    );
                    nqp::until(
                      nqp::eqaddr($value,IterationEnd),
                      nqp::stmts(
                        nqp::unless(  # doesn't sink
                          with($last_val, $value),
                          $target.push($value)
                        ),
                        ($last_val := $value),
                        ($value := $!iter.pull-one)
                      )
                    );
                }
                IterationEnd
            }
            method is-lazy() { $!iter.is-lazy }
        }.new(self, &with))
    }

    proto method pairup(|) is nodal { * }
    multi method pairup(Any:U:) { () }
    multi method pairup(Any:D:) {
        my \iter = nqp::istype(self, Iterable)
            ?? self.iterator
            !! self.list.iterator;
        gather loop {
            my $it := iter.pull-one;
            if nqp::istype($it, Pair) {
                take $it.key => $it.value
            }
            elsif nqp::istype($it, Map) and !nqp::iscont($it) {
                take Slip.new(|$it.pairs)
            }
            elsif $it =:= IterationEnd {
                last
            }
            else {
                my $it-value := iter.pull-one;
                if $it-value =:= IterationEnd {
                    X::Pairup::OddNumber.new.throw;
                }
                take $it => $it-value;
            }
        }
    }

    proto method head(|) { * }
    multi method head(Any:D:) is raw {
        nqp::if(
          nqp::eqaddr((my $pulled := self.iterator.pull-one),IterationEnd),
          Nil,
          $pulled
        )
    }
    multi method head(Any:D: $n) {
        Seq.new(
          Rakudo::Iterator.NextNValues(self.iterator,$n)
        )
    }

    proto method tail(|) { * }
    multi method tail(Any:D:) is raw {
        nqp::if(
          nqp::eqaddr((my $pulled :=
            Rakudo::Iterator.LastValue(self.iterator,'tail')),
            IterationEnd
          ),
          Nil,
          $pulled
        )
    }
    multi method tail(Any:D: $n) {
        Seq.new(
          Rakudo::Iterator.LastNValues(self.iterator,$n,'tail')
        )
    }

    proto method minpairs(|) { * }
    multi method minpairs(Any:D:) {
        my @found;
        my $min = Inf;
        my $value;
        for self.pairs {
            if ($value := .value) < $min {
                @found = $_;
                $min   = $value;
            }
            elsif $value == $min {
                @found.push: $_;
            }
        }
        @found
    }

    proto method maxpairs(|) { * }
    multi method maxpairs(Any:D:) {
        my @found;
        my $max = -Inf;
        my $value;
        for self.pairs {
            if ($value := .value) > $max {
                @found = $_;
                $max   = $value;
            }
            elsif $value == $max {
                @found.push: $_;
            }
        }
        @found
    }

    proto method batch(|) is nodal { * }
    multi method batch(Any:D: Int:D :$elems!) {
        Seq.new(Rakudo::Iterator.Batch(self.iterator,$elems,1))
    }
    multi method batch(Any:D: Int:D $batch) {
        Seq.new(Rakudo::Iterator.Batch(self.iterator,$batch,1))
    }

    proto method rotor(|) is nodal { * }
    multi method rotor(Any:D: Int:D $batch, :$partial) {
        Seq.new(Rakudo::Iterator.Batch(self.iterator,$batch,$partial))
    }
    multi method rotor(Any:D: *@cycle, :$partial) {
        Seq.new(Rakudo::Iterator.Rotor(self.iterator,@cycle,$partial))
    }

    proto method skip(|) { * }
    multi method skip()         { Seq.new(self.iterator).skip }
    multi method skip(Int() $n) { Seq.new(self.iterator).skip($n) }
}

BEGIN Attribute.^compose;

proto sub infix:<min>(|) is pure { * }
multi sub infix:<min>(Mu:D \a, Mu:U) { a }
multi sub infix:<min>(Mu:U, Mu:D \b) { b }
multi sub infix:<min>(Mu:D \a, Mu:D \b) { (a cmp b) < 0 ?? a !! b }
multi sub infix:<min>(+args is raw) { args.min }
sub min(+args, :&by = &infix:<cmp>) { args.min(&by) }

proto sub infix:<max>(|) is pure { * }
multi sub infix:<max>(Mu:D \a, Mu:U) { a }
multi sub infix:<max>(Mu:U, Mu:D \b) { b }
multi sub infix:<max>(Mu:D \a, Mu:D \b) { (a cmp b) > 0 ?? a !! b }
multi sub infix:<max>(+args) { args.max }
sub max(+args, :&by = &infix:<cmp>) { args.max(&by) }

proto sub infix:<minmax>(|) is pure { * }
multi sub infix:<minmax>(+args) { args.minmax }
sub minmax(+args, :&by = &infix:<cmp>) { args.minmax(&by) }

proto sub map(|) {*}
multi sub map(&code, +values) { my $laze = values.is-lazy; values.map(&code).lazy-if($laze) }

proto sub grep(|) {*}
multi sub grep(Mu $test, +values, *%a) {
    my $laze = values.is-lazy;
    values.grep($test,|%a).lazy-if($laze)
}
multi sub grep(Bool:D $t, |) { X::Match::Bool.new(:type<grep>).throw }

proto sub first(|) {*}
multi sub first(Bool:D $t, |) { Failure.new(X::Match::Bool.new(:type<first>)) }
multi sub first(Mu $test, +values, *%a) { values.first($test,|%a) }

proto sub join(|) { * }
multi sub join($sep = '', *@values) { @values.join($sep) }

proto sub reduce (|) { * }
multi sub reduce (&with, +list)  { list.reduce(&with) }

proto sub produce (|) { * }
multi sub produce (&with, +list)  { list.produce(&with) }

proto sub unique(|) { * }
multi sub unique(+values, |c) { my $laze = values.is-lazy; values.unique(|c).lazy-if($laze) }

proto sub squish(|) { * }
multi sub squish(+values, |c) { my $laze = values.is-lazy; values.squish(|c).lazy-if($laze) }

proto sub repeated(|) { * }
multi sub repeated(+values, |c) { my $laze = values.is-lazy; values.repeated(|c).lazy-if($laze) }

proto sub sort(|) {*}
multi sub sort(&by, @values) { @values.sort(&by) }
multi sub sort(&by, +values) { values.sort(&by) }
multi sub sort(@values)      { @values.sort }
multi sub sort(+values)      { values.sort }

# vim: ft=perl6 expandtab sw=4
