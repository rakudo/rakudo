my class X::Cannot::Empty { ... }
my class X::Cannot::Lazy  { ... }
my class X::Cannot::Map   { ... }
my class Rakudo::Sorting  { ... }

# Now that Iterable is defined, we add extra methods into Any for the list
# operations. (They can't go into Any right away since we need Attribute to
# define the various roles, and Attribute inherits from Any. We will do a
# re-compose of Attribute to make sure it gets the list methods at the end
# of this file. Note the general pattern for these list-y methods is that
# they check if they have an Iterable already, and if not obtain one to
# work on by doing a .list coercion.
use MONKEY-TYPING;
augment class Any {

    # Because the first occurrence of "method chrs" is in the intarray
    # role, we need to create the proto earlier in the setting.  That's
    # why it is not in unicodey.
    proto method chrs(*%) is pure {*}

    # A helper method for throwing an exception because of a lazy iterator,
    # to help reduce bytecode size in hot code paths, making it more likely
    # that the (conditional) caller of this method, can be inlined.
    method throw-iterator-cannot-be-lazy(
      str $action, str $what = self.^name
    ) is hidden-from-backtrace is implementation-detail {
        X::Cannot::Lazy.new(:$action, :$what).throw
    }

    # A helper method for creating a failure because of a lazy iterator, to
    # to help reduce bytecode size in hot code paths, making it more likely
    # that the (conditional) caller of this method, can be inlined.
    method fail-iterator-cannot-be-lazy(
      str $action, str $what = self.^name
    ) is hidden-from-backtrace is implementation-detail {
        Failure.new(X::Cannot::Lazy.new(:$action, :$what))
    }

    # A helper method for throwing an exception because of an array being
    # empty, to help reduce bytecode size in hot code paths, making it more
    # likely that the (conditional) caller of this method, can be inlined.
    method throw-cannot-be-empty(
      str $action, str $what = self.^name
    ) is hidden-from-backtrace is implementation-detail {
        X::Cannot::Empty.new(:$action, :$what).throw
    }

    # A helper method for creating a failure because of an array being empty
    # to help reduce bytecode size in hot code paths, making it more likely
    # that the (conditional) caller of this method, can be inlined.
    method fail-cannot-be-empty(
      str $action, str $what = self.^name
    ) is hidden-from-backtrace is implementation-detail {
        Failure.new(X::Cannot::Empty.new(:$action, :$what))
    }

    proto method map(|) is nodal {*}
    multi method map(Hash:D \hash) {
        X::Cannot::Map.new(
          what       => self.^name,
          using      => "a {hash.^name}",
          suggestion =>
"Did you mean to add a stub (\{ ... \}) or did you mean to .classify?"
        ).throw;
    }
    multi method map(Iterable:D \iterable) {
        X::Cannot::Map.new(
          what       => self.^name,
          using      => "a {iterable.^name}",
          suggestion =>
"Did a * (Whatever) get absorbed by a comma, range, series, or list repetition?
Consider using a block if any of these are necessary for your mapping code."
        ).throw;
    }
    multi method map(|c) {
        X::Cannot::Map.new(
          what       => self.^name,
          using      => "'{c.raku.substr(2).chop}'",
          suggestion => "Did a * (Whatever) get absorbed by a list?"
        ).throw;
    }

    multi method map(\SELF: &block;; :$label, :$item) {
        sequential-map(($item ?? (SELF,) !! SELF).iterator, &block, $label);
    }

    my class IterateOneWithPhasers does Rakudo::SlippyIterator {
        has &!block;
        has $!source;
        has $!label;
        has Int $!NEXT;         # SHOULD BE int, but has Int performs better
        has Int $!did-init;     # SHOULD BE int, but has Int performs better
        has Int $!did-iterate;  # SHOULD BE int, but has Int performs better

        method !SET-SELF(\block,\source,\label) {
            &!block  := block;
            $!source := source;
            $!label  := nqp::decont(label);
            $!NEXT = block.has-phaser('NEXT');
            self
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
              &!block.fire_if_phasers('LAST')
            );
            $result
        }

        method push-all(\target --> IterationEnd) {
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

            nqp::if(
              $!slipping,
              nqp::until(
                nqp::eqaddr(($value := self.slip-one),IterationEnd),
                target.push($value)
              )
            );

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
                            self.slip-all($pulled,target),
                            target.push($pulled)
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
            nqp::if($!did-iterate,&!block.fire_if_phasers('LAST'))
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

            nqp::if(
              $!slipping,
              nqp::until(
                nqp::eqaddr(self.slip-one,IterationEnd),
                nqp::null
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
            nqp::if($!did-iterate,&!block.fire_if_phasers('LAST'))
        }
    }

    my class IterateOneNotSlippingWithoutPhasers does Iterator {
        has &!block;
        has $!source;
        has $!label;

        method new(&block,$source,\label) {
            my $iter := nqp::create(self);
            nqp::bindattr($iter, self, '&!block', &block);
            nqp::bindattr($iter, self, '$!source', $source);
            nqp::bindattr($iter, self, '$!label', nqp::decont(label));
            $iter
        }

        method is-lazy() { $!source.is-lazy }

        method pull-one() is raw {
            if nqp::eqaddr((my $pulled := $!source.pull-one),IterationEnd) {
                IterationEnd
            }
            else {
                my $result;
                my int $stopped;
                nqp::stmts(
                  nqp::until(
                    $stopped,
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

        method push-all(\target --> IterationEnd) {
            my $pulled;
            my int $stopped;
            nqp::until(
              nqp::eqaddr(($pulled := $!source.pull-one),IterationEnd),
               nqp::stmts(
                ($stopped = 0),
                nqp::until(
                  $stopped,
                  nqp::stmts(
                    ($stopped = 1),
                    nqp::handle(
                      target.push(&!block($pulled)),
                      'LABELED', $!label,
                      'REDO', ($stopped = 0),
                      'NEXT', nqp::null, # need NEXT for next LABEL support
                      'LAST', return
                    )
                  ),
                  :nohandler
                )
              )
            )
        }

        method sink-all(--> IterationEnd) {
            my $pulled;
            my int $stopped;
            nqp::until(
              nqp::eqaddr(($pulled := $!source.pull-one),IterationEnd),
              nqp::stmts(
                ($stopped = 0),
                nqp::until(
                  $stopped,
                  nqp::stmts(
                    ($stopped = 1),
                    nqp::handle(
                      &!block($pulled),
                      'LABELED', $!label,
                      'REDO', ($stopped = 0),
                      'NEXT', nqp::null, # need NEXT for next LABEL support
                      'LAST', return
                    )
                  ),
                  :nohandler
                )
              )
            )
        }
    }

    my class IterateOneWithoutPhasers does Rakudo::SlippyIterator {
        has &!block;
        has $!source;
        has $!label;

        method new(&block,$source,\label) {
            my $iter := nqp::create(self);
            nqp::bindattr($iter, self, '&!block', &block);
            nqp::bindattr($iter, self, '$!source', $source);
            nqp::bindattr($iter, self, '$!label', nqp::decont(label));
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

        method push-all(\target --> IterationEnd) {
            my $value;
            nqp::if(
              $!slipping,
              nqp::until(
                nqp::eqaddr(($value := self.slip-one),IterationEnd),
                target.push($value)
              )
            );

            nqp::until(
              nqp::eqaddr(($value := $!source.pull-one),IterationEnd),
              nqp::stmts(
                (my int $redo = 1),
                nqp::while(
                  $redo,
                  nqp::stmts(
                    ($redo = 0),
                    nqp::handle(
                      nqp::if(
                        nqp::istype((my $result := &!block($value)),Slip),
                        self.slip-all($result,target),
                        target.push($result)
                      ),
                      'LABELED', $!label,
                      'REDO', ($redo = 1),
                      'LAST', return,
                      'NEXT', nqp::null, # need NEXT for next LABEL support
                    )
                  ),
                  :nohandler
                )
              )
            );
        }

        method sink-all(--> IterationEnd) {
            nqp::if(
              $!slipping,
              nqp::until(
                nqp::eqaddr(self.slip-one,IterationEnd),
                nqp::null
              )
            );

            nqp::until(
              nqp::eqaddr((my $value := $!source.pull-one()),IterationEnd),
              nqp::stmts(
                (my int $redo = 1),
                nqp::while(
                  $redo,
                  nqp::stmts(
                    ($redo = 0),
                    nqp::handle(  # doesn't sink
                      &!block($value),
                      'LABELED', $!label,
                      'NEXT', nqp::null,  # need NEXT for next LABEL support
                      'REDO', ($redo = 1),
                      'LAST', return
                    ),
                  :nohandler
                  )
                )
              )
            );
        }
    }

    my class IterateTwoWithoutPhasers does Rakudo::SlippyIterator {
        has &!block;
        has $!source;
        has $!label;

        method new(&block,$source,\label) {
            my $iter := nqp::create(self);
            nqp::bindattr($iter, self, '&!block', &block);
            nqp::bindattr($iter, self, '$!source', $source);
            nqp::bindattr($iter, self, '$!label', nqp::decont(label));
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

        method push-all(\target --> IterationEnd) {
            my $value;
            nqp::if(
              $!slipping,
              nqp::until(
                nqp::eqaddr(($value := self.slip-one),IterationEnd),
                target.push($value)
              )
            );

            nqp::until(
              nqp::eqaddr(($value := $!source.pull-one),IterationEnd),
              nqp::stmts(
                (my int $redo = 1),
                nqp::while(
                  $redo,
                  nqp::stmts(
                    ($redo = 0),
                    nqp::handle(
                      nqp::if(
                        nqp::eqaddr(
                          (my $value2 := $!source.pull-one),
                          IterationEnd
                        ),
                        nqp::stmts(
                          (my $result := &!block($value)),
                          nqp::if(
                            nqp::istype($result,Slip),
                            self.slip-all($result,target),
                            target.push($result)
                          ),
                          return
                        ),
                        nqp::if(
                          nqp::istype(
                            ($result := &!block($value,$value2)),
                            Slip
                          ),
                          self.slip-all($result,target),
                          target.push($result)
                        )
                      ),
                      'LABELED', $!label,
                      'REDO', ($redo = 1),
                      'LAST', return,
                      'NEXT', nqp::null, # need NEXT for next LABEL support
                    )
                  ),
                  :nohandler
                )
              )
            );
        }

        method sink-all(--> IterationEnd) {
            nqp::if(
              $!slipping,
              nqp::until(
                nqp::eqaddr(self.slip-one,IterationEnd),
                nqp::null,
              )
            );

            nqp::until(
              nqp::eqaddr((my $value := $!source.pull-one()),IterationEnd),
              nqp::stmts(
                (my int $redo = 1),
                nqp::while(
                  $redo,
                  nqp::stmts(
                    ($redo = 0),
                    nqp::handle(  # doesn't sink
                      nqp::if(
                        nqp::eqaddr(
                          (my $value2 := $!source.pull-one),
                          IterationEnd
                        ),
                        nqp::stmts(
                          (&!block($value)),
                          return
                        ),
                        (&!block($value,$value2))
                      ),
                      'LABELED', $!label,
                      'NEXT', nqp::null,  # need NEXT for next LABEL support
                      'REDO', ($redo = 1),
                      'LAST', return
                    )
                  ),
                :nohandler
                )
              )
            );
        }
    }

    my class IterateMoreWithPhasers does Rakudo::SlippyIterator {
        has &!block;
        has $!source;
        has $!count;
        has $!label;
        has $!value-buffer;
        has $!did-init;
        has $!did-iterate;
        has $!NEXT;
        has $!CAN_FIRE_PHASERS;

        method new(&block, $source, $count, \label) {
            my $iter := nqp::create(self);
            nqp::bindattr($iter, self, '&!block', &block);
            nqp::bindattr($iter, self, '$!source', $source);
            nqp::bindattr($iter, self, '$!count', $count);
            nqp::bindattr($iter, self, '$!label', nqp::decont(label));
            $iter
        }

        method is-lazy() { $!source.is-lazy }

        method pull-one() is raw {
            nqp::isconcrete($!value-buffer)
              ?? nqp::setelems($!value-buffer,0)
              !! ($!value-buffer := nqp::create(IterationBuffer));

            my int $redo = 1;
            my $result;

            if !$!did-init && nqp::can(&!block, 'fire_phasers') {
                $!did-init         = 1;
                $!CAN_FIRE_PHASERS = 1;
                $!NEXT             = &!block.has-phaser('NEXT');
                nqp::p6setfirstflag(&!block)
                  if &!block.has-phaser('FIRST');
            }

            if $!slipping && nqp::not_i(
              nqp::eqaddr(($result := self.slip-one),IterationEnd)) {
                # $result will be returned at the end
            }
            elsif nqp::eqaddr(
              $!source.push-exactly($!value-buffer,$!count),IterationEnd)
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
            &!block.fire_if_phasers('LAST')
              if $!CAN_FIRE_PHASERS
              && $!did-iterate
              && nqp::eqaddr($result, IterationEnd);
            $result
        }
    }

    sub sequential-map(\source, &block, \label) {
        # We want map to be fast, so we go to some effort to build special
        # case iterators that can ignore various interesting cases.
        my $count := &block.count;

        Seq.new(
          nqp::istype(&block,Block) && &block.has-phasers
            ?? $count < 2 || $count === Inf
              ?? IterateOneWithPhasers.new(&block,source,label)
              !! IterateMoreWithPhasers.new(&block,source,$count,label)
            !! $count < 2 || $count === Inf
              ?? nqp::istype(Slip,&block.returns)
                ?? IterateOneWithoutPhasers.new(&block,source,label)
                !! IterateOneNotSlippingWithoutPhasers.new(&block,source,label)
              !! $count == 2
                ?? IterateTwoWithoutPhasers.new(&block,source,label)
                !! IterateMoreWithPhasers.new(&block,source,$count,label)
        )
    }

    proto method flatmap (|) is nodal {*}
    multi method flatmap(&block, :$label) {
        self.map(&block, :$label).flat
    }

    my class Grep-K does Iterator {
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
            nqp::until(
              nqp::eqaddr(($_ := $!iter.pull-one),IterationEnd)
                || $!test($_),
              ($!index = nqp::add_i($!index,1))
            );

            nqp::eqaddr($_,IterationEnd)
              ?? IterationEnd
              !! nqp::p6box_i($!index = nqp::add_i($!index,1))
        }
        method push-all(\target --> IterationEnd) {
            my $iter := $!iter;  # lexicals faster than attrs
            my $test := $!test;
            my int $i = $!index;

            nqp::until(
              nqp::eqaddr(($_ := $!iter.pull-one),IterationEnd),
              nqp::stmts(
                ($i = nqp::add_i($i,1)),
                nqp::if(
                  $!test($_),
                  target.push(nqp::p6box_i($i))
                )
              )
            );
            $!index = $i;
        }
        method is-deterministic(--> Bool:D) { $!iter.is-deterministic }
    }
    method !grep-k(Callable:D $test) { Seq.new(Grep-K.new(self,$test)) }

    my class Grep-KV does Iterator {
        has  Mu $!iter;
        has  Mu $!test;
        has int $!index;
        has  Mu $!value;
        method !SET-SELF(\list,Mu \test) {
            $!iter  = list.iterator;
            $!test := test;
            $!index = -1;
            self
        }
        method new(\list,Mu \test) { nqp::create(self)!SET-SELF(list,test) }
        method pull-one() is raw {
            nqp::if(
              nqp::isconcrete($!value),
              nqp::stmts(
                ($_ := $!value),
                ($!value := nqp::null),
                $_
              ),
              nqp::stmts(
                nqp::until(
                  nqp::eqaddr(($_ := $!iter.pull-one),IterationEnd)
                    || $!test($_),
                  ($!index = nqp::add_i($!index,1))
                ),
                nqp::if(
                  nqp::eqaddr($_,IterationEnd),
                  IterationEnd,
                  nqp::stmts(
                    ($!value := $_),
                    nqp::p6box_i($!index = nqp::add_i($!index,1))
                  )
                )
              )
            )
        }
        method push-all(\target --> IterationEnd) {
            nqp::until(
              nqp::eqaddr(($_ := $!iter.pull-one),IterationEnd),
              nqp::stmts(
                $!index = nqp::add_i($!index,1);
                nqp::if(
                  $!test($_),
                  nqp::stmts(  # doesn't sink
                    target.push(nqp::p6box_i($!index));
                    target.push($_);
                  )
                )
              )
            );
        }
        method is-deterministic(--> Bool:D) { $!iter.is-deterministic }
    }
    method !grep-kv(Callable:D $test) { Seq.new(Grep-KV.new(self,$test)) }

    my class Grep-P does Iterator {
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
            nqp::until(
              nqp::eqaddr(($_ := $!iter.pull-one),IterationEnd)
                || $!test($_),
              ($!index = nqp::add_i($!index,1))
            );

            nqp::eqaddr($_,IterationEnd)
              ?? IterationEnd
              !! Pair.new(($!index = nqp::add_i($!index,1)),$_)
        }
        method push-all(\target --> IterationEnd) {
            my $iter := $!iter;   # lexicals are faster than attrs
            my $test := $!test;
            my int $i = $!index;

            nqp::until(
              nqp::eqaddr(($_ := $iter.pull-one),IterationEnd),
              nqp::stmts(
                ($i = nqp::add_i($i,1)),
                nqp::if(
                  $test($_),
                  target.push(Pair.new($i,$_))
                )
              )
            );
            $!index = $i;
        }
        method is-deterministic(--> Bool:D) { $!iter.is-deterministic }
    }
    method !grep-p(Callable:D $test) { Seq.new(Grep-P.new(self,$test)) }

    role Grepper does Iterator {
        has Mu $!iter;
        has Mu $!test;
        method !SET-SELF(\list,Mu \test) {
            $!iter  = list.iterator;
            $!test := test;
            self
        }
        method new(\list,Mu \test) { nqp::create(self)!SET-SELF(list,test) }
        method is-lazy() { $!iter.is-lazy }
        method is-deterministic(--> Bool:D) { $!iter.is-deterministic }
    }
    method !grep-callable(Callable:D $test) {
        nqp::if(
          $test.count == 1,
          sequential-map(
            self.iterator,
            {
                (nqp::istype((my \result := $test($_)),Regex)
                  ?? result.ACCEPTS($_)
                  !! nqp::istype(result,Junction)
                    ?? result.Bool
                    !! result
                ) ?? $_
                  !! Empty
            },
            Any)
          ,
          nqp::stmts(
            (my role CheatArity {
                has $!arity;
                has $!count;

                method set-cheat($new-arity, $new-count --> Nil) {
                    $!arity = $new-arity;
                    $!count = $new-count;
                }

                method arity(Code:D:) { $!arity }
                method count(Code:D:) { $!count }
            }),
            (my &tester = -> |c {
                #note "*cough* {c.raku} -> {$test(|c).raku}";
                next unless $test(|c);
                c.list
            } but CheatArity),
            &tester.set-cheat($test.arity, $test.count),
            self.map(&tester)
          )
        )
    }

    # Create a braid and fail cursor that we can use with all the normal,
    # "boring", regex matches that are on the Regex type. This saves them
    # being created every single time.
    my $cursor := Match.'!cursor_init'('');
    my $braid := $cursor.braid;
    my $fail_cursor := $cursor.'!cursor_start_cur'();

    my class Grep-Regex does Grepper {
        method pull-one() is raw {
            nqp::until(
              nqp::eqaddr(($_ := $!iter.pull-one),IterationEnd)
                || nqp::isge_i(
                     nqp::getattr_i(
                       $!test.(Match.'!cursor_init'(
                         .Str, :c(0), :$braid, :$fail_cursor
                       )),
                       Match,
                       '$!pos'
                     ),
                     0
                   ),
              nqp::null
            );
            $_
        }
        method push-all(\target --> IterationEnd) {
            nqp::until(
              nqp::eqaddr(($_ := $!iter.pull-one),IterationEnd),
              nqp::if(  # doesn't sink
                nqp::isge_i(
                  nqp::getattr_i(
                    $!test.(Match.'!cursor_init'(
                      .Str, :c(0), :$braid, :$fail_cursor
                    )),
                    Match,
                    '$!pos'
                  ),
                  0
                ),
                target.push($_)
              )
            );
        }
    }
    method !grep-regex(Mu $test) { Seq.new(Grep-Regex.new(self,$test)) }

    my class Grep-Accepts does Grepper {
        method pull-one() is raw {
            nqp::until(
              nqp::eqaddr(($_ := $!iter.pull-one),IterationEnd)
                || $!test.ACCEPTS($_),
              nqp::null
            );
            $_
        }
        method push-all(\target --> IterationEnd) {
            nqp::until(
              nqp::eqaddr(($_ := $!iter.pull-one),IterationEnd),
              nqp::if(  # doesn't sink
                $!test.ACCEPTS($_),
                target.push($_)
              )
            );
        }
    }
    method !grep-accepts(Mu $test) { Seq.new(Grep-Accepts.new(self,$test)) }

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
                              :source(try { self.VAR.name } // self.WHAT.raku),
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
                :source(try { self.VAR.name } // self.WHAT.raku),
                :nogo(%a.keys.grep: /k|v|p/),
                :unexpected(%a.keys.grep: { !.match(/k|v|p/) } )))
            ),
            value                                       # no adverb
          )
        )
    }

    proto method grep(|) is nodal {*}
    multi method grep(Bool:D $t) {
        X::Match::Bool.new( type => '.grep').throw
    }
    multi method grep(Mu $t) {
        my $storage := nqp::getattr(%_,Map,'$!storage');
        if nqp::iseq_i(nqp::elems($storage),0) {
            nqp::istype($t,Regex:D)
              ?? self!grep-regex: $t
              !! nqp::istype($t,Callable:D)
                   ?? self!grep-callable: $t
                   !! self!grep-accepts: $t
        }
        elsif nqp::iseq_i(nqp::elems($storage),1) {
            if nqp::atkey($storage,"k") {
                nqp::istype($t,Regex:D)
                  ?? self!grep-k: { $t.ACCEPTS($_) }
                  !! nqp::istype($t,Callable:D)
                       ?? self!grep-k: self!wrap-callable-for-grep($t)
                       !! self!grep-k: { $t.ACCEPTS($_) }
            }
            elsif nqp::atkey($storage,"kv") {
                nqp::istype($t,Regex:D)
                  ?? self!grep-kv: { $t.ACCEPTS($_) }
                  !! nqp::istype($t,Callable:D)
                       ?? self!grep-kv: self!wrap-callable-for-grep($t)
                       !! self!grep-kv: { $t.ACCEPTS($_) }
            }
            elsif nqp::atkey($storage,"p") {
                nqp::istype($t,Regex:D)
                  ?? self!grep-p: { $t.ACCEPTS($_) }
                  !! nqp::istype($t,Callable:D)
                       ?? self!grep-p: self!wrap-callable-for-grep($t)
                       !! self!grep-p: { $t.ACCEPTS($_) }
            }
            elsif nqp::atkey($storage,"v") {
                nqp::istype($t,Regex:D)
                  ?? self!grep-regex: $t
                  !! nqp::istype($t,Callable:D)
                       ?? self!grep-callable: self!wrap-callable-for-grep($t)
                       !! self!grep-accepts: $t
            }
            else {
                my str $key =
                  nqp::iterkey_s(nqp::shift(nqp::iterator($storage)));
                if nqp::iseq_s($key,"k") || nqp::iseq_s($key,"kv") || nqp::iseq_s($key,"p") {
                    nqp::istype($t,Regex:D)
                      ?? self!grep-regex: $t
                      !! nqp::istype($t,Callable:D)
                           ?? self!grep-callable: self!wrap-callable-for-grep($t)
                           !! self!grep-accepts: $t
                }
                else {
                    nqp::iseq_s($key,"k")
                      ?? die "Specified a negated :v adverb"
                      !! X::Adverb.new(
                           :what<grep>,
                           :source(try { self.VAR.name } // self.WHAT.raku),
                           :unexpected($key)
                         ).throw
                }
            }
        }
        else {
            X::Adverb.new(
              :what<grep>,
              :source(try { self.VAR.name } // self.WHAT.raku),
              :nogo(%_.keys.grep: /k|v|kv|p/),
              :unexpected(%_.keys.grep: { !.match(/k|v|kv|p/) } )
            ).throw
        }
    }

    method !wrap-callable-for-grep($test) {
        ({
            nqp::istype((my \result := $test($_)),Regex)
              ?? result.ACCEPTS($_)
              !! nqp::istype(result,Junction)
                ?? result.Bool
                !! result
        })
    }

    proto method first(|) is nodal {*}
    multi method first(Bool:D $t) {
        Failure.new(X::Match::Bool.new( type => '.first' ))
    }
    # need to handle Regex differently, since it is also Callable
    multi method first(Regex:D $test, :$end, *%a) is raw {
        $end
          ?? self!first-regex-end($test,%a)
          !! self!first-regex($test,%a)
    }
    multi method first(Callable:D $test, :$end, *%a is copy) is raw {
        if $end {
            my $elems = self.elems;
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
        }
        else {
            my $iter := self.iterator;
            my int $index;
            nqp::until(
              (nqp::eqaddr(($_ := $iter.pull-one),IterationEnd)
                || $test($_)),
              ($index = nqp::add_i($index,1))
            );

            nqp::eqaddr($_,IterationEnd)
              ?? Nil
              !! self!first-result($index,$_,'first',%a)
        }
    }
    multi method first(Mu $test, :$end, *%a) is raw {
        $end
          ?? self!first-accepts-end($test,%a)
          !! self!first-accepts($test,%a)
    }
    multi method first(:$end, *%a) is raw {
        nqp::elems(nqp::getattr(%a,Map,'$!storage'))
          ?? $end
            ?? self!first-accepts-end(True,%a)
            !! self!first-accepts(True,%a)
          !! $end
            ?? self.tail
            !! self.head
    }
    method !first-regex(Mu $test, %a) is raw {
        my $iter := self.iterator;
        my int $index;

        nqp::until(
          nqp::eqaddr(($_ := $iter.pull-one),IterationEnd)
            || nqp::isge_i(
                 nqp::getattr_i(
                   $test.(Match.'!cursor_init'(
                     .Str, :c(0), :$braid, :$fail_cursor
                   )),
                   Match,
                   '$!pos'
                 ),
                 0
               ),
          ($index = nqp::add_i($index,1))
        );

        nqp::eqaddr($_,IterationEnd)
          ?? Nil
          !! self!first-result($index,$_,'first',%a)
    }
    method !first-regex-end(Mu $test, %a) is raw {
        my $elems = self.elems;
        nqp::if(
          ($elems && nqp::not_i($elems == Inf)),
          nqp::stmts(
            (my int $index = $elems),
            nqp::while(
              nqp::isge_i(($index = nqp::sub_i($index,1)),0),
              nqp::if(
                nqp::isge_i(
                  nqp::getattr_i(
                    $test.(Match.'!cursor_init'(
                      self.AT-POS($index).Str, :c(0), :$braid, :$fail_cursor
                    )),
                    Match,
                    '$!pos'
                  ),
                  0
                ),
                return self!first-result(
                  $index,self.AT-POS($index),'first :end',%a)
              )
            ),
            Nil
          ),
          Nil
        )
    }
    method !first-accepts(Mu $test, %a) is raw {
        my $iter := self.iterator;
        my int $index;

        nqp::until(
          (nqp::eqaddr(($_ := $iter.pull-one),IterationEnd)
            || $test.ACCEPTS($_)),
          ($index = nqp::add_i($index,1))
        );

        nqp::eqaddr($_,IterationEnd)
          ?? Nil
          !! self!first-result($index,$_,'first',%a)
    }
    method !first-accepts-end(Mu $test, %a) is raw {
        my $elems = self.elems;
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

    proto method sum(*%) is nodal {*}
    multi method sum(Any:D:) {
        nqp::if(
          (my \iterator := self.iterator).is-lazy,
          self.fail-iterator-cannot-be-lazy('.sum'),
          nqp::stmts(
            (my $sum := 0),
            nqp::until(
              nqp::eqaddr((my \pulled := iterator.pull-one),IterationEnd),
              ($sum := $sum + pulled)
            ),
            $sum
          )
        )
    }

    proto method min (|) is nodal {*}
    multi method min() {
        nqp::if(
          (my $iter := self!iterator-and-first(".min",my $min)),
          nqp::until(
            nqp::eqaddr((my $pulled := $iter.pull-one),IterationEnd),
            nqp::if(
              (nqp::isconcrete($pulled) && $pulled cmp $min < 0),
              $min = $pulled
            )
          )
        );

        nqp::defined($min) ?? $min !! Inf
    }
    multi method min(&by) {
        my $cmp := nqp::if(
          nqp::iseq_i(&by.arity,2),
          &by,
          { &by($^a) cmp &by($^b) }
        );
        nqp::if(
          (my $iter := self!iterator-and-first(".min",my $min)),
          nqp::until(
            nqp::eqaddr((my $pulled := $iter.pull-one),IterationEnd),
            nqp::if(
              (nqp::isconcrete($pulled) && $cmp($pulled,$min) < 0),
              $min = $pulled
            )
          )
        );

        nqp::defined($min) ?? $min !! Inf
    }

    proto method max (|) is nodal {*}
    multi method max() {
        nqp::if(
          (my $iter := self!iterator-and-first(".max",my $max)),
          nqp::until(
            nqp::eqaddr((my $pulled := $iter.pull-one),IterationEnd),
            nqp::if(
              (nqp::isconcrete($pulled) && $pulled cmp $max > 0),
              $max = $pulled
            )
          )
        );

        nqp::defined($max) ??  $max !! -Inf
    }
    multi method max(&by) {
        my $cmp := nqp::if(
          nqp::iseq_i(&by.arity,2),
          &by,
          { &by($^a) cmp &by($^b) }
        );

        nqp::if(
          (my $iter := self!iterator-and-first(".max",my $max)),
          nqp::until(
            nqp::eqaddr((my $pulled := $iter.pull-one),IterationEnd),
            nqp::if(
              (nqp::isconcrete($pulled) && $cmp($pulled,$max) > 0),
              $max = $pulled
            )
          )
        );

        nqp::defined($max) ?? $max !! -Inf
    }

    method !minmax-range-init(\value,\mi,\exmi,\ma,\exma --> Nil) {
        mi   = value.min;
        exmi = value.excludes-min;
        ma   = value.max;
        exma = value.excludes-max;
    }
    method !minmax-range-check(\value,\mi,\exmi,\ma,\exma --> Nil) {
        nqp::if(
          ((value.min cmp mi) < 0),
          nqp::stmts(
            (mi   = value.min),
            (exmi = value.excludes-min)
          )
        );

        nqp::if(
          ((value.max cmp ma) > 0),
          nqp::stmts(
            (ma   = value.max),
            (exma = value.excludes-max)
          )
        );
    }
    method !cmp-minmax-range-check(\value,$cmp,\mi,\exmi,\ma,\exma --> Nil) {
        nqp::if(                        # $cmp sigillless confuses the optimizer
          ($cmp(value.min,mi) < 0),
          nqp::stmts(
            (mi   = value.min),
            (exmi = value.excludes-min)
          )
        );

        nqp::if(
          ($cmp(value.max,ma) > 0),
          nqp::stmts(
            (ma   = value.max),
            (exma = value.excludes-max)
          )
        );
    }

    proto method minmax (|) is nodal {*}
    multi method minmax() {
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
        );

        nqp::defined($min)
          ?? Range.new($min,$max,:$excludes-min,:$excludes-max)
          !! Range.new(Inf,-Inf)
    }
    multi method minmax(&by) {
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
        );

        nqp::defined($min)
          ?? Range.new($min,$max,:$excludes-min,:$excludes-max)
          !! Range.new(Inf,-Inf)
    }

    proto method sort(|) is nodal {*}
    multi method sort() {
        nqp::eqaddr(
          self.iterator.push-until-lazy(
            my \buf := nqp::create(IterationBuffer)),
          IterationEnd
        ) ?? Seq.new(
               Rakudo::Iterator.ReifiedList(
                 Rakudo::Sorting.MERGESORT-REIFIED-LIST(buf.List)
               )
             )
          !! X::Cannot::Lazy.new(:action<sort>).throw
    }
    multi method sort(&by) {
        nqp::unless(
          nqp::eqaddr(
            self.iterator.push-until-lazy(
              my \buf := nqp::create(IterationBuffer)),
            IterationEnd
          ),
          X::Cannot::Lazy.new(:action<sort>).throw
        );

        Seq.new(
          Rakudo::Iterator.ReifiedList(
            nqp::eqaddr(&by,&infix:<cmp>)
              ?? Rakudo::Sorting.MERGESORT-REIFIED-LIST(buf.List)
              !! &by.count < 2
                ?? Rakudo::Sorting.MERGESORT-REIFIED-LIST-AS(  buf.List,&by)
                !! Rakudo::Sorting.MERGESORT-REIFIED-LIST-WITH(buf.List,&by)
          )
        )
    }

    proto method collate(|) {*}
    multi method collate() { self.sort(&[coll]) }

    sub find-reducer-for-op(&op) {
        nqp::iseq_s(&op.prec("prec"),"f=")
          ?? &METAOP_REDUCE_LISTINFIX
          !! nqp::iseq_i(nqp::chars(my str $assoc = &op.prec("assoc")),0)
            ?? &METAOP_REDUCE_LEFT
            !! ::(nqp::concat('&METAOP_REDUCE_',nqp::uc($assoc)))
    }

    proto method reduce(|) is nodal {*}
    multi method reduce(Any:U: & --> Nil) { }
    multi method reduce(Any:D: &with) {
        (find-reducer-for-op(&with))(&with)(self)
    }

    proto method produce(|) is nodal {*}
    multi method produce(Any:U: & --> Nil) { }
    multi method produce(Any:D: &with) {
        (find-reducer-for-op(&with))(&with,1)(self)
    }

    proto method slice(|) is nodal { * }
    multi method slice(Any:D: *@indices --> Seq:D) { self.Seq.slice(@indices) }

    proto method unique(|) is nodal {*}

    my class Unique does Iterator {
        has $!iter;
        has $!seen;
        method !SET-SELF(\list) {
            $!iter := list.iterator;
            $!seen := nqp::hash;
            self
        }
        method new(\list) { nqp::create(self)!SET-SELF(list) }
        method pull-one() is raw {
            nqp::until(
              nqp::eqaddr((my \pulled := $!iter.pull-one),IterationEnd)
                || (nqp::not_i(nqp::existskey(
                  $!seen,
                  (my \needle := pulled.WHICH)
                )) && nqp::bindkey($!seen,needle,1)),
              nqp::null
            );
            pulled
        }
        method push-all(\target --> IterationEnd) {
            nqp::until(
              nqp::eqaddr((my \pulled := $!iter.pull-one),IterationEnd),
              nqp::unless(
                nqp::existskey($!seen,(my \needle := pulled.WHICH)),
                nqp::stmts(
                  nqp::bindkey($!seen,needle,1),
                  target.push(pulled)
                )
              )
            )
        }
        method is-lazy() { $!iter.is-lazy }
        method is-deterministic(--> Bool:D) { $!iter.is-deterministic }
        method sink-all(--> IterationEnd) { $!iter.sink-all }
    }
    multi method unique() { Seq.new(Unique.new(self)) }

    multi method unique( :&as!, :&with! ) {
        nqp::eqaddr(&with,&[===]) # use optimized version
          ?? self.unique(:&as)
          !! Seq.new(
               Rakudo::Iterator.UniqueRepeatedAsWith(self.iterator,&as,&with,1)
             )
    }

    my class Unique-As does Iterator {
        has Mu $!iter;
        has &!as;
        has $!seen;
        method !SET-SELF(\list, &!as) {
            $!iter  = list.iterator;
            $!seen := nqp::hash();
            self
        }
        method new(\list, &as) { nqp::create(self)!SET-SELF(list, &as) }
        method pull-one() is raw {
            nqp::until(
              nqp::eqaddr((my \value := $!iter.pull-one),IterationEnd),
              nqp::unless(
                nqp::existskey($!seen,my \needle := &!as(value).WHICH),
                nqp::stmts(
                  nqp::bindkey($!seen,needle,1),
                  return-rw value
                )
              )
            );
            IterationEnd
        }
        method push-all(\target --> IterationEnd) {
            nqp::until(
              nqp::eqaddr((my \value := $!iter.pull-one),IterationEnd),
              nqp::unless(
                nqp::existskey($!seen,my \needle := &!as(value).WHICH),
                nqp::stmts(  # doesn't sink
                  nqp::bindkey($!seen,needle,1),
                  target.push(value)
                )
              )
            )
        }
        method is-deterministic(--> Bool:D) { $!iter.is-deterministic }
    }
    multi method unique( :&as! ) { Seq.new(Unique-As.new(self,&as)) }

    multi method unique( :&with! ) {
        nqp::eqaddr(&with,&[===]) # use optimized version
          ?? self.unique
          !! Seq.new(Rakudo::Iterator.UniqueRepeatedWith(self.iterator,&with,1))
    }

    proto method repeated(|) is nodal {*}

    my class Repeated does Iterator {
        has Mu $!iter;
        has $!seen;
        method !SET-SELF(\list) {
            $!iter = list.iterator;
            $!seen := nqp::hash();
            self
        }
        method new(\list) { nqp::create(self)!SET-SELF(list) }
        method pull-one() is raw {
            my Mu $value;
            my str $needle;
            nqp::until(
              nqp::eqaddr(($value := $!iter.pull-one),IterationEnd),
              nqp::existskey($!seen,$needle = nqp::unbox_s($value.WHICH))
                ?? return-rw $value
                !! nqp::bindkey($!seen, $needle, 1)
            );
            IterationEnd
        }
        method push-all(\target --> IterationEnd) {
            my Mu $value;
            my str $needle;
            nqp::until( # doesn't sink
              nqp::eqaddr(($value := $!iter.pull-one),IterationEnd),
              nqp::existskey($!seen,$needle = nqp::unbox_s($value.WHICH))
                ?? target.push($value)
                !! nqp::bindkey($!seen, $needle, 1)
            );
        }
        method is-lazy() { $!iter.is-lazy }
        method is-deterministic(--> Bool:D) { $!iter.is-deterministic }
    }
    multi method repeated() { Seq.new(Repeated.new(self)) }

    multi method repeated( :&as!, :&with! ) {
        nqp::eqaddr(&with,&[===]) # use optimized version
          ?? self.repeated(:&as)
          !! Seq.new(
               Rakudo::Iterator.UniqueRepeatedAsWith(self.iterator,&as,&with,0)
             )
    }

    class Repeated-As does Iterator {
        has Mu $!iter;
        has &!as;
        has $!seen;
        method !SET-SELF(\list, &!as) {
            $!iter  = list.iterator;
            $!seen := nqp::hash();
            self
        }
        method new(\list, &as) { nqp::create(self)!SET-SELF(list, &as) }
        method pull-one() is raw {
            my Mu $value;
            my str $needle;
            nqp::until(
              nqp::eqaddr(($value := $!iter.pull-one),IterationEnd),
              nqp::existskey($!seen,$needle = nqp::unbox_s(&!as($value).WHICH))
                ?? return-rw $value
                !! nqp::bindkey($!seen, $needle, 1)
            );
            IterationEnd
        }
        method push-all(\target --> IterationEnd) {
            my Mu $value;
            my str $needle;
            nqp::until(  # doesn't sink
              nqp::eqaddr(($value := $!iter.pull-one),IterationEnd),
              nqp::existskey($!seen,$needle = nqp::unbox_s(&!as($value).WHICH))
                ?? target.push($value)
                !! nqp::bindkey($!seen, $needle, 1)
            );
        }
        method is-lazy() { $!iter.is-lazy }
        method is-deterministic(--> Bool:D) { $!iter.is-deterministic }
    }
    multi method repeated( :&as! ) { Seq.new(Repeated-As.new(self,&as)) }

    multi method repeated( :&with! ) {
        nqp::eqaddr(&with,&[===]) # use optimized version
          ?? self.repeated
          !! Seq.new(Rakudo::Iterator.UniqueRepeatedWith(self.iterator,&with,0))
    }

    proto method squish(|) is nodal {*}

    my class Squish-As does Iterator {
        has Mu $!iter;
        has &!as;
        has &!with;
        has $!last_as;
        has int $!first;
        method !SET-SELF($!iter, &!as, &!with) {
            $!first = 1;
            self
        }
        method new(\iter, \as, \with) {
            nqp::create(self)!SET-SELF(iter, as, with)
        }
        method pull-one() is raw {
            nqp::if(
              nqp::eqaddr((my $pulled := $!iter.pull-one),IterationEnd),
              IterationEnd,
              nqp::stmts(
                (my $which := &!as($pulled)),
                nqp::if(
                  $!first,
                  ($!first = 0),
                  nqp::until(
                    nqp::isfalse(&!with($!last_as,$which))
                      || nqp::eqaddr(
                           ($pulled := $!iter.pull-one),
                           IterationEnd
                         ),
                    nqp::stmts(
                      ($!last_as := $which),
                      ($which := &!as($pulled))
                    )
                  )
                ),
                ($!last_as := $which),
                $pulled
              )
            )
        }
        method push-all(\target --> IterationEnd) {
            my Mu $value := $!iter.pull-one;
            unless nqp::eqaddr($value,IterationEnd) {
                my $which;
                my $last_as := $!last_as;
                nqp::if(
                  $!first,
                  nqp::stmts(  # doesn't sink
                    (target.push($value)),
                    ($which := &!as($value)),
                    ($last_as := $which),
                    ($value := $!iter.pull-one)
                  )
                );
                nqp::until(
                  nqp::eqaddr($value,IterationEnd),
                  nqp::stmts(
                    nqp::unless(  # doesn't sink
                      &!with($last_as,$which := &!as($value)),
                      target.push($value)
                    ),
                    ($last_as := $which),
                    ($value := $!iter.pull-one)
                  )
                );
            }
        }
        method is-lazy() { $!iter.is-lazy }
        method is-deterministic(--> Bool:D) { $!iter.is-deterministic }
    }
    multi method squish( :&as!, :&with = &[===] ) {
        Seq.new(Squish-As.new(self.iterator, &as, &with))
    }

    my class Squish-With does Iterator {
        has Mu $!iter;
        has &!with;
        has Mu $!last;
        has int $!first;
        method !SET-SELF($!iter, &!with) {
            $!first = 1;
            self
        }
        method new(\iter, \with) { nqp::create(self)!SET-SELF(iter, with) }
        method pull-one() is raw {
            nqp::if(
              nqp::eqaddr((my $pulled := $!iter.pull-one),IterationEnd),
              IterationEnd,
              nqp::stmts(
                nqp::if(
                  $!first,
                  ($!first = 0),
                  nqp::stmts(
                    (my $old := $pulled),
                    nqp::until(
                      nqp::isfalse(&!with($!last,$pulled))
                        || nqp::eqaddr(
                             ($pulled := $!iter.pull-one),
                             IterationEnd
                           ),
                      nqp::stmts(
                        ($!last := $old),
                        ($old := $pulled)
                      )
                    )
                  )
                ),
                ($!last := $pulled)
              )
            )
        }
        method push-all(\target --> IterationEnd) {
            my Mu $value := $!iter.pull-one;
            unless nqp::eqaddr($value,IterationEnd) {
                my $last_val = $!last;
                nqp::if(
                  $!first,
                  nqp::stmts(  # doesn't sink
                    (target.push($value)),
                    ($last_val := $value),
                    ($value := $!iter.pull-one)
                  )
                );
                nqp::until(
                  nqp::eqaddr($value,IterationEnd),
                  nqp::stmts(
                    nqp::unless(  # doesn't sink
                      &!with($last_val, $value),
                      target.push($value)
                    ),
                    ($last_val := $value),
                    ($value := $!iter.pull-one)
                  )
                );
            }
        }
        method is-lazy() { $!iter.is-lazy }
        method is-deterministic(--> Bool:D) { $!iter.is-deterministic }
    }
    multi method squish( :&with = &[===] ) {
        Seq.new(Squish-With.new(self.iterator,&with))
    }

    proto method pairup(|) is nodal {*}
    multi method pairup(Any:U:) { ().Seq }
    multi method pairup(Any:D:) {
        my \iter := self.iterator;
        gather {
            nqp::until(
              nqp::eqaddr((my $pulled := iter.pull-one),IterationEnd),
              nqp::if(
                nqp::istype($pulled,Pair),
                (take nqp::p6bindattrinvres(
                  nqp::clone($pulled),
                  Pair,
                  '$!value',
                  nqp::clone(nqp::decont(nqp::getattr($pulled,Pair,'$!value')))
                )),
                nqp::if(
                  nqp::istype($pulled,Map) && nqp::not_i(nqp::iscont($pulled)),
                  (take Slip.from-iterator($pulled.iterator)),
                  nqp::if(
                    nqp::eqaddr((my $value := iter.pull-one),IterationEnd),
                    X::Pairup::OddNumber.new.throw,
                    take Pair.new($pulled,$value)
                  )
                )
              )
            )
        }
    }

    proto method toggle(|) {*}
    multi method toggle(Any:D: Callable:D \condition, :$off!) {
        Seq.new(
          $off
            ?? Rakudo::Iterator.Until(self.iterator, condition)
            !! Rakudo::Iterator.While(self.iterator, condition)
        )
    }
    multi method toggle(Any:D: Callable:D \condition) {
        Seq.new(Rakudo::Iterator.While(self.iterator, condition))
    }
    multi method toggle(Any:D: *@conditions, :$off) {
        Seq.new(
          Rakudo::Iterator.Toggle(self.iterator, @conditions.iterator, !$off)
        )
    }

    proto method head(|) {*}
    multi method head(Any:U: |c) { (self,).head(|c) }
    multi method head(Any:D:) is raw {
        nqp::eqaddr((my $pulled := self.iterator.pull-one),IterationEnd)
          ?? Nil
          !! $pulled
    }
    multi method head(Any:D: Callable:D $w) {
        Seq.new(
           Rakudo::Iterator.AllButLastNValues(self.iterator,-($w(0).Int))
        )
    }
    multi method head(Any:D: $n) {
        Seq.new(Rakudo::Iterator.NextNValues(self.iterator,$n))
    }

    proto method tail(|) {*}
    multi method tail(Any:U: |c) { (self,).tail(|c) }
    multi method tail(Any:D:) is raw {
        nqp::eqaddr((my $pulled :=
          Rakudo::Iterator.LastValue(self.iterator,'tail')),
          IterationEnd
        ) ?? Nil
          !! $pulled
    }
    multi method tail(Any:D: $n) {
        Seq.new(
          nqp::if(
            nqp::istype($n,Callable),
            nqp::stmts(
              (my $iterator := self.iterator),
              nqp::if(
                nqp::isgt_i((my $skip := -($n(0).Int)),0),
                nqp::if(
                  $iterator.skip-at-least($skip),
                  $iterator,
                  Rakudo::Iterator.Empty),
                $iterator)),
            Rakudo::Iterator.LastNValues(self.iterator,$n,'tail')
          )
        )
    }

    proto method skip(|) {*}
    multi method skip() {
        my $iter := self.iterator;
        Seq.new( $iter.skip-one ?? $iter !! Rakudo::Iterator.Empty )
    }
    multi method skip(Whatever) { Seq.new(Rakudo::Iterator.Empty) }
    multi method skip(Callable:D $w) {
       nqp::isgt_i((my $tail := -($w(0).Int)),0)
         ?? self.tail($tail)
         !! Seq.new(Rakudo::Iterator.Empty)
    }
    multi method skip(Int() $n) {
        my $iter := self.iterator;
        Seq.new( $iter.skip-at-least($n) ?? $iter !! Rakudo::Iterator.Empty )
    }

    # Note that the implementation of minpairs/maxpairs only differs in the
    # value against which the result of cmp is compared (-1 for minpairs,
    # +1 for maxpairs).  Abstracting the logic in a helper sub, did not change
    # the binary size significantly, but would introduce a runtime penalty.
    # Hence the almost identical pieces of code here.
    proto method minpairs(|) {*}
    multi method minpairs(Any:D:) {
        my \iter   := self.pairs.iterator;
        my \result := nqp::create(IterationBuffer);
        nqp::until(
          nqp::eqaddr((my \pair := iter.pull-one),IterationEnd)
            || nqp::isconcrete(my \target := pair.value),
          nqp::null
        );
        nqp::unless(
          nqp::eqaddr(pair,IterationEnd),
          nqp::stmts(                               # found at least one value
            nqp::push(result,pair),
            nqp::until(
              nqp::eqaddr(nqp::bind(pair,iter.pull-one),IterationEnd),
              nqp::if(
                nqp::isconcrete(my \value := pair.value),
                nqp::if(
                  nqp::iseq_i((my \cmp-result := value cmp target),-1),
                  nqp::stmts(                       # new best
                    nqp::push(nqp::setelems(result,0),pair),
                    nqp::bind(target,value)
                  ),
                  nqp::if(                          # additional best
                    nqp::iseq_i(cmp-result,0),
                    nqp::push(result,pair)
                  )
                )
              )
            )
          )
        );
        Seq.new(Rakudo::Iterator.ReifiedList(result))
    }

    proto method maxpairs(|) {*}
    multi method maxpairs(Any:D:) {
        my \iter   := self.pairs.iterator;
        my \result := nqp::create(IterationBuffer);
        nqp::until(
          nqp::eqaddr((my \pair := iter.pull-one),IterationEnd)
            || nqp::isconcrete(my \target := pair.value),
          nqp::null
        );
        nqp::unless(
          nqp::eqaddr(pair,IterationEnd),
          nqp::stmts(                               # found at least one value
            nqp::push(result,pair),
            nqp::until(
              nqp::eqaddr(nqp::bind(pair,iter.pull-one),IterationEnd),
              nqp::if(
                nqp::isconcrete(my \value := pair.value),
                nqp::if(
                  nqp::iseq_i((my \cmp-result := value cmp target),+1),
                  nqp::stmts(                       # new best
                    nqp::push(nqp::setelems(result,0),pair),
                    nqp::bind(target,value)
                  ),
                  nqp::if(                          # additional best
                    nqp::iseq_i(cmp-result,0),
                    nqp::push(result,pair)
                  )
                )
              )
            )
          )
        );
        Seq.new(Rakudo::Iterator.ReifiedList(result))
    }

    proto method batch(|) is nodal {*}
    multi method batch(Any:D: Int:D :$elems!) {
        Seq.new(Rakudo::Iterator.Batch(self.iterator,$elems,1))
    }
    multi method batch(Any:D: Int:D $batch) {
        Seq.new(Rakudo::Iterator.Batch(self.iterator,$batch,1))
    }

    proto method rotor(|) is nodal {*}
    multi method rotor(Any:D: Int:D $batch, :$partial) {
        Seq.new(Rakudo::Iterator.Batch(self.iterator,$batch,$partial))
    }
    multi method rotor(Any:D: +@cycle, :$partial) {
        Seq.new(Rakudo::Iterator.Rotor(self.iterator,@cycle,$partial))
    }

    proto method nodemap(|) is nodal {*}
    multi method nodemap(Associative:D: &op) {
        self.new.STORE: self.keys, self.values.nodemap(&op), :INITIALIZE
    }
    multi method nodemap(&op) {
        my \iterator := self.iterator;
        return Failure.new(X::Cannot::Lazy.new(:action<nodemap>))
          if iterator.is-lazy;

        my \buffer   := nqp::create(IterationBuffer);
        my $value    := iterator.pull-one;

        nqp::until(
          nqp::eqaddr($value,IterationEnd),
          nqp::stmts(
            (my int $redo = 1),
            nqp::while(
              $redo,
              nqp::stmts(
                $redo = 0,
                nqp::handle(
                  nqp::push(buffer,op($value)),
                  'NEXT', nqp::null,
                  'REDO', ($redo = 1),
                  'LAST', ($value := IterationEnd),
                ) 
              ),
              :nohandler
            ),
            ($value := iterator.pull-one)
          ) 
        );
        buffer.List
    }   

    proto method deepmap(|) is nodal {*}
    multi method deepmap(Associative:D: &op) {
        self.new.STORE: self.keys, self.values.deepmap(&op), :INITIALIZE
    }
    multi method deepmap(&op) {
        my \iterator := self.iterator;
        my \buffer   := nqp::create(IterationBuffer);
        my $value    := iterator.pull-one;

        sub deep(\value) is raw { my $ = value.deepmap(&op) }

        nqp::until(
          nqp::eqaddr($value,IterationEnd),
          nqp::stmts(
            (my int $redo = 1),
            nqp::while(
              $redo,
              nqp::stmts(
                $redo = 0,
                nqp::handle(
                  nqp::stmts(
                    (my $result := nqp::if(
                      nqp::istype($value,Iterable) && $value.DEFINITE,
                      deep($value),
                      op($value)
                    )),
                    nqp::if(
                      nqp::istype($result,Slip),
                      $result.iterator.push-all(buffer),
                      nqp::push(buffer,$result)
                    ),
                  ),
                  'NEXT', nqp::null,
                  'REDO', ($redo = 1),
                  'LAST', ($value := IterationEnd),
                ) 
              ),
              :nohandler
            ),
            ($value := iterator.pull-one)
          ) 
        );
        nqp::p6bindattrinvres(
          nqp::if(nqp::istype(self,List),self,List).new, # keep subtypes of List
          List,'$!reified',buffer
        ) 
    }   

    proto method duckmap(|) is nodal {*}
    multi method duckmap(Associative:D: &op) {
        self.new.STORE: self.keys, self.values.duckmap(&op)
    }
    multi method duckmap(&op) {
        my \iterator := self.iterator;
        my \buffer   := nqp::create(IterationBuffer);
        my $value    := iterator.pull-one;

        sub duck(\arg) is raw {
            CATCH {
                return nqp::istype(arg,Iterable:D)
                  ?? (my $ = arg.duckmap(&op))
                  !! arg
            }
            op(arg)
        }

        nqp::until(
          nqp::eqaddr($value,IterationEnd),
          nqp::stmts(
            (my int $redo = 1),
            nqp::while(
              $redo,
              nqp::stmts(
                $redo = 0,
                nqp::handle(
                  nqp::stmts(
                    (my $result := duck($value)),
                    nqp::if(
                      nqp::istype($result,Slip),
                      $result.iterator.push-all(buffer),
                      nqp::push(buffer,$result)
                    ),
                  ),
                  'NEXT', nqp::null,
                  'REDO', ($redo = 1),
                  'LAST', ($value := IterationEnd),
                ) 
              ),
              :nohandler
            ),
            ($value := iterator.pull-one)
          ) 
        );
        nqp::p6bindattrinvres(
          nqp::if(nqp::istype(self,List),self,List).new, # keep subtypes of List
          List,'$!reified',buffer
        ) 
    }   
}

BEGIN Attribute.^compose;

proto sub infix:<min>(|) is pure {*}
multi sub infix:<min>(Mu:D \a, Mu:U) { a }
multi sub infix:<min>(Mu:U, Mu:D \b) { b }
multi sub infix:<min>(Mu:D \a, Mu:D \b) { (a cmp b) < 0 ?? a !! b }
multi sub infix:<min>(Int:D \a, Int:D \b) {
    nqp::islt_i(nqp::cmp_I(nqp::decont(a), nqp::decont(b)), 0) ?? a !! b
}
multi sub infix:<min>(int   \a, int   \b) {
    nqp::islt_i(nqp::cmp_i(a, b), 0) ?? a !! b
}
multi sub infix:<min>(Num:D \a, Num:D \b) {
    nqp::islt_i(nqp::cmp_n(a, b), 0) ?? a !! b
}
multi sub infix:<min>(num   \a, num   \b) {
    nqp::islt_i(nqp::cmp_n(a, b), 0) ?? a !! b
}
multi sub infix:<min>(+args is raw) { args.min }

proto sub min(|) is pure {*}
multi sub min(+args, :&by!) { args.min(&by) }
multi sub min(+args)        { args.min      }

proto sub infix:<max>(|) is pure {*}
multi sub infix:<max>(Mu:D \a, Mu:U) { a }
multi sub infix:<max>(Mu:U, Mu:D \b) { b }
multi sub infix:<max>(Mu:D \a, Mu:D \b) { (a cmp b) > 0 ?? a !! b }
multi sub infix:<max>(Int:D \a, Int:D \b) {
    nqp::isgt_i(nqp::cmp_I(nqp::decont(a), nqp::decont(b)), 0) ?? a !! b
}
multi sub infix:<max>(int   \a, int   \b) {
    nqp::isgt_i(nqp::cmp_i(a, b), 0) ?? a !! b
}
multi sub infix:<max>(Num:D \a, Num:D \b) {
    nqp::isgt_i(nqp::cmp_n(a, b), 0) ?? a !! b
}
multi sub infix:<max>(num   \a, num   \b) {
    nqp::isgt_i(nqp::cmp_n(a, b), 0) ?? a !! b
}
multi sub infix:<max>(+args) { args.max }

proto sub max(|) is pure {*}
multi sub max(+args, :&by!) { args.max(&by) }
multi sub max(+args)        { args.max }

proto sub infix:<minmax>(|) is pure {*}
multi sub infix:<minmax>(+args) { args.minmax }

proto sub minmax(|) is pure {*}
multi sub minmax(+args, :&by!) { args.minmax(&by) }
multi sub minmax(+args)        { args.minmax      }

proto sub map($, |) {*}
multi sub map(&code, +values) { my $laze = values.is-lazy; values.map(&code).lazy-if($laze) }

proto sub grep(Mu, |) {*}
multi sub grep(Mu $test, +values, *%a) {
    my $laze = values.is-lazy;
    values.grep($test,|%a).lazy-if($laze)
}
multi sub grep(Bool:D $t, |) { X::Match::Bool.new(:type<grep>).throw }

proto sub first(Mu, |) {*}
multi sub first(Bool:D $t, |) { Failure.new(X::Match::Bool.new(:type<first>)) }
multi sub first(Mu $test, +values, *%a) { values.first($test,|%a) }

proto sub join($?, |) {*}
multi sub join($sep = '', *@values) { @values.join($sep) }

proto sub reduce ($, |) {*}
multi sub reduce (&with, +list)  { list.reduce(&with) }

proto sub produce ($, |) {*}
multi sub produce (&with, +list)  { list.produce(&with) }

proto sub unique(|) {*}
multi sub unique(+values, |c) { my $laze = values.is-lazy; values.unique(|c).lazy-if($laze) }

proto sub squish(|) {*}
multi sub squish(+values, |c) { my $laze = values.is-lazy; values.squish(|c).lazy-if($laze) }

proto sub repeated(|) {*}
multi sub repeated(+values, |c) { my $laze = values.is-lazy; values.repeated(|c).lazy-if($laze) }

proto sub sort(|) {*}
multi sub sort(&by, @values) { @values.sort(&by) }
multi sub sort(&by, +values) { values.sort(&by) }
multi sub sort(@values)      { @values.sort }
multi sub sort(+values)      { values.sort }

proto sub nodemap($, $, *%) {*}
multi sub nodemap(&op, \obj) { obj.nodemap(&op) }

proto sub deepmap($, $, *%) {*}
multi sub deepmap(&op, \obj) { obj.deepmap(&op) }

proto sub duckmap($, $, *%) {*}
multi sub duckmap(&op, \obj) { obj.duckmap(&op) }

# vim: expandtab shiftwidth=4
