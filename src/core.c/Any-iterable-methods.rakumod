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
        X::Cannot::Lazy.new(:$action, :$what).Failure
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
        X::Cannot::Empty.new(:$action, :$what).Failure
    }

    my class IterateOneWithPhasers does Rakudo::SlippyIterator {
        has &!block;
        has $!source;
        has $!label;
        has $!pulled;
        has $!NEXT;
        has $!LAST;

        method new(&block, Iterator:D $source, $label) {
            nqp::if(
              nqp::eqaddr((my $pulled := $source.pull-one),IterationEnd),
              Rakudo::Iterator.Empty,  # nothing to do
              nqp::stmts(              # iterate at least once
                (my $iter := nqp::create(self)),
                nqp::bindattr($iter,self,'$!slipper',nqp::null),
                nqp::bindattr($iter,self,'$!pulled',$pulled),
                nqp::if(       # set up FIRST phaser execution if needed
                  &block.has-phaser('FIRST'),
                  nqp::p6setfirstflag(nqp::getattr(&block, Code, '$!do'))
                ),
                nqp::bindattr($iter,self,'&!block',&block),
                nqp::bindattr($iter,self,'$!source',$source),
                nqp::bindattr($iter,self,'$!label',nqp::decont($label)),
                nqp::bindattr($iter,self,'$!NEXT',
                  &block.callable_for_phaser('NEXT') // nqp::null),
                nqp::bindattr($iter,self,'$!LAST',
                  &block.callable_for_phaser('LAST')),
                $iter
              )
            )
        }

        method is-lazy() { $!source.is-lazy }

        method pull-one() is raw {
            my $value := nqp::null;

            # handle slipping
            nqp::unless(
              nqp::isnull($!slipper),
              nqp::if(
                nqp::eqaddr(($value := self.slip-one),IterationEnd),
                ($value := nqp::null)
              )
            );

            nqp::while(
              nqp::isnull($value)
                && nqp::not_i(nqp::eqaddr($!pulled,IterationEnd)),
              nqp::handle(       # still something to do
                nqp::stmts(
                  ($value   := &!block($!pulled)),
                  ($!pulled := $!source.pull-one),
                  nqp::unless(
                    nqp::isnull($!NEXT),
                    nqp::handle(
                      $!NEXT(),  # control ops inside NEXT phaser
                      'REDO', self!improper-control('redo', 'NEXT'),
                      'NEXT', nqp::null,
                      'LAST', ($!pulled := IterationEnd)
                    )
                  ),
                  nqp::if(       # check for Slip
                    nqp::istype($value,Slip)
                      && nqp::eqaddr(
                           ($value := self.start-slip($value)),
                           IterationEnd
                         ),
                    ($value := nqp::null)  # nothing in the slip
                  ),
                ),
                'LABELED', $!label,
                'REDO', nqp::null,   # a 'redo' in the block
                'NEXT', nqp::stmts(  # a 'next' in the block
                  ($!pulled := $!source.pull-one),
                  nqp::if(
                    nqp::eqaddr(($value := self.control-payload),IterationEnd),
                    ($value := nqp::null)
                  ),
                  nqp::unless(
                    nqp::isnull($!NEXT),
                    nqp::handle(
                      $!NEXT(),  # control ops inside a NEXT phaser
                      'REDO', self!improper-control('redo', 'NEXT'),
                      'NEXT', nqp::null,
                      'LAST', ($!pulled := IterationEnd)
                    )
                  )
                ),
                'LAST', nqp::if(  # a 'last' in the block
                  nqp::eqaddr(($value := self.control-payload),IterationEnd),
                  self!fire-any-LAST,
                  ($!pulled := IterationEnd)  # done later
                )
              )
            );

            nqp::ifnull($value,self!fire-any-LAST)
        }

        method !improper-control(str $control, str $phaser --> Nil) {
            # XXX make it a proper Exception
            die "Cannot call '$control' inside a $phaser phaser";
        }

        # Fire any LAST phaser, making sure that any invalid control
        # exceptions will be cause an exception.  Returns IterationEnd
        # for convenience.
        method !fire-any-LAST(--> IterationEnd) {
            if $!LAST -> &LAST {
                nqp::handle(
                  LAST(),
                  'REDO', self!improper-control('redo', 'LAST'),
                  'NEXT', self!improper-control('next', 'LAST'),
                  'LAST', nqp::null  # ok to last inside a LAST phaser
                )
            }
        }

        method push-all(\target) {
            my $source := $!source;
            my &block  := &!block;
            my $label  := $!label;
            my $pulled := $!pulled;

            self.push-rest(target) unless nqp::isnull($!slipper);

            if $!NEXT -> &NEXT {
                nqp::until(
                  nqp::eqaddr($pulled,IterationEnd),
                  nqp::handle(
                    nqp::stmts(
                      (my $value := block($pulled)),
                      nqp::if(
                        nqp::istype($value,Slip),
                        self.slip-all($value,target),
                        target.push($value)
                      ),
                      nqp::handle(
                        NEXT(),
                        'REDO', self!improper-control('redo', 'NEXT')
                      ),
                      ($pulled := $source.pull-one)
                    ),
                    'LABELED', $label,
                    'NEXT', nqp::stmts(
                      self.push-control-payload(target),
                      ($pulled := $source.pull-one),
                      nqp::handle(
                        NEXT(),
                        'REDO', self!improper-control('redo', 'NEXT'),
                        'NEXT', nqp::null,
                        'LAST', ($pulled := self!fire-any-LAST)
                      )
                    ),
                    'REDO', nqp::null,
                    'LAST', nqp::stmts(
                      self.push-control-payload(target),
                      ($pulled := self!fire-any-LAST)
                    )
                  ),
                  :nohandler
                );
            }

            # no NEXT phaser
            else {
                nqp::until(
                  nqp::eqaddr($pulled,IterationEnd),
                  nqp::handle(
                    nqp::stmts(
                      (my $value := block($pulled)),
                      nqp::if(
                        nqp::istype($value,Slip),
                        self.slip-all($value,target),
                        target.push($value)
                      ),
                      ($pulled := $source.pull-one)
                    ),
                    'LABELED', $label,
                    'NEXT', nqp::stmts(
                      self.push-control-payload(target),
                      ($pulled := $source.pull-one)
                    ),
                    'REDO', nqp::null,
                    'LAST', nqp::stmts(
                      self.push-control-payload(target),
                      ($pulled := IterationEnd)
                    )
                  ),
                  :nohandler
                );
            }

            self!fire-any-LAST
        }

        method sink-all() {
            my $source := $!source;
            my &block  := &!block;
            my $label  := $!label;
            my $pulled := $!pulled;

            self.sink-rest unless nqp::isnull($!slipper);

            if $!NEXT -> &NEXT {
                nqp::until(
                  nqp::eqaddr($pulled,IterationEnd),
                  nqp::handle(
                    nqp::stmts(
                      block($pulled),
                      nqp::handle(
                        NEXT(),
                        'REDO', self!improper-control('redo', 'NEXT')
                      ),
                      ($pulled := $source.pull-one)
                    ),
                    'LABELED', $label,
                    'NEXT', nqp::stmts(
                      ($pulled := $source.pull-one),
                      nqp::handle(
                        NEXT(),
                        'REDO', self!improper-control('redo', 'NEXT'),
                        'NEXT', nqp::null,
                        'LAST', ($pulled := IterationEnd)
                      )
                    ),
                    'REDO', nqp::null,
                    'LAST', ($pulled := IterationEnd)
                  ),
                  :nohandler
                );
            }

            # no NEXT phaser
            else {
                nqp::until(
                  nqp::eqaddr($pulled,IterationEnd),
                  nqp::handle(
                    nqp::stmts(
                      block($pulled),
                      ($pulled := $source.pull-one)
                    ),
                    'LABELED', $label,
                    'NEXT', ($pulled := $source.pull-one),
                    'REDO', nqp::null,
                    'LAST', ($pulled := IterationEnd)
                  ),
                  :nohandler
                );
            }

            self!fire-any-LAST
        }
    }

    my class IterateOneWithoutPhasers does Rakudo::SlippyIterator {
        has &!block;
        has $!source;
        has $!label;

        method new(&block, Iterator:D $source, $label) {
            my $iter := nqp::create(self);
            nqp::bindattr($iter, self, '$!slipper', nqp::null);
            nqp::bindattr($iter, self, '&!block', &block);
            nqp::bindattr($iter, self, '$!source', $source);
            nqp::bindattr($iter, self, '$!label', nqp::decont($label));
            $iter
        }

        method is-lazy() { $!source.is-lazy }

        method pull-one() is raw {
            my $pulled := nqp::isnull($!slipper)
              || nqp::eqaddr((my $value := self.slip-one),IterationEnd)
              ?? $!source.pull-one
              !! IterationEnd;

            nqp::until(
              nqp::eqaddr($pulled,IterationEnd),
              nqp::handle(
                ($pulled := nqp::if(
                  nqp::istype(($value := &!block($pulled)),Slip)
                    && nqp::eqaddr(
                         ($value := self.start-slip($value)),
                         IterationEnd
                       ),
                    $!source.pull-one,
                    IterationEnd
                )),
                'LABELED', $!label,
                'NEXT', ($pulled := nqp::if(
                  nqp::eqaddr(($value := self.control-payload),IterationEnd),
                  $!source.pull-one,
                  IterationEnd
                )),
                'REDO', nqp::null,
                'LAST', nqp::stmts(
                  ($pulled := IterationEnd),
                  nqp::unless(
                    nqp::eqaddr(($value := self.control-payload),IterationEnd),
                    ($!source := Rakudo::Iterator.Empty)   # also done later
                  )
                )
              ),
              :nohandler
            );

            nqp::ifnull($value,IterationEnd)
        }

        method push-all(\target --> IterationEnd) {
            self.push-rest(target) unless nqp::isnull($!slipper);

            my $source := $!source;
            my $label  := $!label;
            my &block  := &!block;

            my $pulled := $source.pull-one;
            nqp::until(
              nqp::eqaddr($pulled,IterationEnd),
              nqp::handle(
                nqp::stmts(
                  nqp::if(
                    nqp::istype((my $value := &block($pulled)),Slip),
                    self.slip-all($value, target),
                    target.push($value)
                  ),
                  ($pulled := $source.pull-one),
                ),
                'LABELED', $label,
                'REDO', nqp::null,
                'NEXT', nqp::stmts(
                  self.push-control-payload(target),
                  ($pulled := $source.pull-one)
                ),
                'LAST', nqp::stmts(
                  self.push-control-payload(target),
                  ($pulled := IterationEnd)
                )
              ),
              :nohandler
            );
        }

        method sink-all(--> IterationEnd) {
            self.sink-rest unless nqp::isnull($!slipper);

            my $source := $!source;
            my $label  := $!label;
            my &block  := &!block;

            my $pulled := $source.pull-one;
            nqp::until(
              nqp::eqaddr($pulled,IterationEnd),
              nqp::handle(
                nqp::stmts(
                  &!block($pulled),
                  ($pulled := $source.pull-one)
                ),
                'LABELED', $label,
                'NEXT', ($pulled := $source.pull-one),
                'REDO', nqp::null,
                'LAST', ($pulled := IterationEnd)
              ),
              :nohandler
            );
        }
    }

    my class IterateTwoWithoutPhasers does Rakudo::SlippyIterator {
        has &!block;
        has $!source;
        has $!label;

        method new(&block, Iterator:D $source, $label) {
            my $iter := nqp::create(self);
            nqp::bindattr($iter, self, '$!slipper', nqp::null);
            nqp::bindattr($iter, self, '&!block', &block);
            nqp::bindattr($iter, self, '$!source', $source);
            nqp::bindattr($iter, self, '$!label', nqp::decont($label));
            $iter
        }

        method is-lazy() { $!source.is-lazy }

        method pull-one() is raw {
            nqp::if(
              nqp::isnull(
                nqp::if(
                  nqp::isnull($!slipper)
                    || nqp::eqaddr((my $value := self.slip-one),IterationEnd),
                  ($value := nqp::null),
                  $value
                )
              ),
              nqp::unless(
                nqp::eqaddr((my $a := $!source.pull-one),IterationEnd),
                (my $b := $!source.pull-one)
              )
            );

            nqp::while(
              nqp::isnull($value)
                && nqp::not_i(nqp::eqaddr($a,IterationEnd)),
              nqp::if(
                nqp::eqaddr($b,IterationEnd),
                nqp::handle(  # iterator exhausted
                  nqp::stmts(
                    ($!source := Rakudo::Iterator.Empty),
                    ($value := &!block($a)),
                    nqp::if(
                      nqp::istype($value,Slip),
                      ($value := self.start-slip($value))
                    )
                  ),
                  'LABELED', $!label,
                  'NEXT', ($value := self.control-payload),
                  'REDO', ($value := nqp::null),
                  'LAST', ($value := self.control-payload)
                ),
                nqp::handle(  # iterator still good
                  nqp::stmts(
                    ($value := &!block($a, $b)),
                    nqp::if(
                      nqp::istype($value,Slip) && nqp::eqaddr(
                        ($value := self.start-slip($value)),
                        IterationEnd
                      ),
                      nqp::stmts(             # set up next iteration
                        ($value := nqp::null),
                        nqp::unless(
                          nqp::eqaddr(($a := $!source.pull-one),IterationEnd),
                          ($b := $!source.pull-one)
                        )
                      )
                    )
                  ),
                  'LABELED', $!label,
                  'NEXT', nqp::if(
                    nqp::eqaddr(($value := self.control-payload),IterationEnd),
                    nqp::stmts(
                      ($value := nqp::null),  # set up next iteration
                      nqp::unless(
                        nqp::eqaddr(($a := $!source.pull-one),IterationEnd),
                        ($b := $!source.pull-one)
                      )
                    )
                  ),
                  'REDO', nqp::null,
                  'LAST', nqp::if(
                    nqp::eqaddr(($value := self.control-payload),IterationEnd),
                    ($value   := IterationEnd),           # end now
                    ($!source := Rakudo::Iterator.Empty)  # end later
                  )
                )
              )
            );

            nqp::ifnull($value,IterationEnd)
        }

        method push-all(\target --> IterationEnd) {
            self.push-rest(target) unless nqp::isnull($!slipper);

            my $source := $!source;
            my $label  := $!label;
            my &block  := &!block;

            nqp::unless(
              nqp::eqaddr((my $a := $source.pull-one),IterationEnd),
              (my $b := $source.pull-one)
            );

            nqp::until(
              nqp::eqaddr($a,IterationEnd),
              nqp::handle(
                nqp::stmts(
                  nqp::if(
                    nqp::eqaddr($b,IterationEnd),
                    nqp::stmts(
                      (my $value := &block($a)),
                      ($a := IterationEnd)
                    ),
                    nqp::stmts(
                      ($value := &block($a, $b)),
                      nqp::unless(
                        nqp::eqaddr(($a := $source.pull-one),IterationEnd),
                        ($b := $source.pull-one)
                      )
                    )
                  ),
                  nqp::if(
                    nqp::istype($value,Slip),
                    self.slip-all($value,target),
                    target.push($value)
                  )
                ),
                'LABELED', $label,
                'NEXT', nqp::stmts(
                  self.push-control-payload(target),
                  nqp::unless(
                    nqp::eqaddr(($a := $source.pull-one),IterationEnd),
                    ($b := $source.pull-one)
                  )
                ),
                'REDO', nqp::null,
                'LAST', nqp::stmts(
                  self.push-control-payload(target),
                  ($a := IterationEnd)
                )
              ),
              :nohandler
            );
        }

        method sink-all(--> IterationEnd) {
            self.sink-rest unless nqp::isnull($!slipper);

            my $source := $!source;
            my $label  := $!label;
            my &block  := &!block;

            nqp::unless(
              nqp::eqaddr((my $a := $source.pull-one),IterationEnd),
              (my $b := $source.pull-one)
            );

            nqp::until(
              nqp::eqaddr($a,IterationEnd),
              nqp::handle(  # doesn't sink
                nqp::if(
                  nqp::eqaddr($b,IterationEnd),
                  nqp::stmts(
                    &block($a),
                    ($a := IterationEnd)
                  ),
                  nqp::stmts(
                    &block($a, $b),
                    nqp::unless(
                      nqp::eqaddr(($a := $source.pull-one),IterationEnd),
                      ($b := $source.pull-one)
                    )
                  )
                ),
                'LABELED', $label,
                'NEXT', nqp::unless(
                  nqp::eqaddr(($a := $source.pull-one),IterationEnd),
                  ($b := $source.pull-one)
                ),
                'REDO', nqp::null,
                'LAST', ($a := IterationEnd)
              ),
              :nohandler
            );
        }
    }

    my class IterateMoreWithoutPhasers does Rakudo::SlippyIterator {
        has &!block;
        has $!source;
        has $!label;
        has int $!count;

        method new(&block, Iterator:D $source, int $count, $label) {
            my $iter := nqp::create(self);
            nqp::bindattr($iter, self, '$!slipper', nqp::null);
            nqp::bindattr($iter, self, '&!block', &block);
            nqp::bindattr($iter, self, '$!source', $source);
            nqp::bindattr($iter, self, '$!label', nqp::decont($label));
            nqp::bindattr_i($iter, self, '$!count', $count);
            $iter
        }

        method is-lazy() { $!source.is-lazy }

        method pull-one() is raw {
            nqp::if(
              nqp::isnull(
                nqp::if(
                  nqp::isnull($!slipper)
                    || nqp::eqaddr((my $value := self.slip-one),IterationEnd),
                  ($value := nqp::null),
                  $value
                )
              ),
              (my $pulled := $!source.pull-one)
            );

            my $params := nqp::list;
            nqp::while(
              nqp::isnull($value)
                && nqp::not_i(nqp::eqaddr($pulled,IterationEnd)),
              nqp::handle(
                nqp::stmts(
                  nqp::unless(
                    nqp::elems($params),
                    nqp::push($params,$pulled)
                  ),
                  nqp::until(
                    nqp::iseq_i(nqp::elems($params),$!count)
                      || nqp::eqaddr(($pulled := $!source.pull-one),IterationEnd),
                    nqp::push($params,$pulled)
                  ),
                  ($value := nqp::p6invokeflat(&!block,$params)),
                  nqp::if(
                    nqp::istype($value,Slip) && nqp::eqaddr(
                      ($value := self.start-slip($value)),
                      IterationEnd
                    ),
                    nqp::stmts(                # set up next iteration
                      ($value  := nqp::null),
                      ($pulled := $!source.pull-one),
                      nqp::setelems($params,0)
                    )
                  )
                ),
                'LABELED', $!label,
                'NEXT', nqp::if(
                  nqp::eqaddr(($value := self.control-payload),IterationEnd),
                  nqp::stmts(                   # set up next iteration
                    ($value  := nqp::null),
                    ($pulled := $!source.pull-one),
                    nqp::setelems($params,0)
                  )
                ),
                'REDO', ($value := $pulled := nqp::null),
                'LAST', nqp::unless(
                  nqp::eqaddr(($value := self.control-payload),IterationEnd),
                  ($!source := Rakudo::Iterator.Empty)  # end later
                )
              )
            );

            nqp::ifnull($value,IterationEnd)
        }

        method push-all(\target --> IterationEnd) {
            self.push-rest(target) unless nqp::isnull($!slipper);

            my     $source := $!source;
            my     $label  := $!label;
            my     &block  := &!block;
            my int $count   = $!count;

            my $pulled := $source.pull-one;
            my $params := nqp::list;
            nqp::until(
              nqp::eqaddr($pulled,IterationEnd),
              nqp::handle(
                nqp::stmts(
                  nqp::unless(
                    nqp::elems($params),
                    nqp::push($params,$pulled)
                  ),
                  nqp::until(
                    nqp::iseq_i(nqp::elems($params),$count)
                      || nqp::eqaddr(($pulled := $source.pull-one),IterationEnd),
                    nqp::push($params,$pulled)
                  ),
                  (my $value := nqp::p6invokeflat(&!block,$params)),
                  nqp::if(
                    nqp::istype($value,Slip),
                    self.slip-all($value,target),
                    target.push($value)
                  ),
                  ($pulled := $source.pull-one),
                  nqp::setelems($params,0)
                ),
                'LABELED', $label,
                'NEXT', nqp::stmts(
                  self.push-control-payload(target),
                  ($pulled := $source.pull-one),
                  nqp::setelems($params,0)
                ),
                'REDO', nqp::null,
                'LAST', nqp::stmts(
                  self.push-control-payload(target),
                  ($pulled := IterationEnd)
                )
              )
            );
        }

        method sink-all(--> IterationEnd) {
            self.sink-rest unless nqp::isnull($!slipper);

            my     $source := $!source;
            my     $label  := $!label;
            my     &block  := &!block;
            my int $count   = $!count;

            my $pulled := $source.pull-one;
            my $params := nqp::list;
            nqp::until(
              nqp::eqaddr($pulled,IterationEnd),
              nqp::handle(
                nqp::stmts(
                  nqp::unless(
                    nqp::elems($params),
                    nqp::push($params,$pulled)
                  ),
                  nqp::until(
                    nqp::iseq_i(nqp::elems($params),$count)
                      || nqp::eqaddr(($pulled := $source.pull-one),IterationEnd),
                    nqp::push($params,$pulled)
                  ),
                  nqp::p6invokeflat(&block,$params),
                  ($pulled := $source.pull-one),
                  nqp::setelems($params,0)
                ),
                'LABELED', $label,
                'NEXT', nqp::stmts(
                  ($pulled := $source.pull-one),
                  nqp::setelems($params,0)
                ),
                'REDO', nqp::null,
                'LAST', ($pulled := IterationEnd)
              )
            );
        }
    }

    my class IterateMoreWithPhasers does Rakudo::SlippyIterator {
        has     &!block;
        has     $!source;
        has     $!count;
        has     $!label;
        has     $!value-buffer;
        has int $!did-init;
        has int $!did-iterate;
        has int $!CAN_FIRE_PHASERS;
        has     $!NEXT;

        method new(&block, Iterator:D $source, $count, $label) {
            my $iter := nqp::create(self);
            nqp::bindattr($iter, self, '$!slipper', nqp::null);
            nqp::bindattr($iter, self, '&!block', &block);
            nqp::bindattr($iter, self, '$!source', $source);
            nqp::bindattr($iter, self, '$!count', $count);
            nqp::bindattr($iter, self, '$!label', nqp::decont($label));
            $iter
        }

        method is-lazy() { $!source.is-lazy }

        method pull-one() is raw {
            nqp::isconcrete($!value-buffer)
              ?? nqp::setelems($!value-buffer,0)
              !! ($!value-buffer := nqp::create(IterationBuffer));

            my &block := &!block;
            my $result;

            if !$!did-init && nqp::can(&block, 'fire_phasers') {
                $!did-init         = 1;
                $!CAN_FIRE_PHASERS = 1;
                $!NEXT             = &block.has-phaser('NEXT');
                nqp::p6setfirstflag(nqp::getattr(&block, Code, '$!do'))
                  if &block.has-phaser('FIRST');
            }

            if nqp::not_i(nqp::isnull($!slipper)) && nqp::not_i(
              nqp::eqaddr(($result := self.slip-one),IterationEnd)) {
                # $result will be returned at the end
            }
            elsif nqp::eqaddr(
              $!source.push-exactly($!value-buffer,$!count),IterationEnd)
                && nqp::elems($!value-buffer) == 0 {
                $result := IterationEnd
            }
            else {
                my     $source := $!source;
                my     $label  := $!label;
                my     $NEXT   := $!NEXT;
                my int $count   = $!count;
                my int $redo    = 1;

                nqp::while(
                  $redo,
                  nqp::stmts(
                    $redo = 0,
                    nqp::handle(
                      nqp::stmts(
                        ($result := nqp::p6invokeflat(&block, $!value-buffer)),
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
                                    $source.push-exactly($!value-buffer, $count),
                                    IterationEnd)
                                  && nqp::elems($!value-buffer) == 0)
                              )
                            )
                          )
                        ),
                        nqp::if($NEXT, &block.fire_phasers('NEXT')),
                      ),
                      'LABELED', $label,
                      'NEXT', nqp::stmts(
                        ($!did-iterate = 1),
                        nqp::if($NEXT, &block.fire_phasers('NEXT')),
                        nqp::setelems($!value-buffer,0),
                        nqp::if(
                          nqp::isnull($result := nqp::getpayload(nqp::exception)),
                          nqp::stmts(
                            nqp::if(
                              nqp::eqaddr(
                                $source.push-exactly($!value-buffer, $count),
                                IterationEnd
                              ) && nqp::elems($!value-buffer) == 0,
                              ($result := IterationEnd),
                              ($redo = 1)
                            )
                          ),
                          nqp::if(                        # next with value
                            nqp::istype($result,Slip)
                              && nqp::eqaddr(             # it's a Slip
                                   ($result := self.start-slip($result)),
                                   IterationEnd
                                 )
                              && nqp::not_i(nqp::eqaddr(  # an empty Slip
                                   $source.push-exactly($!value-buffer, $!count),
                                   IterationEnd
                                 )),
                            ($redo = 1)                   # process these values
                          )
                        )
                      ),
                      'REDO', $redo = 1,
                      'LAST', nqp::stmts(
                        ($!did-iterate = 1),
                        nqp::if(
                          nqp::isnull($result := nqp::getpayload(nqp::exception)),
                          ($result := IterationEnd),
                          nqp::stmts(
                            nqp::if(
                              nqp::istype($result,Slip),
                              ($result := self.start-slip($result))
                            ),
                            ($!source := Rakudo::Iterator.Empty)
                          )
                        )
                      )
                    )
                  ),
                :nohandler);
            }
            &block.fire_if_phasers('LAST')
              if $!CAN_FIRE_PHASERS
              && $!did-iterate
              && nqp::eqaddr($result, IterationEnd);
            $result
        }
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

    # We want map to be fast, so we go to some effort to build special
    # case iterators that can ignore various interesting cases.
    multi method map(\SELF: &code;; :$label, :$item) {
        my $count  := &code.count;
        my $source := $item
          ?? Rakudo::Iterator.OneValue(SELF)
          !! SELF.iterator;

        Seq.new: &code.?has-loop-phasers
          ?? $count < 2 || $count == Inf
            ?? IterateOneWithPhasers.new(&code, $source, $label)
            !! IterateMoreWithPhasers.new(&code, $source, $count, $label)
          !! $count < 2 || $count == Inf
            ?? IterateOneWithoutPhasers.new(&code, $source, $label)
            !! $count == 2
              ?? IterateTwoWithoutPhasers.new(&code, $source, $label)
              !! IterateMoreWithoutPhasers.new(&code, $source, $count, $label)
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
              ++$!index
            );

            nqp::eqaddr($_,IterationEnd)
              ?? IterationEnd
              !! nqp::p6box_i(++$!index)
        }
        method push-all(\target --> IterationEnd) {
            my $iter := $!iter;  # lexicals faster than attrs
            my $test := $!test;
            my int $i = $!index;

            nqp::until(
              nqp::eqaddr(($_ := $iter.pull-one),IterationEnd),
              nqp::stmts(
                ++$i,
                nqp::if(
                  $test($_),
                  target.push(nqp::p6box_i($i))
                )
              )
            );
        }
        method is-deterministic(--> Bool:D) { $!iter.is-deterministic }
        method is-monotonically-increasing(--> Bool:D) {
            $!iter.is-monotonically-increasing
        }
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
                  ++$!index
                ),
                nqp::if(
                  nqp::eqaddr($_,IterationEnd),
                  IterationEnd,
                  nqp::stmts(
                    ($!value := $_),
                    nqp::p6box_i(++$!index)
                  )
                )
              )
            )
        }
        method push-all(\target --> IterationEnd) {
            my     $iter := $!iter;   # lexicals are faster than attrs
            my     $test := $!test;
            my int $index = $!index;

            nqp::until(
              nqp::eqaddr(($_ := $iter.pull-one),IterationEnd),
              nqp::stmts(
                ++$index,
                nqp::if(
                  $test($_),
                  nqp::stmts(  # doesn't sink
                    target.push(nqp::p6box_i($index));
                    target.push($_);
                  )
                )
              )
            );
        }
        method is-deterministic(--> Bool:D) { $!iter.is-deterministic }
        method is-monotonically-increasing(--> Bool:D) {
            $!iter.is-monotonically-increasing
        }
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
              ++$!index
            );

            nqp::eqaddr($_,IterationEnd)
              ?? IterationEnd
              !! Pair.new(++$!index,$_)
        }
        method push-all(\target --> IterationEnd) {
            my     $iter := $!iter;   # lexicals are faster than attrs
            my     $test := $!test;
            my int $index = $!index;

            nqp::until(
              nqp::eqaddr(($_ := $iter.pull-one),IterationEnd),
              nqp::stmts(
                ++$index,
                nqp::if(
                  $test($_),
                  target.push(Pair.new($index,$_))
                )
              )
            );
        }
        method is-deterministic(--> Bool:D) { $!iter.is-deterministic }
        method is-monotonically-increasing(--> Bool:D) {
            $!iter.is-monotonically-increasing
        }
    }
    method !grep-p(Callable:D $test) { Seq.new(Grep-P.new(self,$test)) }

    my role Grepper does Iterator {
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
        method is-monotonically-increasing(--> Bool:D) {
            $!iter.is-monotonically-increasing
        }
    }
    method !grep-callable(Callable:D $test) {
        sub judge(Mu $result, Mu $value) is raw {
            nqp::if(
              nqp::istype($result,Regex),
              $result.ACCEPTS($value),
              nqp::if(
                nqp::istype($result,Junction),
                $result.Bool,
                $result
              )
            )
        }

        my $count := $test.count;

        Seq.new: $count < 2 || $count == Inf
          ?? IterateOneWithoutPhasers.new(
               -> \a {
                   nqp::stmts(  # don't sink the result
                     nqp::handle(
                        (my $result := $test(a)),
                       'NEXT', nqp::if(
                         judge(
                           nqp::ifnull(nqp::getpayload(nqp::exception),False),
                           a
                         ),
                         THROW(nqp::const::CONTROL_NEXT, a),
                         nqp::throwextype(nqp::const::CONTROL_NEXT)
                       ),
                       'LAST', nqp::if(
                         judge(
                           nqp::ifnull(nqp::getpayload(nqp::exception),False),
                           a
                         ),
                         THROW(nqp::const::CONTROL_LAST, a),
                         nqp::throwextype(nqp::const::CONTROL_LAST)
                       ),
                     ),
                     nqp::if(judge($result, a),a,Empty)
                   )
               },
               self.iterator,
               Any
             )
          !! $count == 2
            ?? IterateTwoWithoutPhasers.new(
                 -> \a, \b {
                     my \params := (a, b);
                     nqp::handle(
                       (my $result := $test(a, b)),
                       'NEXT', nqp::if(
                         judge(
                           nqp::ifnull(nqp::getpayload(nqp::exception),False),
                           params
                         ),
                         THROW(nqp::const::CONTROL_NEXT, params),
                         nqp::throwextype(nqp::const::CONTROL_NEXT)
                       ),
                       'LAST', nqp::if(
                         judge(
                           nqp::ifnull(nqp::getpayload(nqp::exception),False),
                           params
                         ),
                         THROW(nqp::const::CONTROL_LAST, params),
                         nqp::throwextype(nqp::const::CONTROL_LAST)
                       )
                     );
                     judge($result, params) ?? params !! Empty
                 },
                 self.iterator,
                 Any
               )
            !! IterateMoreWithPhasers.new(
                 -> |c {
                     my \params := c.list;
                     nqp::handle(
                       (my $result := $test(|c)),
                       'NEXT', nqp::if(
                         judge(
                           nqp::ifnull(nqp::getpayload(nqp::exception),False),
                           params
                         ),
                         THROW(nqp::const::CONTROL_NEXT, params),
                         nqp::throwextype(nqp::const::CONTROL_NEXT)
                       ),
                       'LAST', nqp::if(
                         judge(
                           nqp::ifnull(nqp::getpayload(nqp::exception),False),
                           params
                         ),
                         THROW(nqp::const::CONTROL_LAST, params),
                         nqp::throwextype(nqp::const::CONTROL_LAST)
                       )
                     );
                     judge($result, params) ?? params !! Empty
                 },
                 self.iterator,
                 $count,
                 Any
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
            my $iter := $!iter;   # lexicals are faster than attrs
            my $test := $!test;

            nqp::until(
              nqp::eqaddr(($_ := $iter.pull-one),IterationEnd),
              nqp::if(  # doesn't sink
                nqp::isge_i(
                  nqp::getattr_i(
                    $test.(Match.'!cursor_init'(
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
            my $iter := $!iter;   # lexicals are faster than attrs
            my $test := $!test;

            nqp::until(
              nqp::eqaddr(($_ := $iter.pull-one),IterationEnd),
              nqp::if(  # doesn't sink
                $test.ACCEPTS($_),
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
                            "Specified a negated :v adverb".Failure,
                            X::Adverb.new(  # :foo ??
                              :$what,
                              :source(try { self.VAR.name } // self.WHAT.raku),
                              :unexpected(%a.keys)
                            ).Failure
                          )
                        )
                      )
                    )
                  )
                )
              ),
              X::Adverb.new(                # multiple adverbs ??
                :$what,
                :source(try { self.VAR.name } // self.WHAT.raku),
                :nogo(%a.keys.grep: /k|v|p/),
                :unexpected(%a.keys.grep: { !.match(/k|v|p/) } )
              ).Failure
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
        X::Match::Bool.new( type => '.first' ).Failure
    }
    # need to handle Regex differently, since it is also Callable
    multi method first(Regex:D $test, :$end, *%a) is raw {
        $end
          ?? self!first-regex-end($test,%a)
          !! self!first-regex($test,%a)
    }
    multi method first(Callable:D $test, :$end, *%a) is raw {
        $end
          ?? self!first-callable-end($test, %a)
          !! self!first-callable($test, %a)
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
    method !first-callable(Callable:D $test, %a) is raw {
        my $iter := self.iterator;
        my int $index;
        nqp::until(
          (nqp::eqaddr(($_ := $iter.pull-one),IterationEnd)
            || $test($_)),
          ++$index
        );

        nqp::eqaddr($_,IterationEnd)
          ?? Nil
          !! self!first-result($index,$_,'first',%a)
    }
    method !first-callable-end(Callable:D $test, %a) is raw {
        my $elems := self.elems;
        nqp::if(
          ($elems && nqp::not_i($elems == Inf)),
          nqp::stmts(
            (my int $index = $elems),
            nqp::while(
              nqp::isge_i(--$index,0),
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
          ++$index
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
              nqp::isge_i(--$index,0),
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
          ++$index
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
              nqp::isge_i(--$index,0),
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

    # Returns an iterator only if there is at least one value that
    # can be produced.  If the iterator is lazy, throws an error
    # with the action that is specified as the first argument.
    # Assigns the produced value in the variable that must be
    # specified as the second argument.
    method iterator-and-first(
      $action, $first is rw
    ) is implementation-detail {
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
                  ($first = $pulled),
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

    proto method sort(|) is nodal {*}
    multi method sort() {
        my $iterator := self.iterator;
        $iterator.is-monotonically-increasing
          ?? Seq.new($iterator)
          !! nqp::eqaddr(
               $iterator.push-until-lazy(
                 my \buf := nqp::create(IterationBuffer)),
               IterationEnd
             ) ?? Seq.new(
                    Rakudo::Iterator.ReifiedListMonotonicallyIncreasing(
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
          nqp::eqaddr(&by,&infix:<cmp>)
            ?? Rakudo::Iterator.ReifiedListMonotonicallyIncreasing(
                 Rakudo::Sorting.MERGESORT-REIFIED-LIST(buf.List)
               )
            !! Rakudo::Iterator.ReifiedList(&by.count < 2
                ?? Rakudo::Sorting.MERGESORT-REIFIED-LIST-AS(  buf.List,&by)
                !! Rakudo::Sorting.MERGESORT-REIFIED-LIST-WITH(buf.List,&by)
               )
        )
    }

    proto method collate(|) {*}
    multi method collate() { self.sort(&[coll]) }

    proto method reduce(|) is nodal {*}
    multi method reduce(Any:U: & --> Nil) { }
    multi method reduce(Any:D: &with) is raw {
        (&with.reducer)(&with)(self)
    }

    proto method produce(|) is nodal {*}
    multi method produce(Any:U: & --> Nil) { }
    multi method produce(Any:D: &with) {
        (&with.reducer)(&with, 1)(self)
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
        method is-monotonically-increasing(--> Bool:D) {
            $!iter.is-monotonically-increasing
        }
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
        method is-monotonically-increasing(--> Bool:D) {
            $!iter.is-monotonically-increasing
        }
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
        method is-monotonically-increasing(--> Bool:D) {
            $!iter.is-monotonically-increasing
        }
    }
    multi method repeated() { Seq.new(Repeated.new(self)) }

    multi method repeated( :&as!, :&with! ) {
        nqp::eqaddr(&with,&[===]) # use optimized version
          ?? self.repeated(:&as)
          !! Seq.new(
               Rakudo::Iterator.UniqueRepeatedAsWith(self.iterator,&as,&with,0)
             )
    }

    my class Repeated-As does Iterator {
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
        method is-monotonically-increasing(--> Bool:D) {
            $!iter.is-monotonically-increasing
        }
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
        method is-monotonically-increasing(--> Bool:D) {
            $!iter.is-monotonically-increasing
        }
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
        method is-monotonically-increasing(--> Bool:D) {
            $!iter.is-monotonically-increasing
        }
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

    proto method are(|) {*}
    multi method are(Any:U:) { self }
    multi method are(Any:D:) {
        my $iterator := self.iterator;
        nqp::if(
          nqp::eqaddr((my $pulled := $iterator.pull-one),IterationEnd),
          Nil,                                            # nothing to check
          nqp::stmts(
            (my $type := $pulled.WHAT),                   # initial type
            nqp::until(
              nqp::eqaddr(($pulled := $iterator.pull-one),IterationEnd)
                || nqp::not_i(nqp::eqaddr($pulled.WHAT,$type)),
              nqp::null
            ),
            nqp::if(
              nqp::eqaddr($pulled,IterationEnd),
              $type,                                      # all the same
              self!slow-infer($iterator, $type, $pulled)  # find out what
            )
          )
        )
    }

    method !slow-infer($iterator, Mu $type is copy, Mu $pulled is copy) {
        # If there are types to check
        my $mro := nqp::clone(
          nqp::getattr($type.^mro(:roles),List,'$!reified')
        ) if nqp::can($type.HOW,"mro");

        nqp::if(
          $mro,
          nqp::repeat_until(
            nqp::eqaddr(($pulled := $iterator.pull-one),IterationEnd)
              || nqp::eqaddr($type,Mu),
            nqp::until(
              nqp::istype($pulled,nqp::atpos($mro,0)),
              nqp::stmts(                       # not the same base type
                nqp::shift($mro),
                ($type := nqp::atpos($mro,0)),  # assume next type for now
              )
            )
          )
        );
        $type
    }

    multi method are(Any:U: Mu:U $type --> Bool:D) {
        nqp::istype(self,$type)
          ?? True
          !! fail("Expected '" ~ $type.^name ~ "' but got '" ~ self.^name ~ "'")
    }
    multi method are(Any:D: Mu:U $type --> Bool:D) {
        my int $i;
        my $iterator := self.iterator;

        nqp::until(
          nqp::eqaddr((my $pulled := $iterator.pull-one),IterationEnd),
          nqp::unless(
            nqp::istype($pulled.WHAT,$type),
            fail("Expected '"
              ~ $type.^name
              ~ "' but got '"
              ~ $pulled.^name
              ~ "' in element $i"
            ),
            ++$i
          )
        );

        True
    }

    proto method nodemap(|) is nodal {*}
    multi method nodemap(Associative:D: &op) {
        self.new.STORE: self.keys, self.values.nodemap(&op), :INITIALIZE
    }
    multi method nodemap(&op) {
        my $source := self.iterator;
        return X::Cannot::Lazy.new(:action<nodemap>).Failure
          if $source.is-lazy;

        my \buffer := nqp::create(IterationBuffer);
        my $value  := $source.pull-one;

        nqp::until(
          nqp::eqaddr($value,IterationEnd),
          nqp::handle(
            nqp::stmts(
              nqp::push(buffer,op($value)),
              ($value := $source.pull-one)
            ),
            'NEXT', nqp::stmts(
              nqp::unless(
                nqp::isnull($value := nqp::getpayload(nqp::exception)),
                nqp::push(buffer,$value)
              ),
              ($value := $source.pull-one)
            ),
            'REDO', nqp::null,
            'LAST', nqp::stmts(
              nqp::unless(
                nqp::isnull($value := nqp::getpayload(nqp::exception)),
                nqp::push(buffer,$value)
              ),
              ($value := IterationEnd)
            )
          ),
          :nohandler
        );

        buffer.List
    }

    proto method deepmap(|) is nodal {*}
    multi method deepmap(Associative:D: &op) {
        self.new.STORE: self.keys, self.values.deepmap(&op), :INITIALIZE
    }
    multi method deepmap(&op) {
        my $source := self.iterator;
        my \buffer := nqp::create(IterationBuffer);
        my $pulled := $source.pull-one;

        sub deep(\value) is raw { my $ = value.deepmap(&op) }

        nqp::until(
          nqp::eqaddr($pulled,IterationEnd),
          nqp::handle(
            nqp::stmts(
              (my $value := nqp::if(
                nqp::istype($pulled,Iterable) && $pulled.DEFINITE,
                deep($pulled),
                op($pulled)
              )),
              nqp::if(
                nqp::istype($value,Slip),
                $value.iterator.push-all(buffer),
                nqp::push(buffer,$value)
              ),
              ($pulled := $source.pull-one)
            ),
            'NEXT', nqp::stmts(
              nqp::unless(
                nqp::isnull($value := nqp::getpayload(nqp::exception)),
                nqp::if(
                  nqp::istype($value,Slip),
                  $value.iterator.push-all(buffer),
                  nqp::push(buffer,$value)
                ),
              ),
              ($pulled := $source.pull-one)
            ),
            'REDO', nqp::null,
            'LAST', nqp::stmts(
              nqp::unless(
                nqp::isnull($value := nqp::getpayload(nqp::exception)),
                nqp::if(
                  nqp::istype($value,Slip),
                  $value.iterator.push-all(buffer),
                  nqp::push(buffer,$value)
                ),
              ),
              ($pulled := IterationEnd)
            )
          ),
          :nohandler
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
        my $source := self.iterator;
        my \buffer := nqp::create(IterationBuffer);
        my $pulled := $source.pull-one;

        sub duck() is raw {
            CATCH {
                return nqp::istype($pulled,Iterable:D)
                  ?? (my $ = $pulled.duckmap(&op))
                  !! $pulled
            }
            op($pulled)
        }

        sub process(Mu \value --> Nil) {
            nqp::istype(value,Slip)
              ?? value.iterator.push-all(buffer)
              !! nqp::push(buffer,value)
        }

        nqp::until(
          nqp::eqaddr($pulled,IterationEnd),
          nqp::handle(
            nqp::stmts(
              process(duck),
              ($pulled := $source.pull-one)
            ),
            'NEXT', nqp::stmts(
              nqp::unless(
                nqp::isnull(my $value := nqp::getpayload(nqp::exception)),
                process($value)
              ),
              ($pulled := $source.pull-one)
            ),
            'REDO', nqp::null,
            'LAST', nqp::stmts(
              nqp::unless(
                nqp::isnull($value := nqp::getpayload(nqp::exception)),
                process($value)
              ),
              ($pulled := IterationEnd)
            )
          ),
          :nohandler
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
multi sub infix:<min>(Mu:D \a, Mu:D \b) { (a cmp b) ≥ 0 ?? b !! a }
multi sub infix:<min>(Int:D $a, Int:D $b) {
    nqp::isgt_i(nqp::cmp_I($a,$b),0) ?? $b !! $a
}
multi sub infix:<min>(int $a, int $b) {
    nqp::isgt_i(nqp::cmp_i($a,$b),0) ?? $b !! $a
}
multi sub infix:<min>(Num:D $a, Num:D $b) {
    nqp::isgt_i(nqp::cmp_n($a,$b),0) ?? $b !! $a
}
multi sub infix:<min>(num $a, num $b) {
    nqp::isgt_i(nqp::cmp_n($a,$b),0) ?? $b !! $a
}
multi sub infix:<min>(+args is raw) { args.min }

proto sub min(|) is pure {*}
multi sub min(+args, :&by!, *%_) { args.min(&by, |%_) }
multi sub min(+args, *%_)        { args.min(|%_)      }

proto sub infix:<max>(|) is pure {*}
multi sub infix:<max>(Mu:D \a, Mu:U) { a }
multi sub infix:<max>(Mu:U, Mu:D \b) { b }
multi sub infix:<max>(Mu:D \a, Mu:D \b) { (a cmp b) ≤ 0 ?? b !! a }
multi sub infix:<max>(Int:D $a, Int:D $b) {
    nqp::islt_i(nqp::cmp_I($a,$b),0) ?? $b !! $a
}
multi sub infix:<max>(int $a, int $b) {
    nqp::islt_i(nqp::cmp_i($a,$b),0) ?? $b !! $a
}
multi sub infix:<max>(Num:D $a, Num:D $b) {
    nqp::islt_i(nqp::cmp_n($a,$b),0) ?? $b !! $a
}
multi sub infix:<max>(num $a, num $b) {
    nqp::islt_i(nqp::cmp_n($a,$b),0) ?? $b !! $a
}
multi sub infix:<max>(+args) { args.max }

proto sub max(|) is pure {*}
multi sub max(+args, :&by!, *%_) { args.max(&by, |%_) }
multi sub max(+args, *%_)        { args.max(|%_)      }

proto sub infix:<minmax>(|) is pure {*}
multi sub infix:<minmax>(+args) { args.minmax }

proto sub minmax(|) is pure {*}
multi sub minmax(+args, :&by!) { args.minmax(&by) }
multi sub minmax(+args)        { args.minmax      }

proto sub map($, |) {*}
multi sub map(&code, +values) { my $laze = values.is-lazy; values.map(&code).lazy-if($laze) }

proto sub grep(Mu, |) {*}
multi sub grep(Mu $test, +values, *%a) {
    my $laze := values.is-lazy;
    values.grep($test,|%a).lazy-if($laze)
}
multi sub grep(Bool:D $t, |) { X::Match::Bool.new(:type<grep>).throw }

proto sub head(Mu, |) {*}
multi sub head($head, +values) { values.head($head) }

proto sub tail(Mu, |) {*}
multi sub tail($tail, +values) { values.tail($tail) }

proto sub skip(Mu, |) {*}
multi sub skip($skip, +values) { values.skip($skip) }

proto sub first(Mu, |) {*}
multi sub first(Bool:D $t, |) { X::Match::Bool.new(:type<first>).Failure }
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
multi sub sort(&by, @values, *%_) { @values.sort(&by, |%_) }
multi sub sort(&by, +values, *%_) {  values.sort(&by, |%_) }
multi sub sort(@values, *%_)      { @values.sort(|%_) }
multi sub sort(+values, *%_)      {  values.sort(|%_) }
multi sub sort(*%_)               { die "Must specify something to sort" }

proto sub nodemap($, $, *%) {*}
multi sub nodemap(&op, \obj) { obj.nodemap(&op) }

proto sub deepmap($, $, *%) {*}
multi sub deepmap(&op, \obj) { obj.deepmap(&op) }

proto sub duckmap($, $, *%) {*}
multi sub duckmap(&op, \obj) { obj.duckmap(&op) }

# vim: expandtab shiftwidth=4
