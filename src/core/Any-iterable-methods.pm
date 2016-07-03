class X::Cannot::Lazy { ... }

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
        sequential-map(($item ?? (SELF,) !! SELF).iterator, &block, :$label);
    }

    multi method map(HyperIterable:D: &block;; :$label) {
        # For now we only know how to parallelize when we've only one input
        # value needed per block. For the rest, fall back to sequential.
        if &block.count != 1 {
            sequential-map(self.iterator, &block, :$label)
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
                    my \buffer-mapper = sequential-map($work.input-iterator, &!block, :$label);
                    buffer-mapper.iterator.push-all($work.output);
                    $work
                }

                method configuration() {
                    $!source.configuration
                }
            }.new(self.hyper-iterator, &block))
        }
    }

    sub sequential-map(\source, &block, :$label) {
        # We want map to be fast, so we go to some effort to build special
        # case iterators that can ignore various interesting cases.
        my $count = &block.count;

        # "loop" taking 0 or 1 parameter
        if $count == 1 || $count == 0 || $count === Inf {
            if nqp::istype(&block,Block) && &block.has-phasers {
                Seq.new(class :: does SlippyIterator {
                    has &!block;
                    has $!source;
                    has $!label;
                    has Int $!NEXT;         # SHOULD BE int
                    has Int $!did-init;     # SHOULD BE int
                    has Int $!did-iterate;  # SHOULD BE int

                    method new(&block, $source, $label) {
                        my $iter := nqp::create(self);
                        nqp::bindattr($iter, self, '&!block', &block);
                        nqp::bindattr($iter, self, '$!source', $source);
                        nqp::bindattr($iter, self, '$!label',
                          nqp::decont($label));
                        nqp::bindattr($iter, self, '$!NEXT',
                          &block.has-phaser('NEXT'));
                        $iter
                    }

                    method is-lazy() { $!source.is-lazy }

                    method pull-one() is raw {
                        my int $redo = 1;
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
                            $result := $value;
                            nqp::if($!did-iterate,&!block.fire_phasers('LAST'));
                        }
                        else {
                            nqp::while(
                              $redo,
                              nqp::stmts(
                                $redo = 0,
                                nqp::handle(
                                  nqp::stmts(
                                    ($result := &!block($value)),
                                    ($!did-iterate = 1),
                                    nqp::if(
                                      nqp::istype($result, Slip),
                                      nqp::if(
                                        nqp::eqaddr(($result := self.start-slip($result)), IterationEnd),
                                        nqp::if(
                                          nqp::not_i(nqp::eqaddr(($value := $!source.pull-one),IterationEnd)),
                                          ($redo = 1)
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
                                       !! ($redo = 1)
                                  ),
                                  'REDO', $redo = 1,
                                  'LAST', nqp::stmts(
                                    ($!did-iterate = 1),
                                    ($result := IterationEnd)
                                  )
                                )
                              ),
                              :nohandler);
                        }
                        nqp::if(
                          $!did-iterate && nqp::eqaddr($result,IterationEnd),
                          &!block.fire_phasers('LAST')
                        );
                        $result
                    }

                    method sink-all() {
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

                        my int $redo;
                        my int $running = 1;
                        my $value;
                        while $running {
                            nqp::eqaddr(($value := $!source.pull-one()), IterationEnd)
                              ?? nqp::stmts(
                                   ($running = 0),
                                   nqp::if(
                                     $!did-iterate,
                                     &!block.fire_phasers('LAST')
                                   );
                                 )
                              !! nqp::stmts(
                                   ($redo = 1),
                                   nqp::while(
                                     $redo,
                                     nqp::stmts(
                                       $redo = 0,
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
                                           nqp::eqaddr(($value := $!source.pull-one), IterationEnd)
                                             ?? ($running = 0)
                                             !! ($redo = 1)),
                                         'REDO', $redo = 1,
                                         'LAST', nqp::stmts(
                                           ($!did-iterate = 1),
                                           ($running = 0))
                                       )  
                                     ),
                                     :nohandler
                                   ),
                                   nqp::if(
                                     nqp::not_i($running) && $!did-iterate,
                                     &!block.fire_phasers('LAST')
                                   )
                                 );
                        }
                        IterationEnd
                    }
                }.new(&block, source, $label))
            }

            # normal, without phasers or label
            else {
                Seq.new(class :: does SlippyIterator {
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
                                'LABELED', $!label,
                                'NEXT', (nqp::eqaddr(($value := $!source.pull-one), IterationEnd)
                                          ?? ($result := IterationEnd)
                                          !! ($redo = 1)),
                                'REDO', $redo = 1,
                                'LAST', $result := IterationEnd
                              ),
                            ),
                          :nohandler);
                        }
                        $result
                    }

                    method sink-all() {
                        my int $redo;
                        my int $running = 1;
                        my $value;

# for some reason, this scope is needed.  Otherwise, settings compilation
# will end in the mast stage with something like:
#   Cannot reference undeclared local '__lowered_lex_3225'
{
                        nqp::while(
                          $running,
                          nqp::if(
                            nqp::eqaddr(
                              ($value := $!source.pull-one()),IterationEnd
                            ),
                            ($running = 0),
                            nqp::stmts(
                              ($redo = 1),
                              nqp::while(
                                $redo,
                                nqp::stmts(
                                  $redo = 0,
                                  nqp::handle(  # doesn't sink
                                    (&!block($value)),
                                    'LABELED',
                                    $!label,
                                    'NEXT',
                                    nqp::if(
                                      nqp::eqaddr(
                                        ($value := $!source.pull-one),
                                        IterationEnd
                                      ),
                                      ($running = 0),
                                      ($redo = 1)
                                    ),
                                    'REDO',
                                    ($redo = 1),
                                    'LAST',
                                    ($running = 0)
                                  )
                                ),
                              :nohandler
                              )
                            )
                          )
                        );
} # needed for some reason
                        IterationEnd
                    }
                }.new(&block,source,$label))
            }
        }

        # loop/map taking more than 1 param
        else {
            Seq.new(class :: does SlippyIterator {
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
            }.new(&block, source, $count, $label))
        }
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
                    $!value := Mu;
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
        method !SET-SELF(\list,Mu \test) {
            $!iter  = list.iterator;
            $!test := test;
            self
        }
        method new(\list,Mu \test) { nqp::create(self)!SET-SELF(list,test) }
    }
    method !grep-regex(Regex:D $test) {
        Seq.new(class :: does Grepper {
            method pull-one() is raw {
                nqp::until(
                  nqp::eqaddr(($_ := $!iter.pull-one),IterationEnd)
                    || $_.match($!test),
                  Nil
                );
                $_
            }
            method push-all($target) {
                nqp::until(
                  nqp::eqaddr(($_ := $!iter.pull-one),IterationEnd),
                  nqp::if(  # doesn't sink
                    $_.match($!test),
                    $target.push($_)
                  )
                );
                IterationEnd
            }
        }.new(self, $test))
    }
    method !grep-callable(Callable:D $test) {
        if ($test.count == 1) {
            $test.?has-phasers
              ?? self.map({ next unless $test($_); $_ })  # cannot go fast
              !! Seq.new(class :: does Grepper {
                     method pull-one() is raw {
                         nqp::until(
                           nqp::eqaddr(($_ := $!iter.pull-one),IterationEnd)
                             || $!test($_),
                           Nil
                         );
                         $_
                     }
                     method push-all($target) {
                         nqp::until(
                           nqp::eqaddr(($_ := $!iter.pull-one),IterationEnd),
                           nqp::if(  # doesn't sink
                             $!test($_),
                             $target.push($_)
                           )
                         );
                         IterationEnd
                     }
                     method sink-all() {
                         nqp::until(
                           nqp::eqaddr(($_ := $!iter.pull-one),IterationEnd),
                           $!test($_)
                         );
                         IterationEnd
                     }
                 }.new(self, $test))
        } else {
            my role CheatArity {
                has $!arity;
                has $!count;

                method set-cheat($new-arity, $new-count) {
                    $!arity = $new-arity;
                    $!count = $new-count;
                }

                method arity(Code:D:) { $!arity }
                method count(Code:D:) { $!count }
            }

            my &tester = -> |c {
                #note "*cough* {c.perl} -> {$test(|c).perl}";
                next unless $test(|c);
                c.list
            } but CheatArity;

            &tester.set-cheat($test.arity, $test.count);

            self.map(&tester);
        }
    }
    method !grep-accepts(Mu $test) {
        Seq.new(class :: does Grepper {
            method pull-one() is raw {
                nqp::until(
                  nqp::eqaddr(($_ := $!iter.pull-one),IterationEnd)
                    || $!test.ACCEPTS($_),
                  Nil
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
        if %a {
            if %a == 1 {
                if %a<k> {
                    nqp::p6box_i(index)
                }
                elsif %a<p> {
                    Pair.new(index,value)
                }
                elsif %a<v> {
                    value
                }
                elsif %a<kv> {
                    (index,value)
                }
                else {
                    my $k = %a.keys[0];
                    if $k eq 'k' || $k eq 'p' || $k eq 'kv' {
                        value
                    }
                    elsif $k eq 'v' {
                        Failure.new("Doesn't make sense to specify a negated :v adverb")
                    }
                    else {
                        Failure.new(X::Adverb.new(
                          :$what,
                          :source(try { self.VAR.name } // self.WHAT.perl),
                          :unexpected(%a.keys)))
                    }
                }
            }
            else {
                Failure.new(X::Adverb.new(
                  :$what,
                  :source(try { self.VAR.name } // self.WHAT.perl),
                  :nogo(%a.keys.grep: /k|v|p/)
                  :unexpected(%a.keys.grep: { !.match(/k|v|p/) } )))
            }
        }
        else {
            value
        }
    }

    proto method grep(|) is nodal { * }
    multi method grep(Bool:D $t) {
        Failure.new(X::Match::Bool.new( type => '.grep' ))
    }
    multi method grep(Mu $t) {
        if %_ == 0 {
            nqp::istype($t,Regex:D)
              ?? self!grep-regex: $t
              !! nqp::istype($t,Callable:D)
                   ?? self!grep-callable: $t
                   !! self!grep-accepts: $t
        }
        elsif %_ == 1 {
            if %_<k> {
                nqp::istype($t,Regex:D)
                  ?? self!grep-k: { $_.match($t) }
                  !! nqp::istype($t,Callable:D)
                       ?? self!grep-k: $t
                       !! self!grep-k: { $t.ACCEPTS($_) }
            }
            elsif %_<kv> {
                nqp::istype($t,Regex:D)
                  ?? self!grep-kv: { $_.match($t) }
                  !! nqp::istype($t,Callable:D)
                       ?? self!grep-kv: $t
                       !! self!grep-kv: { $t.ACCEPTS($_) }
            }
            elsif %_<p> {
                nqp::istype($t,Regex:D)
                  ?? self!grep-p: { $_.match($t) }
                  !! nqp::istype($t,Callable:D)
                       ?? self!grep-p: $t
                       !! self!grep-p: { $t.ACCEPTS($_) }
            }
            elsif %_<v> {
                nqp::istype($t,Regex:D)
                  ?? self!grep-regex: $t
                  !! nqp::istype($t,Callable:D)
                       ?? self!grep-callable: $t
                       !! self!grep-accepts: $t
            }
            else {
                my $k = %_.keys[0];
                if $k eq 'k' || $k eq 'kv' || $k eq 'p' {
                    nqp::istype($t,Regex:D)
                      ?? self!grep-regex: $t
                      !! nqp::istype($t,Callable:D)
                           ?? self!grep-callable: $t
                           !! self!grep-accepts: $t
                }
                else {
                    $k eq 'v'
                      ?? Failure.new("Doesn't make sense to specify a negated :v adverb")
                      !! Failure.new(X::Adverb.new(
                           :what<grep>,
                           :source(try { self.VAR.name } // self.WHAT.perl),
                           :unexpected($k)))
                }
            }
        }
        else {
            Failure.new(X::Adverb.new(
              :what<grep>,
              :source(try { self.VAR.name } // self.WHAT.perl),
              :nogo(%_.keys.grep: /k|v|kv|p/)
              :unexpected(%_.keys.grep: { !.match(/k|v|kv|p/) } )))
        }
    }

    proto method first(|) is nodal { * }
    multi method first(:$end) {
        $end
          ?? ((my $elems = +self) ?? self.AT-POS($elems - 1) !! Nil)
          !! ((my $x := self.iterator.pull-one) =:= IterationEnd ?? Nil !! $x)
    }
    multi method first(Bool:D $t) {
        Failure.new(X::Match::Bool.new( type => '.first' ))
    }
    multi method first(Regex:D $test, :$end, *%a) is raw {
        if $end {
            my $elems = +self;
            if $elems && !($elems == Inf) {
                my int $index = $elems;
                return self!first-result($index,$_,'first :end',%a)
                  if ($_ := self.AT-POS($index)).match($test)
                    while $index--;
            }
            Nil
        }
        else {
            my $iter := self.iterator;
            my int $index;
            $index = $index + 1
              until ($_ := $iter.pull-one) =:= IterationEnd || .match($test);
            $_ =:= IterationEnd
              ?? Nil
              !! self!first-result($index,$_,'first',%a)
        }
    }
    multi method first(Callable:D $test, :$end, *%a is copy) is raw {
        if $end {
            my $elems = +self;
            if $elems && !($elems == Inf) {
                my int $index = $elems;
                return self!first-result($index,$_,'first :end',%a)
                  if $test($_ := self.AT-POS($index))
                    while $index--;
            }
            Nil
        }
        else {
            my $iter := self.iterator;
            my int $index;
            $index = $index + 1
              until ($_ := $iter.pull-one) =:= IterationEnd || $test($_);
            $_ =:= IterationEnd
              ?? Nil
              !! self!first-result($index,$_,'first',%a)
        }
    }
    multi method first(Mu $test, :$end, *%a) is raw {
        if $end {
            my $elems = +self;
            if $elems && !($elems == Inf) {
                my int $index = $elems;
                return self!first-result($index,$_,'first :end',%a)
                  if $test.ACCEPTS($_ := self.AT-POS($index))
                    while $index--;
            }
            Nil
        }
        else {
            my $iter := self.iterator;
            my int $index;
            $index = $index + 1
              until (($_ := $iter.pull-one) =:= IterationEnd) || $test.ACCEPTS($_);
            $_ =:= IterationEnd
              ?? Nil
              !! self!first-result($index,$_,'first',%a)
        }
    }

    method !first-concrete($what,\i,\todo) {
        my $elems = self.cache.elems;
        die "Cannot $what on an infinite list" if $elems == Inf;

        i    = -1;
        todo = $elems;
        my $value;

        return $value
          if nqp::isconcrete($value := self.AT-POS(i))
            while nqp::islt_i(++i,todo);

        $value
    }

    proto method min (|) is nodal { * }
    multi method min() {
        my $min = self!first-concrete(".min", my int $index, my int $todo);

        my $value;
        while nqp::islt_i(++$index,$todo) {
            $value := self.AT-POS($index);
            $min    = $value
              if nqp::isconcrete($value) && $value cmp $min < 0;
        }
        $min // Inf;
    }
    multi method min(&by) {
        my $cmp = &by.arity == 2 ?? &by !! { &by($^a) cmp &by($^b) }
        my $min = self!first-concrete(".min", my int $index, my int $todo);

        my $value;
        while nqp::islt_i(++$index,$todo) {
            $value := self.AT-POS($index);
            $min    = $value
              if nqp::isconcrete($value) && $cmp($value,$min) < 0;
        }
        $min // Inf;
    }

    proto method max (|) is nodal { * }
    multi method max() {
        my $max = self!first-concrete(".max", my int $index, my int $todo);

        my $value;
        while nqp::islt_i(++$index,$todo) {
            $value := self.AT-POS($index);
            $max    = $value
              if nqp::isconcrete($value) && $value cmp $max > 0;
        }
        $max // -Inf;
    }
    multi method max(&by) {
        my $cmp = &by.arity == 2 ?? &by !! { &by($^a) cmp &by($^b) }
        my $max = self!first-concrete(".max", my int $index, my int $todo);

        my $value;
        while nqp::islt_i(++$index,$todo) {
            $value := self.AT-POS($index);
            $max    = $value
              if nqp::isconcrete($value) && $cmp($value,$max) > 0;
        }
        $max // -Inf;
    }

    method !minmax-range-init($value,\mi,\exmi,\ma,\exma --> Nil) {
        mi   = $value.min;
        exmi = $value.excludes-min;
        ma   = $value.max;
        exma = $value.excludes-max;
    }
    method !minmax-range-check($value,$cmp,\mi,\exmi,\ma,\exma --> Nil) {
        if $cmp($value.min,mi) < 0 {
            mi   = $value.min;
            exmi = $value.excludes-min;
        }
        if $cmp($value.max,ma) > 0 {
            ma   = $value.max;
            exma = $value.excludes-max;
        }
    }

    proto method minmax (|) is nodal { * }
    multi method minmax(&by?) {
        my $value := self!first-concrete(".minmax",my int $index,my int $todo);

        my $min;
        my $max;
        my int $excludes-min;
        my int $excludes-max;

        # initializations
        nqp::istype($value,Failure)
          ?? $value.throw
          !! $value.defined
            ?? nqp::istype($value,Range)
              ?? self!minmax-range-init($value,
                   $min,$excludes-min,$max,$excludes-max)
              !! nqp::istype($value,Positional)
                ?? self!minmax-range-init($value.minmax(&by),
                     $min,$excludes-min,$max,$excludes-max)
                !! ($min = $max = $value)
            !! return Range.new(Inf,-Inf);

        # special comparison needed
        if &by && !(&by === &infix:<cmp>) {
            my $cmp = &by.arity == 2 ?? &by !! { &by($^a) cmp &by($^b) };

            # check rest of values
            nqp::istype(($value := self.AT-POS($index)),Failure)
              ?? $value.throw
              !! $value.defined
                ?? nqp::istype($value,Range)
                  ?? self!minmax-range-check($value,
                       $cmp,$min,$excludes-min,$max,$excludes-max)
                  !! nqp::istype($value,Positional)
                    ?? self!minmax-range-check($value.minmax(&by),
                         $cmp,$min,$excludes-min,$max,$excludes-max)
                    !! $cmp($value, $min) < 0
                      ?? ($min = $value)
                      !! $cmp($value, $max) > 0
                        ?? ($max = $value)
                        !! Nil
                !! Nil
              while nqp::islt_i(++$index,$todo);
        }

        # default infix:<cmp> comparison
        else {

            # check rest of values
            nqp::istype(($value := self.AT-POS($index)),Failure)
              ?? $value.throw
              !! $value.defined
                ?? nqp::istype($value,Range)
                  ?? self!minmax-range-check($value,
                       &infix:<cmp>,$min,$excludes-min,$max,$excludes-max)
                  !! nqp::istype($value,Positional)
                    ?? self!minmax-range-check($value.minmax,
                         &infix:<cmp>,$min,$excludes-min,$max,$excludes-max)
                    !! $value cmp $min < 0
                      ?? ($min = $value)
                      !! $value cmp $max > 0
                        ?? ($max = $value)
                        !! Nil
                !! Nil
              while nqp::islt_i(++$index,$todo);
        }

        Range.new($min, $max, :$excludes-min, :$excludes-max)
    }

    method sort(&by?) is nodal {

        # Obtain all the things to sort.
        my \iter = self.iterator;
        my \sort-buffer = IterationBuffer.new;
        unless iter.push-until-lazy(sort-buffer) =:= IterationEnd {
            fail X::Cannot::Lazy.new(:action<sort>);
        }

        # Instead of sorting elements directly, we sort a list of
        # indices from 0..^$list.elems, then use that list as
        # a slice into self. The JVM implementation uses a Java
        # collection sort. MoarVM has its sort algorithm implemented
        # in NQP.
        my int $elems = sort-buffer.elems;
        my \indices  := nqp::setelems(nqp::list,$elems);
        my int $i = -1;   # need to initialize 0th element for rakudo-j
        nqp::bindpos(indices,$i,nqp::decont($i))
          while nqp::islt_i(++$i,$elems);

        # Need to transform
        if &by && (&by.?count // 2) < 2 {
            my \transformed := nqp::setelems(nqp::list,$elems);
            $i = -1;
            nqp::bindpos(transformed,$i,by(nqp::atpos(sort-buffer,$i)))
              while nqp::islt_i(++$i,$elems);

            nqp::p6sort(indices,-> int $a, int $b {
                nqp::atpos(transformed,$a) cmp nqp::atpos(transformed,$b)
                  || nqp::cmp_i($a,$b)
            });
        }

        # Already have the data to sort
        else {
            nqp::p6sort(indices, &by
              ?? (-> int $a, int $b {
                    by(nqp::atpos(sort-buffer,$a),nqp::atpos(sort-buffer,$b))
                      || nqp::cmp_i($a,$b)
                  })
              !! (-> int $a, int $b {
                    nqp::atpos(sort-buffer,$a) cmp nqp::atpos(sort-buffer,$b)
                      || nqp::cmp_i($a,$b)
                  })
            );
        }

        # map the result back
        my \result := nqp::setelems(nqp::list,$elems);
        $i = -1;
        nqp::bindpos(result,$i,nqp::atpos(sort-buffer,nqp::atpos(indices,$i)))
          while nqp::islt_i(++$i,$elems);

        result
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
    multi method head(Any:D: Int(Cool) $n = 1) {
        return () if $n <= 0;

        Seq.new( class :: does Iterator {
            has Mu  $!iter;
            has int $!todo;
            method !SET-SELF(\list,\todo) {
                $!iter = list.iterator;
                $!todo = todo;
                self
            }
            method new(\list,\todo) { nqp::create(self)!SET-SELF(list,todo) }
            method pull-one() is raw {
                $!todo-- ?? $!iter.pull-one !! IterationEnd
            }
        }.new(self,$n))
    }

    proto method tail(|) { * }
    multi method tail(Any:D: Int(Cool) $n = 1) {
        return () if $n <= 0;

        Seq.new( class :: does Iterator {
            has Mu $!iter;
            has Mu $!lastn;
            has int $!size;
            has int $!todo;
            has int $!index;
            method !SET-SELF(\list,\size) {
                $!iter = list.iterator;
                X::Cannot::Lazy.new(:action<tail>).throw if $!iter.is-lazy;

                $!lastn := nqp::list;
                $!size   = size;
                nqp::setelems($!lastn,$!size);  # presize list
                nqp::setelems($!lastn,0);
                self
            }
            method new(\list,\size) { nqp::create(self)!SET-SELF(list,size) }
            method !next() is raw {
                my int $index = $!index;
                $!index = ($!index + 1) % $!size;
                $!todo  = $!todo - 1;
                nqp::atpos($!lastn,$index)
            }
            method pull-one() is raw {
                if $!todo {
                    self!next;
                }
                elsif $!iter.DEFINITE {
                    my Mu $pulled;
                    my int $index;
                    my int $size = $!size;
                    until ($pulled := $!iter.pull-one) =:= IterationEnd {
                        nqp::bindpos($!lastn,$index,$pulled);
                        $index = ($index + 1) % $size;
                    }
                    if nqp::elems($!lastn) == $!size {   # full set for tail
                        $!index = $index;
                        $!todo  = $!size;
                    }
                    else {  # not a full tail, $!index already 0
                        $!todo = nqp::elems($!lastn);
                    }
                    $!iter := Mu;  # mark we're done iterating
                    $!todo ?? self!next !! IterationEnd
                }
                else {
                    IterationEnd
                }
            }
        }.new(self,$n))
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
multi sub grep(Bool:D $t, |) { Failure.new(X::Match::Bool.new(:type<grep>)) }

proto sub first(|) {*}
multi sub first(Bool:D $t, |) { Failure.new(X::Match::Bool.new(:type<first>)) }
multi sub first(Mu $test, +values, *%a) {
    my $laze = values.is-lazy;
    values.first($test,|%a).lazy-if($laze)
}

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
multi sub sort(@values) {
    @values.sort
}
multi sub sort($cmp, +values) {
    nqp::istype($cmp, Callable)
        ?? values.sort($cmp)
        !! (|$cmp,|values).sort;
}

# vim: ft=perl6 expandtab sw=4
