# Now that Iterable is defined, we add extra methods into Any for the list
# operations. (They can't go into Any right away since we need Attribute to
# define the various roles, and Attribute inherits from Any. We will do a
# re-compose of Attribute to make sure it gets the list methods at the end
# of this file. Note the general pattern for these list-y methods is that
# they check if they have an Iterable already, and if not obtain one to
# work on by doing a .list coercion.
use MONKEY-TYPING;
augment class Any {
    sub as-iterable(\iterablish) {
        # XXX used to use a tertiary operator here but that seems to break mutability of iterablish.list
        if iterablish.DEFINITE && nqp::istype(iterablish, Iterable) {
            iterablish
        }
        else {
            iterablish.list;
        }
    }

    proto method map(|) { * }

    multi method map(\SELF: &block;; :$label, :$item) {
        sequential-map(as-iterable($item ?? (SELF,) !! SELF).iterator, &block, :$label);
    }

    multi method map(HyperIterable:D: &block;; :$label) {
        # For now we only know how to parallelize when we've only one input
        # value needed per block. For the rest, fall back to sequential.
        if &block.count != 1 {
            sequential-map(as-iterable(self).iterator, &block, :$label)
        }
        else {
            HyperSeq.new(class :: does HyperIterator {
                has $!source;
                has &!block;

                method new(\source, &block) {
                    my \iter = self.CREATE;
                    nqp::bindattr(iter, self, '$!source', source);
                    nqp::bindattr(iter, self, '&!block', &block);
                    iter
                }

                method fill-buffer(HyperWorkBuffer:D $work, int $items) {
                    $!source.fill-buffer($work, $items);
                }

                method process-buffer(HyperWorkBuffer:D $work) {
                    unless $!source.process-buffer($work) =:= Mu {
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

    my role MapIterCommon does SlippyIterator {
        has &!block;
        has $!source;
        has $!count;
        has $!label;

        method new(&block, $source, $count, $label) {
            my $iter := self.CREATE;
            nqp::bindattr($iter, self, '&!block', &block);
            nqp::bindattr($iter, self, '$!source', $source);
            nqp::bindattr($iter, self, '$!count', $count);
            nqp::bindattr($iter, self, '$!label', $label);
            $iter
        }

        method is-lazy() {
            $!source.is-lazy
        }
    }
    sub sequential-map(\source, &block, :$label) {
        # We want map to be fast, so we go to some effort to build special
        # case iterators that can ignore various interesting cases.
        my $count = &block.count;
        if $count == 1 || $count == 0 || $count === Inf {
            Seq.new(class :: does MapIterCommon {
                has $!did-init;
                has $!did-iterate;
                has $!NEXT;
                has $!CAN_FIRE_PHASERS;

                method pull-one() is raw {
                    my int $redo = 1;
                    my $value;
                    my $result;

                    if !$!did-init && nqp::can(&!block, 'fire_phasers') {
                        $!did-init         = 1;
                        $!CAN_FIRE_PHASERS = 1;
                        $!NEXT             = +&!block.phasers('NEXT');
                        nqp::p6setfirstflag(&!block) if &!block.phasers('FIRST');
                    }

                    if $!slipping && ($result := self.slip-one()) !=:= IterationEnd {
                        # $result will be returned at the end
                    }
                    elsif ($value := $!source.pull-one()) =:= IterationEnd {
                        $result := $value
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
                                nqp::stmts(
                                  ($result := self.start-slip($result)),
                                  nqp::if(
                                    nqp::eqaddr($result, IterationEnd),
                                    nqp::stmts(
                                      ($value := $!source.pull-one()),
                                      ($redo = 1
                                        unless nqp::eqaddr($value,IterationEnd))
                                    )
                                  )
                                )
                              ),
                              nqp::if($!NEXT, &!block.fire_phasers('NEXT')),
                            ),
                            'LABELED', nqp::decont($!label),
                            'NEXT', nqp::stmts(
                               ($!did-iterate = 1),
                               nqp::if($!NEXT, &!block.fire_phasers('NEXT')),
                               ($value := $!source.pull-one()),
                               nqp::eqaddr($value, IterationEnd)
                                 ?? ($result := IterationEnd)
                                 !! ($redo = 1)),
                             'REDO', $redo = 1,
                             'LAST', nqp::stmts(($!did-iterate = 1), ($result := IterationEnd))
                          )
                        ),
                      :nohandler);
                    }
                    &!block.fire_phasers('LAST')
                      if $!CAN_FIRE_PHASERS
                      && $!did-iterate
                      && nqp::eqaddr($result,IterationEnd);
                    $result
                }

                method sink-all() {
                    if !$!did-init && nqp::can(&!block, 'fire_phasers') {
                        $!did-init         = 1;
                        $!CAN_FIRE_PHASERS = 1;
                        $!NEXT             = +&!block.phasers('NEXT');
                        nqp::p6setfirstflag(&!block) if &!block.phasers('FIRST');
                    }
                    my $result;
                    my int $redo;
                    my $value;
                    until nqp::eqaddr($result, IterationEnd) {
                        if nqp::eqaddr(($value := $!source.pull-one()), IterationEnd) {
                            $result := $value
                        }
                        else {
                            $redo = 1;
                            nqp::while(
                              $redo,
                              nqp::stmts(
                                $redo = 0,
                                nqp::handle(
                                  nqp::stmts(
                                    ($result := &!block($value)),
                                    ($!did-iterate = 1),
                                    nqp::if($!NEXT, &!block.fire_phasers('NEXT')),
                                  ),
                                  'LABELED', nqp::decont($!label),
                                  'NEXT', nqp::stmts(
                                    ($!did-iterate = 1),
                                    nqp::if($!NEXT, &!block.fire_phasers('NEXT')),
                                    ($value := $!source.pull-one()),
                                    nqp::eqaddr($value, IterationEnd)
                                      ?? ($result := IterationEnd)
                                      !! ($redo = 1)),
                                  'REDO', $redo = 1,
                                  'LAST', nqp::stmts(($!did-iterate = 1), ($result := IterationEnd))
                                )
                              ),
                            :nohandler);
                        }
                        &!block.fire_phasers('LAST')
                          if $!CAN_FIRE_PHASERS
                          && $!did-iterate
                          && nqp::eqaddr($result, IterationEnd);
                    }
                    IterationEnd
                }
            }.new(&block, source, 1, $label));
        }
        else {
            Seq.new(class :: does MapIterCommon {
                has $!value-buffer;
                has $!did-init;
                has $!did-iterate;
                has $!NEXT;
                has $!CAN_FIRE_PHASERS;

                method pull-one() is raw {
                    $!value-buffer.DEFINITE
                        ?? nqp::setelems($!value-buffer, 0)
                        !! ($!value-buffer := IterationBuffer.new);
                    my int $redo = 1;
                    my $result;

                    if !$!did-init && nqp::can(&!block, 'fire_phasers') {
                        $!did-init         = 1;
                        $!CAN_FIRE_PHASERS = 1;
                        $!NEXT             = +&!block.phasers('NEXT');
                        nqp::p6setfirstflag(&!block) if &!block.phasers('FIRST');
                    }

                    if $!slipping && ($result := self.slip-one()) !=:= IterationEnd {
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
                              'LABELED', nqp::decont($!label),
                              'NEXT', nqp::stmts(
                                ($!did-iterate = 1),
                                nqp::if($!NEXT, &!block.fire_phasers('NEXT')),
                                  (nqp::setelems($!value-buffer, 0)),
                                  nqp::eqaddr($!source.push-exactly($!value-buffer, $!count), IterationEnd)
                                  && nqp::elems($!value-buffer) == 0
                                    ?? ($result := IterationEnd)
                                    !! ($redo = 1)),
                              'REDO', $redo = 1,
                              'LAST', nqp::stmts(($!did-iterate = 1), ($result := IterationEnd))
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
            }.new(&block, source, $count, $label));
        }
    }

    proto method flatmap (|) is nodal { * }
    multi method flatmap(&block, :$label) {
        self.map(&block, :$label).flat
    }

    role Grepper does Iterator {
        has Mu $!iter;
        has Mu $!test;
        method BUILD(\list,Mu \test) {
            $!iter  = as-iterable(list).iterator;
            $!test := test;
            self
        }
        method new(\list,Mu \test) { nqp::create(self).BUILD(list,test) }
    }

    proto method grep(|) is nodal { * }
    multi method grep(Bool:D $t) {
        fail X::Match::Bool.new( type => '.grep' );
    }
    multi method grep(Regex:D $test) {
        Seq.new(class :: does Grepper {
            method pull-one() is raw {
                my Mu $value;
                until ($value := $!iter.pull-one) =:= IterationEnd {
                    return-rw $value if $value.match($!test);
                }
                IterationEnd
            }
            method push-all($target) {
                my Mu $value;
                until ($value := $!iter.pull-one) =:= IterationEnd {
                    $target.push($value) if $value.match($!test);
                }
                IterationEnd
            }
        }.new(self, $test))
    }
    multi method grep(Callable:D $test) {
        if ($test.count == 1) {
            $test.?has-phasers
              ?? self.map({ next unless $test($_); $_ })  # cannot go fast
              !! Seq.new(class :: does Grepper {
                     method pull-one() is raw {
                         my Mu $value;
                         until ($value := $!iter.pull-one) =:= IterationEnd {
                             return-rw $value if $!test($value);
                         }
                         IterationEnd   # in case of last
                     }
                     method push-all($target) {
                         my Mu $value;
                         until ($value := $!iter.pull-one) =:= IterationEnd {
                             $target.push($value) if $!test($value);
                         }
                         IterationEnd   # in case of last
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
    multi method grep(Mu $test) {
        Seq.new(class :: does Grepper {
            method pull-one() is raw {
                my Mu $value;
                until ($value := $!iter.pull-one) =:= IterationEnd {
                    return-rw $value if $!test.ACCEPTS($value);
                }
                IterationEnd
            }
            method push-all($target) {
                my Mu $value;
                until ($value := $!iter.pull-one) =:= IterationEnd {
                    $target.push($value) if $!test.ACCEPTS($value);
                }
                IterationEnd
            }
        }.new(self, $test))
    }

    proto method grep-index(|) is nodal { * }
    multi method grep-index(Bool:D $t) {
        fail X::Match::Bool.new( type => '.grep-index' );
    }
    multi method grep-index(Regex:D $test) {
        my int $index = -1;
        self.map: {
            $index = $index+1;
            next unless .match($test);
            nqp::box_i($index,Int);
        };
    }
    multi method grep-index(Callable:D $test) {
        my int $index = -1;
        self.map: {
            $index = $index + 1;
            next unless $test($_);
            nqp::box_i($index,Int);
        };
    }
    multi method grep-index(Mu $test) {
        my int $index = -1;
        self.map: {
            $index = $index + 1;
            next unless $_ ~~ $test;
            nqp::box_i($index,Int);
        };
    }

    proto method first(|) is nodal { * }
    multi method first(Bool:D $t) {
        fail X::Match::Bool.new( type => '.first' );
    }
    multi method first(Regex:D $test) is raw {
        self.map({ return-rw $_ if .match($test) });
        Nil;
    }
    multi method first(Callable:D $test) is raw {
        self.map({ return-rw $_ if $test($_) });
        Nil;
    }
    multi method first(Mu $test) is raw {
        self.map({ return-rw $_ if $_ ~~ $test });
        Nil;
    }

    proto method first-index(|) is nodal { * }
    multi method first-index(Bool:D $t) {
        fail X::Match::Bool.new( type => '.first-index' );
    }
    multi method first-index(Regex:D $test) {
        my int $index = -1;
        self.map: {
            $index = $index + 1;
            return nqp::box_i($index,Int) if .match($test);
        };
        Nil;
    }
    multi method first-index(Callable:D $test) {
        my int $index = -1;
        self.map: {
            $index = $index + 1;
            return nqp::box_i($index,Int) if $test($_);
        };
        Nil;
    }
    multi method first-index(Mu $test) {
        my int $index = -1;
        self.map: {
            $index = $index + 1;
            return nqp::box_i($index,Int) if $_ ~~ $test;
        };
        Nil;
    }

    proto method last-index(|) is nodal { * }
    multi method last-index(Bool:D $t) {
        fail X::Match::Bool.new( type => '.last-index' );
    }
    multi method last-index(Regex:D $test) {
        my $elems = self.elems;
        return Inf if $elems == Inf;

        my int $index = $elems;
        while $index {
            $index = $index - 1;
            return nqp::box_i($index,Int) if self.AT-POS($index).match($test);
        }
        Nil;
    }
    multi method last-index(Callable:D $test) {
        my $elems = self.elems;
        return Inf if $elems == Inf;

        my int $index = $elems;
        while $index {
            $index = $index - 1;
            return nqp::box_i($index,Int) if $test(self.AT-POS($index));
        }
        Nil;
    }
    multi method last-index(Mu $test) {
        my $elems = self.elems;
        return Inf if $elems == Inf;

        my int $index = $elems;
        while $index {
            $index = $index - 1;
            return nqp::box_i($index,Int) if self.AT-POS($index) ~~ $test;
        }
        Nil;
    }

    method !first-concrete(\i,\todo,\found) {
        my $value;
        while nqp::islt_i(i,todo) {
            $value := self.AT-POS(i);
            i = i + 1;
            if nqp::isconcrete($value) {
                found = $value;
                last;
            }
        }
    }

    proto method min (|) is nodal { * }
    multi method min() {
        my $elems = self.cache.elems;
        die "Cannot .min on an infinite list" if $elems == Inf;

        my $value;
        my $min;
        my int $todo = $elems;
        my int $index;

        self!first-concrete($index,$todo,$min);
        while nqp::islt_i($index,$todo) {
            $value := self.AT-POS($index);
            $index  = $index + 1;
            $min    = $value
              if nqp::isconcrete($value) && $value cmp $min < 0;
        }
        $min // Inf;
    }
    multi method min(&by) {
        my $elems = self.cache.elems;
        die "Cannot .min on an infinite list" if $elems == Inf;

        my $cmp = &by.arity == 2 ?? &by !! { &by($^a) cmp &by($^b) }
        my $value;
        my $min;
        my int $todo = $elems;
        my int $index;

        self!first-concrete($index,$todo,$min);
        while nqp::islt_i($index,$todo) {
            $value := self.AT-POS($index);
            $index  = $index + 1;
            $min    = $value
              if nqp::isconcrete($value) && $cmp($value,$min) < 0;
        }
        $min // Inf;
    }

    proto method max (|) is nodal { * }
    multi method max() {
        my $elems = self.cache.elems;
        die "Cannot .max on an infinite list" if $elems == Inf;

        my $value;
        my $max;
        my int $todo = $elems;
        my int $index;

        self!first-concrete($index,$todo,$max);
        while nqp::islt_i($index,$todo) {
            $value := self.AT-POS($index);
            $index  = $index + 1;
            $max    = $value
              if nqp::isconcrete($value) && $value cmp $max > 0;
        }
        $max // -Inf;
    }
    multi method max(&by) {
        my $elems = self.cache.elems;
        die "Cannot .max on an infinite list" if $elems == Inf;

        my $cmp = &by.arity == 2 ?? &by !! { &by($^a) cmp &by($^b) }
        my $value;
        my $max;
        my int $todo = $elems;
        my int $index;

        self!first-concrete($index,$todo,$max);
        while nqp::islt_i($index,$todo) {
            $value := self.AT-POS($index);
            $index  = $index + 1;
            $max    = $value
              if nqp::isconcrete($value) && $cmp($value,$max) > 0;
        }
        $max // -Inf;
    }

    proto method minmax (|) is nodal { * }
    multi method minmax(&by = &infix:<cmp>) {
        my $cmp = &by.arity == 2 ?? &by !! { &by($^a) cmp &by($^b) };

        my $min;
        my $max;
        my $excludes-min = Bool::False;
        my $excludes-max = Bool::False;

        self.map: {
            .defined or next;

            if .isa(Range) {
                if !$min.defined || $cmp($_.min, $min) < 0 {
                    $min = .min;
                    $excludes-min = $_.excludes-min;
                }
                if !$max.defined || $cmp($_.max, $max) > 0 {
                    $max = .max;
                    $excludes-max = $_.excludes-max;
                }
            } elsif Positional.ACCEPTS($_) {
                my $mm = .minmax(&by);
                if !$min.defined || $cmp($mm.min, $min) < 0 {
                    $min = $mm.min;
                    $excludes-min = $mm.excludes-min;
                }
                if !$max.defined || $cmp($mm.max, $max) > 0 {
                    $max = $mm.max;
                    $excludes-max = $mm.excludes-max;
                }
            } else {
                if !$min.defined || $cmp($_, $min) < 0 {
                    $min = $_;
                    $excludes-min = Bool::False;
                }
                if !$max.defined || $cmp($_, $max) > 0 {
                    $max = $_;
                    $excludes-max = Bool::False;
                }
            }
        }
        Range.new($min // Inf,
                  $max // -Inf,
                  :excludes-min($excludes-min),
                  :excludes-max($excludes-max));
    }

    method sort(&by = &infix:<cmp>) is nodal {
        # Obtain all the things to sort.
        my \iter = as-iterable(self).iterator;
        my \sort-buffer = IterationBuffer.new;
        unless iter.push-until-lazy(sort-buffer) =:= IterationEnd {
            fail X::Cannot::Lazy.new(:action<sort>);
        }

        # Apply any transform.
        my $transform = (&by.?count // 2) < 2;
        my $transform-buffer;
        if $transform {
            $transform-buffer := IterationBuffer.new;
            my \to-map = nqp::p6bindattrinvres(List.CREATE, List, '$!reified',
                sort-buffer);
            to-map.map(&by).iterator.push-all($transform-buffer);
        }

        # Instead of sorting elements directly, we sort a list of
        # indices from 0..^$list.elems, then use that list as
        # a slice into self. The JVM implementation uses a Java
        # collection sort. MoarVM has its sort algorithm implemented
        # in NQP.
        my int $i = 0;
        my int $n = sort-buffer.elems;
        my \indices = IterationBuffer.new;
        while $i < $n {
            nqp::push(indices, nqp::decont($i));
            $i = $i + 1;
        }

        nqp::p6sort(indices, $transform
            ?? (-> int $a, int $b {
                    nqp::atpos($transform-buffer, $a) cmp nqp::atpos($transform-buffer, $b)
                        || $a <=> $b
                })
            !! (-> int $a, int $b {
                    &by(nqp::atpos(sort-buffer, $a), nqp::atpos(sort-buffer, $b))
                        || $a <=> $b
                }));

        my \indices-list = nqp::p6bindattrinvres(List.CREATE, List, '$!reified', indices);
        indices-list.map(-> int $i { nqp::atpos(sort-buffer,       $i) })
    }

    proto method reduce(|) { * }
    multi method reduce(&with) is nodal {
        return unless self.DEFINITE;

        # XXX GLR we really, really should be able to do reduce on the
        # iterable in left-associative cases without having to make a
        # list in memory.
        if &with.count > 2 and &with.count < Inf {
            my int $count = &with.count;
            if try &with.prec<assoc> eq 'right' {
                my \iter = self.reverse.iterator;
                my Mu $val := iter.pull-one;
                return if $val =:= IterationEnd;
                my @args = $val;
                while (my \current = iter.pull-one) !=:= IterationEnd {
                    @args.prepend: current;
                    if @args.elems == $count {
                        $val := with(|@args);
                        @args = $val;
                    }
                }
                return $val;
            }
            else {
                my \iter = nqp::istype(self, Iterator)
                           ?? self.iterator
                           !! self.list.iterator;
                my Mu $val := iter.pull-one;
                return if $val =:= IterationEnd;
                my @args = $val;
                while (my \current = iter.pull-one) !=:= IterationEnd {
                    @args.append: current;
                    if @args.elems == $count {
                        $val := with(|@args);
                        @args = $val;
                    }
                }
                return $val;
            }
        }
        my $reducer := find-reducer-for-op(&with);
        $reducer(&with)(self) if $reducer;
    }

    proto method unique(|) is nodal {*}
    multi method unique() {
        Seq.new(class :: does Iterator {
            has Mu $!iter;
            has $!seen;
            method BUILD(\list) {
                $!iter = as-iterable(list).iterator;
                $!seen := nqp::hash();
                self
            }
            method new(\list) { nqp::create(self).BUILD(list) }
            method pull-one() {
                my Mu $value;
                my str $needle;
                until ($value := $!iter.pull-one) =:= IterationEnd {
                    $needle = nqp::unbox_s($value.WHICH);
                    unless nqp::existskey($!seen, $needle) {
                        nqp::bindkey($!seen, $needle, 1);
                        return $value;
                    }
                }
                IterationEnd
            }
            method push-all($target) {
                my Mu $value;
                my str $needle;
                until ($value := $!iter.pull-one) =:= IterationEnd {
                    $needle = nqp::unbox_s($value.WHICH);
                    unless nqp::existskey($!seen, $needle) {
                        nqp::bindkey($!seen, $needle, 1);
                        $target.push($value);
                    }
                }
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
            method BUILD(\list, &!as) {
                $!iter  = as-iterable(list).iterator;
                $!seen := nqp::hash();
                self
            }
            method new(\list, &as) { nqp::create(self).BUILD(list, &as) }
            method pull-one() {
                my Mu $value;
                my str $needle;
                until ($value := $!iter.pull-one) =:= IterationEnd {
                    $needle = nqp::unbox_s(&!as($value).WHICH);
                    unless nqp::existskey($!seen, $needle) {
                        nqp::bindkey($!seen, $needle, 1);
                        return $value;
                    }
                }
                IterationEnd
            }
            method push-all($target) {
                my Mu $value;
                my str $needle;
                until ($value := $!iter.pull-one) =:= IterationEnd {
                    $needle = nqp::unbox_s(&!as($value).WHICH);
                    unless nqp::existskey($!seen, $needle) {
                        nqp::bindkey($!seen, $needle, 1);
                        $target.push($value);
                    }
                }
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
            method BUILD(\list) {
                $!iter = as-iterable(list).iterator;
                $!seen := nqp::hash();
                self
            }
            method new(\list) { nqp::create(self).BUILD(list) }
            method pull-one() {
                my Mu $value;
                my str $needle;
                until ($value := $!iter.pull-one) =:= IterationEnd {
                    $needle = nqp::unbox_s($value.WHICH);
                    nqp::existskey($!seen, $needle)
                      ?? return $value
                      !! nqp::bindkey($!seen, $needle, 1);
                }
                IterationEnd
            }
            method push-all($target) {
                my Mu $value;
                my str $needle;
                until ($value := $!iter.pull-one) =:= IterationEnd {
                    $needle = nqp::unbox_s($value.WHICH);
                    nqp::existskey($!seen, $needle)
                      ?? $target.push($value)
                      !! nqp::bindkey($!seen, $needle, 1);
                }
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
            method BUILD(\list, &!as) {
                $!iter  = as-iterable(list).iterator;
                $!seen := nqp::hash();
                self
            }
            method new(\list, &as) { nqp::create(self).BUILD(list, &as) }
            method pull-one() {
                my Mu $value;
                my str $needle;
                until ($value := $!iter.pull-one) =:= IterationEnd {
                    $needle = nqp::unbox_s(&!as($value).WHICH);
                    nqp::existskey($!seen, $needle)
                      ?? return $value
                      !! nqp::bindkey($!seen, $needle, 1);
                }
                IterationEnd
            }
            method push-all($target) {
                my Mu $value;
                my str $needle;
                until ($value := $!iter.pull-one) =:= IterationEnd {
                    $needle = nqp::unbox_s(&!as($value).WHICH);
                    nqp::existskey($!seen, $needle)
                      ?? $target.push($value)
                      !! nqp::bindkey($!seen, $needle, 1);
                }
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
            has $!last;
            method BUILD(\list, &!as, &!with) {
                $!iter  = as-iterable(list).iterator;
                self
            }
            method new(\list, &as, &with) {
                nqp::create(self).BUILD(list, &as, &with)
            }
            method pull-one() {
                my Mu $value;
                my $which;
                until ($value := $!iter.pull-one) =:= IterationEnd {
                    $which = &!as($value);
                    once { $!last = $which; return $value }
                    unless with($which,$!last) {
                        $!last = $which;
                        return $value;
                    }
                }
                IterationEnd
            }
            method push-all($target) {
                my Mu $value;
                my $which;
                until ($value := $!iter.pull-one) =:= IterationEnd {
                    $which = &!as($value);
                    once { $!last = $which; $target.push($value); next }
                    unless with($which,$!last) {
                        $!last = $which;
                        $target.push($value);
                    }
                }
                IterationEnd
            }
        }.new(self, &as, &with))
    }
    multi method squish( :&with = &[===] ) {
        Seq.new(class :: does Iterator {
            has Mu $!iter;
            has &!with;
            has $!last;
            method BUILD(\list, &!with) {
                $!iter  = as-iterable(list).iterator;
                self
            }
            method new(\list, &with) { nqp::create(self).BUILD(list, &with) }
            method pull-one() {
                my Mu $value;
                until ($value := $!iter.pull-one) =:= IterationEnd {
                    once { $!last = $value; return $value }
                    unless with($value,$!last) {
                        $!last = $value;
                        return $value;
                    }
                }
                IterationEnd
            }
            method push-all($target) {
                my Mu $value;
                until ($value := $!iter.pull-one) =:= IterationEnd {
                    once { $!last = $value; $target.push($value); next }
                    unless with($value,$!last) {
                        $!last = $value;
                        $target.push($value);
                    }
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
multi sub grep(Mu $test, +values) { my $laze = values.is-lazy; values.grep($test).lazy-if($laze) }
multi sub grep(Bool:D $t, |) { fail X::Match::Bool.new( type => 'grep' ) }

proto sub grep-index(|) {*}
multi sub grep-index(Mu $test, +values) { my $laze = values.is-lazy; values.grep-index($test).lazy-if($laze) }
multi sub grep-index(Bool:D $t, |) {
    fail X::Match::Bool.new(type => 'grep-index');
}

proto sub first(|) {*}
multi sub first(Mu $test, +values) { my $laze = values.is-lazy; values.first($test).lazy-if($laze) }
multi sub first(Bool:D $t, |) { fail X::Match::Bool.new( type => 'first' ) }

proto sub first-index(|) {*}
multi sub first-index(Mu $test, +values) { my $laze = values.is-lazy; values.first-index($test).lazy-if($laze) }
multi sub first-index(Bool:D $t,|) {
    fail X::Match::Bool.new(type => 'first-index');
}

proto sub last-index(|) {*}
multi sub last-index(Mu $test, +values) { my $laze = values.is-lazy; values.last-index($test).lazy-if($laze) }
multi sub last-index(Bool:D $t, |) {
    fail X::Match::Bool.new(type => 'last-index');
}

proto sub join(|) { * }
multi sub join($sep = '', *@values) { @values.join($sep) }

sub reduce (&with, +list)  { list.reduce(&with) }

proto sub unique(|) { * }
multi sub unique(+values, |c) { my $laze = values.is-lazy; values.unique(|c).lazy-if($laze) }

proto sub squish(|) { * }
multi sub squish(+values, |c) { my $laze = values.is-lazy; values.squish(|c).lazy-if($laze) }

proto sub sort(|) {*}
multi sub sort($cmp, +values)      {
    nqp::istype($cmp, Callable)
        ?? values.sort($cmp)
        !! (|$cmp,|values).sort;
}

# vim: ft=perl6 expandtab sw=4
