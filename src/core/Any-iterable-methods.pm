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

    my role MapIterCommon does SlippyIterator {
        has &!block;
        has $!source;
        has $!count;
        has $!label;

        method new(&block, $source, $count, $label) {
            my $iter := nqp::create(self);
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

        # "loop" taking 0 or 1 parameter
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

                    if $!slipping && !(($result := self.slip-one()) =:= IterationEnd) {
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
                    }
                    IterationEnd
                }
            }.new(&block, source, 1, $label));
        }

        # loop/map taking more than 1 param
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
            }.new(&block, source, $count, $label));
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
            method BUILD(\list,Mu \test) {
                $!iter  = list.iterator;
                $!test := test;
                $!index = -1;
                self
            }
            method new(\list,Mu \test) { nqp::create(self).BUILD(list,test) }
            method pull-one() is raw {
                $!index = $!index + 1
                  until ($_ := $!iter.pull-one) =:= IterationEnd || $!test($_);
                $_ =:= IterationEnd
                  ?? IterationEnd
                  !! nqp::p6box_i($!index = $!index + 1)
            }
            method push-exactly($target, int $n) {
                my int $done;
                while $done < $n {
                    return IterationEnd
                      if IterationEnd =:= ($_ := $!iter.pull-one);
                    $!index = $!index + 1;
                    if $!test($_) {
                        $target.push(nqp::p6box_i($!index));
                        $done = $done + 1;
                    }
                }
                $done
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
            method BUILD(\list,Mu \test) {
                $!iter  = list.iterator;
                $!test := test;
                $!index = -1;
                self
            }
            method new(\list,Mu \test) { nqp::create(self).BUILD(list,test) }
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
            method push-exactly($target, int $n) {
                my int $done;
                my $no-sink;
                if $!value.DEFINITE {
                    $no-sink := $target.push($!value);
                    $!value  := Mu;
                    $done = $done + 1;
                }
                while $done < $n {
                    return IterationEnd
                      if IterationEnd =:= ($_ := $!iter.pull-one);
                    $!index = $!index + 1;
                    if $!test($_) {
                        $target.push(nqp::p6box_i($!index));
                        if ($done = $done + 1) < $n {
                            $no-sink := $target.push($_);
                            $done = $done + 1;
                        }
                        else {
                            $!value := $_;
                        }
                    }
                }
                $done
            }
            method push-all($target) {
                my $no-sink;
                until ($_ := $!iter.pull-one) =:= IterationEnd {
                    $!index = $!index + 1;
                    if $!test($_) {
                        $target.push(nqp::p6box_i($!index));
                        $no-sink := $target.push($_);
                    }
                }
                IterationEnd
            }
        }.new(self, $test))
    }
    method !grep-p(Callable:D $test) {
        Seq.new(class :: does Iterator {
            has  Mu $!iter;
            has  Mu $!test;
            has int $!index;
            method BUILD(\list,Mu \test) {
                $!iter  = list.iterator;
                $!test := test;
                $!index = -1;
                self
            }
            method new(\list,Mu \test) { nqp::create(self).BUILD(list,test) }
            method pull-one() is raw {
                $!index = $!index + 1
                  until ($_ := $!iter.pull-one) =:= IterationEnd || $!test($_);
                $_ =:= IterationEnd
                  ?? IterationEnd
                  !! Pair.new($!index = $!index + 1,$_)
            }
            method push-exactly($target, int $n) {
                my int $done;
                while $done < $n {
                    return IterationEnd
                      if IterationEnd =:= ($_ := $!iter.pull-one);
                    $!index = $!index + 1;
                    if $!test($_) {
                        $target.push(Pair.new($!index,$_));
                        $done = $done + 1;
                    }
                }
                $done
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
        method BUILD(\list,Mu \test) {
            $!iter  = list.iterator;
            $!test := test;
            self
        }
        method new(\list,Mu \test) { nqp::create(self).BUILD(list,test) }
    }
    method !grep-regex(Regex:D $test) {
        Seq.new(class :: does Grepper {
            method pull-one() is raw {
                Nil until ($_ := $!iter.pull-one) =:= IterationEnd
                  || $_.match($!test);
                $_
            }
            method push-exactly($target, int $n) {
                my int $done;
                my $no-sink;
                while $done < $n {
                    return IterationEnd
                      if IterationEnd =:= ($_ := $!iter.pull-one);
                    if $_.match($!test) {
                        $no-sink := $target.push($_);
                        $done = $done + 1;
                    }
                }
                $done
            }
            method push-all($target) {
                my $no-sink;
                $no-sink := $target.push($_) if $_.match($!test)
                  until ($_ := $!iter.pull-one) =:= IterationEnd;
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
                         Nil until ($_ := $!iter.pull-one) =:= IterationEnd
                           || $!test($_);
                         $_
                     }
                     method push-exactly($target, int $n) {
                         my int $done;
                         my $no-sink;
                         while $done < $n {
                             return IterationEnd
                               if IterationEnd =:= ($_ := $!iter.pull-one);
                             if $!test($_) {
                                 $no-sink := $target.push($_);
                                 $done = $done + 1;
                             }
                         }
                         $done
                     }
                     method push-all($target) {
                         my $no-sink;
                         $no-sink := $target.push($_) if $!test($_)
                           until ($_ := $!iter.pull-one) =:= IterationEnd;
                         IterationEnd
                     }
                     method sink-all() {
                         $!test($_)
                           until ($_ := $!iter.pull-one) =:= IterationEnd;
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
                Nil until ($_ := $!iter.pull-one) =:= IterationEnd
                  || $!test.ACCEPTS($_);
                $_
            }
            method push-exactly($target, int $n) {
                my int $done;
                my $no-sink;
                while $done < $n {
                    return IterationEnd
                      if IterationEnd =:= ($_ := $!iter.pull-one);
                    if $!test.ACCEPTS($_) {
                        $no-sink := $target.push($_);
                        $done = $done + 1;
                    }
                }
                $done
            }
            method push-all($target) {
                my $no-sink;
                $no-sink := $target.push($_) if $!test.ACCEPTS($_)
                  until ($_ := $!iter.pull-one) =:= IterationEnd;
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
                else {
                    my $k = %a.keys[0];
                    if $k eq 'k' || $k eq 'p' {
                        value
                    }
                    elsif $k eq 'v' {
                        fail "Doesn't make sense to specify a negated :v adverb"
                    }
                    else {
                        fail X::Adverb.new(
                          :$what,
                          :source(try { self.VAR.name } // self.WHAT.perl),
                          :unexpected(%a.keys))
                    }
                }
            }
            else {
                fail X::Adverb.new(
                  :$what,
                  :source(try { self.VAR.name } // self.WHAT.perl),
                  :nogo(%a.keys.grep: /k|v|p/)
                  :unexpected(%a.keys.grep: { !.match(/k|v|p/) } ))
            }
        }
        else {
            value
        }
    }

    proto method grep(|) is nodal { * }
    multi method grep(Bool:D $t) {
        fail X::Match::Bool.new( type => '.grep' );
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
                      ?? fail "Doesn't make sense to specify a negated :v adverb"
                      !! fail X::Adverb.new(
                           :what<grep>,
                           :source(try { self.VAR.name } // self.WHAT.perl),
                           :unexpected($k))
                }
            }
        }
        else {
            fail X::Adverb.new(
              :what<grep>,
              :source(try { self.VAR.name } // self.WHAT.perl),
              :nogo(%_.keys.grep: /k|v|kv|p/)
              :unexpected(%_.keys.grep: { !.match(/k|v|kv|p/) } ))
        }
    }

    proto method first(|) is nodal { * }
    multi method first(:$end) {
        $end
          ?? ((my $elems = self.elems) ?? self.AT-POS($elems - 1) !! Nil)
          !! ((my $x := self.iterator.pull-one) =:= IterationEnd ?? Nil !! $x)
    }
    multi method first(Bool:D $t) {
        fail X::Match::Bool.new( type => '.first' );
    }
    multi method first(Regex:D $test, :$end, *%a) is raw {
        if $end {
            my $elems = self.elems;
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
            my $elems = self.elems;
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
            my $elems = self.elems;
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
        my \iter = self.iterator;
        my \sort-buffer = IterationBuffer.new;
        unless iter.push-until-lazy(sort-buffer) =:= IterationEnd {
            fail X::Cannot::Lazy.new(:action<sort>);
        }

        # Apply any transform.
        my $transform = (&by.?count // 2) < 2;
        my $transform-buffer;
        if $transform {
            $transform-buffer := IterationBuffer.new;
            my \to-map = nqp::p6bindattrinvres(nqp::create(List), List, '$!reified',
                sort-buffer);
            to-map.map(&by).iterator.push-all($transform-buffer);
        }

        # Instead of sorting elements directly, we sort a list of
        # indices from 0..^$list.elems, then use that list as
        # a slice into self. The JVM implementation uses a Java
        # collection sort. MoarVM has its sort algorithm implemented
        # in NQP.
        my int $i = -1;
        my int $n = sort-buffer.elems;
        my $indices := nqp::list;
        nqp::setelems($indices,$n);
        nqp::bindpos($indices,$i,nqp::decont($i)) while ++$i < $n;

        nqp::p6sort($indices, $transform
            ?? (-> int $a, int $b {
                    nqp::atpos($transform-buffer, $a) cmp nqp::atpos($transform-buffer, $b)
                        || $a <=> $b
                })
            !! (-> int $a, int $b {
                    &by(nqp::atpos(sort-buffer, $a), nqp::atpos(sort-buffer, $b))
                        || $a <=> $b
                }));

        $i = -1;
        my $result := nqp::list;
        nqp::setelems($result,$n);
        nqp::bindpos($result,$i,nqp::atpos(sort-buffer,nqp::atpos($indices,$i)))
          while ++$i < $n;
        $result
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
            method BUILD(\list) {
                $!iter = list.iterator;
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
            method push-exactly($target, int $n) {
                my Mu $value;
                my str $needle;
                my int $done;
                my $no-sink;
                while $done < $n {
                    return IterationEnd
                      if IterationEnd =:= ($value := $!iter.pull-one);
                    $needle = nqp::unbox_s($value.WHICH);
                    unless nqp::existskey($!seen, $needle) {
                        nqp::bindkey($!seen, $needle, 1);
                        $no-sink := $target.push($value);
                        $done = $done + 1;
                    }
                }
                $done
            }
            method push-all($target) {
                my Mu $value;
                my str $needle;
                my $no-sink;
                until ($value := $!iter.pull-one) =:= IterationEnd {
                    $needle = nqp::unbox_s($value.WHICH);
                    unless nqp::existskey($!seen, $needle) {
                        nqp::bindkey($!seen, $needle, 1);
                        $no-sink := $target.push($value);
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
                $!iter  = list.iterator;
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
            method push-exactly($target, int $n) {
                my Mu $value;
                my str $needle;
                my int $done;
                my $no-sink;
                while $done < $n {
                    return IterationEnd
                      if IterationEnd =:= ($value := $!iter.pull-one);
                    $needle = nqp::unbox_s(&!as($value).WHICH);
                    unless nqp::existskey($!seen, $needle) {
                        nqp::bindkey($!seen, $needle, 1);
                        $no-sink := $target.push($value);
                        $done = $done + 1;
                    }
                }
                $done
            }
            method push-all($target) {
                my Mu $value;
                my str $needle;
                my $no-sink;
                until ($value := $!iter.pull-one) =:= IterationEnd {
                    $needle = nqp::unbox_s(&!as($value).WHICH);
                    unless nqp::existskey($!seen, $needle) {
                        nqp::bindkey($!seen, $needle, 1);
                        $no-sink := $target.push($value);
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
                $!iter = list.iterator;
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
            method push-exactly($target, int $n) {
                my Mu $value;
                my str $needle;
                my int $done;
                my $no-sink;
                while $done < $n {
                    return IterationEnd
                      if IterationEnd =:= ($value := $!iter.pull-one);
                    $needle = nqp::unbox_s($value.WHICH);
                    if nqp::existskey($!seen, $needle) {
                        $no-sink := $target.push($value);
                        $done = $done + 1;
                    }
                    else {
                        nqp::bindkey($!seen, $needle, 1);
                    }
                }
                $done
            }
            method push-all($target) {
                my Mu $value;
                my str $needle;
                my $no-sink;
                until ($value := $!iter.pull-one) =:= IterationEnd {
                    $needle = nqp::unbox_s($value.WHICH);
                    nqp::existskey($!seen, $needle)
                      ?? ($no-sink := $target.push($value))
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
                $!iter  = list.iterator;
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
            method push-exactly($target, int $n) {
                my Mu $value;
                my str $needle;
                my int $done;
                my $no-sink;
                while $done < $n {
                    return IterationEnd
                      if IterationEnd =:= ($value := $!iter.pull-one);
                    $needle = nqp::unbox_s(&!as($value).WHICH);
                    if nqp::existskey($!seen, $needle) {
                        $no-sink := $target.push($value);
                        $done = $done + 1;
                    }
                    else {
                        nqp::bindkey($!seen, $needle, 1);
                    }
                }
                $done
            }
            method push-all($target) {
                my Mu $value;
                my str $needle;
                my $no-sink;
                until ($value := $!iter.pull-one) =:= IterationEnd {
                    $needle = nqp::unbox_s(&!as($value).WHICH);
                    nqp::existskey($!seen, $needle)
                      ?? ($no-sink := $target.push($value))
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
            has int $!first;
            method BUILD(\list, &!as, &!with) {
                $!iter  = list.iterator;
                $!first = 1;
                self
            }
            method new(\list, &as, &with) {
                nqp::create(self).BUILD(list, &as, &with)
            }
            method pull-one() {
                my Mu $value := $!iter.pull-one;
                my $which = &!as($value);
                if $!first {
                    $!first = 0;
                }
                else {
                    until IterationEnd =:= $value || !with($which,$!last) {
                        $value := $!iter.pull-one;
                        $which = &!as($value);
                    }
                }
                $!last = $which;
                $value
            }
            method push-all($target) {
                my Mu $value := $!iter.pull-one;
                my $which = &!as($value);
                my $no-sink;
                if $!first {
                    $!first = 0;
                    unless IterationEnd =:= $value {
                        $no-sink := $target.push($value);
                        $!last = $which;
                        $value := $!iter.pull-one;
                    }
                }
                until IterationEnd =:= $value {
                    $which = &!as($value);
                    unless with($which,$!last) {
                        $no-sink := $target.push($value);
                        $!last = $which;
                    }
                    $value := $!iter.pull-one;
                }
                $value
            }
        }.new(self, &as, &with))
    }
    multi method squish( :&with = &[===] ) {
        Seq.new(class :: does Iterator {
            has Mu $!iter;
            has &!with;
            has Mu $!last;
            has int $!first;
            method BUILD(\list, &!with) {
                $!iter  = list.iterator;
                $!first = 1;
                self
            }
            method new(\list, &with) { nqp::create(self).BUILD(list, &with) }
            method pull-one() {
                my Mu $value := $!iter.pull-one;
                if $!first {
                    $!first = 0;
                }
                else {
                    $value := $!iter.pull-one
                      until IterationEnd =:= $value || !with($value,$!last);
                }
                $!last = $value
            }
            method push-all($target) {
                my Mu $value := $!iter.pull-one;
                my $no-sink;
                if $!first {
                    $!first = 0;
                    unless IterationEnd =:= $value {
                        $no-sink := $target.push($value);
                        $!last = $value;
                        $value := $!iter.pull-one;
                    }
                }
                until IterationEnd =:= $value {
                    unless with($value,$!last) {
                        $no-sink := $target.push($value);
                        $!last = $value;
                    }
                    $value := $!iter.pull-one;
                }
                $value
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
            method BUILD(\list,\todo) {
                $!iter = list.iterator;
                $!todo = todo;
                self
            }
            method new(\list,\todo) { nqp::create(self).BUILD(list,todo) }
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
            method BUILD(\list,\size) {
                $!iter = list.iterator;
                X::Cannot::Lazy.new(:action<tail>).throw if $!iter.is-lazy;

                $!lastn := nqp::list;
                $!size   = size;
                nqp::setelems($!lastn,$!size);  # presize list
                nqp::setelems($!lastn,0);
                self
            }
            method new(\list,\size) { nqp::create(self).BUILD(list,size) }
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
multi sub grep(Bool:D $t, |) { fail X::Match::Bool.new( type => 'grep' ) }

proto sub first(|) {*}
multi sub first(Bool:D $t, |) { fail X::Match::Bool.new( type => 'first' ) }
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
