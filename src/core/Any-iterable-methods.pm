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
        iterablish.DEFINITE && nqp::istype(iterablish, Iterable)
            ?? iterablish
            !! iterablish.list;
    }

    proto method map(|) { * }

    multi method map(&block, :$label) {
        sequential-map(as-iterable(self).iterator, &block, :$label);
    }

    multi method map(HyperIterable:D: &block, :$label) {
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

    sub sequential-map(\source, &block, :$label) {
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

        # We want map to be fast, so we go to some effort to build special
        # case iterators that can ignore various interesting cases.
        my $count = &block.count;
        $count = 1 if $count == Inf || $count == 0;
        if $count == 1 {
            # XXX We need a funkier iterator to care about phasers. Will
            # put that on a different code-path to keep the commonest
            # case fast.
            # XXX Support labels
            Seq.new(class :: does MapIterCommon {
                method pull-one() is rw {
                    my int $redo = 1;
                    my $value;
                    my $result;
                    if $!slipping && ($result := self.slip-one()) !=:= IterationEnd {
                        $result
                    }
                    elsif ($value := $!source.pull-one()) =:= IterationEnd {
                        $value
                    }
                    else {
                        nqp::while(
                            $redo,
                            nqp::stmts(
                                $redo = 0,
                                nqp::handle(
                                    nqp::stmts(
                                        ($result := &!block($value)),
                                        nqp::if(
                                            nqp::istype($result, Slip),
                                            nqp::stmts(
                                                ($result := self.start-slip($result)),
                                                nqp::if(
                                                    nqp::eqaddr($result, IterationEnd),
                                                    nqp::stmts(
                                                        ($value := $!source.pull-one()),
                                                        ($redo = 1 unless nqp::eqaddr($value, IterationEnd))
                                                ))
                                            ))
                                    ),
                                    'LABELED', nqp::decont($!label),
                                    'NEXT', nqp::stmts(
                                        ($value := $!source.pull-one()),
                                        nqp::eqaddr($value, IterationEnd)
                                            ?? ($result := IterationEnd)
                                            !! ($redo = 1)),
                                    'REDO', $redo = 1,
                                    'LAST', ($result := IterationEnd))),
                            :nohandler);
                        $result
                    }
                }
            }.new(&block, source, 1, $label));
        }
        else {
            Seq.new(class :: does MapIterCommon {
                has $!value-buffer;

                method pull-one() is rw {
                    $!value-buffer.DEFINITE
                        ?? nqp::setelems($!value-buffer, 0)
                        !! ($!value-buffer := IterationBuffer.new);
                    my int $redo = 1;
                    my $result;
                    if $!slipping && ($result := self.slip-one()) !=:= IterationEnd {
                        $result
                    }
                    elsif $!source.push-exactly($!value-buffer, $!count) =:= IterationEnd
                            && nqp::elems($!value-buffer) == 0 {
                        IterationEnd
                    }
                    else {
                        nqp::while(
                            $redo,
                            nqp::stmts(
                                $redo = 0,
                                nqp::handle(
                                    nqp::stmts(
                                        ($result := nqp::p6invokeflat(&!block, $!value-buffer)),
                                        nqp::if(
                                            nqp::istype($result, Slip),
                                            nqp::stmts(
                                                ($result := self.start-slip($result)),
                                                nqp::if(
                                                    nqp::eqaddr($result, IterationEnd),
                                                    nqp::stmts(
                                                        (nqp::setelems($!value-buffer, 0)),
                                                        ($redo = 1 unless nqp::eqaddr(
                                                                $!source.push-exactly($!value-buffer, $!count),
                                                                IterationEnd)
                                                            && nqp::elems($!value-buffer) == 0)
                                                ))
                                            ))
                                    ),
                                    'LABELED', nqp::decont($!label),
                                    'NEXT', nqp::stmts(
                                        (nqp::setelems($!value-buffer, 0)),
                                        nqp::eqaddr($!source.push-exactly($!value-buffer, $!count), IterationEnd)
                                                && nqp::elems($!value-buffer) == 0
                                            ?? ($result := IterationEnd)
                                            !! ($redo = 1)),
                                    'REDO', $redo = 1,
                                    'LAST', ($result := IterationEnd))),
                            :nohandler);
                        $result
                    }
                }
            }.new(&block, source, $count, $label));
        }
    }

    proto method flatmap (|) is nodal { * }
    multi method flatmap(&block, :$label) is rw {
        self.map(&block, :$label).flat
    }

    method for(|c) is nodal {
        DEPRECATED('flatmap',|<2015.05 2015.09>);
        self.flatmap(|c);
    }

    proto method grep(|) is nodal { * }
    multi method grep(Bool:D $t) is rw {
        fail X::Match::Bool.new( type => '.grep' );
    }
    multi method grep(Regex:D $test) is rw {
        self.map({ next unless .match($test); $_ });
    }
    multi method grep(Callable:D $test) is rw {
        if ($test.count == 1) {
            self.map({ next unless $test($_); $_ });
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
    multi method grep(Mu $test) is rw {
        self.map({ next unless $_ ~~ $test; $_ });
    }

    proto method grep-index(|) is nodal { * }
    multi method grep-index(Bool:D $t) is rw {
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
    multi method first(Bool:D $t) is rw {
        fail X::Match::Bool.new( type => '.first' );
    }
    multi method first(Regex:D $test) is rw {
        self.map({ return-rw $_ if .match($test) });
        Nil;
    }
    multi method first(Callable:D $test) is rw {
        self.map({ return-rw $_ if $test($_) });
        Nil;
    }
    multi method first(Mu $test) is rw {
        self.map({ return-rw $_ if $_ ~~ $test });
        Nil;
    }

    proto method first-index(|) is nodal { * }
    multi method first-index(Bool:D $t) is rw {
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
    multi method last-index(Bool:D $t) is rw {
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

    proto method min (|) is nodal { * }
    multi method min() {
        my $min;
        self.map: {
            $min = $_ if .defined and !$min.defined || $_ cmp $min < 0;
        }
        $min // Inf;
    }
    multi method min(&by) {
        my $cmp = &by.arity == 2 ?? &by !! { &by($^a) cmp &by($^b) }
        my $min;
        self.map: {
            $min = $_ if .defined and !$min.defined || $cmp($_, $min) < 0;
        }
        $min // Inf;
    }

    proto method max (|) is nodal { * }
    multi method max() {
        my $max;
        self.map: {
            $max = $_ if .defined and !$max.defined || $_ cmp $max > 0;
        }
        $max // -Inf;
    }
    multi method max(&by) {
        my $cmp = &by.arity == 2 ?? &by !! { &by($^a) cmp &by($^b) }
        my $max;
        self.map: {
            $max = $_ if .defined and !$max.defined || $cmp($_, $max) > 0;
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

        # Instead of sorting elements directly, we sort a Parcel of
        # indices from 0..^$list.elems, then use that Parcel as
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
                    @args.unshift: (current,);
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
                    @args.push: (current,);
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
        my $seen := nqp::hash();
        my str $target;
        gather self.map: {
            $target = nqp::unbox_s($_.WHICH);
            unless nqp::existskey($seen, $target) {
                nqp::bindkey($seen, $target, 1);
                take $_;
            }
        }
    }
    multi method unique( :&as!, :&with! ) {
        my @seen;  # should be Mu, but doesn't work in settings :-(
        my Mu $target;
        gather self.map: {
            $target = &as($_);
            if first( { with($target,$_) }, @seen ) =:= Nil {
                @seen.push(($target,));
                take $_;
            }
        };
    }
    multi method unique( :&as! ) {
        my $seen := nqp::hash();
        my str $target;
        gather self.map: {
            $target = &as($_).WHICH;
            unless nqp::existskey($seen, $target) {
                nqp::bindkey($seen, $target, 1);
                take $_;
            }
        }
    }
    multi method unique( :&with! ) {
        nextwith() if &with === &[===]; # use optimized version

        my @seen;  # should be Mu, but doesn't work in settings :-(
        my Mu $target;
        gather self.map: {
            $target := $_;
            if first( { with($target,$_) }, @seen ) =:= Nil {
                @seen.push(($target,));
                take $_;
            }
        }
    }

    method uniq(|c) is nodal {
        DEPRECATED('unique', |<2014.11 2015.09>);
        self.unique(|c);
    }

    my @secret;
    proto method squish(|) is nodal {*}
    multi method squish( :&as!, :&with = &[===] ) {
        my $last = @secret;
        my str $which;
        my \res := gather self.map: {
            $which = &as($_).Str;
            unless with($which,$last) {
                $last = $which;
                take $_;
            }
        }
        self.is-lazy ?? res.lazy !! res
    }
    multi method squish( :&with = &[===] ) {
        my $last = @secret;
        my \res := gather self.map: {
            unless with($_,$last) {
                $last = $_;
                take $_;
            }
        }
        self.is-lazy ?? res.lazy !! res
    }

    proto method pairup(|) is nodal { * }
    multi method pairup(Any:U:) { () }
    multi method pairup(Any:D:) {
        my \iter = nqp::istype(self, Iterable)
            ?? self.iterator
            !! self.list.iterator;
        gather loop {
            my $it := iter.pull-one;
            if nqp::istype($it, Enum) {
                take $it.key => $it.value
            }
            elsif nqp::istype($it, EnumMap) and !nqp::iscont($it) {
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
multi sub infix:<min>(*@args is rw) { @args.min }
# XXX the multi version suffers from a multi dispatch bug
# where the mandatory named is ignored in the presence of a slurpy
#proto sub min(|)     { * }
#multi sub min(*@args) { @args.min() }
#multi sub min(*@args, :&by!) { @args.min(&by) }
sub min(*@args is rw, :&by = &infix:<cmp>) { @args.min(&by) }

proto sub infix:<max>(|) is pure { * }
multi sub infix:<max>(Mu:D \a, Mu:U) { a }
multi sub infix:<max>(Mu:U, Mu:D \b) { b }
multi sub infix:<max>(Mu:D \a, Mu:D \b) { (a cmp b) > 0 ?? a !! b }
multi sub infix:<max>(*@args is rw) { @args.max }
sub max(*@args is rw, :&by = &infix:<cmp>) { @args.max(&by) }

proto sub infix:<minmax>(|) is pure { * }
multi sub infix:<minmax>(**@args is rw) { @args.minmax }
sub minmax(**@args is rw, :&by = &infix:<cmp>) { @args.minmax(&by) }

proto sub map(|) {*}
# fails integration/99problems-21-to-30, test 12/13
#multi sub map(&code, @values) { @values.map(&code) }
multi sub map(&code, *@values is rw) { @values.map(&code) }
multi sub map(Whatever, \a)    { a }
multi sub map(&code, Whatever) { (1..Inf).map(&code) }

proto sub grep(|) {*}
multi sub grep(Mu $test, @values) { @values.grep($test) }
multi sub grep(Mu $test, *@values) { @values.grep($test) }
multi sub grep(Bool:D $t, *@v) { fail X::Match::Bool.new( type => 'grep' ) }

proto sub grep-index(|) {*}
multi sub grep-index(Mu $test, @values) { @values.grep-index($test) }
multi sub grep-index(Mu $test, **@values) { @values.grep-index($test) }
multi sub grep-index(Bool:D $t, *@v) {
    fail X::Match::Bool.new(type => 'grep-index');
}

proto sub first(|) {*}
multi sub first(Mu $test, @values) { @values.first($test) }
multi sub first(Mu $test, *@values) { @values.first($test) }
multi sub first(Bool:D $t, *@v) { fail X::Match::Bool.new( type => 'first' ) }

proto sub first-index(|) {*}
multi sub first-index(Mu $test, @values) { @values.first-index($test) }
multi sub first-index(Mu $test, *@values) { @values.first-index($test) }
multi sub first-index(Bool:D $t,*@v) {
    fail X::Match::Bool.new(type => 'first-index');
}

proto sub last-index(|) {*}
multi sub last-index(Mu $test, @values) { @values.last-index($test) }
multi sub last-index(Mu $test, *@values) { @values.last-index($test) }
multi sub last-index(Bool:D $t, *@v) {
    fail X::Match::Bool.new(type => 'last-index');
}

proto sub join(|) { * }
multi sub join($sep = '', *@values) { @values.join($sep) }

sub reduce (&with, *@list)  { @list.reduce(&with) }

proto sub uniq(|) { * }
multi sub uniq(*@values, |c) {
    DEPRECATED('unique', |<2014.12 2015.09>);
    @values.unique(|c)
}

proto sub unique(|) { * }
multi sub unique(*@values, |c) { @values.unique(|c) }

proto sub squish(|) { * }
multi sub squish(*@values, |c) { @values.squish(|c) }

proto sub sort(|) {*}
multi sub sort(*@values)      {
    nqp::istype(@values.AT-POS(0), Callable)
        ?? SEQ(my $cmp := @values.shift; @values.sort($cmp) )
        !! @values.sort;
}

# vim: ft=perl6 expandtab sw=4
