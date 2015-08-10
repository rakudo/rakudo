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

    multi method map(&block) {
        sequential-map(as-iterable(self).iterator, &block);
    }

    multi method map(HyperIterable:D: &block) {
        # For now we only know how to parallelize when we've only one input
        # value needed per block. For the rest, fall back to sequential.
        if &block.count != 1 {
            my $source = self.DEFINITE && nqp::istype(self, Iterable)
                ?? self.iterator
                !! self.list.iterator;
            sequential-map($source, &block)
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
                    my \buffer-mapper = sequential-map($work.input-iterator, &!block);
                    buffer-mapper.iterator.push-all($work.output);
                    $work
                }

                method configuration() {
                    $!source.configuration
                }
            }.new(self.hyper-iterator, &block))
        }
    }

    sub sequential-map(\source, &block) {
        my role MapIterCommon does SlippyIterator {
            has &!block;
            has $!source;

            method new(&block, $source) {
                my $iter := self.CREATE;
                nqp::bindattr($iter, self, '&!block', &block);
                nqp::bindattr($iter, self, '$!source', $source);
                $iter
            }

            method lazy() {
                $!source.lazy
            }
        }

        # We want map to be fast, so we go to some effort to build special
        # case iterators that can ignore various interesting cases.
        my $count = &block.count;
        if $count == 1 {
            # XXX We need a funkier iterator to care about phasers. Will
            # put that on a different code-path to keep the commonest
            # case fast.
            # XXX Support labels
            Seq.new(class :: does MapIterCommon {
                method pull-one() {
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
                                                        ($value = $!source.pull-one()),
                                                        ($redo = 1 unless nqp::eqaddr($value, IterationEnd))
                                                ))
                                            ))
                                    ),
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
            }.new(&block, source));
        }
        else {
            die "map with .count > 1 NYI";
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
    multi method min(Any:D:) {
        my $min;
        self.map: {
            $min = $_ if .defined and !$min.defined || $_ cmp $min < 0;
        }
        $min // Inf;
    }
    multi method min(Any:D: &by) {
        my $cmp = &by.arity == 2 ?? &by !! { &by($^a) cmp &by($^b) }
        my $min;
        self.map: {
            $min = $_ if .defined and !$min.defined || $cmp($_, $min) < 0;
        }
        $min // Inf;
    }

    proto method max (|) is nodal { * }
    multi method max(Any:D:) {
        my $max;
        self.map: {
            $max = $_ if .defined and !$max.defined || $_ cmp $max > 0;
        }
        $max // -Inf;
    }
    multi method max(Any:D: &by) {
        my $cmp = &by.arity == 2 ?? &by !! { &by($^a) cmp &by($^b) }
        my $max;
        self.map: {
            $max = $_ if .defined and !$max.defined || $cmp($_, $max) > 0;
        }
        $max // -Inf;
    }

    proto method minmax (|) is nodal { * }
    multi method minmax(Any:D: &by = &infix:<cmp>) {
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

    # XXX GLR fix this to work on an Iterable rather than forcing a List
    proto method pairup(|) is nodal { * }
    multi method pairup(Any:U:) { () }
    multi method pairup(Any:D:) {
        my $list := self.list;
        my int $i;
        my int $elems = $list.elems;

        gather while $i < $elems {
            my Mu $it := $list.AT-POS($i++);
            if nqp::istype($it,Enum) {
                take $it.key => $it.value;
            }
            elsif nqp::istype($it,EnumMap) and !nqp::iscont($it) {
                take $it.pairs;
            }
            elsif $i < $elems {
                take $it => $list.AT-POS($i++);
            }
            else {
                X::Pairup::OddNumber.new.throw;
            }
        }
    }

    method join($separator = '') is nodal {
        # XXX GLR
        nqp::die('join needs re-implementing after GLR');
        #my $list = (self,).eager;
        #my Mu $rsa := nqp::list_s();
        #$list.gimme(4);        # force reification of at least 4 elements
        #unless $list.infinite {  # presize array
        #    nqp::setelems($rsa, nqp::unbox_i($list.elems));
        #    nqp::setelems($rsa, 0);
        #}
        #my $tmp;
        #while $list.gimme(0) {
        #    $tmp := $list.shift;
        #    nqp::push_s($rsa,
        #      nqp::unbox_s(nqp::istype($tmp, Str) && nqp::isconcrete($tmp) ?? $tmp !! $tmp.Str));
        #}
        #nqp::push_s($rsa, '...') if $list.infinite;
        #nqp::p6box_s(nqp::join(nqp::unbox_s($separator.Str), $rsa))
    }
}

BEGIN Attribute.^compose;

# vim: ft=perl6 expandtab sw=4
