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
}

BEGIN Attribute.^compose;

# vim: ft=perl6 expandtab sw=4
