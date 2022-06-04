# The monic machine is a monoid of sorts, capable of selective injections of
# type objects into typecheck lists (e.g. the MRO, the role typecheck list) via
# composable summonings (flat, role-ish) and beckonings (hierarchical,
# MRO-ish). The typecheck list may be a monic machine itself; in fact, this
# should carry a powerful enough API to handle list of type objects in the MOP
# in general. On the other hand, this shouldn't be terribly complicated; the
# MRO, the RTL, even buffers of type objects involved in a type object's
# composition are more similar than they are different.
class Perl6::Metamodel::MonicMachine is repr('VMArray') {
    # The machine is a ring buffer itself.
    method new() {
        nqp::create(self)
    }

    # Appends an object to the buffer.
    method accept($member) {
        nqp::push(self, $member);
        self
    }

    # Appends more members to this buffer one by one.
    method veneer(@members) {
        nqp::splice(self, @members, nqp::elems(self), 0)
    }

    # Makes this a copy of a more HLLish slurpy list.
    method embody(*@members) {
        nqp::splice(self, @members, 0, nqp::elems(self))
    }

    # Appends more members to this buffer as a group.
    method emboss(*@members) {
        nqp::push(self, nqp::splice(nqp::create(self), @members, 0, 0));
        self
    }

    # Iterates a buffer, appending mapped values along the way.
    method summon($evoke) {
        if nqp::elems(self) -> uint $cursor {
            repeat { $evoke(self, nqp::shift(self)) } while --$cursor;
        }
        self
    }

    # Performs a summoning, moving the result to another buffer.
    method banish($evoke, @keep) {
        if nqp::elems(self) -> uint $cursor {
            repeat { $evoke(self, nqp::shift(self)) } while --$cursor;
            nqp::splice(@keep, self, nqp::elems(@keep), 0);
            nqp::setelems(self, 0);
        }
        @keep
    }

    # Performs a C3-ish linearization on a machine containing a merge list. As
    # opposed to the C3 merge routine, a beckoning need not be appended with a
    # list of parents included by prior lists as their head, since the order
    # they appear is maintained.
    method beckon(@keep) {
        my @safe;
        my uint $cursor;
        while nqp::elems(self) -> uint $n {
            repeat {
                my @members := self[$cursor];
                next unless nqp::elems(@members);

                my $member := @members[0];
                my uint $i;
                repeat {
                    my @blocks := self[$i];
                    next if @blocks =:= @members;
                    next unless my uint $b := nqp::elems(@blocks);
                    my uint $j;
                    last if @blocks[$j] =:= $member while ++$j < $b;
                    last if $j < $b;
                } while ++$i < $n;
                last if $i == $n;
            } while ++$cursor < $n;
            last if $cursor == $n;

            nqp::push(@safe, my $member := self[$cursor][0]);
            $cursor := nqp::elems(self);
            repeat {
                my @members := nqp::pop(self);
                next unless nqp::elems(@members);
                nqp::shift(@members) if @members[0] =:= $member;
                nqp::unshift(self, @members) if nqp::elems(@members);
            } while --$cursor;
        }
        if $cursor && @safe {
            nqp::die("Could not build C3 linearization: ambiguous hierarchy");
        }
        nqp::splice(@keep, @safe, nqp::elems(@keep), 0)
    }

    # Erases members of a block list from a buffer. There are a couple special
    # values that can be given as the block list: passing the source buffer
    # eliminates duplicate members weighted towards their final appearance,
    # whereas passing the target buffer will also eliminate duplicate members,
    # but weighted towards their first appearance instead.
    method efface(@drop, @keep) {
        if nqp::elems(self) -> uint $cursor {
            repeat {
                my $member := nqp::shift(self);
                last if my int $guard := $_ =:= $member for @drop;
                nqp::push(@keep, $member) unless $guard;
            } while --$cursor;
        }
        @keep
    }

    method list() {
        nqp::splice(nqp::list(), self, 0, 0)
    }

    # This will break tests that want silent output, but is here because it
    # helps to keep an eye on MRO and role typecheck lists during builds to
    # catch errors ASAP. Noteworthy types include Buf, Blob, PseudoStash, and
    # Callable mixins.
#   method trace(@members) {
#       if @members {
#           nqp::say('.+*+&+*+.');
#           nqp::say($_.HOW.name($_)) for @members;
#           nqp::say('*+.,?,.+*');
#       }
#       else {
#           nqp::say('  .+!+.');
#           nqp::say('  *,.,*');
#       }
#       @members
#   }
}

my $monic_machine := Perl6::Metamodel::MonicMachine;
