# Batches values sourced from an iterator, producing a work batch from them.
my role Rakudo::Internals::HyperIteratorBatcher does Rakudo::Internals::HyperBatcher {
    my constant NO_LOOKAHEAD = Mu.CREATE;
    has Iterator $!iterator;
    has $!lookahead;
   
    submethod BUILD(Iterator :$iterator!) {
        $!iterator := $iterator;
        $!lookahead := NO_LOOKAHEAD;
    }
    
    method produce-batch(int $batch-size --> Rakudo::Internals::HyperWorkBatch) {
        my IterationBuffer $items .= new;
        my Bool $first;
        my Bool $last;
        if $!lookahead =:= NO_LOOKAHEAD {
            $first = True;
            if $!iterator.push-exactly($items, $batch-size) =:= IterationEnd {
                $last = True;
            }
            else {
                $!lookahead := $!iterator.pull-one;
                $last = True if $!lookahead =:= IterationEnd;
            }
        }
        else {
            $first = False;
            $items.push($!lookahead);
            if $!iterator.push-exactly($items, $batch-size - 1) =:= IterationEnd {
                $last = True;
            }
            else {
                $!lookahead := $!iterator.pull-one;
                $last = True if $!lookahead =:= IterationEnd;
            }
        }
        my $sequence-number = self.next-sequence-number();
        return Rakudo::Internals::HyperWorkBatch.new(:$sequence-number, :$items, :$first, :$last);
    }
}

# vim: ft=perl6 expandtab sw=4
