augment class Seq {
    multi method ACCEPTS(@topic) {
        my $self_it = self.iterator();
        my $topic_it = @topic.iterator();
        loop {
            my $cur_self_elem = $self_it.get;
            if $cur_self_elem ~~ EMPTY { last }
            if $cur_self_elem ~~ Whatever {
                # If we just have * left, we're done. Otherwise, we have a
                # "target" to look for.
                loop {
                    $cur_self_elem = $self_it.get;
                    if $cur_self_elem ~~ EMPTY { return True }
                    unless $cur_self_elem ~~ Whatever {
                        last;
                    }
                }

                # Need to find our target in the topic, if possible.
                loop {
                    my $cur_topic_elem = $topic_it.get;
                    if $cur_topic_elem ~~ EMPTY {
                        # Ran out before finding what we wanted.
                        return False;
                    }
                    elsif $cur_topic_elem === $cur_self_elem {
                        last;
                    }
                }
            }
            else {
                my $cur_topic_elem = $topic_it.get;
                if $cur_topic_elem ~~ EMPTY || $cur_topic_elem !=== $cur_self_elem {
                    return False;
                }
            }
        }

        # If we've nothing left to match, we're successful.
        $topic_it.get ~~ EMPTY
    }

    multi method ACCEPTS($topic) {
        self.ACCEPTS(@($topic))
    }

    method elems() { pir::set__IP(self!fill); }

    method Str() {
        pir::join(' ', self!fill);
    }

    multi method Bool() {
        self!fill(1) ?? Bool::True !! Bool::False;
    }

    multi method sort(&by = &infix:<cmp>) {
        # Parrot already provides a sort method that works on
        # ResizablePMCArray, so we aim to make use of that here.
        # Instead of sorting the elements directly, we sort an RPA
        # of indices (from 0 to $list.elems), then use that RPA
        # as a slice into self.

        my $index_PARROT_RPA = pir::new__PS("ResizablePMCArray");
        pir::push__vPP($index_PARROT_RPA, $_) for ^self.elems;

        # If &by.arity < 2, then it represents a block to be applied
        # to the elements to obtain the values for sorting.
        if (&by.?arity // 2) < 2 {
            my $list = self.map(&by).eager;
            self[$index_PARROT_RPA.sort(
                -> $a, $b { $list[$a] cmp $list[$b] || $a <=> $b }
            )];
        }
        else {
            my $list = self.eager;
            self[$index_PARROT_RPA.sort(
                -> $a, $b { &by($list[$a],$list[$b]) || $a <=> $b }
            )];
        }
    }

    multi method rotate(Int $n = 1) is export {
        my Int $k = $n % self.elems;
        self[$k .. self.elems-1, 0 .. $k-1];
    }

    multi method fmt($format = '%s', $seperator = ' ') {
        self.map({ .fmt($format)}).join($seperator);
    }

    method perl() {
        '(' ~ self.map({ $^a.perl }).join(', ') ~ ')';
    }
}

multi sub sort (@x, :&by = &infix:<cmp>) { @x.sort(&by) }
multi sub sort (&by, @x) { @x.sort(&by) }

# vim: ft=perl6
