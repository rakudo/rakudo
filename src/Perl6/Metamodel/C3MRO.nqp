role Perl6::Metamodel::C3MRO {
    # Storage of the MRO.
    has @!mro;

    # The MRO minus anything that is hidden.
    has @!mro_unhidden;

    # Computes C3 MRO.
    method compute_mro($class) {
        my @immediate_parents := $class.HOW.parents($class, :local);

        # Provided we have immediate parents...
        my @result;
        if +@immediate_parents {
            if +@immediate_parents == 1 {
                @result := nqp::clone(@immediate_parents[0].HOW.mro(@immediate_parents[0]));
            } else {
                # Build merge list of linearizations of all our parents, add
                # immediate parents and merge.
                my @merge_list;
                for @immediate_parents {
                    @merge_list.push($_.HOW.mro($_));
                }
                @merge_list.push(@immediate_parents);
                @result := self.c3_merge(@merge_list);
            }
        }

        # Put this class on the start of the list, and we're done.
        @result.unshift($class);
        @!mro := @result;

        # Also compute the unhidden MRO (all the things in the MRO that
        # are not somehow hidden).
        my @unhidden;
        my @hidden;
        for @result -> $c {
            unless nqp::can($c.HOW, 'hidden') && $c.HOW.hidden($c) {
                my $is_hidden := 0;
                for @hidden {
                    if nqp::decont($c) =:= nqp::decont($_) {
                        $is_hidden := 1;
                    }
                }
                nqp::push(@unhidden, $c) unless $is_hidden;
            }
            if nqp::can($c.HOW, 'hides') {
                for $c.HOW.hides($c) {
                    nqp::push(@hidden, $_);
                }
            }
        }
        @!mro_unhidden := @unhidden;

        @!mro
    }

    # C3 merge routine.
    method c3_merge(@merge_list) {
        my @result;
        my $accepted;
        my $something_accepted := 0;
        my $cand_count := 0;

        # Try to find something appropriate to add to the MRO.
        for @merge_list {
            my @cand_list := $_;
            if +@cand_list {
                my $rejected := 0;
                my $cand_class := @cand_list[0];
                $cand_count := $cand_count + 1;
                for @merge_list {
                    # Skip current list.
                    unless $_ =:= @cand_list {
                        # Is current candidate in the tail? If so, reject.
                        my $cur_pos := 1;
                        while $cur_pos <= +$_ {
                            if nqp::decont($_[$cur_pos]) =:= nqp::decont($cand_class) {
                                $rejected := 1;
                            }
                            $cur_pos := $cur_pos + 1;
                        }
                    }

                }
                # If we didn't reject it, this candidate will do.
                unless $rejected {
                    $accepted := $cand_class;
                    $something_accepted := 1;
                    last;
                }
            }
        }

        # If we never found any candidates, return an empty list.
        if $cand_count == 0 {
            return @result;
        }

        # If we didn't find anything to accept, error.
        unless $something_accepted {
            nqp::die("Could not build C3 linearization: ambiguous hierarchy");
        }

        # Otherwise, remove what was accepted from the merge lists.
        my $i := 0;
        while $i < +@merge_list {
            my @new_list;
            for @merge_list[$i] {
                unless nqp::decont($_) =:= nqp::decont($accepted) {
                    @new_list.push($_);
                }
            }
            @merge_list[$i] := @new_list;
            $i := $i + 1;
        }

        # Need to merge what remains of the list, then put what was accepted on
        # the start of the list, and we're done.
        @result := self.c3_merge(@merge_list);
        @result.unshift($accepted);
        return @result;
    }

    # Introspects the Method Resolution Order.
    method mro($obj) {
        my @result := @!mro;
        if +@result {
            @result
        }
        else {
            # Never computed before; do it best we can so far (and it will
            # be finalized at compose time).
            self.compute_mro($obj)
        }
    }

    # Introspects the Method Resolution Order without anything that has
    # been hidden.
    method mro_unhidden($obj) {
        my @result := @!mro_unhidden;
        if +@result {
            @result
        }
        else {
            # Never computed before; do it best we can so far (and it will
            # be finalized at compose time).
            self.compute_mro($obj);
            @!mro_unhidden
        }
    }
}
