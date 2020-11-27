role Perl6::Metamodel::C3MRO {
    # Storage of the MRO.
    has %!mro;

    # Computes C3 MRO.
    method compute_mro($class) {
        %!mro := nqp::hash(
            'all', nqp::hash(
                'all', nqp::list(),
                'no_roles', nqp::list(),
            ),
            'unhidden', nqp::hash(
                'all', nqp::list(),
                'no_roles', nqp::list(),
            ),
        );
        my @immediate_parents := $class.HOW.parents($class, :local);
        my @immediate_roles;

        if nqp::can($class.HOW, 'concretizations') {
            @immediate_roles := $class.HOW.concretizations($class, :local, :transitive);
        }

        # Provided we have immediate parents...
        my @all;        # MRO with classes and roles
        my @no_roles;   # MRO with classes only
        if +@immediate_parents {
            if (+@immediate_parents == 1) && (+@immediate_roles == 0) {
                my $parent := @immediate_parents[0];
                @all := nqp::clone(
                            nqp::istype($parent.HOW, Perl6::Metamodel::C3MRO)
                            ?? $parent.HOW.mro($parent, :roles)
                            !! $parent.HOW.mro($parent)
                        );
            }
            else {
                # Build merge list of linearizations of all our parents, add
                # immediate parents and merge.
                my @merge_list;
                @merge_list.push(@immediate_roles);
                for @immediate_parents {
                    @merge_list.push(
                        nqp::istype($_.HOW, Perl6::Metamodel::C3MRO) ?? $_.HOW.mro($_, :roles) !! $_.HOW.mro($_)
                    );
                }
                @merge_list.push(@immediate_parents);
                @all := self.c3_merge(@merge_list);
            }
        }

        # Put this class on the start of the list, and we're done.
        @all.unshift($class);

        for @all {
            if $_.HOW.archetypes.inheritable || nqp::istype($_.HOW, Perl6::Metamodel::NativeHOW) { # i.e. classes or natives
                nqp::push(@no_roles, $_);
            }
        }

        # Also compute the unhidden MRO (all the things in the MRO that
        # are not somehow hidden).
        my @unhidden_all;
        my @unhidden_no_roles;
        my %hidden;
        my $skip_hidden_roles := 0;
        for @all -> $c {
            my $is_inheritable := $c.HOW.archetypes.inheritable;

            next if $skip_hidden_roles && !$is_inheritable;
            $skip_hidden_roles := 0;

            if %hidden{~nqp::objectid(nqp::decont($c))} || (nqp::can($c.HOW, 'hidden') && $c.HOW.hidden($c)) {
                $skip_hidden_roles := 1
            }
            else {
                nqp::push(@unhidden_all, $c);
                nqp::push(@unhidden_no_roles, $c) if $is_inheritable || nqp::istype($c.HOW, Perl6::Metamodel::NativeHOW);
            }
            if nqp::can($c.HOW, 'hides') {
                for $c.HOW.hides($c) {
                    %hidden{~nqp::objectid(nqp::decont($_))} := 1;
                }
            }
        }

        %!mro := nqp::hash(
            'all', nqp::hash(
                'all', @all,
                'no_roles', @no_roles,
            ),
            'unhidden', nqp::hash(
                'all', @unhidden_all,
                'no_roles', @unhidden_no_roles,
            ),
        );
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
            if nqp::elems(@cand_list) {
                my $rejected := 0;
                my $cand_class := @cand_list[0];
                $cand_count := $cand_count + 1;
                for @merge_list {
                    # Skip current list.
                    unless $_ =:= @cand_list {
                        # Is current candidate in the tail? If so, reject.
                        my $cur_pos := 1;
                        while $cur_pos <= nqp::elems($_) {
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
        my int $i := -1;
        while ++$i < nqp::elems(@merge_list) {
            my @new_list;
            for @merge_list[$i] {
                unless nqp::decont($_) =:= nqp::decont($accepted) {
                    @new_list.push($_);
                }
            }
            @merge_list[$i] := @new_list;
        }

        # Need to merge what remains of the list, then put what was accepted on
        # the start of the list, and we're done.
        @result := self.c3_merge(@merge_list);
        @result.unshift($accepted);
        return @result;
    }

    # Introspects the Method Resolution Order.
    method mro($obj, :$roles = 0, :$unhidden = 0) {
        unless nqp::existskey(%!mro, 'all') {
            self.compute_mro($obj);
        }
        nqp::atkey(
            nqp::atkey(%!mro, $unhidden ?? 'unhidden' !! 'all'),
            $roles ?? 'all' !! 'no_roles'
        )
    }

    # Introspects the Method Resolution Order without anything that has
    # been hidden.
    method mro_unhidden($obj, :$roles = 0) {
        self.mro($obj, :$roles, :unhidden)
    }

    method mro_hash() {
        %!mro
    }
}

# vim: expandtab sw=4
