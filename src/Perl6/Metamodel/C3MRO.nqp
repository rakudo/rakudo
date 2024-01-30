role Perl6::Metamodel::C3MRO {
    # Storage of the MRO.
    has %!mro;
    has $!mro_lock;

    method setup_mro_engine() {
        $!mro_lock := NQPLock.new();
    }

    # Computes C3 MRO.
    method compute_mro($class) {
        my %mro := nqp::hash(
            'all', nqp::hash(
                'no_roles', nqp::list(),    # MRO with roles excluded
                'all', nqp::list(),         # MRO with roles as parametric groups
                'all_conc', nqp::list(),    # MRO with roles as concretizations
            ),
            'unhidden', nqp::hash(
                'no_roles', nqp::list(),
                'all', nqp::list(),
                'all_conc', nqp::list(),    # MRO with roles as concretizations
            ),
        );
        my @immediate_parents := $class.HOW.parents($class, :local);
        my @immediate_roles;

        if nqp::can($class.HOW, 'concretizations') {
            @immediate_roles := $class.HOW.concretizations($class, :local, :transitive);
        }

        # Provided we have immediate parents...
        my @all;        # MRO with classes and roles as groups
        my @all_conc;   # MRO with classes and roles as concretizations
        my @no_roles;   # MRO with classes only
        if +@immediate_parents {
            if (+@immediate_parents == 1) && (+@immediate_roles == 0) {
                my $parent := @immediate_parents[0];
                @all_conc := nqp::clone(
                                nqp::istype($parent.HOW, Perl6::Metamodel::C3MRO)
                                ?? $parent.HOW.mro($parent, :concretizations)
                                !! $parent.HOW.mro($parent));
            }
            else {
                # Build merge list of linearizations of all our parents, add
                # immediate parents and merge.
                my @merge_list;
                @merge_list.push(@immediate_roles);
                for @immediate_parents {
                    @merge_list.push(
                        nqp::istype($_.HOW, Perl6::Metamodel::C3MRO) ?? $_.HOW.mro($_, :concretizations) !! $_.HOW.mro($_)
                    );
                }
                @merge_list.push(@immediate_parents);
                @all_conc := self.c3_merge(@merge_list);
            }
        }

        # Put this class on the start of the list, and we're done.
        @all_conc.unshift($class);

        for @all_conc {
            if $_.HOW.archetypes.inheritable || nqp::istype($_.HOW, Perl6::Metamodel::NativeHOW) { # I.e. classes or natives
                nqp::push(@no_roles, $_);
                nqp::push(@all, $_);
            }
            elsif nqp::istype($_.HOW, Perl6::Metamodel::ConcreteRoleHOW) { # For concretizations fetch their respective parametric groups
                my $parametric := $_.HOW.roles($_, :!transitive)[0];
                nqp::push(@all, $parametric.HOW.group($parametric));
            }
            else {
                nqp::push(@all, $_);
            }
        }

        # Also compute the unhidden MRO (all the things in the MRO that
        # are not somehow hidden).
        my @unhidden_all_conc;
        my @unhidden_all;
        my @unhidden_no_roles;
        my %hidden;
        my $skip_hidden_roles := 0;
        my $i := -1;
        while ++$i < nqp::elems(@all_conc) {
            my $c := @all_conc[$i];
            my $is_inheritable := $c.HOW.archetypes.inheritable;

            next if $skip_hidden_roles && !$is_inheritable;
            $skip_hidden_roles := 0;

            if %hidden{~nqp::objectid(nqp::decont($c))} || (nqp::can($c.HOW, 'hidden') && $c.HOW.hidden($c)) {
                $skip_hidden_roles := 1
            }
            else {
                nqp::push(@unhidden_all_conc, $c);
                nqp::push(@unhidden_all, @all[$i]);
                nqp::push(@unhidden_no_roles, $c) if $is_inheritable || nqp::istype($c.HOW, Perl6::Metamodel::NativeHOW);
            }
            if nqp::can($c.HOW, 'hides') {
                for $c.HOW.hides($c) {
                    %hidden{~nqp::objectid(nqp::decont($_))} := 1;
                }
            }
        }

        $!mro_lock.protect({
            %!mro := nqp::hash(
                'all', nqp::hash(
                    'all', @all,
                    'all_conc', @all_conc,
                    'no_roles', @no_roles,
                ),
                'unhidden', nqp::hash(
                    'all', @unhidden_all,
                    'all_conc', @unhidden_all_conc,
                    'no_roles', @unhidden_no_roles,
                ),
            )
        })
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
    method mro($obj, :$roles = 0, :$concretizations = 0, :$unhidden = 0) {
        # Make sure we get a snapshot of MRO hash without competing with compute_mro working in another thread.
        # It should be safe to pull in just %!mro without cloning it because the hash itself remains immutable, it's
        # only the %!mro attribute that gets updated with new object.
        my %mro := $!mro_lock.protect({ %!mro });
        unless nqp::existskey(%mro, 'all') {
            %mro := self.compute_mro($obj);
        }
        my $all_key := $concretizations ?? 'all_conc' !! 'all';
        nqp::atkey(
            nqp::atkey(%mro, $unhidden ?? 'unhidden' !! 'all'),
            $concretizations ?? 'all_conc' !! ($roles ?? 'all' !! 'no_roles')
        );
    }

    # Introspects the Method Resolution Order without anything that has
    # been hidden.
    method mro_unhidden($obj, :$roles = 0, :$concretizations = 0) {
        self.mro($obj, :$roles, :$concretizations, :unhidden)
    }
}

# vim: expandtab sw=4
