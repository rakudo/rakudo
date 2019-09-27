role Perl6::Metamodel::C3MRO {
    # Storage of the MRO.
    has %!mro;
    # has @!mro;
    has $!nest;

    # The MRO minus anything that is hidden.
    # has @!mro_unhidden;

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
        nqp::say("+ compute_mro of " ~ $class.HOW.name($class) ~ " // " ~ $class.HOW.HOW.name($class.HOW) ~ " which is " ~ ($class.HOW.is_composed($class) ?? "" !! "not ") ~ "composed") if nqp::getenvhash<RAKUDO_DEBUG>;
        my @immediate_parents := $class.HOW.parents($class, :local);
        my @immediate_roles;

        if nqp::can($class.HOW, 'roles') {
            nqp::say("asking for roles") if nqp::getenvhash<RAKUDO_DEBUG>;
            for $class.HOW.roles($class, :local, :transitive) {
                nqp::say("got role " ~ $_.HOW.name($_) ~ " of " ~ $_.HOW.HOW.name($_.HOW) ~ " can concretization_lookup? " ~ nqp::can($class.HOW, 'concretization_lookup')) if nqp::getenvhash<RAKUDO_DEBUG>;
                # XXX Could be optimized with just getting all concretizations if it's guaranteed that the two lists are
                # always a match. Very likely, this is how things are.
                my @conc_res := $class.HOW.concretization_lookup($class, $_, :local, :transitive);
                nqp::say("got concretization") if nqp::getenvhash<RAKUDO_DEBUG>;
                nqp::push(@immediate_roles, @conc_res[0] ?? @conc_res[1] !! $_);
            }
        } else {
            @immediate_roles := [];
        }

        if nqp::getenvhash<RAKUDO_DEBUG> {
            nqp::say($class.HOW.name($class) ~ " parents:" ~ +@immediate_parents ~ ", roles:" ~ +@immediate_roles);
            for @immediate_roles {
                nqp::say("  . role: " ~ $_.HOW.name($_));
            }
        }

        # Provided we have immediate parents...
        my @all;        # MRO with classes and roles
        my @no_roles;   # MRO with classes only
        if +@immediate_parents {
            if (+@immediate_parents == 1) && (+@immediate_roles == 0) {
                nqp::say("GETTING ALL MRO OF " ~ @immediate_parents[0].HOW.name(@immediate_parents[0])) if nqp::getenvhash<RAKUDO_DEBUG>;
                @all := nqp::clone(
                            nqp::istype(@immediate_parents[0].HOW, Perl6::Metamodel::C3MRO)
                            ?? @immediate_parents[0].HOW.mro(@immediate_parents[0], :roles)
                            !! @immediate_parents[0].HOW.mro(@immediate_parents[0])
                        );
            } else {
                # Build merge list of linearizations of all our parents, add
                # immediate parents and merge.
                my @merge_list;
                @merge_list.push(@immediate_roles);
                for @immediate_parents {
                    nqp::say("GETTING MRO FOR MERGE LIST FROM " ~ $_.HOW.name($_) ~ ", does HOW C3MRO? " ~ nqp::istype($_.HOW, self)) if nqp::getenvhash<RAKUDO_DEBUG>;
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
            # nqp::print(" -> " ~ $_.HOW.name($_)) if nqp::getenvhash<RAKUDO_DEBUG>;
            if $_.HOW.archetypes.inheritable || nqp::istype($_.HOW, Perl6::Metamodel::NativeHOW) { # i.e. classes or natives
                # nqp::print("+") if nqp::getenvhash<RAKUDO_DEBUG>;
                nqp::push(@no_roles, $_);
            }
        }

        # Also compute the unhidden MRO (all the things in the MRO that
        # are not somehow hidden).
        my @unhidden_all;
        my @unhidden_no_roles;
        my @hidden;
        for @all -> $c {
            unless nqp::can($c.HOW, 'hidden') && $c.HOW.hidden($c) {
                my $is_hidden := 0;
                for @hidden {
                    if nqp::decont($c) =:= nqp::decont($_) {
                        $is_hidden := 1;
                    }
                }
                unless $is_hidden {
                    nqp::push(@unhidden_all, $c);
                    nqp::push(@unhidden_no_roles, $c) if $c.HOW.archetypes.inheritable || nqp::istype($_.HOW, Perl6::Metamodel::NativeHOW);
                }
            }
            if nqp::can($c.HOW, 'hides') {
                for $c.HOW.hides($c) {
                    nqp::push(@hidden, $_);
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
    method mro($obj, :$roles = 0, :$unhidden = 0) {
        # nqp::say("mro(" ~ $obj.HOW.name($obj) ~ ")") if nqp::getenvhash<RAKUDO_DEBUG>;
        unless nqp::existskey(%!mro, 'all') {
            self.compute_mro($obj);
        }

        nqp::atkey(
            nqp::atkey(%!mro, $unhidden ?? 'unhidden' !! 'all'),
            $roles ?? 'all' !! 'no_roles'
        )
        # my @result := @!mro;
        # if +@result {
        #     @result
        # }
        # else {
        #     # Never computed before; do it best we can so far (and it will
        #     # be finalized at compose time).
        #     self.compute_mro($obj, :$roles)
        # }
    }

    # Introspects the Method Resolution Order without anything that has
    # been hidden.
    method mro_unhidden($obj, :$roles = 0) {
        # my @result := @!mro_unhidden;
        # if +@result {
        #     @result
        # }
        # else {
        #     # Never computed before; do it best we can so far (and it will
        #     # be finalized at compose time).
        #     self.compute_mro($obj, :$roles);
        #     @!mro_unhidden
        # }
        self.mro($obj, :$roles, :unhidden)
    }

    method mro_hash() {
        %!mro
    }
}
