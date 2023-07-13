role Perl6::Metamodel::C3MRO {
    # Storage of the MRO.
    has %!mro;

    # Computes C3 MRO.
    method compute_mro($class) {
        %!mro := nqp::hash(
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
                @all_conc := Perl6::Metamodel::Configuration.c3_merge(@merge_list);
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
        );
    }

    # Introspects the Method Resolution Order.
    method mro($obj, :$roles = 0, :$concretizations = 0, :$unhidden = 0) {
        unless nqp::existskey(%!mro, 'all') {
            self.compute_mro($obj);
        }
        my $all_key := $concretizations ?? 'all_conc' !! 'all';
        nqp::atkey(
            nqp::atkey(%!mro, $unhidden ?? 'unhidden' !! 'all'),
            $concretizations ?? 'all_conc' !! ($roles ?? 'all' !! 'no_roles')
        );
    }

    # Introspects the Method Resolution Order without anything that has
    # been hidden.
    method mro_unhidden($obj, :$roles = 0, :$concretizations = 0) {
        self.mro($obj, :$roles, :$concretizations, :unhidden)
    }

    method mro_hash() {
        %!mro
    }
}

# vim: expandtab sw=4
