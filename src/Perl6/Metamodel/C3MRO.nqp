role Perl6::Metamodel::C3MRO {
    # Storage of the MRO.
    has $!mro;

    # Computes C3 MRO.
    method compute_mro($class) {
        my $mro := nqp::hash(
          'all', nqp::hash(
            'no_roles', nqp::list,    # MRO with roles excluded
            'all', nqp::list,         # MRO with roles as parametric groups
            'all_conc', nqp::list,    # MRO with roles as concretizations
          ),
          'unhidden', nqp::hash(
            'no_roles', nqp::list,
            'all', nqp::list,
            'all_conc', nqp::list,    # MRO with roles as concretizations
          ),
        );
        my @immediate_parents := $class.HOW.parents($class, :local);
        my @immediate_roles;

        @immediate_roles := $class.HOW.concretizations(
          $class, :local, :transitive
        ) if nqp::can($class.HOW, 'concretizations');

        # Provided we have immediate parents...
        my @all;        # MRO with classes and roles as groups
        my @all_conc;   # MRO with classes and roles as concretizations
        my @no_roles;   # MRO with classes only

        if nqp::elems(@immediate_parents) {
            if nqp::elems(@immediate_parents) == 1
              && nqp::elems(@immediate_roles) == 0 {
                my $parent := nqp::atpos(@immediate_parents,0);
                @all_conc := nqp::clone(
                  nqp::istype($parent.HOW, Perl6::Metamodel::C3MRO)
                    ?? $parent.HOW.mro($parent, :concretizations)
                    !! $parent.HOW.mro($parent)
                );
            }
            else {
                # Build merge list of linearizations of all our parents, add
                # immediate parents and merge.
                my @merge_list;
                @merge_list.push(@immediate_roles);
                my int $elems := nqp::elems(@immediate_parents);
                my int $i;
                while $i < $elems {
                    my $parent := nqp::atpos(@immediate_parents, $i);
                    my $how := $parent.HOW;
                    @merge_list.push(
                      nqp::istype($how, Perl6::Metamodel::C3MRO)
                        ?? $how.mro($parent, :concretizations)
                        !! $how.mro($parent)
                    );
                    ++$i;
                }
                @merge_list.push(@immediate_parents);
                @all_conc := self.c3_merge(@merge_list);
            }
        }

        # Put this class on the start of the list, and we're done.
        @all_conc.unshift($class);

        my int $elems := nqp::elems(@all_conc);
        my int $i;
        while $i < $elems {
            my $c := nqp::atpos(@all_conc, $i);
            my $how := $c.HOW;
            if $how.archetypes.inheritable
              # I.e. classes or natives
              || nqp::istype($how, Perl6::Metamodel::NativeHOW) {
                nqp::push(@no_roles, $c);
                nqp::push(@all, $c);
            }

            # For concretizations fetch their respective parametric groups
            elsif nqp::istype($how, Perl6::Metamodel::ConcreteRoleHOW) {
                my $parametric := $how.roles($c, :!transitive)[0];
                nqp::push(@all, $parametric.HOW.group($parametric));
            }
            else {
                nqp::push(@all, $c);
            }
            ++$i;
        }

        # Also compute the unhidden MRO (all the things in the MRO that
        # are not somehow hidden).
        my @unhidden_all_conc;
        my @unhidden_all;
        my @unhidden_no_roles;
        my %hidden;
        my int $skip_hidden_roles;

        $i := 0;
        while $i < nqp::elems(@all_conc) {
            my $c   := nqp::atpos(@all_conc,$i);
            my $how := $c.HOW;
            my $is_inheritable := $how.archetypes.inheritable;

            if $skip_hidden_roles && !$is_inheritable {
                ++$i;
                next;
            }

            $skip_hidden_roles := 0;
            if nqp::existskey(%hidden, nqp::objectid(nqp::decont($c)))
              || (nqp::can($how, 'hidden') && $how.hidden($c)) {
                $skip_hidden_roles := 1;
            }

            else {
                nqp::push(@unhidden_all_conc, $c);
                nqp::push(@unhidden_all, nqp::atpos(@all, $i));
                nqp::push(@unhidden_no_roles, $c)
                  if $is_inheritable
                  || nqp::istype($how, Perl6::Metamodel::NativeHOW);
            }

            if nqp::can($how, 'hides') {
                my int $elems := nqp::elems(my @hides := $how.hides($c));
                my int $j;
                while $j < $elems {
                    nqp::bindkey(
                      %hidden,
                      nqp::objectid(nqp::decont(nqp::atpos(@hides, $j))),
                      1
                    );
                    ++$j;
                }
            }
            ++$i;
        }

        self.protect({
            $!mro := nqp::hash(
              'all', nqp::hash(
                'all',      @all,
                'all_conc', @all_conc,
                'no_roles', @no_roles,
              ),
              'unhidden', nqp::hash(
                'all',      @unhidden_all,
                'all_conc', @unhidden_all_conc,
                'no_roles', @unhidden_no_roles,
              ),
            )
        })
    }

    # C3 merge routine
    method c3_merge(@merge_list) {
        my @result;
        my $accepted;
        my int $something_accepted;
        my int $cand_count;

        # Try to find something appropriate to add to the MRO.
        my int $m := nqp::elems(@merge_list);
        my int $i;
        while $i < $m {
            my @cand_list := nqp::atpos(@merge_list, $i);
            if nqp::elems(@cand_list) {
                my int $rejected;
                my $cand_class := nqp::atpos(@cand_list,0);
                ++$cand_count;

                # Go over it again
                my int $j;
                while $j < $m {
                    my $c := nqp::atpos(@merge_list, $j);

                    # Skip current list.
                    unless nqp::isnull($c) || nqp::eqaddr($c,@cand_list) {

                        # Is current candidate in the tail? If so, reject.
                        my int $elems   := nqp::elems($c);
                        my int $cur_pos := 1;
                        while $cur_pos < $elems {
                            $rejected := 1
                              if nqp::eqaddr(
                                nqp::decont(nqp::atpos($c,$cur_pos)),
                                nqp::decont($cand_class)
                              );

                            ++$cur_pos;
                        }
                    }
                    ++$j
                }
                # If we didn't reject it, this candidate will do.
                unless $rejected {
                    $accepted := $cand_class;
                    $something_accepted := 1;
                    last;
                }
            }
            ++$i;
        }

        # If we never found any candidates, return an empty list.
        return @result if $cand_count == 0;

        # If we didn't find anything to accept, error.
        nqp::die("Could not build C3 linearization: ambiguous hierarchy")
          unless $something_accepted;

        # Otherwise, remove what was accepted from the merge lists.
        $m := nqp::elems(@merge_list);
        $i := 0;
        while $i < $m {
            my @new_list;

            my @list := nqp::atpos(@merge_list, $i);
            my int $n := nqp::elems(@list);
            my int $j;
            while $j < $n {
                my $c := nqp::atpos(@list, $j);
                @new_list.push($c)
                  unless nqp::eqaddr(
                    nqp::decont($c), nqp::decont($accepted)
                  );
                ++$j;
            }
            nqp::bindpos(@merge_list, $i, @new_list);
            ++$i;
        }

        # Need to merge what remains of the list, then put what was accepted
        # on the start of the list, and we're done
        @result := self.c3_merge(@merge_list);
        @result.unshift($accepted);
        @result
    }

    # Introspects the Method Resolution Order.
    method mro($target, :$roles, :$concretizations, :$unhidden) {
        # Make sure we get a snapshot of MRO hash without competing
        # with compute_mro working in another thread.  It should be
        # safe to pull in just $!mro without cloning it because the
        # hash itself remains immutable, it's only the $!mro attribute
        # that gets updated with new object.
        my $mro := $!mro;

        # Compute the MRO if there is none yet (???)
        $mro := self.compute_mro($target) if nqp::eqaddr($mro,NQPMu);

        nqp::atkey(
          nqp::atkey($mro, $unhidden ?? 'unhidden' !! 'all'),
          $concretizations
            ?? 'all_conc'
            !! $roles
              ?? 'all'
              !! 'no_roles'
        );
    }

    method invalidate_mro_cache($target) {
        $!mro := NQPMu;
    }

    # Introspects the Method Resolution Order without anything that has
    # been hidden.
    method mro_unhidden($target, :$roles, :$concretizations) {
        self.mro($target, :$roles, :$concretizations, :unhidden)
    }
}

# vim: expandtab sw=4
