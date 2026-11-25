#- Metamodel::Concretization ---------------------------------------------------
# Support for mapping of non-specialized roles into their concretized state.
role Perl6::Metamodel::Concretization {
    has @!concretizations;
    has %!conc_table;

    method add_concretization($XXX, $role, $concrete) {
        self.protect({
            my @concretizations := nqp::clone(@!concretizations);
            nqp::push(@concretizations, nqp::list($role, $concrete));
            @!concretizations := @concretizations;
        });
    }

    method concretizations($target, :$local, :$transitive = 1) {
        my @concretizations := @!concretizations;
        my int $m := nqp::elems(@concretizations);
        my @conc;

        if $transitive {
            my int $i;
            while $i < $m {
                my $concretization := nqp::atpos(
                  nqp::atpos(@concretizations, $i), 1
                );
                if nqp::can($concretization.HOW, 'concretizations') {
                    my @c := nqp::clone($concretization.HOW.concretizations(
                      $concretization, :$local
                    ));
                    nqp::unshift(@c, $concretization);
                    nqp::push(@conc, @c);
                }
                else {
                    nqp::push(@conc, nqp::list($concretization));
                }
                ++$i;
            }
            @conc := self.c3_merge(@conc);
        }

        # Not transitive, simply collect
        else {
            my int $i;
            while $i < $m {
                nqp::push(@conc,nqp::atpos(nqp::atpos(@concretizations,$i), 1));
                ++$i;
            }
        }

        unless $local {
            my @parents := self.parents($target, :local);

            my int $m := nqp::elems(@parents);
            my int $i;
            while $i < $m {
                my $parent := nqp::atpos(@parents, $i);
                nqp::splice(
                  @conc,
                  $parent.HOW.concretizations($parent, :$local, :$transitive),
                  nqp::elems(@conc),
                  0
                ) if nqp::can($parent.HOW, 'concretizations');
                ++$i;
            }
        }

        @conc
    }

    method !maybe_rebuild_table() {
        # Capturing the concretization list is first and foremost because we
        # depend on its size to know whether or not a rebuild is necessary, but
        # there may be a wait before then. Try for more predictable output.
        my @concretizations := @!concretizations;
        my int $captured    := nqp::elems(@concretizations);

        self.protect({
            # The concretization table can be depended on outside a
            # thread-safe context, e.g. MRO-based method dispatch. Parsing
            # a grammar from a start block can lead to a concurrent access
            # and modification, for instance.
            my %conc_table := nqp::clone(%!conc_table);
            my int $cached := nqp::elems(%conc_table);

            if $cached < $captured {
                %conc_table := nqp::clone(%conc_table);
                repeat { # Update.
                    my @c := nqp::atpos(@concretizations, $cached);
                    nqp::bindkey(
                      %conc_table,
                      ~nqp::objectid(nqp::decont(nqp::atpos(@c, 0))),
                      nqp::decont(nqp::atpos(@c, 1))
                    );
                } while ++$cached < $captured;

                nqp::scwbdisable;
                %!conc_table := %conc_table;
                nqp::scwbenable;
            }

            %conc_table
        })
    }

    my $no_roles := nqp::list(0);

    # Returns a list where the first element is the number of roles found
    # and the rest are actual type objects.
    method concretization_lookup(
      $target, $ptype, :$local, :$transitive = 1, :$relaxed
    ) {
        my %working_conc_table := self.'!maybe_rebuild_table'();
        return $no_roles
          unless nqp::not_i($local)
            || $transitive
            || nqp::elems(%working_conc_table);

        $ptype     := nqp::decont($ptype);
        my str $id := ~nqp::objectid($ptype);
        return nqp::list(1, nqp::atkey(%working_conc_table, $id))
          if nqp::existskey(%working_conc_table, $id);

        if $relaxed {
            my @concretizations := @!concretizations;

            # Try search by role group for curryings. The first match is ok.
            # Used by FQN method calls.
            my @result;
            my int $m := nqp::elems(@concretizations);
            my int $i;
            while $i < $m {
                my $type := nqp::atpos(nqp::atpos(@concretizations, $i), 0);
                if $type.HOW.archetypes.parametric {
                    $type := $type.HOW.curried_role($type)
                      if nqp::can($type.HOW, 'curried_role');
                    nqp::push(
                      @result,
                      nqp::atpos(nqp::atpos(@concretizations, $i), 1)
                    ) if nqp::eqaddr($type, $ptype);
                }
                ++$i;
            }

            if nqp::elems(@result) {
                nqp::unshift(@result, nqp::elems(@result));
                return @result;
            }
        }

        return $no_roles
          if nqp::not_i($relaxed)
          && self.is_composed($target)
          && nqp::not_i(nqp::istype($target, $ptype));

        if $transitive {
            my @concretizations := @!concretizations;

            my int $m := nqp::elems(@concretizations);
            my int $i;
            while $i < $m {
                my $concretization :=
                  nqp::atpos(nqp::atpos(@concretizations, $i), 1);
                if nqp::istype($concretization, $ptype) {
                    my @result := $concretization.HOW.concretization_lookup(
                      $concretization, $ptype, :$local, :transitive, :$relaxed
                    );
                    return @result if nqp::atpos(@result, 0);
                }
                ++$i;
            }
        }

        unless $local {
            my @parents := self.parents($target, :local);

            my int $m := nqp::elems(@parents);
            my int $i;
            while $i < $m {
                my $parent := nqp::atpos(@parents, $i);
                my @result := $parent.HOW.concretization_lookup(
                  $parent, $ptype, :$transitive, :$relaxed
                );
                nqp::atpos(@result, 0)
                  ?? (return @result)
                  !! ++$i;
            }
        }

        $no_roles
    }

    method concretization(
      $target, $ptype, :$local, :$transitive = 1, :$relaxed
    ) {
        my @result := self.concretization_lookup(
          $target, $ptype, :$local, :$transitive, :$relaxed
        );

        (my $nr_roles := nqp::atpos(@result, 0)) == 1
          ?? nqp::atpos(@result, 1)
          !! nqp::die(($nr_roles
               ?? "Ambiguous concretization lookup for "
               !! "No concretization found for "
             ) ~ $ptype.HOW.name($ptype))
    }
}

# vim: expandtab sw=4
