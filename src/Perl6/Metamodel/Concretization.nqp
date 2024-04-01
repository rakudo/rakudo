#- Metamodel::Concretization ---------------------------------------------------
# Support for mapping of non-specialized roles into their concretized state.
role Perl6::Metamodel::Concretization {
    has @!concretizations;
    has %!conc_table;

    method add_concretization($XXX, $role, $concrete) {
        nqp::push(@!concretizations, nqp::list($role, $concrete))
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
        my int $captured := nqp::elems(@!concretizations);
        self.protect: {
            # The concretization table can be depended on outside a
            # thread-safe context, e.g. MRO-based method dispatch. Parsing
            # a grammar from a start block can lead to a concurrent access
            # and modification, for instance.
            my %conc_table := %!conc_table;
            my int $cached := nqp::elems(%conc_table);
            if $cached < $captured {
                %conc_table := nqp::clone(%conc_table);
                repeat { # Update.
                    my @c := @!concretizations[$cached];
                    %conc_table{~nqp::objectid(nqp::decont(@c[0]))} := nqp::decont(@c[1]);
                } while ++$cached < $captured;
                nqp::scwbdisable();
                %!conc_table := %conc_table;
                nqp::scwbenable();
            }
            %conc_table
        }
    }

    # Returns a list where the first element is the number of roles found and the rest are actual type objects.
    method concretization_lookup($target, $ptype, :$local = 0, :$transitive = 1, :$relaxed = 0) {
        my %working_conc_table := self.'!maybe_rebuild_table'();
        return [0] unless !$local || $transitive || nqp::elems(%working_conc_table);
        $ptype := nqp::decont($ptype);
        my $id := ~nqp::objectid($ptype);
        my @result;
        if nqp::existskey(%working_conc_table, $id) {
            return [1, %working_conc_table{$id}];
        }
        if $relaxed {
            # Try search by role group for curryings. The first match is ok. Used by FQN method calls.
            @result[0] := 0;
            for @!concretizations {
                next unless $_[0].HOW.archetypes.parametric;
                my $conc := nqp::can($_[0].HOW, 'curried_role') ?? $_[0].HOW.curried_role($_[0]) !! $_[0];
                if $conc =:= $ptype {
                    ++@result[0];
                    nqp::push(@result, $_[1]);
                }
            }
            return @result if @result[0];
        }
        return [0] if !$relaxed && self.is_composed($target) && nqp::not_i(nqp::istype(nqp::decont($target), $ptype));
        if $transitive {
            for @!concretizations {
                if nqp::istype($_[1], $ptype) {
                    @result := $_[1].HOW.concretization_lookup($_[1], $ptype, :$local, :transitive, :$relaxed);
                    return @result if @result[0];
                }
            }
        }
        unless $local {
            for self.parents($target, :local) {
                @result := $_.HOW.concretization_lookup($_, $ptype, :local(0), :$transitive, :$relaxed);
                return @result if @result[0];
            }
        }
        [0]
    }

    method concretization($target, $ptype, :$local = 0, :$transitive = 1, :$relaxed = 0) {
        my @result := self.concretization_lookup($target, $ptype, :$local, :$transitive, :$relaxed);
        nqp::die("No concretization found for " ~ $ptype.HOW.name($ptype)) unless @result[0];
        nqp::die("Ambiguous concretization lookup for " ~ $ptype.HOW.name($ptype)) if @result[0] > 1;
        @result[1]
    }
}

# vim: expandtab sw=4
