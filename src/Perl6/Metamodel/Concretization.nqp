# Support for mapping of non-specialized roles into their concretized state.
role Perl6::Metamodel::Concretization {
    has @!concretizations;
    has %!conc_table;

    method add_concretization($obj, $role, $concrete) {
        @!concretizations[+@!concretizations] := [$role, $concrete];
        nqp::scwbdisable();
        %!conc_table := nqp::hash(); # reset the cache
        nqp::scwbenable();
    }

    method concretizations($obj, :$local = 0, :$transitive = 1) {
        my @conc;
        for @!concretizations {
            my @c := $transitive ?? [] !! @conc;
            nqp::push(@c, $_[1]);
            if $transitive && nqp::can($_[1].HOW, 'concretizations') {
                for $_[1].HOW.concretizations($_[1], :$local) {
                    nqp::push(@c, $_);
                }
            }
            nqp::push(@conc, @c) if $transitive;
        }
        @conc := self.c3_merge(@conc) if $transitive;
        unless $local {
            for self.parents($obj, :local) {
                if nqp::can($_.HOW, 'concretizations') {
                    for $_.HOW.concretizations($_, :$local, :$transitive) {
                        nqp::push(@conc, $_)
                    }
                }
            }
        }
        @conc
    }

    method !rebuild_table() {
        for @!concretizations {
            nqp::scwbdisable();
            %!conc_table{~nqp::objectid(nqp::decont($_[0]))} := nqp::decont($_[1]);
            nqp::scwbenable();
        }
    }

    # Returns a list where the first element is the number of roles found and the rest are actual type objects.
    method concretization_lookup($obj, $ptype, :$local = 0, :$transitive = 1, :$relaxed = 0) {
        self.'!rebuild_table'() if nqp::elems(%!conc_table) < nqp::elems(@!concretizations);
        return [0] unless !$local || $transitive || nqp::elems(%!conc_table);
        $ptype := nqp::decont($ptype);
        my $id := ~nqp::objectid($ptype);
        my @result;
        if nqp::existskey(%!conc_table, $id) {
            return [1, %!conc_table{$id}];
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
        return [0] if !$relaxed && $obj.HOW.is_composed($obj) && !nqp::istype(nqp::decont($obj), $ptype);
        if $transitive {
            for @!concretizations {
                if nqp::istype($_[1], $ptype) {
                    @result := $_[1].HOW.concretization_lookup($_[1], $ptype, :$local, :transitive, :$relaxed);
                    return @result if @result[0];
                }
            }
        }
        unless $local {
            for self.parents($obj, :local) {
                @result := $_.HOW.concretization_lookup($_, $ptype, :local(0), :$transitive, :$relaxed);
                return @result if @result[0];
            }
        }
        [0]
    }

    method concretization($obj, $ptype, :$local = 0, :$transitive = 1, :$relaxed = 0) {
        my @result := self.concretization_lookup($obj, $ptype, :$local, :$transitive, :$relaxed);
        nqp::die("No concretization found for " ~ $ptype.HOW.name($ptype)) unless @result[0];
        nqp::die("Ambiguous concretization lookup for " ~ $ptype.HOW.name($ptype)) if @result[0] > 1;
        @result[1]
    }
}

# vim: expandtab sw=4
