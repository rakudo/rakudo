# Support for mapping of non-specialized roles into their concretized state.
role Perl6::Metamodel::Concretization {
    has @!concretizations;
    has %!conc_table;

    method add_concretization($obj, $role, $concrete) {
        @!concretizations[+@!concretizations] := [$role, $concrete];
        %!conc_table := nqp::hash(); # reset the cache
    }

    method concretizations($obj, :$local = 0, :$transitive = 1) {
        my @conc;
        for @!concretizations {
            nqp::push(@conc, $_[1]);
            if $transitive && nqp::can($_[1].HOW, 'concretizations') {
                for $_[1].HOW.concretizations($_[1], :$local) {
                    nqp::push(@conc, $_);
                }
            }
        }
        unless $local {
            for self.parents($obj, :local) {
                for $_.HOW.concretizations($_, :$local) {
                    nqp::push(@conc, $_)
                }
            }
        }
        @conc
    }

    method !rebuild_table() {
        for @!concretizations {
            %!conc_table{~nqp::objectid(nqp::decont($_[0]))} := nqp::decont($_[1]);
        }
    }

    method concretization_lookup($obj, $ptype, :$local = 0, :$transitive = 1, :$relaxed = 0) {
        $ptype := nqp::decont($ptype);
        my $id := ~nqp::objectid($ptype);
        self.'!rebuild_table'() if nqp::elems(%!conc_table) < nqp::elems(@!concretizations);
        return [0] unless $local || $transitive || nqp::elems(%!conc_table);
        my @result;
        if nqp::existskey(%!conc_table, $id) {
            return [1, %!conc_table{$id}];
        }
        if $relaxed {
            # Try search by role group for curryings. The first match is ok. Used by FQN method calls.
            for @!concretizations {
                next unless $_[0].HOW ~~ Perl6::Metamodel::CurriedRoleHOW;
                if nqp::decont($_[0].HOW.curried_role($_[0])) =:= $ptype {
                    return [1, $_[1]];
                }
            }
        }
        if $transitive {
            for @!concretizations {
                if nqp::istype($_[1], $ptype) {
                    @result := $_[1].HOW.concretization_lookup($_[1], $ptype, :$local, :transitive(1));
                    return @result if @result[0];
                }
            }
        }
        unless $local {
            for self.parents($obj, :local) {
                @result := $_.HOW.concretization_lookup($_, $ptype, :local(0), :$transitive);
                return @result if @result[0];
            }
        }
        [0]
    }

    method concretization($obj, $ptype, :$local = 0, :$transitive = 1, :$relaxed = 0) {
        my @result := self.concretization_lookup($obj, $ptype, :$local, :$transitive, :$relaxed);
        nqp::die("No concretization found for " ~ $ptype.HOW.name($ptype)) unless @result[0];
        @result[1]
    }
}
