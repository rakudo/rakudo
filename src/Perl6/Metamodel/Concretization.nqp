# Support for mapping of non-specialized roles into their concretized state.
role Perl6::Metamodel::Concretization {
    has @!concretizations;

    method add_concretization($obj, $role, $concrete) {
        @!concretizations[+@!concretizations] := [$role, $concrete];
    }

    method concretization_lookup($obj, $ptype, :$local, :$transitive) {
        my @result;
        if nqp::istype($obj, $ptype) {
            for @!concretizations {
                if nqp::decont($_[0]) =:= nqp::decont($ptype) {
                    return [1, $_[1]];
                }
                # Do preliminary type check for concrete role to avoid extra calls.
                if $transitive && nqp::istype($_[1], $ptype) {
                    @result := $_[1].HOW.concretization_lookup($_[1], $ptype, :$local, :transitive(1));
                    return @result if @result[0];
                }
            }
            unless $local {
                for self.parents($obj, :local(1)) {
                    @result := $_.HOW.concretization_lookup($_, $ptype, :local(0), :$transitive);
                    return @result if @result[0];
                }
            }
        }
        [0]
    }

    method concretization($obj, $ptype, :$local = 0, :$transitive = 1) {
        my @result := self.concretization_lookup($obj, $ptype, :$local, :$transitive);
        nqp::die("No concretization found for " ~ $ptype.HOW.name($ptype)) unless @result[0];
        @result[1]
    }
}
