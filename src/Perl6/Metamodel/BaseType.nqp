# Implemented by meta-objects that don't do inheritance per se,
# but want to base themselves on another type and mostly behave
# like they support it.
role Perl6::Metamodel::BaseType {
    has $!base_type;
    has $!base_type_set;
    has @!mro;

    method set_base_type($target, $base_type) {
        if $!base_type_set {
            nqp::die("Base type has already been set for " ~ self.name($target));
        }
        $!base_type := $base_type;
        $!base_type_set := 1;
    }

    # Our MRO is just that of base type.
    method mro($target, :$roles = 0, :$concretizations = 0, :$unhidden = 0) {
        unless @!mro {
            @!mro := nqp::list();
            @!mro[0] := $target;
            for $!base_type.HOW.mro($!base_type, :$roles, :$concretizations, :$unhidden) {
                @!mro.push($_);
            }
        }
        @!mro
    }

    method parents($XXX?, :$local, :$excl, :$all) {
        my @parents := [$!base_type];
        unless $local {
            for $!base_type.HOW.parents($!base_type, :$excl, :$all) {
                @parents.push($_);
            }
        }
        @parents
    }
}

# vim: expandtab sw=4
