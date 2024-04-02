#- Metamodel::BaseType ---------------------------------------------------------
# Implemented by meta-objects that don't do inheritance per se,
# but want to base themselves on another type and mostly behave
# like they support it.

role Perl6::Metamodel::BaseType {
    has $!base_type;
    has @!mro;

    method TWEAK(*%_) {
        $!base_type := nqp::null;
    }

    method set_base_type($target, $base_type) {
        nqp::isnull($!base_type)
          ?? ($!base_type := $base_type)
          !! nqp::die(
               "Base type has already been set for " ~ self.name($target)
             )
    }

    # Our MRO is just that of base type.
    method mro($target, :$roles, :$concretizations, :$unhidden) {
        # XXX it feels incorrect that the first call to .mro with any given
        # set of named arguments, should determine any future returns. Yet
        # if we remove the @!mro attribute and create the @mro each call on
        # the fly (like in the else block here), we get spectest failures in
        # integration/advent2012-day13.t
        if nqp::elems(@!mro) {
            @!mro
        }
        else {
            my @mro := nqp::clone($!base_type.HOW.mro(
              $!base_type, :$roles, :$concretizations, :$unhidden
            ));
            nqp::unshift(@mro, $target);
            @!mro := @mro
        }
    }

    method parents($XXX?, :$local, :$excl, :$all) {
        if $local {
            nqp::list($!base_type)
        }
        else {
            my $base_type := $!base_type;
            my @parents := nqp::clone($base_type.HOW.parents(
              $base_type, :$excl, :$all
            ));
            nqp::unshift(@parents, $base_type);
            @parents
        }
    }
}

# vim: expandtab sw=4
