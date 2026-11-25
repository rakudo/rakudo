#- Metamodel::Trusting ---------------------------------------------------------
# Implements managing trust relationships between types.
role Perl6::Metamodel::Trusting {
    # Who do we trust?
    has @!trustees;

    # Adds a type that we trust.
    method add_trustee($XXX, $trustee) {
        self.protect({
            my @trustees := nqp::clone(@!trustees);
            nqp::push(@trustees, $trustee);
            @!trustees := @trustees;
        });
    }

    # Introspect the types that we trust.
    method trusts($XXX?) { @!trustees }

    # Checks if we trust a certain type. Can be used by the compiler
    # to check if a private call is allowable.
    method is_trusted($target, $claimant) {
        my $WHAT := $claimant.WHAT;

        # Always trust ourself.
        return 1 if nqp::eqaddr($target.WHAT, $WHAT);

        # Otherwise, look through our trustee list.
        if nqp::elems(@!trustees) {
            my @trustees := @!trustees;

            my int $m := nqp::elems(@trustees);
            my int $i;
            while $i < $m {
                nqp::eqaddr(nqp::atpos(@trustees, $i).WHAT, $WHAT)
                  ?? (return 1)
                  !! ++$i;
            }
        }

        # If we get here, not trusted.
        0
    }
}

# vim: expandtab sw=4
