#- Metamodel::ConcretizationCache ----------------------------------------------
# Cache concretizations on a class. Avoid re-specializing a role if its
# concretization already exists for the target type object and matches
# same arguments.
#
# This is different from Perl6::Metamodel::Concretization in the way that it
# provides interface for role specialization to find out if identical
# specialization has been done already
role Perl6::Metamodel::ConcretizationCache {
    has %!conc_cache;

    my $Capture := nqp::null;
    method !make_capture(@pos, %named) {

        if nqp::isnull($Capture) {
            # Fetch and preserve Capture type object. But only if it's fullyi
            # composed already.
            my $type := nqp::gethllsym('Raku', 'Capture');
            nqp::isnull($type) || nqp::not_i($type.HOW.is_composed($type))
              ?? (return nqp::null)
              !! ($Capture := $type);
        }

        my $capture := nqp::create($Capture);

        # We need this at class compilation time. But the class itself isn't
        # composed yet and cannot be used with Capture. For this reason we
        # remove it from the positionals. It's ok as long as we only operate
        # on the currently compiled class.
        @pos := nqp::clone(@pos);
        nqp::shift(@pos);
        nqp::bindattr($capture, $Capture, '@!list', @pos);
        nqp::bindattr($capture, $Capture, '%!hash', %named);
        $capture
    }

    method add_conc_to_cache($class, $role, @pos, %named, $concretization) {
        my $capture := self.'!make_capture'(@pos, %named);

        unless nqp::isnull($capture) {
            self.protect({
                my %conc_cache := nqp::clone(%!conc_cache);
                my str $obj-id := ~nqp::objectid($role);

                nqp::push(
                  nqp::ifnull(
                    nqp::atkey(%conc_cache, $obj-id),
                    nqp::bindkey(%conc_cache, $obj-id, nqp::list)
                  ),
                  nqp::list($capture, $concretization)
                );

                nqp::scwbdisable;
                %!conc_cache := %conc_cache;
                nqp::scwbenable;
            });
        }

        $concretization
    }

    method get_cached_conc($class, $role, @pos, %named) {
        my $capture := self.'!make_capture'(@pos, %named);
        unless nqp::isnull($capture) {
            my %conc_cache := %!conc_cache;
            my str $obj-id := ~nqp::objectid($role);

            if nqp::existskey(%conc_cache, $obj-id) {
                my @cached := nqp::atkey(%conc_cache, $obj-id);

                my int $m := nqp::elems(@cached);
                my int $i;
                while $i < $m {
                    my $entry := nqp::atpos(@cached, $i);
                    (try $capture.ACCEPTS(nqp::atpos($entry, 0)))
                      ?? (return nqp::atpos($entry, 1))
                      !! ++$i;
                }
            }
        }

        nqp::null
    }

    method wipe_conc_cache() { %!conc_cache := nqp::hash }
}

# vim: expandtab sw=4
