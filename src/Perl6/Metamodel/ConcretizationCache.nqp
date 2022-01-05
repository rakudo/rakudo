# Cache concretizations on a class. Avoid re-specializing a role if its concretization already exists for the target
# type object and matches same arguments.
# This is different from Perl6::Metamodel::Concretization in the way that it:
# - only used at compile time
# - provides interface for role specialization to find out if identical specialization has been done already
# - is not an introspection mechanism
role Perl6::Metamodel::ConcretizationCache {
    has %!conc_cache;

    my $capture_type := nqp::null();
    method !make_capture(@pos, %named) {
        if nqp::isnull($capture_type) {
            # Fetch and preserve Capture type object. But don't do so until it's fully ready.
            $capture_type := nqp::gethllsym('Raku', 'Capture');
            return nqp::null()
                if nqp::isnull($capture_type) || !$capture_type.HOW.is_composed($capture_type)
        }
        my $capture := nqp::create($capture_type);
        # We need this at class compilation time. But the class itself isn't composed yet and cannot be used with
        # Capture. For this reason we remove it from the positionals. It's ok as long as we only operate on the
        # currently compiled class.
        my @cpos := nqp::clone(@pos);
        nqp::shift(@cpos);
        nqp::bindattr($capture, $capture_type, '@!list', @cpos);
        nqp::bindattr($capture, $capture_type, '%!hash', %named);
        $capture
    }

    method add_conc_to_cache($class, $role, @pos, %named, $concretization) {
        my $capture := self.'!make_capture'(@pos, %named);
        unless nqp::isnull($capture) {
            my $obj-id := ~nqp::objectid($role);
            nqp::scwbdisable();
            %!conc_cache{$obj-id} := [] unless %!conc_cache{$obj-id};
            nqp::push(%!conc_cache{$obj-id}, [$capture, $concretization]);
            nqp::scwbenable();
        }
        $concretization
    }

    method get_cached_conc($class, $role, @pos, %named) {
        my $capture := self.'!make_capture'(@pos, %named);
        unless nqp::isnull($capture) {
            my $obj-id := ~nqp::objectid($role);
            if nqp::existskey(%!conc_cache, $obj-id) {
                for %!conc_cache{$obj-id} {
                    return $_[1] if try $capture.ACCEPTS($_[0]);
                }
            }
        }
        nqp::null()
    }

    method wipe_conc_cache() { %!conc_cache := nqp::hash() }
}

# vim: expandtab sw=4
