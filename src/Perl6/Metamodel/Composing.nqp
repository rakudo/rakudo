#- Metamodel::Composing --------------------------------------------------------
# Base logic for marking a metaclass as composed, and allow introspection
# into its state.  Also provides basic compose nothing support for those
# metaclasses that appear to need composing, but really don't.
role Perl6::Metamodel::Composing {
    has int $!composed;

    # Null operation, if the consumer doesn't supply a "compose" of its own
    method compose($XXX?, :$compiler_services) { $!composed := 1 }

    method set_composed($XXX?) { $!composed := 1 }
    method is_composed($XXX?)  { $!composed      }

    # Helper method to run code if the object was not composed yet, set
    # the flag to being composed, and return whether the object was
    # composed already.
    method run_if_not_composed($code) {
        my int $was_composed := $!composed;

        # Run code and set flag if we were not composed yet
        unless $was_composed {
            $code();
            $!composed := 1;
        }

        $was_composed
    }
}

# vim: expandtab sw=4
