# Note: this file probably wants to be in some Perl6::CompilerGuts namespace.

role SubType {
    has $!checker;
    method ACCEPTS(Mu $topic) {
        unless callsame() {
            return Bool::False
        }
        $!checker.ACCEPTS($topic)
    }
}

our sub CREATE_SUBSET_TYPE(Mu \$original, $checker) {
    # XXX Ideally we'd be able to just replace all of what follows
    # with a simple:
    #     my $subtype = $original but SubType($checker);
    # However, that won't quite work until we improve type object
    # handling.
    Q:PIR {
        $P0 = find_lex '$original'
        $P1 = class $P0
        $P2 = get_hll_global 'SubType'
        $P2 = $P2.'!select'()
        $P3 = new ['Class']
        $P3.'add_parent'($P1)
        $P3.'add_role'($P2)
        addattribute $P3, '$!checker'
        $P4 = getprop 'metaclass', $P1
        setprop $P3, 'metaclass', $P4
        %r = new $P3
        transform_to_p6opaque %r
        $P5 = find_lex '$checker'
        setattribute %r, '$!checker', $P5
        $P6 = getprop 'subtype_realtype', $P0
        if null $P6 goto original_unrefined
        setprop %r, 'subtype_realtype', $P6
        goto refinement_done
      original_unrefined:
        setprop %r, 'subtype_realtype', $P0
      refinement_done:
    };
}
