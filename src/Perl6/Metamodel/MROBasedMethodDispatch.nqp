role Perl6::Metamodel::MROBasedMethodDispatch {
    # While we normally end up locating methods through the method cache,
    # this is here as a fallback.
    method find_method($obj, $name, :$no_fallback, *%adverbs) {

# uncomment line below for verbose information about uncached method lookups
#nqp::say( "looking for " ~ $name ~ " in " ~ $obj.HOW.name($obj) );
#
        if nqp::can($obj.HOW, 'submethod_table') {
            my %submethods := nqp::hllize($obj.HOW.submethod_table($obj));
            if nqp::existskey(%submethods, $name) {
                return %submethods{$name}
            }
        }
        my %methods;
        for self.mro($obj) {
            %methods := nqp::hllize($_.HOW.method_table($_));
            if nqp::existskey(%methods, $name) {
                return %methods{$name}
            }
        }
        !$no_fallback && nqp::can(self, 'find_method_fallback') ??
            self.find_method_fallback($obj, $name) !!
            nqp::null();
    }

    method find_method_qualified($obj, $qtype, $name) {
        if $qtype.HOW.archetypes.parametric && nqp::can(self, 'concretization') {
            # Resolve it via the concrete form of this parametric. Look deep for a candidate.
            my $conc := self.concretization($obj, $qtype, :local(0), :transitive(1), :relaxed(1));
            nqp::hllize($conc.HOW.method_table($conc)){$name}
                || nqp::hllize($conc.HOW.submethod_table($conc)){$name}
        }
        else {
            # Non-parametric, so just locate it from the already concrete
            # type (or fallback to this if no .concretization on ourself).
            nqp::findmethod($qtype, $name)
        }
    }

    # Maybe this belongs on a role. Also, may be worth memoizing.
    method can($obj, $name) {
        my @meths;
        my %smt := nqp::hllize(self.submethod_table($obj));
        if nqp::existskey(%smt, $name) {
            @meths.push(%smt{$name});
        }
        for self.mro($obj) {
            my %mt := nqp::hllize($_.HOW.method_table($_));
            if nqp::existskey(%mt, $name) {
                @meths.push(%mt{$name})
            }
        }
        @meths
    }

    method publish_method_cache($obj) {
        # Walk MRO and add methods to cache, unless another method
        # lower in the class hierarchy "shadowed" it.
        my %cache;
        my @mro_reversed;
        my $authable := 1;
        for self.mro($obj) {
            @mro_reversed.unshift($_);
        }
        for @mro_reversed {
            for nqp::hllize($_.HOW.method_table($_)) {
                %cache{$_.key} := nqp::decont($_.value);
            }
            if nqp::can($_.HOW, 'is_composed') && !$_.HOW.is_composed($_) {
                $authable := 0;
            }
        }

        # Also add submethods.
        for nqp::hllize($obj.HOW.submethod_table($obj)) {
            %cache{$_.key} := nqp::decont($_.value);
        }

        nqp::setmethcache($obj, %cache);
        unless nqp::can(self, 'has_fallbacks') && self.has_fallbacks($obj) {
            nqp::setmethcacheauth($obj, $authable);
        }
    }
}

# vim: expandtab sw=4
