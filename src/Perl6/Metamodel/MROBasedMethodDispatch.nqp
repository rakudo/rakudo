role Perl6::Metamodel::MROBasedMethodDispatch {
    # If needed, a cached flattened method table accounting for all methods in
    # this class and its parents. This is only needed in the situation that a
    # megamorphic callsite involves the class, so calculated and cached on
    # demand.
    has $!cached_all_method_table;
    has $!cache_is_valid;

    # Resolve a method. On MoarVM, with the generalized dispatch mechanism,
    # this is called to bootstrap callsites. On backends without that, it
    # is only called on a published cache miss.
    method find_method($obj, $name, :$no_fallback, *%adverbs) {
        # uncomment line below for verbose information about uncached method lookups
        #nqp::say( "looking for " ~ $name ~ " in " ~ $obj.HOW.name($obj) );

        my $obj_how := nqp::how_nd($obj);
        if nqp::can($obj_how, 'submethod_table') {
            my %submethods := nqp::hllize($obj_how.submethod_table($obj));
            my $found := nqp::atkey(%submethods, $name);
            return $found if nqp::isconcrete($found);
        }
        my %methods;
        my @mro := self.mro($obj);
        my int $i := 0;
        my int $n := nqp::elems(@mro);
        while $i < $n {
            my $class := nqp::atpos(@mro, $i);
            %methods := nqp::hllize($class.HOW.method_table($class));
            my $found := nqp::atkey(%methods, $name);
            return $found if nqp::isconcrete($found);
            $i++;
        }
        !$no_fallback && nqp::can(self, 'find_method_fallback')
            ?? self.find_method_fallback($obj, $name)
            !!nqp::null()
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
            $qtype.HOW.find_method($qtype, $name)
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
#?if !moar
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
#?endif
    }

    method all_method_table($obj) {
        return $!cached_all_method_table if $!cache_is_valid;

        my $table := nqp::hash();
        my @mro := self.mro($obj);
        my int $i := nqp::elems(@mro);
        while $i-- {
            my $class := nqp::atpos(@mro, $i);
            for nqp::hllize($class.HOW.method_table($class)) {
                $table{$_.key} := nqp::decont($_.value);
            }
        }
        for nqp::hllize($obj.HOW.submethod_table($obj)) {
            $table{$_.key} := nqp::decont($_.value);
        }
        nqp::scwbdisable();
#?if moar
        nqp::atomicbindattr(self, $?CLASS, '$!cached_all_method_table', $table);
        nqp::atomicbindattr(self, $?CLASS, '$!cache_is_valid', 1);
#?endif
#?if !moar
        $!cached_all_method_table := $table;
        $!cache_is_valid := 1;
#?endif
        nqp::scwbenable();
        $table
    }

    method invalidate_method_caches($obj) {
        nqp::scwbdisable();
#?if moar
        nqp::atomicbindattr(self, $?CLASS, '$!cache_is_valid', 0);
#?endif
#?if !moar
        $!cache_is_valid := 0;
#?endif
        nqp::scwbenable();
#?if !moar
        nqp::setmethcacheauth($obj, 0);
#?endif
    }
}

# vim: expandtab sw=4
