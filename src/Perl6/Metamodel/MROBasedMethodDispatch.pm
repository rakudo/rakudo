role Perl6::Metamodel::MROBasedMethodDispatch {
    # While we normally end up locating methods through the method cache,
    # this is here as a fallback.
    method find_method($obj, $name, :$no_fallback, *%adverbs) {
        my %methods;
        for self.mro($obj) {
            %methods := $_.HOW.method_table($_);
            if nqp::existskey(%methods, $name) {
                return %methods{$name}
            }
        }
        my %submethods := $obj.HOW.submethod_table($obj);
        if nqp::existskey(%submethods, $name) {
            return %submethods{$name}
        }
        !$no_fallback && nqp::can(self, 'find_method_fallback') ??
            self.find_method_fallback($obj, $name) !!
            nqp::null();
    }
    
    method find_method_qualified($obj, $qtype, $name) {
        if $qtype.HOW.archetypes.parametric && nqp::can(self, 'concretization') {
            # Resolve it via the concrete form of this parametric.
            my $conc := self.concretization($obj, $qtype);
            $conc.HOW.method_table($conc){$name}
        }
        else {
            # Non-parametric, so just locate it from the already concrete
            # type (or fallback to this if no .concretization on ourself).
            nqp::findmethod($qtype, $name)
        }
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
            for $_.HOW.method_table($_) {
                %cache{$_.key} := $_.value;
            }
            if nqp::can($_.HOW, 'is_composed') && !$_.HOW.is_composed($_) {
                $authable := 0;
            }
        }
        
        # Also add submethods.
        for $obj.HOW.submethod_table($obj) {
            %cache{$_.key} := $_.value;
        }
        
        nqp::setmethcache($obj, %cache);
        unless nqp::can(self, 'has_fallbacks') && self.has_fallbacks($obj) {
            nqp::setmethcacheauth($obj, $authable);
        }
    }
}
