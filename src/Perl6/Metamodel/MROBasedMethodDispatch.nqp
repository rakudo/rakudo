#- Metamodel::MROBasedMethodDispatch -------------------------------------------
role Perl6::Metamodel::MROBasedMethodDispatch {

    # If needed, a cached flattened method table accounting for all methods in
    # this class and its parents. This is only needed in the situation that a
    # megamorphic callsite involves the class, so calculated and cached on
    # demand.
    has $!cached_all_method_table;

    # Resolve a method. On MoarVM, with the generalized dispatch mechanism,
    # this is called to bootstrap callsites. On backends without that, it
    # is only called on a published cache miss.
    method find_method($target, str $name, :$no_fallback, *%_) {

        # uncomment line below for verbose information about uncached
        # method lookups
        #nqp::say( "looking for " ~ $name ~ " in " ~ self.name($target));

        my $HOW := nqp::how_nd($target);
        if nqp::can($HOW, 'submethod_table') {
            my $found := nqp::atkey($HOW.submethod_table($target), $name);
            return $found unless nqp::isnull($found);
        }

        my %methods;
        my $mro := self.mro($target);

        my int $m := nqp::elems($mro);
        my int $i;
        while $i < $m {
            my $class := nqp::atpos($mro, $i);
            my $found := nqp::atkey($class.HOW.method_table($class), $name);
            nqp::isnull($found)
              ?? ++$i
              !! (return $found);
        }

        nqp::not_i($no_fallback) && nqp::can(self, 'find_method_fallback')
            ?? self.find_method_fallback($target, $name)
            !! nqp::null
    }

    method find_method_qualified($target, $qtype, str $name) {

        # Parametric
        if $qtype.HOW.archetypes.parametric
          && nqp::can(self, 'concretization') {
            # Resolve it via the concrete form of this parametric.
            # Look deep for a candidate.
            my $class := self.concretization(
              $target, $qtype, :transitive, :relaxed
            );
            $class.HOW.code_of_method($class, $name)
        }

        # Non-parametric, so just locate it from the already concrete
        # type (or fallback to this if no .concretization on ourself).
        else {
            $qtype.HOW.find_method($qtype, $name)
        }
    }

    # Maybe this belongs on a role. Also, may be worth memoizing.
    method can($target, str $name) {
        my @methods;

        my $method := nqp::atkey(self.submethod_table($target), $name);
        nqp::push(@methods, $method) unless nqp::isnull($method);

        my $mro   := self.mro($target);
        my int $m := nqp::elems($mro);
        my int $i;
        while $i < $m {
            my $class  := nqp::atpos($mro, $i);
            my $method := nqp::atkey($class.HOW.method_table($class), $name);
            nqp::push(@methods, $method) unless nqp::isnull($method);

            ++$i;
        }

        @methods
    }

    method publish_method_cache($target) {
#?if !moar
        # Walk MRO and add methods to cache, unless another method
        # lower in the class hierarchy "shadowed" it.
        my %cache;
        my @mro_reversed;
        my $authable := 1;
        for self.mro($target) {
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
        for nqp::hllize(self.submethod_table($target)) {
            %cache{$_.key} := nqp::decont($_.value);
        }

        nqp::setmethcache($target, %cache);
        unless nqp::can(self, 'has_fallbacks') && self.has_fallbacks($target) {
            nqp::setmethcacheauth($target, $authable);
        }
#?endif
    }

    method all_method_table($target) {
        my $table := $!cached_all_method_table;
        unless nqp::isconcrete($table) {
            $table  := nqp::hash;
            my $mro := self.mro($target);

            # Walk MRO in reverse order as to overwrite later methods with
            # earlier ones when doing normal lookup
            my int $i := nqp::elems($mro);
            while $i-- {
                my $class := nqp::atpos($mro, $i);
                for $class.HOW.method_table($class) {
                    nqp::bindkey($table,$_.key, nqp::decont($_.value));
                }
            }
            for self.submethod_table($target) {
                nqp::bindkey($table,$_.key, nqp::decont($_.value));
            }
            nqp::scwbdisable;
            $!cached_all_method_table := $table;
            nqp::scwbenable;
        }

        $table
    }

    method invalidate_method_caches($target) {
        nqp::scwbdisable;
        $!cached_all_method_table := nqp::null;
        nqp::scwbenable;
#?if !moar
        nqp::setmethcacheauth($target, 0);
#?endif
    }
}

# vim: expandtab sw=4
