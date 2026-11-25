#- Metamodel::MethodContainer --------------------------------------------------
# Handle the aspects of a HOW that can contain methods and submethods, and
# their regexy counterparts.

role Perl6::Metamodel::MethodContainer {
    # Lookup table of the methods.
    has %!methods;
    has %!submethods;

    # The order that the methods were added in.
    has @!method_order;
    has @!method_names;

    # Cache that expires when we add methods (primarily to support NFA stuff).
    # The hash here is readonly; we copy/replace in on addition, for thread
    # safety (additions are dominated by lookups, so a lock - even a rw-lock -
    # is not ideal here).
    has %!cache;

    # Add a method.
    method add_method($target, $name, $code, :$handles = 1) {
        # Ensure we haven't already got it.
        $code := nqp::decont(  $code);
        $name := nqp::decont_s($name);

        my str $attr_name :=
#?if jvm
          !nqp::isnull(Perl6::Metamodel::Configuration.submethod_type) &&
#?endif
          nqp::istype($code, Perl6::Metamodel::Configuration.submethod_type)
            ?? '%!submethods'
            !! '%!methods';

        # Add to correct table depending on if it's a Submethod.
        my %table;
        self.protect({
            self.throw_duplicate($target, $name, $code)
              if nqp::existskey(%!methods, $name)
              || nqp::existskey(%!submethods, $name);

            %table := nqp::clone(nqp::getattr(self, $?CLASS, $attr_name));
            nqp::bindkey(%table, $name, $code);
            nqp::bindattr(self, $?CLASS, $attr_name, %table);

            # See if trait `handles` has been applied and we can use it on
            # the target type.
            # XXX Also skip this step if method is being added under a
            # different name but the original code object has been installed
            # earlier. This step is here until Method::Also incorporates
            # support for :!handles argument.
            if $handles
              && nqp::can($code, 'apply_handles')
              && nqp::can(self,  'find_method_fallback') {
                my @method_order := @!method_order;

                my int $m := nqp::elems(@method_order);
                my int $i;
                while $i < $m {
                    nqp::eqaddr(nqp::atpos(@method_order, $i), $code)
                      ?? (last)
                      !! ++$i;
                }

                # Apply if none of the methods matched
                $code.apply_handles($target) if $i == $m;
            }

            # Adding a method means any cache is no longer authoritative.
            self.invalidate_method_caches($target)
              if nqp::can(self, "invalidate_method_caches");
            %!cache := nqp::hash;

            my @method_order := nqp::clone(@!method_order);
            my @method_names := nqp::clone(@!method_names);
            nqp::push(@method_order, $code);
            nqp::push(@method_names, $name);
            @!method_order := @method_order;
            @!method_names := @method_names;
        });

        # Return updated (sub)method table
        %table
    }

    # Gets the method hierarchy.
    method methods($target, :$local, :$excl, :$all, :$implementation-detail) {
        my @methods;

        sub add_methods($source) {
            my int $m := nqp::elems($source);
            if $m {

                # Convert to values if given a hash
                if nqp::ishash($source) {
                    my $values := nqp::list;
                    for $source {
                        nqp::push($values, $_.value);
                    }
                    $source := $values;
                }

                # Include implementation detail methods only.  Since this is
                # a HLL construct, we don't need to do any HLLizing
                if $implementation-detail {
                    my int $i;
                    while $i < $m {
                        my $method := nqp::atpos($source, $i);
                        nqp::push(@methods, $method)
                          if nqp::can($method, 'is-implementation-detail')
                          && $method.is-implementation-detail;
                        ++$i;
                    }
                }

                # Include all methods, possibly from NQP as well, so make
                # sure they're HLLized
                else {
                    my int $i;
                    while $i < $m {
                        nqp::push(
                          @methods,
                          nqp::hllizefor(nqp::atpos($source, $i), 'Raku')
                        );
                        ++$i;
                    }
                }
            }
        }

        # Always need local methods on the list.
        add_methods(@!method_order);

        # If local flag was not passed, include those from parents.
        unless $local {
            my @parents := self.parents($target, :$all, :$excl);

            my int $m := nqp::elems(@parents);
            my int $i;
            while $i < $m {
                my $parent := nqp::atpos(@parents, $i);
                add_methods($parent.HOW.method_order(   $parent));
                add_methods($parent.HOW.submethod_table($parent));
                ++$i;
            }
        }

        @methods
    }

    method method_order($XXX?) { @!method_order }
    method method_names($XXX?) { @!method_names }

    # Get the method table. Only contains methods directly declared here,
    # and excludes submethods.
    method method_table($XXX?) { %!methods }

    # Gets the submethods table.
    method submethod_table($XXX?) { %!submethods }

    # Checks if this package (not its parents) declares a given
    # method. Checks submethods also.
    method declares_method($XXX, str $name) {
        nqp::existskey(%!methods, $name) || nqp::existskey(%!submethods, $name)
    }
    method code_of_method( $XXX, str $name) {
        nqp::ifnull(
          nqp::atkey(%!methods, $name),
          nqp::atkey(%!submethods, $name)
        )
    }

    # Looks up a method with the provided name, for introspection purposes.
    # Returns nqp::null if not found
    method lookup($target, str $name) {
        my @mro := self.mro($target);

        my int $m := nqp::elems(@mro);
        my int $i;
        while $i < $m {
            my $code := nqp::atpos(
              @mro, $i
            ).HOW.code_of_method($target, $name);

            nqp::isnull($code)
              ?? ++$i
              !! (return $code);
        }
        nqp::null
    }

    # Caches or updates a cached value.
    method cache($target, str $key, $value_generator) {
        nqp::ifnull(
          nqp::atkey(%!cache, $key),
          self.cache_add($target, $key, $value_generator())
        )
    }

    method cache_get($XXX, str $key) {
        nqp::atkey(%!cache, $key)
    }

    method cache_add($XXX, str $key, $value) {
        self.protect({
            my %cache := nqp::clone(%!cache);
            nqp::bindkey(%cache, $key, $value);
            %!cache := %cache;
            $value
        })
    }

    # Helper method to throw a duplicate method error
    method throw_duplicate($target, str $method, $code) {

        # XXX try within nqp::die() causes a hang. Pre-cache the result
        # and use it later.
        my str $typename  := self.name($target);
        my   $method-type := try { nqp::lc($code.HOW.name($code)) } // 'method';

        Perl6::Metamodel::Configuration.throw_or_die(
          'X::Method::Duplicate',
          "Package '"
            ~ $typename
            ~ "' already has a "
            ~ $method-type
            ~ " '"
            ~ $method
            ~ "' (did you mean to declare a multi method?)",
          :$method, :$method-type, :$typename
        );
    }
}

# vim: expandtab sw=4
