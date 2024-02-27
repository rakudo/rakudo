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
    method add_method($target, $name, $code_obj, :$handles = 1) {
        # Ensure we haven't already got it.
        $code_obj := nqp::decont($code_obj);
        $name := nqp::decont_s($name);
        if nqp::existskey(%!methods, $name) || nqp::existskey(%!submethods, $name) {
            # XXX try within nqp::die() causes a hang. Pre-cache the result and use it later.
            my $method-type := try { nqp::lc($code_obj.HOW.name($code_obj)) } // 'method';
            Perl6::Metamodel::Configuration.throw_or_die(
                'X::Method::Duplicate',
                "Package '"
                    ~ self.name($target)
                    ~ "' already has a "
                    ~ $method-type
                    ~ " '"
                    ~ $name
                    ~ "' (did you mean to declare a multi method?)",
                :$method-type,
                :method($name),
                :typename(self.name($target))
            );
        }

        # Add to correct table depending on if it's a Submethod.
        if !nqp::isnull(Perl6::Metamodel::Configuration.submethod_type)
            && nqp::istype($code_obj, Perl6::Metamodel::Configuration.submethod_type) {
            %!submethods{$name} := $code_obj;
        }
        else {
            %!methods{$name} := $code_obj;
        }

        # See if trait `handles` has been applied and we can use it on the target type.
        # XXX Also skip this step if method is being added under a different name but the original code object has been
        # installed earlier. This step is here until Method::Also incorporates support for :!handles argument.
        if $handles
            && nqp::can($code_obj, 'apply_handles') 
            && nqp::can(self, 'find_method_fallback') 
        {
            my $do_apply := 1;
            for @!method_order {
                if $_ =:= $code_obj {
                    $do_apply := 0;
                    last
                }
            }
            $code_obj.apply_handles($target) if $do_apply;
        }

        # Adding a method means any cache is no longer authoritative.
        if nqp::can(self, "invalidate_method_caches") {
            self.invalidate_method_caches($target);
        }
        %!cache := {};
        @!method_order[+@!method_order] := $code_obj;
        @!method_names[+@!method_names] := $name;
    }

    # Gets the method hierarchy.
    method methods($target, :$local, :$excl, :$all, :$implementation-detail) {
        my @meths;

        my $check-implementation-detail := !$implementation-detail;

        # Always need local methods on the list.
        for @!method_order {
            @meths.push(nqp::hllizefor($_,'Raku'))
              unless $check-implementation-detail
                && nqp::can($_,'is-implementation-detail')
                && $_.is-implementation-detail;
        }

        # If local flag was not passed, include those from parents.
        unless $local {
            for self.parents($target, :all($all), :excl($excl)) {
                for nqp::hllize($_.HOW.method_table($_)) {
                    @meths.push(nqp::hllizefor(nqp::decont($_.value),'Raku'))
                      unless $check-implementation-detail
                        && nqp::can($_,'is-implementation-detail')
                        && $_.is-implementation-detail;
                }
                for nqp::hllize($_.HOW.submethod_table($_)) {
                    @meths.push(nqp::hllizefor(nqp::decont($_.value),'Raku'))
                      unless $check-implementation-detail
                        && nqp::can($_,'is-implementation-detail')
                        && $_.is-implementation-detail;
                }
            }
        }

        @meths
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
    method declares_method($XXX, $name) {
        %!methods{$name} || %!submethods{$name} ?? 1 !! 0
    }

    # Looks up a method with the provided name, for introspection purposes.
    method lookup($target, $name) {
        for self.mro($target) {
            my %meth := nqp::hllize($_.HOW.method_table($target));
            if nqp::existskey(%meth, $name) {
                return nqp::decont(%meth{$name});
            }
            if nqp::can($_.HOW, 'submethod_table') {
                my %submeth := nqp::hllize($_.HOW.submethod_table($target));
                if nqp::existskey(%submeth, $name) {
                    return nqp::decont(%submeth{$name});
                }
            }
        }
        nqp::null()
    }

    # Caches or updates a cached value.
    method cache($target, str $key, $value_generator) {
        my %orig_cache := %!cache;
        nqp::ishash(%orig_cache) && nqp::existskey(%!cache, $key)
            ?? %!cache{$key}
            !! self.cache_add($target, $key, $value_generator())
    }

    method cache_get($XXX, str $key) {
        my %caches := %!cache;
        nqp::ishash(%caches) ?? nqp::atkey(%caches, $key) !! nqp::null()
    }

    method cache_add($XXX, str $key, $value) {
        my %orig_cache := %!cache;
        my %copy := nqp::ishash(%orig_cache) ?? nqp::clone(%orig_cache) !! {};
        %copy{$key} := $value;
        %!cache := %copy;
        $value
    }
}

# vim: expandtab sw=4
