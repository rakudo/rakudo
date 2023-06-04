    ##
    ## Various utility subs to help us produce types that look Raku-like.
    ##

    sub parent($class, $parent) {
        $class.HOW.add_parent($class, $parent);
    }

    sub add-attribute($class, $type, $name) {
        $class.HOW.add_attribute($class, Attribute.new(
            :$name, :$type, :package($class), :auto_viv_primitive($type)
        ));
    }

    sub add-method($class, $name, @parameters, $impl) {
        # Assemble a signature object for introspection purposes.
        my @params;
        my $first := 1;
        for @parameters -> $type, $name, $named, $optional {
            my $param := nqp::create(Parameter);
            nqp::bindattr($param, Parameter, '$!type', $type);
            nqp::bindattr_s($param, Parameter, '$!variable_name', $name);
            my int $flags := 128; # Multi-invocant
            $flags := $flags + 64 if $first; # Invocant
            $flags := $flags + 2048 if $optional;
            nqp::bindattr_i($param, Parameter, '$!flags', $flags);
            if $named {
                nqp::bindattr($param, Parameter, '@!named_names',
                    nqp::list_s(nqp::substr($name, 1)));
            }
            nqp::push(@params, $param);
        }
        my $signature := nqp::create(Signature);
        nqp::bindattr($signature, Signature, '@!params', @params);
        nqp::bindattr($signature, Signature, '$!returns', Mu);

        # Wrap code up in a Method object.
        my $static-code := nqp::getstaticcode($impl);
        my $wrapper := nqp::create(Method);
        nqp::bindattr($wrapper, Code, '$!do', $static-code);
        nqp::bindattr($wrapper, Code, '$!signature', $signature);
        nqp::bindattr($wrapper, Routine, '$!package', $class);
        $wrapper.set_name($name);
        $class.HOW.add_method($class, $name, $wrapper);
    }

    sub compose($type) {
        $type.HOW.compose_repr($type);
        $type.HOW.publish_type_cache($type);
        $type.HOW.publish_method_cache($type);
    }
