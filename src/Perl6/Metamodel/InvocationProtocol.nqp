role Perl6::Metamodel::InvocationProtocol {
    has int $!has_invocation_attr;
    has $!invocation_attr_class;
    has str $!invocation_attr_name;

    has int $!has_invocation_handler;
    has $!invocation_handler;

    has int $!has_multi_attrs;
    has $!md_attr_class;
    has str $!md_valid_attr_name;
    has str $!md_cache_attr_name;

    my $default_invoke_handler;
    method set_default_invoke_handler($h) {
        $default_invoke_handler := $h;
    }

    method set_invocation_attr($obj, $class, str $name) {
        $!has_invocation_attr   := 1;
        $!invocation_attr_class := $class;
        $!invocation_attr_name  := $name;
    }

    method set_invocation_handler($obj, $handler) {
        $!has_invocation_handler := 1;
        $!invocation_handler     := $handler;
    }

    method has_invocation_attr($obj) { $!has_invocation_attr }
    method invocation_attr_class($obj) { $!invocation_attr_class }
    method invocation_attr_name($obj) { $!invocation_attr_name }

    method has_invocation_handler($obj) { $!has_invocation_handler }
    method invocation_handler($obj) { $!invocation_handler }

    method set_multi_invocation_attrs($obj, $class, str $valid_name, str $cache_name) {
        $!has_multi_attrs    := 1;
        $!md_attr_class      := $class;
        $!md_valid_attr_name := $valid_name;
        $!md_cache_attr_name := $cache_name;
    }
    method has_multi_invocation_attrs($obj) { $!has_multi_attrs }
    method multi_attr_class($obj) { $!md_attr_class }
    method multi_valid_attr_name($obj) { $!md_valid_attr_name }
    method multi_cache_attr_name($obj) { $!md_cache_attr_name }

    method compose_invocation($obj) {
        # Check if we have a invoke, and if so install
        # the default invocation forwarder. Otherwise, see if we or
        # a parent has an invocation attr.
        if $obj.HOW.archetypes.composable {
            # We special case roles by using only default handler
            nqp::setinvokespec($obj, nqp::null(), nqp::null_s(),
                $default_invoke_handler);
        }
        else {
            my $pcmeth := self.find_method($obj, 'CALL-ME', :no_fallback(1));
            if nqp::defined($pcmeth) {
                nqp::die('Default invocation handler is not invokable')
                    unless nqp::isinvokable($default_invoke_handler);
                nqp::setinvokespec($obj, nqp::null(), nqp::null_s(),
                    $default_invoke_handler);
            }
            else {
                for self.mro($obj) -> $class {
                    if nqp::can($class.HOW, 'has_invocation_attr') {
                        if $class.HOW.has_invocation_attr($class) {
                            nqp::setinvokespec($obj,
                                $class.HOW.invocation_attr_class($class),
                                $class.HOW.invocation_attr_name($class),
                                nqp::null());
                            last;
                        }
                    }
                    if nqp::can($class.HOW, 'has_invocation_handler') {
                        if $class.HOW.has_invocation_handler($class) {
                            nqp::setinvokespec($obj,
                                nqp::null(), nqp::null_s(),
                                $class.HOW.invocation_handler($class));
                            last;
                        }
                    }
                }
#?if moar
                for self.mro($obj) -> $class {
                    if nqp::can($class.HOW, 'has_multi_invocation_attrs') {
                        if $class.HOW.has_multi_invocation_attrs($class) {
                            nqp::setmultispec($obj,
                                $class.HOW.multi_attr_class($class),
                                $class.HOW.multi_valid_attr_name($class),
                                $class.HOW.multi_cache_attr_name($class));
                            last;
                        }
                    }
                }
#?endif
            }
        }
    }
}

# vim: expandtab sw=4
