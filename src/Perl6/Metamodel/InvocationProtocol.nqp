#?if !moar
role Perl6::Metamodel::InvocationProtocol {
    has int $!has_invocation_attr;
    has $!invocation_attr_class;
    has str $!invocation_attr_name;

    has int $!has_invocation_handler;
    has $!invocation_handler;

    my $default_invoke_handler;
    method set_default_invoke_handler($h) {
        $default_invoke_handler := $h;
    }

    method set_invocation_attr($XXX, $class, str $name) {
        $!has_invocation_attr   := 1;
        $!invocation_attr_class := $class;
        $!invocation_attr_name  := $name;
    }

    method set_invocation_handler($XXX, $handler) {
        $!has_invocation_handler := 1;
        $!invocation_handler     := $handler;
    }

    method has_invocation_attr(   $XXX?) { $!has_invocation_attr    }
    method invocation_attr_class( $XXX?) { $!invocation_attr_class  }
    method invocation_attr_name(  $XXX?) { $!invocation_attr_name   }
    method has_invocation_handler($XXX?) { $!has_invocation_handler }
    method invocation_handler(    $XXX?) { $!invocation_handler     }

    method compose_invocation($target) {
        # Check if we have a invoke, and if so install
        # the default invocation forwarder. Otherwise, see if we or
        # a parent has an invocation attr.
        if self.archetypes.composable {
            # We special case roles by using only default handler
            nqp::setinvokespec($target, nqp::null(), nqp::null_s(),
                $default_invoke_handler);
        }
        else {
            my $pcmeth := self.find_method($target, 'CALL-ME', :no_fallback(1));
            if nqp::defined($pcmeth) {
                nqp::die('Default invocation handler is not invokable')
                    unless nqp::isinvokable($default_invoke_handler);
                nqp::setinvokespec($target, nqp::null(), nqp::null_s(),
                    $default_invoke_handler);
            }
            else {
                for self.mro($target) -> $class {
                    if nqp::can($class.HOW, 'has_invocation_attr') {
                        if $class.HOW.has_invocation_attr($class) {
                            nqp::setinvokespec($target,
                                $class.HOW.invocation_attr_class($class),
                                $class.HOW.invocation_attr_name($class),
                                nqp::null());
                            last;
                        }
                    }
                    if nqp::can($class.HOW, 'has_invocation_handler') {
                        if $class.HOW.has_invocation_handler($class) {
                            nqp::setinvokespec($target,
                                nqp::null(), nqp::null_s(),
                                $class.HOW.invocation_handler($class));
                            last;
                        }
                    }
                }
            }
        }
    }
}
#?endif

# vim: expandtab sw=4
