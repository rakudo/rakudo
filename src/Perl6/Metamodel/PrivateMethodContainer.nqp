#- Metamodel::PrivateMethodContainer -------------------------------------------
# Handle the aspects of a HOW that can contain private methods
role Perl6::Metamodel::PrivateMethodContainer {
    has %!private_methods;
    has @!private_methods;
    has @!private_method_names;

    # Adds a private method.
    method add_private_method($target, $name, $code) {
        $name := nqp::decont_s($name);
        nqp::die("Private method '$name' already declared in package "
          ~ self.name($target)
        ) if nqp::existskey(%!private_methods, $name);

        nqp::bindkey(%!private_methods, $name, $code);
        nqp::push(@!private_methods, $code);
        nqp::push(@!private_method_names, $name);
    }

    method private_method_table($XXX?) { %!private_methods      }
    method private_methods(     $XXX?) { @!private_methods      }
    method private_method_names($XXX?) { @!private_method_names }

    # Locates a private method, and hands back null if it doesn't exist.
    method find_private_method($XXX, $name) {
        nqp::atkey(%!private_methods, $name)
    }
}

# vim: expandtab sw=4
