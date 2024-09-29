#- Metamodel::PrivateMethodContainer -------------------------------------------
# Handle the aspects of a HOW that can contain private methods
role Perl6::Metamodel::PrivateMethodContainer {
    has %!private_methods;
    has @!private_methods;
    has @!private_method_names;

    # Adds a private method.
    method add_private_method($target, $name, $code) {
        $name := nqp::decont_s($name);

        self.protect({
            nqp::die("Private method '$name' already declared in package "
              ~ self.name($target)
            ) if nqp::existskey(%!private_methods, $name);

            my %private_methods      := nqp::clone(%!private_methods);
            my @private_methods      := nqp::clone(@!private_methods);
            my @private_method_names := nqp::clone(@!private_method_names);

            nqp::bindkey(%private_methods, $name, $code);
            nqp::push(@private_methods, $code);
            nqp::push(@private_method_names, $name);

            # Still has a risk of other threads reading inconsistent state here
            %!private_methods      := %private_methods;
            @!private_methods      := @private_methods;
            @!private_method_names := @private_method_names;
        });
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
