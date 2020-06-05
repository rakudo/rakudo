role Perl6::Metamodel::PrivateMethodContainer {
    has %!private_methods;
    has @!private_methods;
    has @!private_method_names;

    # Adds a private method.
    method add_private_method($obj, $name, $code) {
        $name := nqp::decont_s($name);
        if nqp::existskey(%!private_methods, $name) {
            nqp::die("Private method '$name' already declared in package " ~
                self.name($obj));
        }
        %!private_methods{$name} := $code;
        nqp::push(@!private_methods, $code);
        nqp::push(@!private_method_names, $name);
    }

    # Gets the table of private methods.
    method private_method_table($obj) {
        %!private_methods
    }

    method private_methods($obj) {
        @!private_methods
    }

    method private_method_names($obj) {
        @!private_method_names
    }

    # Locates a private method, and hands back null if it doesn't exist.
    method find_private_method($obj, $name) {
        nqp::existskey(%!private_methods, $name) ??
            %!private_methods{$name} !!
            nqp::null()
    }
}

# vim: expandtab sw=4
