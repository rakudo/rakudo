role Perl6::Metamodel::PrivateMethodContainer {
    has %!private_methods;
    
    # Adds a private method.
    method add_private_method($obj, $name, $code) {
        if pir::exists(%!private_methods, $name) {
            pir::die("Private method '$name' already declared in package " ~
                self.name($obj));
        }
        %!private_methods{$name} := $code;
    }
    
    # Gets the table of private methods.
    method private_method_table($obj) {
        %!private_methods
    }
    
    # Locates a private method, and hands back null if it doesn't exist.
    method find_private_method($obj, $name) {
        pir::exists(%!private_methods, $name) ??
            %!private_methods{$name} !!
            nqp::null()
    }
}
