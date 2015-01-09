class Perl6::Metamodel::JavaHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Stashing
    does Perl6::Metamodel::TypePretense
    does Perl6::Metamodel::AttributeContainer
{
    has %!methods;
    has %!cache;

    my $archetypes := Perl6::Metamodel::Archetypes.new( );
    method archetypes() {
        $archetypes
    }
    
    method is_composed($obj) {
        1
    }

    method methods($obj) {
        my $iter := nqp::iterator(%!methods);
        my @methods;
        while $iter {
            my $it := nqp::shift($iter);
            @methods[+@methods] := nqp::hllizefor(nqp::iterkey_s($it), 'perl6');
        }
        @methods;
    }

    method find_method($obj, $name) {
        my $iter := nqp::iterator(%!methods);
        while $iter {
            my $it := nqp::shift($iter);
            my $methname := nqp::iterkey_s($it);
            if $methname eq nqp::unbox_s($name) {
                return nqp::iterval($it);
            }
        }
        nqp::null()
    }

    # Add a method.
    method add_method($obj, $name, $code_obj) {
        # We may get VM-level subs in here during BOOTSTRAP; the try is to cope
        # with them.
        my $method_type := "Method";
        try { $method_type := $code_obj.HOW.name($code_obj) };

        # Ensure we haven't already got it.
        if nqp::existskey(%!methods, $name) {
            nqp::die("Package '"
              ~ self.name($obj)
              ~ "' already has a "
              ~ $method_type
              ~ " '"
              ~ $name
              ~ "' (did you mean to declare a multi-method?)");
        }

        %!methods{$name} := $code_obj;

        # Adding a method means any cache is no longer authoritative.
        nqp::setmethcacheauth($obj, 0);
        %!cache := {};
    }

}
