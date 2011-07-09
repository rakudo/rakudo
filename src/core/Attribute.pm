# Attributes for this are already set up in bootstrap.
my class Attribute {
    method compose(Mu $package) {
        # Generate accessor method, if we're meant to have one.
        if self.has_accessor {
            my $name      := self.name;
            my $meth_name := nqp::substr(nqp::unbox_s($name), 2);
            unless $package.HOW.declares_method($package, $meth_name) {
                my $dcpkg := pir::perl6_decontainerize__PP($package);
                my $meth  := self.rw
                    ??
                    method (Mu $self:) is rw {
                        nqp::getattr(
                            pir::perl6_decontainerize__PP($self),
                            $dcpkg,
                            nqp::unbox_s($name))
                    }
                    !!
                    method (Mu $self:) {
                        nqp::getattr(
                            pir::perl6_decontainerize__PP($self),
                            $dcpkg,
                            nqp::unbox_s($name))
                    };
                $package.HOW.add_method($package, $meth_name, $meth);
            }
        }
    }
    
    method get_value(Mu $obj) {
        my $decont := pir::perl6_decontainerize__PP($obj);
        given nqp::p6box_i(pir::repr_get_primitive_type_spec__IP($!type)) {
            when 0 { nqp::getattr($decont, $!package, $!name) }
            when 1 { nqp::p6box_i(nqp::getattr_i($decont, $!package, $!name)) }
            when 2 { nqp::p6box_n(nqp::getattr_n($decont, $!package, $!name)) }
            when 3 { nqp::p6box_s(nqp::getattr_s($decont, $!package, $!name)) }
        }
    }
}
