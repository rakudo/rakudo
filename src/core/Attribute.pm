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
}
