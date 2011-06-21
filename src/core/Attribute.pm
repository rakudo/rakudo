# Attributes for this are already set up in bootstrap.
my class Attribute {
    method compose(Mu $package) {
        # Generate accessor method, if we're meant to have one.
        if self.has_accessor {
            my $name  := self.name;
            my $dcpkg := pir::perl6_decontainerize__PP($package);
            my $meth  := self.rw
                ??
                method (Mu $self:) {
                    pir::getattribute__PPPs(
                        pir::perl6_decontainerize__PP($self),
                        $dcpkg,
                        nqp::unbox_s($name))
                }
                !!
                method (Mu $self:) {
                    pir::perl6_decontainerize__PP(
                        pir::getattribute__PPPs(
                            pir::perl6_decontainerize__PP($self),
                            $dcpkg,
                            nqp::unbox_s($name)))
                };
            $package.HOW.add_method($package, pir::substr__SSI(
                nqp::unbox_s($name), nqp::unbox_i(2)),
                $meth);
        }
    }
}
