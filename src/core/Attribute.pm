# Attributes for this are already set up in bootstrap.
my class Attribute {
    method compose(Mu $package) {
        # Generate accessor method, if we're meant to have one.
        if self.has_accessor {
            my $name      := self.name;
            my $meth_name := nqp::substr(nqp::unbox_s($name), 2);
            unless $package.HOW.declares_method($package, $meth_name) {
                my $dcpkg := nqp::p6decont($package);
                my $meth;
                my int $attr_type = pir::repr_get_primitive_type_spec__IP($!type);
                if self.rw {
                    $meth  := nqp::p6bool(nqp::iseq_i($attr_type, 0))
                        ??
                        method (Mu $self:) is rw {
                            nqp::getattr(
                                nqp::p6decont($self),
                                $dcpkg,
                                nqp::unbox_s($name))
                        }
                        !!
                        pir::die("Cannot create rw-accessors for natively typed attribute '$name'");
                } else {
                    # ro accessor
                    $meth  := nqp::p6bool(nqp::iseq_i($attr_type, 0))
                        ??
                        method (Mu $self:) {
                            nqp::getattr(
                                nqp::p6decont($self),
                                $dcpkg,
                                nqp::unbox_s($name))
                        }
                        !!
                        nqp::p6bool(nqp::iseq_i($attr_type, 1))
                        ??
                        method (Mu $self:) {
                            nqp::p6box_i(
                                nqp::getattr_i(
                                    nqp::p6decont($self),
                                    $dcpkg,
                                    nqp::unbox_s($name))
                            );
                        }
                        !!
                        nqp::p6bool(nqp::iseq_i($attr_type, 2))
                        ??
                        method (Mu $self:) {
                            nqp::p6box_n(
                                nqp::getattr_n(
                                    nqp::p6decont($self),
                                    $dcpkg,
                                    nqp::unbox_s($name))
                            );
                        }
                        !!
                        method (Mu $self:) {
                            nqp::p6box_s(
                                nqp::getattr_s(
                                    nqp::p6decont($self),
                                    $dcpkg,
                                    nqp::unbox_s($name))
                            );
                        }

                }
                $meth.set_name($meth_name);
                $package.HOW.add_method($package, $meth_name, $meth);
            }
        }
        
        # Apply any handles trait we may have.
        self.apply_handles($package);
    }
    
    method apply_handles(Mu $pkg) {
        # None by default.
    }
    
    method get_value(Mu $obj) {
        my $decont := nqp::p6decont($obj);
        given nqp::p6box_i(pir::repr_get_primitive_type_spec__IP($!type)) {
            when 0 { nqp::getattr($decont, $!package, $!name) }
            when 1 { nqp::p6box_i(nqp::getattr_i($decont, $!package, $!name)) }
            when 2 { nqp::p6box_n(nqp::getattr_n($decont, $!package, $!name)) }
            when 3 { nqp::p6box_s(nqp::getattr_s($decont, $!package, $!name)) }
        }
    }
    
    method set_value(Mu $obj, Mu \$value) {
        my $decont := nqp::p6decont($obj);
        given nqp::p6box_i(pir::repr_get_primitive_type_spec__IP($!type)) {
            when 0 { nqp::bindattr($decont, $!package, $!name, $value) }
            when 1 { nqp::p6box_i(nqp::bindattr_i($decont, $!package, $!name, $value)) }
            when 2 { nqp::p6box_n(nqp::bindattr_n($decont, $!package, $!name, $value)) }
            when 3 { nqp::p6box_s(nqp::bindattr_s($decont, $!package, $!name, $value)) }
        }
    }
    
    method container() is rw { nqp::isnull($!auto_viv_container) ?? Mu !! $!auto_viv_container }
    method has-accessor() { ?$!has_accessor }
    method readonly() { !self.rw }
    method package() { $!package }
    multi method Str(Attribute:D:) { self.name }
    multi method gist(Attribute:D:) { self.name }
}
