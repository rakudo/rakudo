my class Attribute { # declared in BOOTSTRAP
    # class Attribute is Any {
    #     has str $!name;
    #     has int $!rw;
    #     has int $!has_accessor;
    #     has Mu $!type;
    #     has Mu $!container_descriptor;
    #     has Mu $!auto_viv_container;
    #     has Mu $!build_closure;
    #     has Mu $!package;
    #     has int $!positional_delegate;
    #     has int $!associative_delegate;
    #     has Mu $!why;

    method compose(Mu $package) {
        # Generate accessor method, if we're meant to have one.
        if self.has_accessor {
            my str $name   = nqp::unbox_s(self.name);
            my $meth_name := nqp::substr($name, 2);
            unless $package.HOW.declares_method($package, $meth_name) {
                my $dcpkg := nqp::decont($package);
                my $meth;
                my int $attr_type = nqp::objprimspec($!type);
                if self.rw {
                    $meth  := nqp::p6bool(nqp::iseq_i($attr_type, 0))
                        ??
                        method (Mu \fles:) is rw {
                            nqp::getattr(nqp::decont(fles), $dcpkg, $name)
                        }
                        !!
                        nqp::p6bool(nqp::iseq_i($attr_type, 1))
                        ??
                        method (Mu \fles:) is rw {
                            nqp::getattrref_i(nqp::decont(fles), $dcpkg, $name)
                        }
                        !!
                        nqp::p6bool(nqp::iseq_i($attr_type, 2))
                        ??
                        method (Mu \fles:) is rw {
                            nqp::getattrref_n(nqp::decont(fles), $dcpkg, $name)
                        }
                        !!
                        method (Mu \fles:) is rw {
                            nqp::getattrref_s(nqp::decont(fles), $dcpkg, $name)
                        }
                } else {
                    # ro accessor
                    $meth  := nqp::p6bool(nqp::iseq_i($attr_type, 0))
                        ??
                        method (Mu \fles:) {
                            nqp::getattr(nqp::decont(fles), $dcpkg, $name)
                        }
                        !!
                        nqp::p6bool(nqp::iseq_i($attr_type, 1))
                        ??
                        method (Mu \fles:) {
                            nqp::p6box_i(
                                nqp::getattr_i(nqp::decont(fles), $dcpkg, $name)
                            );
                        }
                        !!
                        nqp::p6bool(nqp::iseq_i($attr_type, 2))
                        ??
                        method (Mu \fles:) {
                            nqp::p6box_n(
                                nqp::getattr_n(nqp::decont(fles), $dcpkg, $name)
                            );
                        }
                        !!
                        method (Mu \fles:) {
                            nqp::p6box_s(
                                nqp::getattr_s(nqp::decont(fles), $dcpkg, $name)
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
        my $decont := nqp::decont($obj);
        given nqp::p6box_i(nqp::objprimspec($!type)) {
            when 0 { nqp::getattr($decont, $!package, $!name) }
            when 1 { nqp::p6box_i(nqp::getattr_i($decont, $!package, $!name)) }
            when 2 { nqp::p6box_n(nqp::getattr_n($decont, $!package, $!name)) }
            when 3 { nqp::p6box_s(nqp::getattr_s($decont, $!package, $!name)) }
        }
    }

    method set_value(Mu $obj, Mu \value) {
        my $decont := nqp::decont($obj);
        given nqp::p6box_i(nqp::objprimspec($!type)) {
            when 0 { nqp::bindattr($decont, $!package, $!name, value) }
            when 1 { nqp::p6box_i(nqp::bindattr_i($decont, $!package, $!name, value)) }
            when 2 { nqp::p6box_n(nqp::bindattr_n($decont, $!package, $!name, value)) }
            when 3 { nqp::p6box_s(nqp::bindattr_s($decont, $!package, $!name, value)) }
        }
    }

    method container() is rw { nqp::isnull($!auto_viv_container) ?? Mu !! $!auto_viv_container }
    method has-accessor() { ?$!has_accessor }
    method readonly() { !self.rw }
    method package() { $!package }
    multi method Str(Attribute:D:) { self.name }
    multi method gist(Attribute:D:) { self.type.^name ~ " " ~ self.name }

    method WHY() {
        if nqp::isnull($!why) {
            Any
        } else {
            $!why.set_docee(self);
            $!why
        }
    }

    method set_why($why) {
        $!why := $why;
    }
}

# vim: ft=perl6 expandtab sw=4
