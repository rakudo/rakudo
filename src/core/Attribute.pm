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
    #     has int $!inlined;
    #     has int $!positional_delegate;
    #     has int $!associative_delegate;
    #     has Mu $!why;
    #     has int $!required;
    #     has Mu $!container_initializer;

    method compose(Mu $package, :$compiler_services) {
        # Generate accessor method, if we're meant to have one.
        if self.has_accessor {
            my str $name   = nqp::unbox_s(self.name);
            my $meth_name := nqp::substr($name, 2);
            unless $package.^declares_method($meth_name) {
                my $dcpkg := nqp::decont($package);
                my $meth;
                my int $attr_type = nqp::objprimspec($!type);

                # Get the compiler to generate us an accessor when possible.
                if $compiler_services.DEFINITE {
                    $meth := $compiler_services.generate_accessor($meth_name,
                        $dcpkg, $name, $!type, self.rw ?? 1 !! 0);
                }

                # No compiler services available, so do it as a closure.
                elsif self.rw {
                    $meth  := nqp::p6bool(nqp::iseq_i($attr_type, 0))
                        ??
                        method (Mu:D \fles:) is raw {
                            nqp::getattr(nqp::decont(fles), $dcpkg, $name)
                        }
                        !!
                        nqp::p6bool(nqp::iseq_i($attr_type, 1))
                        ??
                        method (Mu:D \fles:) is raw {
                            nqp::getattrref_i(nqp::decont(fles), $dcpkg, $name)
                        }
                        !!
                        nqp::p6bool(nqp::iseq_i($attr_type, 2))
                        ??
                        method (Mu:D \fles:) is raw {
                            nqp::getattrref_n(nqp::decont(fles), $dcpkg, $name)
                        }
                        !!
                        method (Mu:D \fles:) is raw {
                            nqp::getattrref_s(nqp::decont(fles), $dcpkg, $name)
                        }
                    $meth.set_name($meth_name);
                } else {
                    # ro accessor
                    $meth  := nqp::p6bool(nqp::iseq_i($attr_type, 0))
                        ??
                        method (Mu:D \fles:) {
                            nqp::getattr(nqp::decont(fles), $dcpkg, $name)
                        }
                        !!
                        nqp::p6bool(nqp::iseq_i($attr_type, 1))
                        ??
                        method (Mu:D \fles:) {
                            nqp::p6box_i(
                                nqp::getattr_i(nqp::decont(fles), $dcpkg, $name)
                            );
                        }
                        !!
                        nqp::p6bool(nqp::iseq_i($attr_type, 2))
                        ??
                        method (Mu:D \fles:) {
                            nqp::p6box_n(
                                nqp::getattr_n(nqp::decont(fles), $dcpkg, $name)
                            );
                        }
                        !!
                        method (Mu:D \fles:) {
                            nqp::p6box_s(
                                nqp::getattr_s(nqp::decont(fles), $dcpkg, $name)
                            );
                        }
                    $meth.set_name($meth_name);
                }
                $package.^add_method($meth_name, $meth);
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

    method container() is raw { nqp::isnull($!auto_viv_container) ?? Nil !! $!auto_viv_container }
    method readonly() { !self.rw }
    method package() { $!package }
    method inlined() { $!inlined }
    multi method Str(Attribute:D:) { self.name }
    multi method gist(Attribute:D:) { self.type.^name ~ " " ~ self.name }

    method WHY() {
        if nqp::isnull($!why) {
            Nil
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
