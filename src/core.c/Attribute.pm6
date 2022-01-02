my class Attribute { # declared in BOOTSTRAP
    # class Attribute is Any
    #     has str $!name;
    #     has int $!rw;
    #     has int $!is_built;
    #     has int $!is_bound;
    #     has int $!has_accessor;
    #     has Mu $!type;
    #     has Mu $!container_descriptor;
    #     has Mu $!auto_viv_container;
    #     has Mu $!build_closure;
    #     has Mu $!package;
    #     has int $!inlined;
    #     has Mu $!dimensions;
    #     has int $!positional_delegate;
    #     has int $!associative_delegate;
    #     has Mu $!why;
    #     has $!required;
    #     has Mu $!container_initializer;
    #     has Attribute $!original;
    #     has Attribute $!composed;

    method compose(Mu $package, :$compiler_services) {
        return if $!composed;
        my $dcpkg := nqp::decont($package);
        nqp::bindattr(self, Attribute, '$!package', $dcpkg);
        # Generate accessor method, if we're meant to have one.
        if self.has_accessor {
            my str $name   = nqp::unbox_s(self.name);
            my $meth_name := nqp::substr($name, 2);
            unless nqp::existskey($package.^method_table, $meth_name)
                    || nqp::existskey($package.^submethod_table, $meth_name)
                    || (nqp::can($package.HOW, 'has_multi_candidate')
                        && $package.^has_multi_candidate($meth_name))
            {
                my $meth;
                my int $attr_type = nqp::objprimspec($!type);

                if nqp::can(self,"DEPRECATED")
                  && self.DEPRECATED -> $alternative {
                    my $what = "Method $meth_name (from $package.^name())";
                    if self.rw {
                        $meth := nqp::iseq_i($attr_type, 0)
                            ??
                            method (Mu:D \fles:) is raw {
                                Rakudo::Deprecations.DEPRECATED($alternative,:$what);
                                nqp::getattr(nqp::decont(fles), $dcpkg, $name)
                            }
                            !!
                            nqp::iseq_i($attr_type, 1)
                            ??
                            method (Mu:D \fles:) is raw {
                                Rakudo::Deprecations.DEPRECATED($alternative,:$what);
                                nqp::getattrref_i(nqp::decont(fles), $dcpkg, $name)
                            }
                            !!
                            nqp::iseq_i($attr_type, 2)
                            ??
                            method (Mu:D \fles:) is raw {
                                Rakudo::Deprecations.DEPRECATED($alternative,:$what);
                                nqp::getattrref_n(nqp::decont(fles), $dcpkg, $name)
                            }
                            !!
                            method (Mu:D \fles:) is raw {
                                Rakudo::Deprecations.DEPRECATED($alternative,:$what);
                                nqp::getattrref_s(nqp::decont(fles), $dcpkg, $name)
                            }
                        $meth.set_name($meth_name);
                    }
                    else { # DEPRECATED ro accessor
                        $meth := nqp::iseq_i($attr_type, 0)
                            ??
                            method (Mu:D \fles:) {
                                Rakudo::Deprecations.DEPRECATED($alternative,:$what);
                                nqp::getattr(nqp::decont(fles), $dcpkg, $name)
                            }
                            !!
                            nqp::iseq_i($attr_type, 1)
                            ??
                            method (Mu:D \fles:) {
                                Rakudo::Deprecations.DEPRECATED($alternative,:$what);
                                nqp::p6box_i(
                                    nqp::getattr_i(nqp::decont(fles), $dcpkg, $name)
                                );
                            }
                            !!
                            nqp::iseq_i($attr_type, 2)
                            ??
                            method (Mu:D \fles:) {
                                Rakudo::Deprecations.DEPRECATED($alternative,:$what);
                                nqp::p6box_n(
                                    nqp::getattr_n(nqp::decont(fles), $dcpkg, $name)
                                );
                            }
                            !!
                            method (Mu:D \fles:) {
                                Rakudo::Deprecations.DEPRECATED($alternative,:$what);
                                nqp::p6box_s(
                                    nqp::getattr_s(nqp::decont(fles), $dcpkg, $name)
                                );
                            }
                        $meth.set_name($meth_name);
                    }
                }
                # Get the compiler to generate us an accessor when possible.
                elsif $compiler_services.DEFINITE {
                    $meth := $compiler_services.generate_accessor($meth_name,
                        $dcpkg, $name, $!type, self.rw ?? 1 !! 0);
                }

                # No compiler services available, so do it as a closure.
                elsif self.rw {
                    $meth := nqp::iseq_i($attr_type, 0)
                        ??
                        method (Mu:D \fles:) is raw {
                            nqp::getattr(nqp::decont(fles), $dcpkg, $name)
                        }
                        !!
                        nqp::iseq_i($attr_type, 1)
                        ??
                        method (Mu:D \fles:) is raw {
                            nqp::getattrref_i(nqp::decont(fles), $dcpkg, $name)
                        }
                        !!
                        nqp::iseq_i($attr_type, 2)
                        ??
                        method (Mu:D \fles:) is raw {
                            nqp::getattrref_n(nqp::decont(fles), $dcpkg, $name)
                        }
                        !!
                        method (Mu:D \fles:) is raw {
                            nqp::getattrref_s(nqp::decont(fles), $dcpkg, $name)
                        }
                    $meth.set_name($meth_name);
                }
                else { # ro accessor
                    $meth := nqp::iseq_i($attr_type, 0)
                        ??
                        method (Mu:D \fles:) {
                            nqp::getattr(nqp::decont(fles), $dcpkg, $name)
                        }
                        !!
                        nqp::iseq_i($attr_type, 1)
                        ??
                        method (Mu:D \fles:) {
                            nqp::p6box_i(
                                nqp::getattr_i(nqp::decont(fles), $dcpkg, $name)
                            );
                        }
                        !!
                        nqp::iseq_i($attr_type, 2)
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

        nqp::bindattr_i(self, Attribute, '$!composed', 1);

        # Apply any handles trait we may have.
        self.apply_handles($package);
    }

    method apply_handles(Mu $pkg) {
        # None by default.
    }

    method get_value(Mu $obj) is raw {
        (my int $t = nqp::objprimspec($!type))
          ?? nqp::iseq_i($t,1)
            ?? nqp::getattr_i(nqp::decont($obj),$!package,$!name)
            !! nqp::iseq_i($t,2)
              ?? nqp::getattr_n(nqp::decont($obj),$!package,$!name)
              !! nqp::getattr_s(nqp::decont($obj),$!package,$!name) # assume t=3
          !! nqp::getattr(nqp::decont($obj),$!package,$!name)
    }

    method set_value(Mu $obj, Mu \value) is raw {
        (my int $t = nqp::objprimspec($!type))
          ?? nqp::iseq_i($t,1)
            ?? nqp::bindattr_i(nqp::decont($obj),$!package,$!name,value)
            !! nqp::iseq_i($t,2)
              ?? nqp::bindattr_n(nqp::decont($obj),$!package,$!name,value)
              !! nqp::bindattr_s(nqp::decont($obj),$!package,$!name,value) # t=3
          !! nqp::bindattr(nqp::decont($obj),$!package,$!name,value)
    }

    method container() is raw { nqp::ifnull($!auto_viv_container,Nil) }
    method readonly() { !self.rw }
    method package() { $!package }
    method inlined() { $!inlined }
    method dimensions() { $!dimensions } # turn list_i into List
    multi method Str(Attribute:D:) { self.name }
    multi method gist(Attribute:D:) { self.type.^name ~ " " ~ self.name }

    method WHY() {
        if nqp::isnull($!why) {
            nextsame
        } else {
            $!why.set_docee(self);
            $!why
        }
    }

    method set_why($why) {
        $!why := $why;
    }
}

# does trait
multi sub trait_mod:<does>(Attribute:D $a, Mu:U $role) {
    if $role.HOW.archetypes.composable() {
        nqp::getattr($a,Attribute,'$!auto_viv_container').VAR
          does $role;
    }
    elsif $role.HOW.archetypes.composalizable() {
        nqp::getattr($a,Attribute,'$!auto_viv_container').VAR
          does $role.HOW.composalize($role);
    }
    else {
        X::Composition::NotComposable.new(
            target-name => 'an attribute',
            composer    => $role,
        ).throw;
    }
}

multi sub trait_mod:<is>(Attribute:D $a, :$built!) {
    if nqp::istype($built,Bool) {
        nqp::bindattr_i($a,Attribute,'$!is_built',+$built);
    }
    elsif nqp::istype($built,Pair) {
        if $built.key eq 'bind' {
            nqp::bindattr_i($a,Attribute,'$!is_built',1);
            nqp::bindattr_i($a,Attribute,'$!is_bound',+$built.value);
        }
        else {
            die "Don't know how to handle 'is built($built.raku())' trait";
        }
    }
    else {
        die "Don't know how to handle 'is built($built.raku())' trait";
    }
}

# vim: expandtab shiftwidth=4
