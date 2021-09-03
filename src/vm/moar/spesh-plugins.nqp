## Return value decontainerization plugin

# Often we have nothing at all to do, in which case we can make it a no-op.
# Other times, we need a decont. In a few, we need to re-wrap it.

sub identity($obj) { $obj }

{
    # We look up Iterable when the plugin is used.
    my $Iterable := nqp::null();

    sub mu($replaced) { Mu }
    sub decont($obj) { nqp::decont($obj) }
    sub recont($obj) {
        my $rc := nqp::create(Scalar);
        nqp::bindattr($rc, Scalar, '$!value', nqp::decont($obj));
        $rc
    }
    sub decontrv($cont) {
        if nqp::isrwcont($cont) {
            # It's an RW container, so we really need to decont it.
            my $rv := nqp::decont($cont);
            if nqp::istype($rv, $Iterable) {
                my $rc := nqp::create(Scalar);
                nqp::bindattr($rc, Scalar, '$!value', $rv);
                $rc
            }
            else {
                $rv
            }
        }
        else {
            # A read-only container, so just return it.
            $cont
        }
    }

    sub decontrv_plugin($rv) {
        $Iterable := nqp::gethllsym('Raku', 'Iterable') if nqp::isnull($Iterable);
        nqp::speshguardtype($rv, nqp::what_nd($rv));
        if nqp::isconcrete_nd($rv) && nqp::iscont($rv) {
            # Guard that it's concrete, so this only applies for container
            # instances.
            nqp::speshguardconcrete($rv);

            # If it's a Scalar container then we can optimize further.
            if nqp::eqaddr(nqp::what_nd($rv), Scalar) {
                # Grab the descriptor.
                my $desc := nqp::speshguardgetattr($rv, Scalar, '$!descriptor');
                if nqp::isconcrete($desc) {
                    # Descriptor, so `rw`. Guard on type of value. If it's
                    # Iterable, re-containerize. If not, just decont.
                    nqp::speshguardconcrete($desc);
                    my $value := nqp::speshguardgetattr($rv, Scalar, '$!value');
                    nqp::speshguardtype($value, nqp::what_nd($value));
                    return nqp::istype($value, $Iterable) ?? &recont !! &decont;
                }
                else {
                    # No descriptor, so it's already readonly. Identity.
                    nqp::speshguardtypeobj($desc);
                    return &identity;
                }
            }

            # Otherwise, full decont.
            return &decontrv;
        }
        else {
            # No decontainerization to do, so just produce identity or, if
            # it's null turn it into a Mu.
            unless nqp::isconcrete($rv) {
                # Needed as a container's type object is not a container, but a
                # concrete instance would be.
                nqp::speshguardtypeobj($rv);
            }
            return nqp::isnull($rv) ?? &mu !! &identity;
        }
    }

    nqp::speshreg('Raku', 'decontrv', &decontrv_plugin);
    nqp::speshreg('Raku', 'decontrv_6c', -> $rv {
        # This emulates a bug where Proxy was never decontainerized no
        # matter what. The ecosystem came to depend on that, so we will
        # accept it for now. We need to revisit this in the future.
        if nqp::eqaddr(nqp::what_nd($rv), Proxy) && nqp::isconcrete_nd($rv) {
            nqp::speshguardtype($rv, nqp::what_nd($rv));
            nqp::speshguardconcrete($rv);
            &identity
        }
        else {
            decontrv_plugin($rv)
        }
    });
}

## Return value type check plugin

{
    sub coercion_error($from_name, $to_name) {
        nqp::die("Unable to coerce the return value from $from_name to $to_name; " ~
            "no coercion method defined");
    }

    sub return_error($got, $wanted) {
        Perl6::Metamodel::Configuration.throw_or_die(
            'X::TypeCheck::Return',
            "Type check failed for return value; expected '" ~
                $wanted.HOW.name($wanted) ~ "' but got '" ~
                $got.HOW.name($got) ~ "'",
            :$got,
            :expected($wanted)
        );
    }

    sub make-unchecked-coercion($rv, $coerce_to) {
        # We already have the type fixed, so we can resolve to the coercion
        # method if available.
        my $name := $coerce_to.HOW.name($coerce_to);
        my $meth := nqp::tryfindmethod($rv, $name);
        return nqp::isnull($meth)
            ?? -> $ret { coercion_error($ret.HOW.name($ret), $name) }
            !! $meth;
    }

    sub check_type_typeobj($type, $orig_type) {
        -> $ret {
            nqp::istype($ret, $type) && !nqp::isconcrete($ret) || nqp::istype($ret, Nil)
                ?? $ret
                !! return_error($ret, $orig_type)
        }
    }
    sub check_type_concrete($type, $orig_type) {
        -> $ret {
            nqp::istype($ret, $type) && nqp::isconcrete($ret) || nqp::istype($ret, Nil)
                ?? $ret
                !! return_error($ret, $orig_type)
        }
    }
    sub check_type($type, $orig_type) {
        -> $ret {
            nqp::istype($ret, $type) || nqp::istype($ret, Nil)
                ?? $ret
                !! return_error($ret, $orig_type)
        }
    }

    sub check_type_typeobj_coerce($type, $name, $orig_type) {
        -> $ret {
            nqp::istype($ret, $type) && !nqp::isconcrete($ret)
                ?? (nqp::isnull(my $cmeth := nqp::tryfindmethod($ret, $name))
                        ?? coercion_error($ret.HOW.name($ret), $name)
                        !! $cmeth($ret))
                !! return_error($ret, $orig_type)
        }
    }
    sub check_type_concrete_coerce($type, $name, $orig_type) {
        -> $ret {
            nqp::istype($ret, $type) && nqp::isconcrete($ret)
                ?? (nqp::isnull(my $cmeth := nqp::tryfindmethod($ret, $name))
                        ?? coercion_error($ret.HOW.name($ret), $name)
                        !! $cmeth($ret))
                !! return_error($ret, $orig_type)
        }
    }
    sub check_type_coerce($type, $name, $orig_type) {
        -> $ret {
            nqp::istype($ret, $type) || nqp::istype($ret, Nil)
                ?? (nqp::isnull(my $cmeth := nqp::tryfindmethod($ret, $name))
                        ?? coercion_error($ret.HOW.name($ret), $name)
                        !! $cmeth($ret))
                !! return_error($ret, $orig_type)
        }
    }

    nqp::speshreg('Raku', 'typecheckrv', sub ($rv, $type) {
        my $orig_type := $type;

        # If the type is Mu or unset, then we can resolve to identity.
        if nqp::isnull($type) || $type =:= Mu {
            return &identity;
        }

        # Gather information about coercive and definite types, and resolve
        # to the base type.
        my $coerce_to := nqp::null();
        my int $definite_check := -1;
        if $type.HOW.archetypes.coercive {
            $coerce_to := $type.HOW.target_type($type);
            $type := $type.HOW.constraint_type($type);
        }
        if $type.HOW.archetypes.definite {
            $definite_check := $type.HOW.definite($type);
            $type := $type.HOW.base_type($type);
        }

        # See if the return value is containerized; if not, we can do some
        # guarding/checking and maybe toss the checks altogether.
        unless nqp::iscont($rv) {
            if $type.HOW.archetypes.nominal &&
                    # Allow through Nil/Failure
                    (nqp::istype($rv, Nil) || (nqp::istype($rv, $type) &&
                    # Enforce definite checks.
                    ($definite_check == 0 ?? !nqp::isconcrete($rv) !!
                     $definite_check == 1 ?? nqp::isconcrete($rv) !! 1))) {
                # Type matches; add a type guard and we can elide checking
                # that.
                nqp::speshguardtype($rv, $rv.WHAT);

                # If there's a definedness check, add guards for those too.
                if $definite_check == 0 {
                    nqp::speshguardtypeobj($rv);
                }
                elsif $definite_check == 1 && !nqp::istype($rv, Nil) {
                    nqp::speshguardconcrete($rv);
                }

                # Now it's either an unchecked coercion or identity.
                return nqp::isnull($coerce_to)
                    ?? &identity
                    !! make-unchecked-coercion($rv, $coerce_to);
            }
        }

        # If we get here, we've got a case we can't simplify much. Pick an
        # appropriate variant that will do the type checks and coercions as
        # needed.
        if nqp::isnull($coerce_to) {
            return $definite_check == 0 ?? check_type_typeobj($type, $orig_type) !!
                   $definite_check == 1 ?? check_type_concrete($type, $orig_type) !!
                                           check_type($type, $orig_type);
        }
        else {
            my $name := $coerce_to.HOW.name($coerce_to);
            return $definite_check == 0 ?? check_type_typeobj_coerce($type, $name, $orig_type) !!
                   $definite_check == 1 ?? check_type_concrete_coerce($type, $name, $orig_type) !!
                                           check_type_coerce($type, $name, $orig_type);
        }
    });
}


## Assignment plugin

# We case-analyze assignments and provide these optimized paths for a range of
# common situations.
sub assign-type-error($desc, $value) {
    Perl6::Metamodel::Configuration.throw_or_die(
        'X::TypeCheck::Assignment',
        "Type check failed in assignment",
        :symbol($desc.name),
        :got($value),
        :expected($desc.of)
    );
}
sub assign-fallback($cont, $value) {
    nqp::assign($cont, $value)
}
sub assign-scalar-no-whence-no-typecheck($cont, $value) {
    nqp::bindattr($cont, Scalar, '$!value', $value);
}
sub assign-scalar-nil-no-whence($cont, $value) {
    my $desc := nqp::getattr($cont, Scalar, '$!descriptor');
    nqp::bindattr($cont, Scalar, '$!value',
        nqp::getattr($desc, ContainerDescriptor, '$!default'))
}
sub assign-scalar-no-whence($cont, $value) {
    my $desc := nqp::getattr($cont, Scalar, '$!descriptor');
    my $type := nqp::getattr($desc, ContainerDescriptor, '$!of');
    if nqp::istype($value, $type) {
        if $type.HOW.archetypes.coercive {
            $value := $type.HOW.coerce($type, $value);
        }
        nqp::bindattr($cont, Scalar, '$!value', $value);
    }
    else {
        assign-type-error($desc, $value);
    }
}
sub assign-scalar-bindpos-no-typecheck($cont, $value) {
    nqp::bindattr($cont, Scalar, '$!value', $value);
    my $desc := nqp::getattr($cont, Scalar, '$!descriptor');
    nqp::bindpos(
        nqp::getattr($desc, ContainerDescriptor::BindArrayPos, '$!target'),
        nqp::getattr_i($desc, ContainerDescriptor::BindArrayPos, '$!pos'),
        $cont);
    nqp::bindattr($cont, Scalar, '$!descriptor',
        nqp::getattr($desc, ContainerDescriptor::BindArrayPos, '$!next-descriptor'));
}
sub assign-scalar-bindpos($cont, $value) {
    my $desc := nqp::getattr($cont, Scalar, '$!descriptor');
    my $next := nqp::getattr($desc, ContainerDescriptor::BindArrayPos, '$!next-descriptor');
    my $type := nqp::getattr($next, ContainerDescriptor, '$!of');
    if nqp::istype($value, $type) {
        if $type.HOW.archetypes.coercive {
            $value := $type.HOW.coerce($type, $value);
        }
        nqp::bindattr($cont, Scalar, '$!value', $value);
        nqp::bindpos(
            nqp::getattr($desc, ContainerDescriptor::BindArrayPos, '$!target'),
            nqp::getattr_i($desc, ContainerDescriptor::BindArrayPos, '$!pos'),
            $cont);
        nqp::bindattr($cont, Scalar, '$!descriptor', $next);
    }
    else {
        assign-type-error($next, $value);
    }
}

# Assignment to a $ sigil variable, usually Scalar.
nqp::speshreg('Raku', 'assign', sub ($cont, $value) {
    # Whatever we do, we'll guard on the type of the container and its
    # concreteness.
    nqp::speshguardtype($cont, nqp::what_nd($cont));
    nqp::isconcrete_nd($cont)
        ?? nqp::speshguardconcrete($cont)
        !! nqp::speshguardtypeobj($cont);

    # We have various fast paths for an assignment to a Scalar.
    if nqp::eqaddr(nqp::what_nd($cont), Scalar) && nqp::isconcrete_nd($cont) {
        # Now see what the Scalar descriptor type is.
        my $desc := nqp::speshguardgetattr($cont, Scalar, '$!descriptor');
        if nqp::eqaddr($desc.WHAT, ContainerDescriptor) && nqp::isconcrete($desc) {
            # Simple assignment, no whence. But is Nil being assigned?
            my $of := $desc.of;
            if nqp::eqaddr($value, Nil) {
                # Yes; just copy in the default, provided we've a simple type.
                if $of.HOW.archetypes.nominal {
                    nqp::speshguardtype($value, $value.WHAT);
                    nqp::speshguardtype($desc, $desc.WHAT);
                    nqp::speshguardconcrete($desc);
                    my $of := nqp::speshguardgetattr($desc, ContainerDescriptor, '$!of');
                    nqp::speshguardobj($of);
                    return &assign-scalar-nil-no-whence;
                }
            }
            else {
                # No whence, no Nil. Is it a nominal type? If yes, we can check
                # it here.
                unless $of.HOW.archetypes.nominal {
                    nqp::speshguardobj($desc);
                    return &assign-scalar-no-whence;
                }
                if nqp::istype($value, $of) {
                    nqp::speshguardtype($desc, $desc.WHAT);
                    nqp::speshguardconcrete($desc);
                    my $of := nqp::speshguardgetattr($desc, ContainerDescriptor, '$!of');
                    nqp::speshguardobj($of);
                    nqp::speshguardtype($value, $value.WHAT);
                    return &assign-scalar-no-whence-no-typecheck;
                }
                else {
                    # Will fail the type check and error always.
                    return &assign-scalar-no-whence;
                }
            }
        }
        elsif nqp::eqaddr($desc.WHAT, ContainerDescriptor::Untyped) && nqp::isconcrete($desc) {
            # Assignment to an untyped container descriptor; handle this
            # more simply.
            if nqp::eqaddr($value, Nil) {
                # Nil case is NYI.
            }
            else {
                nqp::speshguardtype($desc, $desc.WHAT);
                nqp::speshguardnotobj($value, Nil);
                return &assign-scalar-no-whence-no-typecheck;
            }
        }
        elsif nqp::eqaddr($desc.WHAT, ContainerDescriptor::BindArrayPos) {
            # Bind into an array. We can produce a fast path for this, though
            # should check what the ultimate descriptor is. It really should
            # be a normal, boring, container descriptor.
            nqp::speshguardtype($desc, $desc.WHAT);
            nqp::speshguardconcrete($desc);
            my $next := nqp::speshguardgetattr($desc, ContainerDescriptor::BindArrayPos,
                '$!next-descriptor');
            if nqp::eqaddr($next.WHAT, ContainerDescriptor) {
                # Ensure we're not assigning Nil. (This would be very odd, as
                # a Scalar starts off with its default value, and if we are
                # vivifying we'll likely have a new container).
                unless nqp::eqaddr($value.WHAT, Nil) {
                    # Go by whether we can type check the target.
                    nqp::speshguardobj($next);
                    nqp::speshguardtype($value, $value.WHAT);
                    my $of := $next.of;
                    if $of.HOW.archetypes.nominal &&
                            (nqp::eqaddr($of, Mu) || nqp::istype($value, $of)) {
                        return &assign-scalar-bindpos-no-typecheck;
                    }
                    else {
                        # No whence, not a Nil, but still need to type check
                        # (perhaps subset type, perhaps error).
                        return &assign-scalar-bindpos;
                    }
                }
            }
        }
    }

    # If we get here, then we didn't have a specialized case to put in
    # place.
    return &assign-fallback;
});

# vim: expandtab sw=4
