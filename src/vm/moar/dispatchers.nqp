# Return value decontainerization dispatcher. Often we have nothing at all
# to do, in which case we can make it identity. Other times, we need a
# decont. In a few, we need to re-wrap it.
{
    my $container-fallback := -> $Iterable, $cont {
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

    my $recont := -> $obj {
        my $rc := nqp::create(Scalar);
        nqp::bindattr($rc, Scalar, '$!value', nqp::decont($obj));
        $rc
    }

    nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-rv-decont', -> $capture {
        # We always need to guard on type and concreteness.
        my $rv := nqp::captureposarg($capture, 0);
        my $rv_arg := nqp::dispatch('boot-syscall', 'dispatcher-track-arg',
                $capture, 0);
        nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $rv_arg);
        nqp::dispatch('boot-syscall', 'dispatcher-guard-concreteness', $rv_arg);

        # Is it a container?
        if nqp::isconcrete_nd($rv) && nqp::iscont($rv) {
            # It's a container. We have special cases for Scalar.
            if nqp::istype_nd($rv, Scalar) {
                # Check if the descriptor is undefined, in which case it's read-only.
                my $desc := nqp::getattr($rv, Scalar, '$!descriptor');
                my $desc_arg := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                        $rv_arg, Scalar, '$!descriptor');
                nqp::dispatch('boot-syscall', 'dispatcher-guard-concreteness', $desc_arg);
                if nqp::isconcrete($desc) {
                    # Writeable, so we may need to recontainerize the value if
                    # the type is iterable, otherwise we can decont it.
                    my $value := nqp::getattr($rv, Scalar, '$!value');
                    my $value_arg := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                        $rv_arg, Scalar, '$!value');
                    nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $value_arg);
                    if nqp::istype_nd($value, nqp::gethllsym('Raku', 'Iterable')) {
                        # Need to recont in order to preserve item nature.
                        # Shuffle in the recont code to invoke. We already
                        # read the deconted value, so we insert that as the
                        # arg so it needn't be dereferenced again.
                        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-code-constant',
                            nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                                nqp::dispatch('boot-syscall', 'dispatcher-insert-arg',
                                    nqp::dispatch('boot-syscall', 'dispatcher-drop-arg',
                                        $capture, 0),
                                    0, $value_arg),
                                0, $recont));
                    }
                    else {
                        # Decont, so just evaluate to the read attr (boot-value
                        # ignores all put the first argument).
                        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-value',
                            nqp::dispatch('boot-syscall', 'dispatcher-insert-arg',
                                $capture, 0, $value_arg));
                    }
                }
                else {
                    # Read-only, so identity will do.
                    nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-value', $capture);
                }
            }
            else {
                # Delegate to non-Scalar container fallback.
                nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-code-constant',
                    nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                        nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                            $capture, 0, nqp::gethllsym('Raku', 'Iterable')),
                        0, $container-fallback));
            }
        }
        else {
            # Not containerizied, so identity shall do.
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-value', $capture);
        }
    });

    nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-rv-decont-6c', -> $capture {
        # This emulates a bug where Proxy was never decontainerized no
        # matter what. The ecosystem came to depend on that, so we will
        # accept it for now. We need to revisit this in the future.
        my $rv := nqp::captureposarg($capture, 0);
        if nqp::eqaddr(nqp::what_nd($rv), Proxy) && nqp::isconcrete_nd($rv) {
            my $rv_arg := nqp::dispatch('boot-syscall', 'dispatcher-track-arg',
                    $capture, 0);
            nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $rv_arg);
            nqp::dispatch('boot-syscall', 'dispatcher-guard-concreteness', $rv_arg);
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-value', $capture);
        }
        else {
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-rv-decont', $capture);
        }
    });
}

# Return value type-check dispatcher. The first value is the return value,
# the second is the type that is expected, which may be a definiteness or
# coercion type.
{
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

    my $check_type_typeobj := -> $ret, $orig_type, $type {
        nqp::istype($ret, $type) && !nqp::isconcrete($ret) || nqp::istype($ret, Nil)
            ?? $ret
            !! return_error($ret, $orig_type)
    }

    my $check_type_concrete := -> $ret, $orig_type, $type {
        nqp::istype($ret, $type) && nqp::isconcrete($ret) || nqp::istype($ret, Nil)
            ?? $ret
            !! return_error($ret, $orig_type)
    }

    my $check_type := -> $ret, $orig_type, $type {
        nqp::istype($ret, $type) || nqp::istype($ret, Nil)
            ?? $ret
            !! return_error($ret, $orig_type)
    }

    my $check_type_typeobj_coerce := -> $ret, $orig_type, $type, $name {
        nqp::istype($ret, $type) && !nqp::isconcrete($ret)
            ?? (nqp::isnull(my $cmeth := nqp::tryfindmethod($ret, $name))
                    ?? coercion_error($ret.HOW.name($ret), $name)
                    !! $cmeth($ret))
            !! return_error($ret, $orig_type)
    }

    my $check_type_concrete_coerce := -> $ret, $orig_type, $type, $name {
        nqp::istype($ret, $type) && nqp::isconcrete($ret)
            ?? (nqp::isnull(my $cmeth := nqp::tryfindmethod($ret, $name))
                    ?? coercion_error($ret.HOW.name($ret), $name)
                    !! $cmeth($ret))
            !! return_error($ret, $orig_type)
    }

    my $check_type_coerce := -> $ret, $orig_type, $type, $name {
        nqp::istype($ret, $type) || nqp::istype($ret, Nil)
            ?? (nqp::isnull(my $cmeth := nqp::tryfindmethod($ret, $name))
                    ?? coercion_error($ret.HOW.name($ret), $name)
                    !! $cmeth($ret))
            !! return_error($ret, $orig_type)
    }

    nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-rv-typecheck', -> $capture {
        # If the type is Mu or unset, then nothing is needed except identity.
        my $type := nqp::captureposarg($capture, 1);
        if nqp::isnull($type) || $type =:= Mu {
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-value',
                nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 1))
        }

        # Otherwise, need to look at the type.
        else {
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

            # If the return value isn't containerized, and type matches what we
            # expect, we can add guards and produce identity or an unchecked
            # coercion.
            my $rv := nqp::captureposarg($capture, 0);
            if !nqp::iscont($rv) &&
                    $type.HOW.archetypes.nominal &&
                    # Allow through Nil/Failure
                    (nqp::istype($rv, Nil) || (nqp::istype($rv, $type) &&
                    # Enforce definite checks.
                    ($definite_check == 0 ?? !nqp::isconcrete($rv) !!
                     $definite_check == 1 ?? nqp::isconcrete($rv) !! 1))) {
                # Add required guards.
                my $value-arg := nqp::dispatch('boot-syscall', 'dispatcher-track-arg',
                        $capture, 0);
                nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $value-arg);
                if $definite_check == 0 || $definite_check == 1 && !nqp::istype($rv, Nil) {
                    nqp::dispatch('boot-syscall', 'dispatcher-guard-concreteness', $value-arg);
                }
                if nqp::isnull($coerce_to) {
                    # The guards cover all we care about, so it's identity.
                    nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-value',
                        nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 1))
                }
                else {
                    # The type is fine, so we need to coerce. We do that by delegating
                    # to a coerce dispatcher. We replace the type to check against with
                    # with the type to coerce to.
                    my $coerce_capture := nqp::dispatch('boot-syscall',
                        'dispatcher-insert-arg-literal-obj',
                        nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 1),
                        1, $coerce_to);
                    nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-coerce',
                        $coerce_capture);
                }
            }
            else {
                if nqp::isnull($coerce_to) {
                    # Need to delegate to some code that will do the check. It
                    # needs both the unwrapped type and the original type; we
                    # add the unwrapped on as an extra arg.
                    my $del-capture := nqp::dispatch('boot-syscall',
                        'dispatcher-insert-arg-literal-obj', $capture, 2, $type);
                    my $target := $definite_check == 0 ?? $check_type_typeobj !!
                                  $definite_check == 1 ?? $check_type_concrete !!
                                                          $check_type;
                    my $target-capture := nqp::dispatch('boot-syscall',
                        'dispatcher-insert-arg-literal-obj', $del-capture, 0, $target);
                    nqp::dispatch('boot-syscall', 'dispatcher-delegate',
                            'boot-code-constant', $target-capture);
                }
                else {
                    # Need to delegate to code that will do the check and then
                    # do a coercion. We give it two extra args: the unwrapped
                    # type and the name of the type, for the purpose of coercion.
                    my $with-unwrapped-type-capture := nqp::dispatch('boot-syscall',
                        'dispatcher-insert-arg-literal-obj', $capture, 2, $type);
                    my str $name := $coerce_to.HOW.name($coerce_to);
                    my $with-name-capture := nqp::dispatch('boot-syscall',
                        'dispatcher-insert-arg-literal-str', $with-unwrapped-type-capture,
                        3, $name);
                    my $target := $definite_check == 0 ?? $check_type_typeobj_coerce !!
                                  $definite_check == 1 ?? $check_type_concrete_coerce !!
                                                          $check_type_coerce;
                    my $target-capture := nqp::dispatch('boot-syscall',
                        'dispatcher-insert-arg-literal-obj', $with-name-capture, 0, $target);
                    nqp::dispatch('boot-syscall', 'dispatcher-delegate',
                            'boot-code-constant', $target-capture);
                }
            }
        }
    });
}

# Assignment dispatcher, which case-analyzes assignments and provides optimized
# paths for a range of common situations, and typically lifting type checks to
# be guards.
{
    sub assign-type-error($desc, $value) {
        Perl6::Metamodel::Configuration.throw_or_die(
            'X::TypeCheck::Assignment',
            "Type check failed in assignment",
            :symbol($desc.name),
            :got($value),
            :expected($desc.of)
        );
    }

    my $assign-fallback := -> $cont, $value {
        nqp::assign($cont, $value)
    }

    my $assign-scalar-no-whence-no-typecheck := -> $cont, $value {
        nqp::bindattr($cont, Scalar, '$!value', $value)
    }

    my $assign-scalar-nil-no-whence := -> $cont, $value {
        my $desc := nqp::getattr($cont, Scalar, '$!descriptor');
        nqp::bindattr($cont, Scalar, '$!value',
            nqp::getattr($desc, ContainerDescriptor, '$!default'))
    }

    my $assign-scalar-no-whence := -> $cont, $value {
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

    my $assign-scalar-bindpos-no-typecheck := -> $cont, $value {
        nqp::bindattr($cont, Scalar, '$!value', $value);
        my $desc := nqp::getattr($cont, Scalar, '$!descriptor');
        nqp::bindpos(
            nqp::getattr($desc, ContainerDescriptor::BindArrayPos, '$!target'),
            nqp::getattr_i($desc, ContainerDescriptor::BindArrayPos, '$!pos'),
            $cont);
        nqp::bindattr($cont, Scalar, '$!descriptor',
            nqp::getattr($desc, ContainerDescriptor::BindArrayPos, '$!next-descriptor'));
    }

    my $assign-scalar-bindpos := -> $cont, $value {
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

    nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-assign', -> $capture {
        # Whatever we do, we'll guard on the type of the container and its
        # concreteness.
        my $tracked-cont := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0);
        nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $tracked-cont);
        nqp::dispatch('boot-syscall', 'dispatcher-guard-concreteness', $tracked-cont);

        # We have various fast paths for an assignment to a Scalar.
        my int $optimized := 0;
        my $cont := nqp::captureposarg($capture, 0);
        if nqp::eqaddr(nqp::what_nd($cont), Scalar) && nqp::isconcrete_nd($cont) {
            # Now see what the Scalar descriptor type is.
            my $tracked-desc := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                    $tracked-cont, Scalar, '$!descriptor');
            my $desc := nqp::getattr($cont, Scalar, '$!descriptor');
            my $tracked-value := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 1);
            my $value := nqp::captureposarg($capture, 1);
            if nqp::eqaddr($desc.WHAT, ContainerDescriptor) && nqp::isconcrete($desc) {
                # Simple assignment, no whence. But is Nil being assigned?
                my $of := $desc.of;
                if nqp::eqaddr($value, Nil) {
                    # Yes; just copy in the default, provided we've a simple type.
                    if $of.HOW.archetypes.nominal {
                        nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $tracked-value);
                        nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $tracked-desc);
                        my $tracked-of := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                                $tracked-desc, ContainerDescriptor, '$!of');
                        nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $tracked-of);
                        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-code-constant',
                            nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                                $capture, 0, $assign-scalar-nil-no-whence));
                        $optimized := 1;
                    }
                }
                else {
                    # No whence, no Nil. Is it a nominal type? If yes, we can check
                    # it here.
                    if $of.HOW.archetypes.nominal && nqp::istype($value, $of) {
                        # Nominal and passes type check; stack up gurads and delegate to
                        # simple bind.
                        nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $tracked-desc);
                        my $tracked-of := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                                $tracked-desc, ContainerDescriptor, '$!of');
                        nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $tracked-of);
                        nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $tracked-value);
                        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-code-constant',
                            nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                                $capture, 0, $assign-scalar-no-whence-no-typecheck));
                        $optimized := 1;
                    }
                    else {
                        # Non-nominal or type check error.
                        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-code-constant',
                            nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                                $capture, 0, $assign-scalar-no-whence));
                        $optimized := 1;
                    }
                }
            }
            elsif nqp::eqaddr($desc.WHAT, ContainerDescriptor::Untyped) && nqp::isconcrete($desc) {
                if nqp::eqaddr($value, Nil) {
                    # Nil case is NYI.
                }
                else {
                    # Assignment to an untyped container descriptor; no type check
                    # is required, just bind the value into place.
                    nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $tracked-desc);
                    nqp::dispatch('boot-syscall', 'dispatcher-guard-not-literal-obj',
                        $tracked-value, Nil);
                    nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-code-constant',
                        nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                            $capture, 0, $assign-scalar-no-whence-no-typecheck));
                    $optimized := 1;
                }
            }
            elsif nqp::eqaddr($desc.WHAT, ContainerDescriptor::BindArrayPos) {
                # Bind into an array. We can produce a fast path for this, though
                # should check what the ultimate descriptor is. It really should
                # be a normal, boring, container descriptor.
                nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $tracked-desc);
                my $next := nqp::getattr($desc, ContainerDescriptor::BindArrayPos,
                    '$!next-descriptor');
                if nqp::eqaddr($next.WHAT, ContainerDescriptor) {
                    # Ensure we're not assigning Nil. (This would be very odd, as
                    # a Scalar starts off with its default value, and if we are
                    # vivifying we'll likely have a new container).
                    unless nqp::eqaddr($value.WHAT, Nil) {
                        # Go by whether we can type check the target.
                        my $tracked-next := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                            $tracked-desc, ContainerDescriptor::BindArrayPos,
                            '$!next-descriptor');
                        nqp::dispatch('boot-syscall', 'dispatcher-guard-literal',
                            $tracked-next);
                        nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $tracked-value);
                        my $of := $next.of;
                        my $delegate := $of.HOW.archetypes.nominal &&
                                (nqp::eqaddr($of, Mu) || nqp::istype($value, $of))
                            ?? $assign-scalar-bindpos-no-typecheck
                            !! $assign-scalar-bindpos;
                        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-code-constant',
                            nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                                $capture, 0, $delegate));
                        $optimized := 1;
                    }
                }
            }
        }

        # If nothing we could optimize, go for the fallback.
        unless $optimized {
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-code-constant',
                nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                    $capture, 0, $assign-fallback));
        }
    });
}

# Binding type assertion dispatcher, used to do type checks of binds. Evaluates
# to the value itself when the type check passes, installing a guard along the
# way. Otherwise, throws.
{
    sub bind-error($value, $type) {
        Perl6::Metamodel::Configuration.throw_or_die(
            'X::TypeCheck::Binding',
            "Type check failed in binding; expected '" ~
                $type.HOW.name($value) ~ "' but got '" ~
                nqp::how_nd($value).HOW.name($value) ~ "'",
            :got($value),
            :expected($type)
        );
    }

    my $bind-check := -> $value, $value-decont, $type {
        nqp::istype_nd($value-decont, $type) ?? $value !! bind-error($value, $type)
    }

    nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-bind-assert', -> $capture {
        my $value-decont := nqp::captureposarg($capture, 1);
        my $type := nqp::captureposarg($capture, 2);
        if nqp::how_nd($type).archetypes.nominal {
            if nqp::istype_nd($value-decont, $type) {
                # Nominal, so a type guard on the decont'd value will suffice,
                # then produce the original value.
                nqp::dispatch('boot-syscall', 'dispatcher-guard-type',
                        nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 1));
                nqp::dispatch('boot-syscall', 'dispatcher-guard-type',
                        nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 2));
                nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-value', $capture);
            }
            else {
                my $value := nqp::captureposarg($capture, 0);
                bind-error($value, $type)
            }
        }
        else {
            # Not a nominal type, can't guard it, so set up a call to do the
            # check late-bound.
            nqp::dispatch('boot-syscall', 'dispatcher-guard-type',
                    nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 2));
            my $delegate := nqp::dispatch('boot-syscall',
                'dispatcher-insert-arg-literal-obj', $capture, 0, $bind-check);
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-code-constant',
                $delegate);
        }
    });
}

# Sink dispatcher. Called in void context with the decontainerized value to sink.
nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-sink', -> $capture {
    # Guard on the type and concreteness.
    my $sinkee := nqp::captureposarg($capture, 0);
    my $track-sinkee := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0);
    nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $track-sinkee);
    nqp::dispatch('boot-syscall', 'dispatcher-guard-concreteness', $track-sinkee);

    # Now consider what we're sinking.
    if nqp::isconcrete_nd($sinkee) {
        # Concrete. See if it has a `sink` method, and then also if it's not
        # Mu.sink, which is a no-op.
        my $meth := nqp::how_nd($sinkee).find_method($sinkee, 'sink');
        if nqp::isconcrete($meth) && !nqp::eqaddr($meth, Mu.HOW.find_method(Mu, 'sink')) {
            # Need to do a call to the sink method. Since sink is a Raku thing,
            # assume we can go straight for the Raku method dispatcher.
            my $with-name := nqp::dispatch('boot-syscall',
                'dispatcher-insert-arg-literal-str', $capture, 0, 'sink');
            my $with-type := nqp::dispatch('boot-syscall',
                'dispatcher-insert-arg-literal-obj', $with-name, 0, nqp::what_nd($sinkee));
            my $with-callee := nqp::dispatch('boot-syscall',
                'dispatcher-insert-arg-literal-obj', $with-type, 0, $meth);
            nqp::dispatch('boot-syscall', 'dispatcher-delegate',
                'raku-meth-call-resolved', $with-callee);
        }
        else {
            # Nothing to do (use boot-value and let void context take care of
            # discarding the value).
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-value', $capture);
        }
    }
    else {
        # Not concrete, nothing to do.
        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-value', $capture);
    }
});

# Coercion dispatcher. The first argument is the value to coerce, the second
# is what to coerce it into.
{
    sub coercion_error($from_name, $to_name) {
        nqp::die("Unable to coerce the from $from_name to $to_name; " ~
            "no coercion method defined");
    }

    nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-coerce', -> $capture {
        my $to_coerce := nqp::captureposarg($capture, 0);
        my $coerce_type := nqp::captureposarg($capture, 1);

        # See if there is a method named for the type.
        my str $to_name := $coerce_type.HOW.name($coerce_type);
        my $coerce_method := $to_coerce.HOW.find_method($to_coerce, $to_name);
        if nqp::isconcrete($coerce_method) {
            # Delegate to the resolved method call dispatcher. We need to drop
            # the arg of the coercion type, then insert three args:
            # 1. The method we resolved to.
            # 2. The type object of the coercion type (used in deferral)
            # 3. The name of the method (used in deferral)
            my $without_coerce_type := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg',
                    $capture, 1);
            my $meth_capture := nqp::dispatch('boot-syscall',
                'dispatcher-insert-arg-literal-obj',
                nqp::dispatch('boot-syscall',
                    'dispatcher-insert-arg-literal-obj',
                    nqp::dispatch('boot-syscall',
                        'dispatcher-insert-arg-literal-str',
                        $without_coerce_type,
                        0, $to_name),
                    0, nqp::what(to_coerce)),
                0, $coerce_method);
            nqp::dispatch('boot-syscall', 'dispatcher-delegate',
                    'raku-meth-call-resolved', $meth_capture);
        }
        else {
            coercion_error($to_coerce.HOW.name($to_coerce), $to_name);
        }
    });
}

# A standard call (such as `func($arg)`, `$obj($arg)`, etc.) It receives the
# decontainerized callee as the first argument, followed by the arguments. Its
# primary purpose is to deal with multi dispatch vs. single dispatch and then
# delegate on to the appropriate dispatcher.
nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-call', -> $capture {
    # Guard on the type and, if it's a routine, on the dispatchees. (We assume
    # that the set of dispatchees shall not change, even over closure clones -
    # this may not always be a good assumption - and so we guard on that. If
    # it's not a dispatcher, we'll be guarding on a literal type object.)
    my $track_callee := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0);
    nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $track_callee);
    my $callee := nqp::captureposarg($capture, 0);
    if nqp::istype_nd($callee, Routine) {
        nqp::dispatch('boot-syscall', 'dispatcher-guard-literal',
            nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                $track_callee, Routine, '@!dispatchees'));
        nqp::dispatch('boot-syscall', 'dispatcher-delegate',
            $callee.is_dispatcher ?? 'raku-multi' !! 'raku-invoke', $capture);
    }
    else {
        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-invoke', $capture);
    }

});

# A standard method call of the form $obj.meth($arg); also used for the
# indirect form $obj."$name"($arg). It receives the decontainerized invocant,
# the method name, and the the args (starting with the invocant including any
# container).
nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-meth-call', -> $capture {
    # Try to resolve the method call.
    # TODO Assorted optimizations are possible here later on to speed up some
    # kinds of dispatch, including:
    # * Using the dispatcher to directly rewrite args and invoke FALLBACK if
    #   needed
    # * Rewriting the args to use a formed role pun, thus caching the lookup
    #   of the pun and an extra level of call
    # * Handling some forms of delegation via the dispatcher mechanism
    my $obj := nqp::captureposarg($capture, 0);
    my str $name := nqp::captureposarg_s($capture, 1);
    my $how := nqp::how_nd($obj);
    my $meth := $how.find_method($obj, $name);

    # Report an error if there is no such method.
    unless nqp::isconcrete($meth) {
        my $class := nqp::getlexcaller('$?CLASS');
        my $msg := "Method '$name' not found for invocant of class '{$how.name($obj)}'";
        if $name eq 'STORE' {
            Perl6::Metamodel::Configuration.throw_or_die(
                'X::Assignment::RO', $msg, :value($obj));
        }
        else {
            Perl6::Metamodel::Configuration.throw_or_die(
                'X::Method::NotFound',
                $msg,
                :invocant($obj),
                :method($name),
                :typename($how.name($obj)),
                :private(nqp::hllboolfor(0, 'Raku')),
                :in-class-call(nqp::hllboolfor(nqp::eqaddr(nqp::what($obj), $class), 'Raku'))
            );
        }
    }

    # Establish a guard on the invocant type and method name (however the name
    # may well be a literal, in which case this is free).
    nqp::dispatch('boot-syscall', 'dispatcher-guard-type',
        nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0));
    nqp::dispatch('boot-syscall', 'dispatcher-guard-literal',
        nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 1));

    # Add the resolved method and delegate to the resolved method dispatcher.
    my $capture_delegate := nqp::dispatch('boot-syscall',
        'dispatcher-insert-arg-literal-obj', $capture, 0, $meth);
    nqp::dispatch('boot-syscall', 'dispatcher-delegate',
        'raku-meth-call-resolved', $capture_delegate);
});

# Qualified method call dispatcher. This is used for calls of the form
# $foo.Some::ClassOrRole::bar($arg). It receives the decontainerized
# invocant, the method name, the type qualifier, and then the args
# (starting with the invocant including any container).
nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-meth-call-qualified', -> $capture {
    # Lookup outers of the caller and locate the first occurence of the
    # symbols of interest, which tell us about the caller type.
    my $ctx := nqp::ctxcaller(nqp::ctx());
    my $caller-type := nqp::null();
    nqp::repeat_while(
        nqp::isnull($caller-type) && !nqp::isnull($ctx),
        nqp::stmts(
            (my $pad := nqp::ctxlexpad($ctx)),
            nqp::if(
                nqp::existskey($pad, '$?CONCRETIZATION'),
                ($caller-type := nqp::atkey($pad, '$?CONCRETIZATION')),
                nqp::if(
                    nqp::existskey($pad, '$?CLASS'),
                    ($caller-type := nqp::atkey($pad, '$?CLASS')),
                )
            ),
            ($ctx := nqp::ctxouterskipthunks($ctx)),
        )
    );

    # Establish a guard on the invocant and qualifying type.
    nqp::dispatch('boot-syscall', 'dispatcher-guard-type',
        nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0));
    nqp::dispatch('boot-syscall', 'dispatcher-guard-type',
        nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 2));

    # Try to resolve the method.
    my $obj := nqp::captureposarg($capture, 0);
    my str $name := nqp::captureposarg_s($capture, 1);
    my $type := nqp::captureposarg($capture, 2);
    my $meth;
    for ($caller-type, $obj.WHAT) {
        if nqp::istype($_, $type) {
            $meth := $_.HOW.find_method_qualified($_, $type, $name);
            last if nqp::isconcrete($meth);
        }
    }

    # If it's resolved, then:
    # 1. Drop the invocant and type arguments targetted at this resolution
    # 2. Insert the type we resolved the method against before those, for
    #    deferral (the name is retained ahead of this)
    # 3. Finally, prepend the resolved method, and delegate to the resolved
    #    method dispatcher.
    if nqp::isconcrete($meth) {
        my $with_name_and_args := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg',
            nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 2),
            0);
        my $with_resolution_start := nqp::dispatch('boot-syscall',
            'dispatcher-insert-arg-literal-obj', $with_name_and_args, 0, $type);
        my $capture_delegate := nqp::dispatch('boot-syscall',
            'dispatcher-insert-arg-literal-obj', $with_resolution_start, 0, $meth);
        nqp::dispatch('boot-syscall', 'dispatcher-delegate',
                'raku-meth-call-resolved', $capture_delegate);
    }

    # Otherwise, exception.
    else {
        Perl6::Metamodel::Configuration.throw_or_die(
            'X::Method::InvalidQualifier',
            "Cannot dispatch to method $name on " ~ $type.HOW.name($type) ~
                " because it is not inherited or done by " ~ $obj.HOW.name($obj),
            :method($name), :invocant($obj), :qualifier-type($type));
    }
});

# Maybe method dispatch, of the form $obj.?foo.
nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-meth-call-me-maybe', -> $capture {
    # Establish a guard on the invocant type.
    nqp::dispatch('boot-syscall', 'dispatcher-guard-type',
        nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0));

    # Try to find the method.
    my $invocant := nqp::captureposarg($capture, 0);
    my str $name := nqp::captureposarg_s($capture, 1);
    my $meth := $invocant.HOW.find_method($invocant, $name);
    if nqp::isconcrete($meth) {
        # Found it. Put in resolved method and leave the rest to the resolved
        # method call dispatcher.
        my $capture_delegate := nqp::dispatch('boot-syscall',
            'dispatcher-insert-arg-literal-obj', $capture, 0, $meth);
        nqp::dispatch('boot-syscall', 'dispatcher-delegate',
                'raku-meth-call-resolved', $capture_delegate);
    }
    else {
        # Not found. Insert a Nil value at the start (boot-constant ignores
        # the rest of the args).
        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-constant',
            nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                $capture, 0, Nil));
    }
});

# Private method dispatch. This is actually a fallback, since in the best
# case we can resolve the private method into a constant at code-gen time
# and just invoke that. This happens with private methods in roles.
nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-meth-private', -> $capture {
    # Find the private method.
    my $type := nqp::captureposarg($capture, 0);
    my str $name := nqp::captureposarg_s($capture, 1);
    my $meth := $type.HOW.find_private_method($type, $name);

    # If it's found, then we drop the first two arguments, insert the
    # resolved callee, and invoke it. This goes directly to invoke, as
    # there's no deferral (due to no inheritance relationship) or multi
    # dispatch for private methods.
    if nqp::isconcrete($meth) {
        nqp::dispatch('boot-syscall', 'dispatcher-guard-type',
            nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0));
        my $capture_delegate := nqp::dispatch('boot-syscall',
            'dispatcher-insert-arg-literal-obj',
            nqp::dispatch('boot-syscall', 'dispatcher-drop-arg',
                nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 0),
                0),
            0, $meth);
        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-invoke',
            $capture_delegate);
    }
    else {
        # TODO typed exception
        nqp::die("No such private method '$name' on " ~ $type.HOW.name($type));
    }
});

# A linked list is used to model the state of a dispatch that is deferring
# through a set of methods, multi candidates, or wrappers. The Exhausted class
# is used as a sentinel for the end of the chain. The current state of the
# dispatch points into the linked list at the appropriate point; the chain
# itself is immutable, and shared over (runtime) dispatches.
my class DeferralChain {
    has $!code;
    has $!next;
    method new($code, $next) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, DeferralChain, '$!code', $code);
        nqp::bindattr($obj, DeferralChain, '$!next', $next);
        $obj
    }
    method code() { $!code }
    method next() { $!next }
};
my class Exhausted {};

# Resolved method call dispatcher. This is used to call a method, once we have
# already resolved it to a callee. Its first arg is the callee, the second and
# third are the type and name (used in deferral), and the rest are the args to
# the method.
nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-meth-call-resolved',
    # Initial dispatch
    -> $capture {
        # Save dispatch state for resumption. We don't need the method that will
        # be called now, so drop it.
        my $resume-capture := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg',
            $capture, 0);
        nqp::dispatch('boot-syscall', 'dispatcher-set-resume-init-args', $resume-capture);

        # Drop the dispatch start type and name, and delegate to multi-dispatch or
        # just invoke if it's single dispatch.
        my $delegate_capture := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg',
            nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 1), 1);
        my $method := nqp::captureposarg($delegate_capture, 0);
        if nqp::istype($method, Routine) && $method.is_dispatcher {
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-multi', $delegate_capture);
        }
        else {
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-invoke', $delegate_capture);
        }
    },
    # Resumption. The capture itself has a first argument indicating the kind
    # of resumption operation we're doing. The resume init capture's first two
    # arguments are the type that we initially did a method dispatch against
    # and the method name respectively. In this resumption we calculate the
    # set of methods we will defer through. We then delegate to another
    # dispatcher to handle movement through that list (this structure helps
    # us to handle `callwith`).
    -> $capture {
        # We put a sentinel value into the resume state in the case that we have
        # already set up the method resumption. We always guard on it too.
        my $state := nqp::dispatch('boot-syscall', 'dispatcher-get-resume-state');
        my $track_state := nqp::dispatch('boot-syscall', 'dispatcher-track-resume-state');
        nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track_state);

        # Mark that the deferral was already set up, so we don't do this
        # again.
        nqp::dispatch('boot-syscall', 'dispatcher-set-resume-state-literal', Exhausted);

        # Guard on the kind of resume we're doing, and get that flag.
        my $track_kind := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0);
        nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track_kind);
        my int $kind := nqp::captureposarg_i($capture, 0);

        # If the state is null, it's we are entering a walk through the methods.
        # We can short-circuit this if it's a lastcall (and the Exhausted above
        # puts it into effect).
        if nqp::isnull($state) || $kind == nqp::const::DISP_LASTCALL {
            # No state, so just starting the resumption. Guard on the
            # invocant type and name.
            my $init := nqp::dispatch('boot-syscall', 'dispatcher-get-resume-init-args');
            my $track_start_type := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $init, 0);
            nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $track_start_type);
            my $track_name := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $init, 1);
            nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track_name);

            # Build up the list of methods to defer through.
            my $start_type := nqp::captureposarg($init, 0);
            my str $name := nqp::captureposarg_s($init, 1);
            my @mro := nqp::can($start_type.HOW, 'mro_unhidden')
                ?? $start_type.HOW.mro_unhidden($start_type)
                !! $start_type.HOW.mro($start_type);
            my @methods;
            for @mro {
                my %mt := nqp::hllize($_.HOW.method_table($_));
                if nqp::existskey(%mt, $name) {
                    @methods.push(%mt{$name});
                }
            }

            # Turn it into a linked list.
            my $chain := Exhausted;
            if nqp::elems(@methods) >= 2 {
                @methods.shift; # Discard the first one, which we initially called
                while @methods {
                    $chain := DeferralChain.new(@methods.pop, $chain);
                }
            }

            # Determine the args to pass to the method we defer to. If it's a
            # callwith, then the arguments are given to us here. Otherwise,
            # they are those from the initial capture.
            my $args_with_kind;
            if $kind == nqp::const::DISP_CALLWITH {
                # Rewrite the kind into callsame, since we've already accounted
                # for the callwith. We do need to insert the original invocant.
                my $args := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 0);
                my $with_invocant := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg',
                    $args, 0,
                    nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $init, 2));
                $args_with_kind := nqp::dispatch('boot-syscall',
                    'dispatcher-insert-arg-literal-int', $with_invocant, 0,
                    nqp::const::DISP_CALLSAME);
            }
            else {
                my $args := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg',
                    nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $init, 0), 0);
                $args_with_kind := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg',
                    $args, 0, $track_kind);
            }

            # Prepend the chain of methods we dispatch through and defer.
            my $delegate := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                $args_with_kind, 0, $chain);
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-meth-deferral',
                $delegate);
        }

        # Otherwise, we already set up - and presumably completed - walking
        # through the methods.
        else {
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-constant',
                nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                    $capture, 0, Nil));
        }
    });

# Method deferral dispatcher.
nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-meth-deferral',
    # Entry to the deferral, with the chain of methods to walk through as the
    # first argument, the resumption kind as the second argument, and the
    # args to the method (including the invocant) coming next. We assume here
    # that we already established guards on kind and similar, and that if we
    # have a lastcall we'll have handled it rather than sending it here.
    -> $capture {
        # If the chain is Exhausted, then we will delegate to Nil.
        my $chain := nqp::captureposarg($capture, 0);
        if nqp::eqaddr($chain, Exhausted) {
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-constant',
                nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                    $capture, 0, Nil));
        }

        # Otherwise, need to do a dispatch step.
        else {
            # The resume init state for a multi-step deferral is the next
            # thing in the chain prepended to the dispatch arguments.
            my $args := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg',
                nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 0), 0);
            my $resume-capture := nqp::dispatch('boot-syscall',
                'dispatcher-insert-arg-literal-obj', $args, 0, $chain.next);
            nqp::dispatch('boot-syscall', 'dispatcher-set-resume-init-args', $resume-capture);

            # Now perform the action needed based upon the kind of resumption
            # we have.
            method-deferral-step($chain, nqp::captureposarg_i($capture, 1), $args);
        }
    },
    # Resumption, wherein we walk another step through the chain.
    -> $capture {
        # Guard on the kind of resume we're doing, and get that flag.
        my $track_kind := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0);
        nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track_kind);
        my int $kind := nqp::captureposarg_i($capture, 0);

        # If we're doing a lastcall, set the state to exhausted and we're done.
        if $kind == nqp::const::DISP_LASTCALL {
            nqp::dispatch('boot-syscall', 'dispatcher-set-resume-state-literal', Exhausted);
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-constant',
                nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                    $capture, 0, Nil));
        }

        else {
            # If there's no dispatch state yet, we're on our first round of
            # resumption for this dispatcher. Otherwise, look to the state to
            # find the next method.
            my $init := nqp::dispatch('boot-syscall', 'dispatcher-get-resume-init-args');
            my $state := nqp::dispatch('boot-syscall', 'dispatcher-get-resume-state');
            my $track_state := nqp::dispatch('boot-syscall', 'dispatcher-track-resume-state');
            my $chain;
            my $track_chain;
            if nqp::isnull($state) {
                # Guard that the resume state is null, and then extract the chain
                # from the init state.
                nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track_state);
                $chain := nqp::captureposarg($init, 0);
                $track_chain := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $init, 0);
            }
            else {
                # The chain is the state.
                $chain := $state;
                $track_chain := $track_state;
            }

            # If we're exhausted already, then produce Nil.
            if nqp::istype($chain, Exhausted) {
                nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track_chain);
                nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-constant',
                    nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                        $capture, 0, Nil));
            }

            else {
                # Otherwise, guard on the candidate that we shall be invoking.
                my $track_method := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                    $track_chain, DeferralChain, '$!code');
                nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track_method);

                # Now perform the action needed based upon the kind of resumption
                # we have.
                my $args := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $init, 0);
                if $kind == nqp::const::DISP_CALLWITH {
                    # Set the state to exhausted as we're abandoning the walk through
                    # the methods with these args.
                    nqp::dispatch('boot-syscall', 'dispatcher-set-resume-state-literal',
                        Exhausted);

                    # Re-enter this dispatcher with the new args.
                    my $new_args := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg',
                        $capture, 0);
                    my $with_invocant := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg',
                        $new_args, 0,
                        nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $args, 0));
                    my $new_args_with_kind := nqp::dispatch('boot-syscall',
                        'dispatcher-insert-arg-literal-int', $with_invocant, 0,
                        nqp::const::DISP_CALLSAME);
                    my $delegate := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg',
                        $new_args_with_kind, 0, $track_chain);
                    nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-meth-deferral',
                        $delegate);
                }
                else {
                    # Update dispatch state to point to the next method.
                    my $track_next := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                        $track_chain, DeferralChain, '$!next');
                    nqp::dispatch('boot-syscall', 'dispatcher-set-resume-state', $track_next);

                    # It's a normal step.
                    method-deferral-step($chain, $kind, $args);
                }
            }
        }
    });
sub method-deferral-step($chain-head, int $kind, $args) {
    # Look at the kind of deferral we have to decide what to do.
    my $next_method := $chain-head.code;
    if $kind == nqp::const::DISP_CALLSAME {
        # Call with same (that is, original) arguments. Invoke with those.
        # We drop the first two arguments (which are only there for the
        # resumption), add the code object to invoke, and then leave it
        # to the invoke dispatcher.
        my $delegate_capture := nqp::dispatch('boot-syscall',
            'dispatcher-insert-arg-literal-obj', $args, 0, $next_method);
        if nqp::istype($next_method, Routine) && $next_method.is_dispatcher {
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-multi',
                    $delegate_capture);
        }
        else {
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-invoke',
                    $delegate_capture);
        }
    }
    elsif $kind == nqp::const::DISP_NEXTCALLEE {
        # We just want method itself, not to invoke it.
        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-constant',
            nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                $args, 0, $next_method));
    }
    else {
        nqp::die('Unexpected resumption kind in method dispatch');
    }
}

# Multi-dispatch dispatcher, used for both multi sub and multi method dispatch.
# Assumes that we have already guarded on a literal code object (methods) or
# ensured consistency of routine (subs where closure cloning may take place).
# This does not do the heart of the dispatch itself, but rather determines if
# we have a simple or complex proto, and thus whether we need to invoke the
# proto at all. In the case of a complex proto, we use dispatch resumption to
# continue with the dispatch.
nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-multi',
    # Initial dispatch, only setting up resumption if we need to invoke the
    # proto.
    -> $capture {
        my $callee := nqp::captureposarg($capture, 0);
        my int $onlystar := nqp::getattr_i($callee, Routine, '$!onlystar');
        if $onlystar && !nqp::can($callee, 'WRAPPERS') {
            # Don't need to invoke the proto itself, so just get on with the
            # candidate dispatch.
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-multi-core', $capture);
        }
        else {
            # Set resume init args and run the proto.
            nqp::dispatch('boot-syscall', 'dispatcher-set-resume-init-args', $capture);
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-invoke', $capture);
        }
    },
    # Resumption means that we have reached the {*} in the proto and so now
    # should go ahead and do the dispatch. Make sure we only do this if we
    # are signalled to that it's a resume for an onlystar.
    -> $capture {
        my $track_kind := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0);
        nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track_kind);
        my int $kind := nqp::captureposarg_i($capture, 0);
        if $kind == nqp::const::DISP_ONLYSTAR {
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-multi-core',
                nqp::dispatch('boot-syscall', 'dispatcher-get-resume-init-args'));
        }
        elsif !nqp::dispatch('boot-syscall', 'dispatcher-next-resumption') {
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-constant',
                nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                    $capture, 0, Nil));
        }
    });

# We we invoke a multi with an argument that is a Proxy (or some other non-Scalar
# container), we need to read the value(s) from the Proxy argument(s) and then go
# on with the dispatch. The ProxyReaderFactory produces code objects that do
# that. We key it on total positional arguments, whether there are nameds to
# strip, and then the indices of the arguments to strip.
class ProxyReaderFactory {
    has $!lock;
    has %!readers;

    method new() {
        self.bless: lock => NQPLock.new, readers => nqp::hash()
    }

    method reader-for($capture, $indices) {
        $!lock.protect: {
            # Form a key.
            my int $num-args := nqp::captureposelems($capture);
            my int $has-nameds := nqp::capturehasnameds($capture);
            my str $key := $num-args ~ ($has-nameds ?? '%' !! '|');
            my int $i;
            while $i < nqp::elems($indices) {
                $key := $key ~ nqp::atpos_i($indices, $i) ~ ',';
                $i++;
            }

            # If we don't already have a reader for this key, produce it.
            unless nqp::existskey(%!readers, $key) {
                %!readers{$key} := self.'!produce-reader'($num-args, $has-nameds, $indices);
            }

            %!readers{$key}
        }
    }

    method !produce-reader($num-args, $has-nameds, $indices) {
        # Create a block taking each positional arg required, adding an
        # slurpy named if needed.
        my $block := QAST::Block.new(:is_thunk);
        my int $i := 0;
        while $i < $num-args {
            $block.push(QAST::Var.new( :name("a$i"), :decl<param>, :scope<local> ));
            $i++;
        }
        if $has-nameds {
            $block.push(QAST::Var.new( :name<n>, :decl<param>, :scope<local>, :named, :slurpy ));
        }

        # Produce a dispatch op with the required arguments decontainerized.
        my $dispatch := QAST::Op.new:
            :op('dispatch'),
            QAST::SVal.new( :value('boot-resume') ),
            QAST::IVal.new( :value(nqp::const::DISP_DECONT) );
        $i := 0;
        my $decont-index := 0;
        while $i < $num-args {
            my $var := QAST::Var.new( :name("a$i"), :scope<local> );
            if nqp::atpos_i($indices, $decont-index) == $i {
                $dispatch.push(QAST::Op.new( :op<decont>, $var ));
                $decont-index++;
            }
            else {
                $dispatch.push($var);
            }
            $i++;
        }
        if $has-nameds {
            $dispatch.push(QAST::Var.new( :name<n>, :scope<local>, :named, :flat ));
        }
        $block.push($dispatch);

        # Compile and return it.
        nqp::getcomp('Raku').compile($block, :from<optimize>)
    }
}
my $PROXY-READERS := ProxyReaderFactory.new;

# The core of multi dispatch. Once we are here, either there was a simple
# proto that we don't need to inovke, or we already did invoke the proto.
#
# Multiple dispatch is relatively complex in the most general case. However,
# the most common case by far consists of:
#
# * Arguments are not in any containers or in Scalar containers, which we can
#   dereference as part of the dispatch program
# * A candidate that is identified by arity and nominal type, and thus can be
#   identified just using the callsite and guards on argument types
# * No resumption via callsame and friends
#
# This is the case we want to optimize for, and is called a "trivial" multiple
# dispatch. All other cases are "non-trivial", including the case where a
# trivial multiple dispatch is resumed. In a trivial multiple dispatch, we
# save the incoming argument tuple as dispatch state, establish guards, and
# then delegate to raku-invoke to invoke the chose candidate.
#
# The case with non-Scalar containers is handled by first extracting those and
# then proceeding with the dispatch.
#
# The non-trivial multiple dispatch establishes the guards that a trivial
# multiple dispatch does, and then works through the dispatch plan. This is a
# linked list featuring candidates to try calling, terminated either with a
# fallback to the next resumable dispatcher (if we're in a resume) or to the
# production of an error (ambiguous dispatch or no applicable candidate).
#
# The plan nodes are as follows:
# * A candidate to call; we don't expect any bind failure (that is, we
#   consider it safe from nominality alone).
my class MultiDispatchCall {
    has $!candidate;
    has $!next;
    method new($candidate) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, MultiDispatchCall, '$!candidate', $candidate);
        $obj
    }
    method set-next($next) {
        $!next := $next;
    }
    method candidate() { $!candidate }
    method next() { $!next }
    method debug() {
        "Candidate " ~ $!candidate.signature.raku ~ "\n" ~ $!next.debug
    }
}
# * A candidate to try and invoke; if there's a bind failure, it will be
#   mapped into a resumption
my class MultiDispatchTry is MultiDispatchCall {
    method debug() {
        "Try candidate " ~ self.candidate.signature.raku ~ "\n" ~ self.next.debug
    }
}
# * An ambiguity. It's only an error if it's reached during the initial
#   phase of dispatch, not during a callsame-alike, thus why the chain
#   continues beyond this point.
my class MultiDispatchAmbiguous {
    has $!next;
    method set-next($next) {
        $!next := $next;
    }
    method next() { $!next }
    method debug() {
        "Ambiguous\n" ~ $!next.debug
    }
}
# * The end of the candidates. Either a "no applicable candidates" error
#   if we are in the initial phase of dispatch, or a next resumption (or
#   Nil) otherwise.
my class MultiDispatchEnd {
    method debug() {
        "End"
    }
}
# * Not actually used in a plan, but instead conveys that we have containers
#   that need complex (running code) removal. This is created with the indices
#   of the arguments that need it removing.
my class MultiDispatchNonScalar {
    has $!args;
    method new($args) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, MultiDispatchNonScalar, '$!args', $args);
        $obj
    }
    method args() { $!args }
    method debug() {
        "Non-Scalar container dispatch"
    }
}
# Apart from the last one, these are used as the dispatch state; this is the
# same immutable linked list traversal approach as used in other kinds of
# resumption.
#
# The plan is calculated by the following code. Note that the trivial case
# also uses this plan calculation routine, but in a "stop on trivial success"
# mode. A resumption of a trivial dispatch will call this again, but with that
# flag not set, and then drop the first candidate from the plan, which was
# already invoked. It will then walk the candidate list as usual.
my int $DEFCON_DEFINED    := 1;
my int $DEFCON_UNDEFINED  := 2;
my int $DEFCON_MASK       := $DEFCON_DEFINED +| $DEFCON_UNDEFINED;
my int $TYPE_NATIVE_INT   := 4;
my int $TYPE_NATIVE_NUM   := 8;
my int $TYPE_NATIVE_STR   := 16;
my int $TYPE_NATIVE_MASK  := $TYPE_NATIVE_INT +| $TYPE_NATIVE_NUM +| $TYPE_NATIVE_STR;
my int $BIND_VAL_OBJ      := 0;
my int $BIND_VAL_INT      := 1;
my int $BIND_VAL_NUM      := 2;
my int $BIND_VAL_STR      := 3;
sub has-named-args-mismatch($capture, %info) {
    # First consider required nameds.
    my $required-name-sets := %info<required_names>;
    my $nameds-list := nqp::dispatch('boot-syscall', 'capture-names-list', $capture);
    if $required-name-sets {
        # Quick exit if we required names, but have none.
        return 1 unless $nameds-list;

        # Otherwise check that required nameds are present.
        my int $i := 0;
        my int $n := nqp::elems($required-name-sets);
        while $i < $n {
            my int $found := 0;
            my $names := nqp::atpos($required-name-sets, $i);
            my int $j := 0;
            my int $m := nqp::elems($names);
            while $j < $m {
                if nqp::captureexistsnamed($capture, nqp::atpos_s($names, $j)) {
                    $found := 1;
                    last;
                }
                $j++;
            }
            return 1 unless $found;
            $i++;
        }
    }

    # If we don't accept all nameds, then check there are acceptable nameds.
    if $nameds-list && !%info<allows_all_names> {
        # Check exit if there are no allowed nameds.
        my $allowed-names := %info<allowed_names>;
        return 1 unless $allowed-names;

        # Go through the nameds and check they are allowed.
        my int $i;
        my int $n := nqp::elems($nameds-list);
        while $i < $n {
            return 1 unless nqp::existskey($allowed-names, nqp::atpos_s($nameds-list, $i));
            $i++;
        }
    }

    # Otherwise, no mismatch.
    0
}
sub raku-multi-plan(@candidates, $capture, int $stop-at-trivial, $orig-capture = $capture) {
    # First check there's no non-Scalar containers in the positional arguments.
    # If there are, establish guards relating to those and we're done. Native
    # references don't count; we know the native types they shall match up with
    # and don't need to dereference them.
    my int $num_args := nqp::captureposelems($capture);
    my int $i;
    my $non-scalar := nqp::list_i();
    while $i < $num_args {
        my int $got_prim := nqp::captureposprimspec($capture, $i);
        if $got_prim == 0 {
            my $value := nqp::captureposarg($capture, $i);
            if nqp::isconcrete_nd($value) &&
                nqp::iscont($value) && !nqp::istype_nd($value, Scalar) &&
                !(nqp::iscont_i($value) || nqp::iscont_n($value) || nqp::iscont_s($value)) {
                nqp::push_i($non-scalar, $i);
            }
        }
        $i++;
    }
    if nqp::elems($non-scalar) {
        # Establish guards on types of all positionals, but not on the values
        # inside of them if they are Scalar containers; we just need to make
        # sure we have the appropriate tuple of Proxy vs non-Proxy for the
        # Proxy removal code we'll invoke.
        my int $i;
        while $i < $num_args {
            if nqp::captureposprimspec($capture, $i) == 0 {
                nqp::dispatch('boot-syscall', 'dispatcher-guard-type',
                    nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, $i));
            }
            $i++;
        }

        # Hand back the indices we need to strip Proxy from.
        return MultiDispatchNonScalar.new($non-scalar);
    }

    # We keep track of the head of the plan as well as the tail node of it,
    # so we know where to add the next step.
    my $current-head := nqp::null();
    my $current-tail := nqp::null();

    # Look through all candidates. Eliminate those that can be ruled out by
    # setting guards on the incoming arguments OR by the shape of the
    # callsite. That callsite shape includes argument count, which named
    # arguments are present, and which arguments are natively typed.
    my int $cur_idx := 0;
    my int $done := 0;
    my $need_scalar_read := nqp::list_i();
    my $need_scalar_rw_check := nqp::list_i();
    my $need_type_guard := nqp::list_i();
    my $need_conc_guard := nqp::list_i();
    my @possibles;
    my $Positional := nqp::gethllsym('Raku', 'MD_Pos');
    until $done {
        # The candidate list is broken into tied groups (that is, groups of
        # candidates that are equally narrow). Those are seperated by a
        # type object sentinel.
        my $cur_candidate := nqp::atpos(@candidates, $cur_idx++);
        if nqp::isconcrete($cur_candidate) {
            # Candidate; does the arity fit? (If not, it drops out on callsite
            # shape.)
            if $num_args >= nqp::atkey($cur_candidate, 'min_arity') &&
                    $num_args <= nqp::atkey($cur_candidate, 'max_arity') {
                # Arity OK; now go through the arguments and see if we can
                # eliminate any of them based on guardable properties.
                my int $type_check_count := nqp::atkey($cur_candidate, 'num_types') > $num_args
                    ?? $num_args
                    !! nqp::atkey($cur_candidate, 'num_types');
                my int $type_mismatch := 0;
                my int $rwness_mismatch := 0;
                my int $i := 0;
                while $i < $type_check_count && !($type_mismatch +| $rwness_mismatch) {
                    # Obtain parameter properties.
                    my $type := nqp::atpos(nqp::atkey($cur_candidate, 'types'), $i);
                    my int $type_flags := nqp::atpos_i(nqp::atkey($cur_candidate, 'type_flags'), $i);
                    my int $definedness := $type_flags +& $DEFCON_MASK;
                    my int $rwness := nqp::atpos_i(nqp::atkey($cur_candidate, 'rwness'), $i);

                    # Get the primitive type of the argument, and go on whether it's an
                    # object or primitive type.
                    my int $got_prim := nqp::captureposprimspec($capture, $i);
                    my int $want_prim := $type_flags +& $TYPE_NATIVE_MASK;
                    if $got_prim == 0 && $want_prim == 0 {
                        # It's an object type. Obtain the value, and go by if it's a
                        # container or not.
                        my $value := nqp::captureposarg($capture, $i);
                        nqp::bindpos_i($need_type_guard, $i, 1);
                        my int $promoted_primitive;
                        if nqp::iscont($value) && nqp::isconcrete_nd($value) {
                            # Containerized. Scalar we handle specially.
                            if nqp::istype_nd($value, Scalar) {
                                nqp::bindpos_i($need_scalar_read, $i, 1);
                                if $rwness {
                                    nqp::bindpos_i($need_scalar_rw_check, $i, 1);
                                    my $desc := nqp::getattr($value, Scalar, '$!descriptor');
                                    unless nqp::isconcrete($desc) {
                                        $rwness_mismatch := 1;
                                    }
                                }
                                $value := nqp::getattr($value, Scalar, '$!value');
                            }
                            # Otherwise, it should be a native reference. We'll
                            # promote these to their boxed type.
                            elsif nqp::iscont_i($value) {
                                $value := Int;
                                $promoted_primitive := 1;
                            }
                            elsif nqp::iscont_n($value) {
                                $value := Num;
                                $promoted_primitive := 1;
                            }
                            elsif nqp::iscont_s($value) {
                                $value := Str;
                                $promoted_primitive := 1;
                            }
                            else {
                                nqp::die('Unknown kind of l-value in multiple dispatch');
                            }
                        }
                        else {
                            # If we need an rw argument and didn't get a container,
                            # we're out of luck. Before asserting this, we should make
                            # sure that the original container (which maybe was a
                            # Proxy that was removed in the args we're doing the
                            # dispatch over) was not itself rw.
                            if $rwness && !nqp::iscont(nqp::captureposarg($orig-capture, $i)) {
                                $rwness_mismatch := 1;
                            }
                        }

                        # Ensure the value meets the required type constraints.
                        unless nqp::eqaddr($type, Mu) ||
                                nqp::istype_nd(nqp::hllizefor($value, 'Raku'), $type) {
                            if $type =:= $Positional {
                                # Things like Seq can bind to an @ sigil.
                                my $PositionalBindFailover := nqp::gethllsym('Raku', 'MD_PBF');
                                unless nqp::istype_nd($value, $PositionalBindFailover) {
                                    $type_mismatch := 1;
                                }
                            } else {
                                $type_mismatch := 1;
                            }
                        }

                        # Also ensure any concreteness constraints are unheld.
                        if !$type_mismatch && $definedness {
                            my int $got := $promoted_primitive || nqp::isconcrete_nd($value);
                            if ($got && $definedness == $DEFCON_UNDEFINED) ||
                                    (!$got && $definedness == $DEFCON_DEFINED) {
                                $type_mismatch := 1;
                            }
                            nqp::bindpos_i($need_conc_guard, $i, 1);
                        }
                    }
                    elsif $got_prim == 0 { # and $want_prim != 0 per last condition
                        # Make sure it's the expected kind of native container.
                        nqp::bindpos_i($need_type_guard, $i, 1);
                        my $contish := nqp::captureposarg($capture, $i);
                        unless (($type_flags +& $TYPE_NATIVE_INT) && nqp::iscont_i($contish)) ||
                               (($type_flags +& $TYPE_NATIVE_NUM) && nqp::iscont_n($contish)) ||
                               (($type_flags +& $TYPE_NATIVE_STR) && nqp::iscont_s($contish)) {
                            $type_mismatch := 1;
                        }
                    }
                    else {
                        # It's a primitive type. If we want rw, then we ain't got it.
                        if $rwness {
                            $rwness_mismatch := 1;
                        }
                        # If we want a type object, it's certainly not that either.
                        elsif $definedness == $DEFCON_UNDEFINED {
                            $type_mismatch := 1;
                        }
                        # If we want a primitive type, but got the wrong one, then it's
                        # a mismatch.
                        elsif $want_prim {
                            if (($type_flags +& $TYPE_NATIVE_INT) && $got_prim != $BIND_VAL_INT) ||
                                    (($type_flags +& $TYPE_NATIVE_NUM) && $got_prim != $BIND_VAL_NUM) ||
                                    (($type_flags +& $TYPE_NATIVE_STR) && $got_prim != $BIND_VAL_STR) {
                                $type_mismatch := 1;
                            }
                        }
                        # Otherwise, we want an object type. Figure out the correct
                        # one that we shall box to.
                        else {
                            my $test_type := $got_prim == $BIND_VAL_INT ?? Int !!
                                             $got_prim == $BIND_VAL_NUM ?? Num !!
                                                                           Str;
                            $type_mismatch := 1 unless nqp::istype($test_type, $type);
                        }
                    }
                    $i++;
                }

                # Add it to the possibles list of this group.
                unless $type_mismatch || $rwness_mismatch {
                    nqp::push(@possibles, $cur_candidate);
                }
#                            if $rwness && !nqp::isrwcont(nqp::captureposarg($capture, $i)) {
#                                # If we need a container but don't have one it clearly can't work.
#                                $rwness_mismatch := 1;
#                            }
#                            elsif $type_flags +& $TYPE_NATIVE_MASK {
#                                # Looking for a natively typed value. Did we get one?
#                                if $got_prim == $BIND_VAL_OBJ {
#                                    # Object, but could be a native container. If not, mismatch.
#                                    my $contish := nqp::captureposarg($capture, $i);
#                                    unless (($type_flags +& $TYPE_NATIVE_INT) && nqp::iscont_i($contish)) ||
#                                           (($type_flags +& $TYPE_NATIVE_NUM) && nqp::iscont_n($contish)) ||
#                                           (($type_flags +& $TYPE_NATIVE_STR) && nqp::iscont_s($contish)) {
#                                        $type_mismatch := 1;
#                                    }
#                                }
#                            }
#                            else {
#                                my $param;
#                                if $got_prim == $BIND_VAL_OBJ {
#                                    $param := nqp::captureposarg($capture, $i);
#                                    if    nqp::iscont_i($param) { $param := Int; $primish := 1; }
#                                    elsif nqp::iscont_n($param) { $param := Num; $primish := 1; }
#                                    elsif nqp::iscont_s($param) { $param := Str; $primish := 1; }
#                                    else { $param := nqp::hllizefor($param, 'Raku') }
#                                }
#                                if nqp::eqaddr($type_obj, Mu) || nqp::istype($param, $type_obj) {
#                                    if $i == 0 && nqp::existskey($cur_candidate, 'exact_invocant') {
#                                        unless $param.WHAT =:= $type_obj {
#                                            $type_mismatch := 1;
#                                        }
#                                    }
#                                }
#                                else {
#                                    if $type_obj =:= $Positional {
#                                        my $PositionalBindFailover := nqp::gethllsym('Raku', 'MD_PBF');
#                                        unless nqp::istype($param, $PositionalBindFailover) {
#                                            $type_mismatch := 1;
#                                        }
#                                    } else {
#                                        $type_mismatch := 1;
#                                    }
#                                }
#                            }
#                            ++$i;
#                        }
            }
        }
        else {
            # End of tied group. If there's possibles...
            if nqp::elems(@possibles) {
                # Build a new list of filtered possibles by ruling out any
                # that have unaccepted of missing nameds. Track if we need bind
                # checks or if we have declarative candidates. Also check for
                # defaults and exact arity matches, which we can use for
                # tie-breaking if there are ambiguities.
                my int $i;
                my int $n := nqp::elems(@possibles);
                my int $need-bind-check;
                my int $first-group := nqp::isnull($current-head);
                my @filtered-possibles;
                my @defaults;
                my @exact-arity;
                while $i < $n {
                    my %info := @possibles[$i];
                    unless has-named-args-mismatch($capture, %info) {
                        nqp::push(@filtered-possibles, %info);
                        $need-bind-check++ if nqp::existskey(%info, 'bind_check');
                        my $sub := %info<sub>;
                        nqp::push(@defaults, %info) if nqp::can($sub, 'default') && $sub.default;
                        nqp::push(@exact-arity, %info) if %info<min_arity> == $num_args &&
                           %info<max_arity> == $num_args;
                    }
                    $i++;
                }

                # If we still have multiple possibles and we don't need a bind
                # check, try tie-breakers, and failing that add an ambiguity
                # marker.
                if !$need-bind-check && nqp::elems(@filtered-possibles) > 1 {
                    if nqp::elems(@defaults) == 1 {
                        @filtered-possibles := @defaults;
                    }
                    elsif nqp::elems(@exact-arity) == 1 {
                        @filtered-possibles := @exact-arity;
                    }
                    else {
                        my $node := MultiDispatchAmbiguous.new();
                        if nqp::isnull($current-head) {
                            $current-head := $node;
                        }
                        else {
                            $current-tail.set-next($node);
                        }
                        $current-tail := $node;
                    }
                }

                # Add the filtered possibles to the plan.
                $i := 0;
                $n := nqp::elems(@filtered-possibles);
                while $i < $n {
                    my %info := @filtered-possibles[$i];
                    my $node := $need-bind-check
                        ?? MultiDispatchTry.new(%info<sub>)
                        !! MultiDispatchCall.new(%info<sub>);
                    if nqp::isnull($current-head) {
                        $current-head := $node;
                    }
                    else {
                        $current-tail.set-next($node);
                    }
                    $current-tail := $node;
                    $i++;
                }

                # If we are to stop at a trivial match and nothing needs a
                # bind check, and we've no results before now, we're done.
                if $stop-at-trivial && $first-group &&
                        nqp::elems(@filtered-possibles) == 1 && $need-bind-check == 0 {
                    $done := 1;
                }

                # Otherwise, clear the set of possibles for the next group.
                else {
                    nqp::setelems(@possibles, 0);
                }
            }

            # If we're really at the end of the list, we're done.
            unless nqp::isconcrete(nqp::atpos(@candidates, $cur_idx)) {
                $done := 1;
            }
        }
    }

    # Add an end node.
    if nqp::isnull($current-head) {
        $current-head := MultiDispatchEnd;
    }
    else {
        $current-tail.set-next(MultiDispatchEnd);
    }

    # Install guards as required.
    $i := 0;
    while $i < $num_args {
        if nqp::atpos_i($need_scalar_read, $i) {
            my $tracked := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, $i);
            if nqp::atpos_i($need_scalar_rw_check, $i) {
                my $tracked_desc := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                        $tracked, Scalar, '$!descriptor');
                nqp::dispatch('boot-syscall', 'dispatcher-guard-concreteness', $tracked_desc);
            }
            my $tracked_value := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                    $tracked, Scalar, '$!value');
            nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $tracked_value);
            if nqp::atpos_i($need_conc_guard, $i) {
                nqp::dispatch('boot-syscall', 'dispatcher-guard-concreteness', $tracked_value);
            }
        }
        elsif nqp::atpos_i($need_type_guard, $i) {
            my $tracked := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, $i);
            nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $tracked);
            if nqp::atpos_i($need_conc_guard, $i) {
                nqp::dispatch('boot-syscall', 'dispatcher-guard-concreteness', $tracked);
            }
        }
        $i++;
    }

    # Return the dispatch plan.
    $current-head
}
sub form-raku-capture($vm-capture) {
    my $raku-capture := nqp::create(Capture);
    nqp::bindattr($raku-capture, Capture, '@!list',
        nqp::dispatch('boot-syscall', 'capture-pos-args', $vm-capture));
    nqp::bindattr($raku-capture, Capture, '%!hash',
        nqp::dispatch('boot-syscall', 'capture-named-args', $vm-capture));
    $raku-capture
}
sub multi-junction-failover($capture) {
    # Take a first pass to see if there's a Junction arg.
    my int $num-args := nqp::captureposelems($capture);
    my int $i;
    my $found-junction;
    while $i < $num-args {
        my int $got-prim := nqp::captureposprimspec($capture, $i);
        if $got-prim == 0 {
            my $value := nqp::captureposarg($capture, $i);
            if nqp::isconcrete($value) && nqp::istype($value, Junction) {
                $found-junction := 1;
                last;
            }
        }
        $i++;
    }

    # If there is a Junction arg, then take another pass through to put type
    # guards on all positional argument types.
    if $found-junction {
        $i := 0;
        while $i < $num-args {
            if nqp::captureposprimspec($capture, $i) == 0 {
                my $arg := nqp::captureposarg($capture, $i);
                my $tracked := nqp::dispatch('boot-syscall', 'dispatcher-track-arg',
                    $capture, $i);
                if nqp::istype_nd($arg, Scalar) {
                    $tracked := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                        $tracked, Scalar, '$!value');
                }
                nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $tracked);
                nqp::dispatch('boot-syscall', 'dispatcher-guard-concreteness', $tracked);
            }
            $i++;
        }
    }

    $found-junction
}
sub multi-no-match-handler($target, $dispatch-arg-capture, $orig-capture, $orig-arg-capture) {
    # If no candidates are found but there is a Junction argument, we'll
    # dispatch to that.
    if multi-junction-failover($dispatch-arg-capture) { # Guards added here
        my $with-invocant := nqp::dispatch('boot-syscall',
            'dispatcher-insert-arg-literal-obj', $orig-capture, 0, Junction);
        my $threader := Junction.HOW.find_method(Junction, 'AUTOTHREAD') //
            nqp::die('Junction auto-thread method not found');
        my $capture-delegate := nqp::dispatch('boot-syscall',
            'dispatcher-insert-arg-literal-obj', $with-invocant, 0, $threader);
        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-invoke',
            $capture-delegate);
    }
    # Otherwise, it's just an error.
    else {
        Perl6::Metamodel::Configuration.throw_or_die(
            'X::Multi::NoMatch',
            "Cannot call " ~ $target.name() ~ "; no signatures match",
            :dispatcher($target), :capture(form-raku-capture($orig-arg-capture)));
    }
}
sub multi-ambiguous-handler($dispatch-plan, $target, $arg-capture) {
    my @ambiguous;
    my $ambig-call := $dispatch-plan.next;
    while nqp::istype($ambig-call, MultiDispatchCall) {
        nqp::push(@ambiguous, $ambig-call.candidate);
        $ambig-call := $ambig-call.next;
    }
    Perl6::Metamodel::Configuration.throw_or_die(
        'X::Multi::Ambiguous',
        "Ambiguous call to " ~ $target.name(),
        :dispatcher($target), :@ambiguous,
        :capture(form-raku-capture($arg-capture)));
}
nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-multi-core',
    # Initial dispatch. Tries to find an initial candidate.
    -> $capture {
        # Obtain the candidate list, producing it if it doesn't already exist.
        my $target := nqp::captureposarg($capture, 0);
        my @candidates := nqp::getattr($target, Routine, '@!dispatch_order');
        if nqp::isnull(@candidates) {
            nqp::scwbdisable();
            @candidates := $target.'!sort_dispatchees_internal'();
            nqp::bindattr($target, Routine, '@!dispatch_order', @candidates);
            nqp::scwbenable();
        }

        # Drop the first argument, to get just the arguments to dispatch on, and
        # then produce a multi-dispatch plan. Decide what to do based upon it.
        my $arg-capture := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 0);
        my $dispatch-plan := raku-multi-plan(@candidates, $arg-capture, 1);
        if nqp::istype($dispatch-plan, MultiDispatchCall) &&
                !nqp::istype($dispatch-plan, MultiDispatchTry) &&
                nqp::istype($dispatch-plan.next, MultiDispatchEnd) {
            # Trivial multi dispatch. Set dispatch state for resumption, and
            # then delegate to raku-invoke to run it.
            nqp::dispatch('boot-syscall', 'dispatcher-set-resume-init-args', $capture);
            my $capture-delegate := nqp::dispatch('boot-syscall',
                'dispatcher-insert-arg-literal-obj', $arg-capture, 0,
                $dispatch-plan.candidate);
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-invoke',
                $capture-delegate);
        }
        elsif nqp::istype($dispatch-plan, MultiDispatchNonScalar) {
            # Need to strip the Proxy arguments and then try again. Produce a
            # proxy reader code object to do so, insert it as the first arg,
            # and delegate to a dispatcher to manage reading the args and
            # then retrying with the outcome.
            my $reader := $PROXY-READERS.reader-for($arg-capture, $dispatch-plan.args);
            my $capture-delegate := nqp::dispatch('boot-syscall',
                'dispatcher-insert-arg-literal-obj', $capture, 0, $reader);
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-multi-remove-proxies',
                $capture-delegate);
        }
        elsif nqp::istype($dispatch-plan, MultiDispatchAmbiguous) &&
                nqp::istype($dispatch-plan.next, MultiDispatchCall) {
            multi-ambiguous-handler($dispatch-plan, $target, $arg-capture);
        }
        elsif nqp::istype($dispatch-plan, MultiDispatchEnd) {
            multi-no-match-handler($target, $arg-capture, $capture, $arg-capture);
        }
        else {
            # It's a non-trivial multi dispatch. Prefix the capture with
            # the dispatch plan, and also a zero to indicate this is not a
            # resumption of any kind. The delegate to the non-trivial multi
            # dispatcher.
            my $capture-with-plan := nqp::dispatch('boot-syscall',
                'dispatcher-insert-arg-literal-obj', $capture, 0,
                $dispatch-plan);
            my $capture-delegate := nqp::dispatch('boot-syscall',
                'dispatcher-insert-arg-literal-int', $capture-with-plan, 0, 0);
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-multi-non-trivial',
                $capture-delegate);
        }
    },
    # Resume of a trivial dispatch.
    -> $capture {
        # We'll delegate the hard work to the non-trivial dispatcher. We
        # only want to do that once, however, and so set the dispatch state
        # to exhausted if we've already done it. Check that's not so.
        my $track-state := nqp::dispatch('boot-syscall', 'dispatcher-track-resume-state');
        nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track-state);
        my $state := nqp::dispatch('boot-syscall', 'dispatcher-get-resume-state');
        if nqp::isnull($state) {
            # First time. Set state to exhausted.
            nqp::dispatch('boot-syscall', 'dispatcher-set-resume-state-literal', Exhausted);

            # Obtain resume initialization arguments and form the plan.
            my $init := nqp::dispatch('boot-syscall', 'dispatcher-get-resume-init-args');
            my $target := nqp::captureposarg($init, 0);
            my @candidates := nqp::getattr($target, Routine, '@!dispatch_order');
            my $arg-capture := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $init, 0);
            my $dispatch-plan := raku-multi-plan(@candidates, $arg-capture, 0);

            # We already called the first candidate in the trivial plan, so
            # drop it.
            $dispatch-plan := $dispatch-plan.next;

            # Delegate to the non-trivial dispatcher, passing along the kind
            # of dispatch we're doing and the plan.
            my $capture-with-plan := nqp::dispatch('boot-syscall',
                'dispatcher-insert-arg-literal-obj', $init, 0,
                $dispatch-plan);
            my $track-kind := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0);
            my $capture-delegate := nqp::dispatch('boot-syscall',
                'dispatcher-insert-arg', $capture-with-plan, 0, $track-kind);
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-multi-non-trivial',
                $capture-delegate);
        }
        else {
            # Resume next disaptcher, if any, otherwise hand back Nil.
            if !nqp::dispatch('boot-syscall', 'dispatcher-next-resumption') {
                nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-constant',
                    nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                        $capture, 0, Nil));
            }
        }
    });

# The non-trivial multi dispatch has quite similar initial and resume steps,
# and thus the majority of the work is factored out into a subroutine.
nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-multi-non-trivial',
    # Initialization of a non-trivial dispatch. Receives a dispatch resumption
    # kind, which is zero if we're not resuming.
    -> $capture {
        # Extract and guard on the kind (first argument).
        my $track-kind := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0);
        nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track-kind);
        my int $kind := nqp::captureposarg_i($capture, 0);

        # Extract and track the current state.
        my $track-cur-state := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 1);
        my $cur-state := nqp::captureposarg($capture, 1);

        # Drop the leading two arguments to get the argument capture prefixed
        # with the original dispatch target, and one more to get the argument
        # capture.
        my $orig-capture := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg',
            nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 0), 0);
        my $arg-capture := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg',
            $orig-capture, 0);

        # Perform the step.
        raku-multi-non-trivial-step($kind, $track-cur-state, $cur-state, $orig-capture,
            $arg-capture, 0);
    },
    -> $capture {
        # Extract and guard on the kind (first argument).
        my $track-kind := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0);
        nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track-kind);
        my int $kind := nqp::captureposarg_i($capture, 0);

        # Drop the leading argument to get the argument capture prefixed
        # with the original target, one more to get the arguments.
        my $init := nqp::dispatch('boot-syscall', 'dispatcher-get-resume-init-args');
        my $orig-capture := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg',
            $init, 0);
        my $arg-capture := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg',
            $orig-capture, 0);

        # Have dispatch state already, or first resume?
        my $track-state := nqp::dispatch('boot-syscall', 'dispatcher-track-resume-state');
        my $state := nqp::dispatch('boot-syscall', 'dispatcher-get-resume-state');
        if nqp::isnull($state) {
            # First resumption. Guard that it is so.
            nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track-state);

            # Obtain plan and args from init state.
            my $track-cur-state := nqp::dispatch('boot-syscall', 'dispatcher-track-arg',
                $init, 0);
            my $cur-state := nqp::captureposarg($init, 0);
            raku-multi-non-trivial-step($kind, $track-cur-state, $cur-state, $orig-capture,
                $arg-capture, 1);
        }
        else {
            raku-multi-non-trivial-step($kind, $track-state, $state, $orig-capture,
                $arg-capture, 1);
        }
    });
sub raku-multi-non-trivial-step(int $kind, $track-cur-state, $cur-state, $orig-capture,
        $arg-capture, $is-resume) {
    if nqp::istype($cur-state, MultiDispatchCall) {
        # Guard on the current state and on the callee (the type guards are
        # implicitly established when we guard the callee).
        my $track-candidate := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
            $track-cur-state, MultiDispatchCall, '$!candidate');
        nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track-candidate);

        # Peel off one candidate and use that as the next state.
        if $is-resume {
            my $track-next := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                $track-cur-state, MultiDispatchCall, '$!next');
            nqp::dispatch('boot-syscall', 'dispatcher-set-resume-state', $track-next);
        }
        else {
            my $next := $cur-state.next;
            my $init-state := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                $orig-capture, 0, $next);
            nqp::dispatch('boot-syscall', 'dispatcher-set-resume-init-args', $init-state);
        }

        # If it needs a bind check, set us up to resume if it fails.
        if nqp::istype($cur-state, MultiDispatchTry) {
            nqp::dispatch('boot-syscall', 'dispatcher-resume-on-bind-failure', $kind);
        }

        # Set up the call.
        my $capture-delegate := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg',
            $arg-capture, 0, $track-candidate);
        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-invoke', $capture-delegate);
    }
    elsif nqp::istype($cur-state, MultiDispatchEnd) {
        # If this is the initial dispatch, then error, otherwise hand back Nil.
        if $kind == nqp::const::DISP_CALLSAME {
            my $target := nqp::captureposarg($orig-capture, 0);
            multi-no-match-handler($target, $arg-capture, $orig-capture, $arg-capture);
        }
        else {
            nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $track-cur-state);
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-constant',
                nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                    $arg-capture, 0, Nil));
        }
    }
    else {
        nqp::die('Non-trivial multi dispatch step NYI for ' ~ $cur-state.HOW.name($cur-state));
    }
}

# Proxy removal for multiple dispatch.
nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-multi-remove-proxies',
    # The dispatch receives (remover, original invokee, args...).
    -> $capture {
        # The resume init state drops the remover.
        nqp::dispatch('boot-syscall', 'dispatcher-set-resume-init-args',
            nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 0));

        # We then invoke the remover with the arguments (so need to drop the
        # original invokee).
        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-code-constant',
            nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 1));
    },
    # The resumption is done with the args with proxies stripped.
    -> $capture {
        # Make sure this really is the resume with the proxies stripped,
        # not some inner resume, which we should just pass along.
        my $track_kind := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0);
        nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track_kind);
        my int $kind := nqp::captureposarg_i($capture, 0);
        if $kind == nqp::const::DISP_DECONT {
            # Yes, it's the resume we're looking for. Locate the candidates by
            # using the resume init args.
            my $orig-capture := nqp::dispatch('boot-syscall', 'dispatcher-get-resume-init-args');
            my $target := nqp::captureposarg($orig-capture, 0);
            my @candidates := nqp::getattr($target, Routine, '@!dispatch_order');

            # Put a guard on the dispatchees. (TODO This risks the callsite in
            # the generated removers becoming a polymorphic blow-up point; when
            # we can associate it with the dispatch program of the initial
            # dispatch, that will be rather better.)
            my $track_callee := nqp::dispatch('boot-syscall', 'dispatcher-track-arg',
                $orig-capture, 0);
            nqp::dispatch('boot-syscall', 'dispatcher-guard-literal',
                nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                    $track_callee, Routine, '@!dispatchees'));

            # We now make the dispatch plan using the arguments with proxies
            # removed, put pass along the original arg capture to, for use
            # in `rw`-ness testing.
            my $no-proxy-arg-capture := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg',
                $capture, 0);
            my $orig-arg-capture := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg',
                $orig-capture, 0);
            my $dispatch-plan := raku-multi-plan(@candidates, $no-proxy-arg-capture, 0,
                $orig-arg-capture);

            # Consider the dispatch plan. Note we should always pass along the original
            # arguments when invoking, so anything `is rw` gets the Proxy. We for now
            # also send everything through the non-trivial dispatch path to keep it
            # a little simpler.
            if nqp::istype($dispatch-plan, MultiDispatchNonScalar) {
                nqp::die('FETCH from a Proxy unexpectedly returned another Proxy');
            }
            elsif nqp::istype($dispatch-plan, MultiDispatchAmbiguous) &&
                    nqp::istype($dispatch-plan.next, MultiDispatchCall) {
                multi-ambiguous-handler($dispatch-plan, $target, $orig-arg-capture);
            }
            elsif nqp::istype($dispatch-plan, MultiDispatchEnd) {
                multi-no-match-handler($target, $no-proxy-arg-capture, $orig-capture,
                    $orig-arg-capture);
            }
            else {
                my $capture-with-plan := nqp::dispatch('boot-syscall',
                    'dispatcher-insert-arg-literal-obj', $orig-capture, 0,
                    $dispatch-plan);
                my $capture-delegate := nqp::dispatch('boot-syscall',
                    'dispatcher-insert-arg-literal-int', $capture-with-plan, 0, 0);
                nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-multi-non-trivial',
                    $capture-delegate);
            }
        }
        elsif !nqp::dispatch('boot-syscall', 'dispatcher-next-resumption') {
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-constant',
                nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                    $capture, 0, Nil));
        }
    });

# This is where invocation bottoms out, however we reach it. By this point, we
# just have something to invoke, which is either a code object (potentially with
# wrappers) or something that hopefully has a CALL-ME.
my $late-coerce := -> $target, $val {
    my $how := $target.HOW;
    my $coercion-type := Perl6::Metamodel::CoercionHOW.new_type(
        (nqp::istype($how, Perl6::Metamodel::ClassHOW) && $how.is_pun($target)
            ?? $target.HOW.pun_source($target)
            !! $target.WHAT),
        $val.WHAT);
    $coercion-type.HOW.coerce($coercion-type, $val)
}
my $listy-coercion := -> $coercion-type, *@args {
    my $list := nqp::create(List);
    nqp::bindattr($list, List, '$!reified', @args);
    $coercion-type.HOW.coerce($coercion-type, $list)
}
nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-invoke', -> $capture {
    # Guard type and concreteness of code object. This is a no-op in the case
    # that it's been determined a constant by an upper dispatcher.
    my $code := nqp::captureposarg($capture, 0);
    my $code_arg := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0);
    nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $code_arg);
    nqp::dispatch('boot-syscall', 'dispatcher-guard-concreteness', $code_arg);

    # If it's already a VM-level code reference, just invoke it.
    if nqp::reprname($code) eq 'MVMCode' {
        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-code',
            $capture);
    }

    # If it's a routine that has wrappers (the guard on the type covers this,
    # since such a routine will have been mxied in to).
    elsif nqp::istype($code, Routine) && nqp::can($code, 'WRAPPERS') {
        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-invoke-wrapped',
            $capture);
    }

    # If it's a code object...
    elsif nqp::istype($code, Code) {
        # Concrete code object: extract the $!do, replace the code object,
        # and delegate to boot-code.
        if nqp::isconcrete($code) {
            my $do_attr := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                $code_arg, Code, '$!do');
            my $delegate_capture := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg',
                nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 0),
                0, $do_attr);
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-code',
                $delegate_capture);
        }

        # Invoking non-concrete code object is an error.
        else {
            nqp::die('Cannot invoke a ' ~ $code.HOW.name($code) ~ ' type object');
        }
    }

    # If it's ForeignCode, extract the wrapped code object and delegate to
    # lang-code to run whatever is wrapped there.
    elsif nqp::istype($code, ForeignCode) {
        if nqp::isconcrete($code) {
            my $do_attr := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                $code_arg, ForeignCode, '$!do');
            my $delegate_capture := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg',
                nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 0),
                0, $do_attr);
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'lang-call',
                $delegate_capture);
        }
        else {
            nqp::die('Cannot invoke a ' ~ $code.HOW.name($code) ~ ' type object');
        }
    }

    # If it's NQP code (ideally we want a more flexible cross-language solution
    # here) then unrap it also.
    elsif nqp::istype($code, NQPRoutine) {
        if nqp::isconcrete($code) {
            my $do_attr := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                $code_arg, NQPRoutine, '$!do');
            my $delegate_capture := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg',
                nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 0),
                0, $do_attr);
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-code',
                $delegate_capture);
        }
        else {
            nqp::die('Cannot invoke a ' ~ $code.HOW.name($code) ~ ' type object');
        }
    }

    # Otherwise, try the CALL-ME or coercion paths.
    else {
        my $call-me := $code.HOW.find_method($code, 'CALL-ME');
        if nqp::isconcrete($call-me) {
            # A CALL-ME method is found; set up a resolved method call to it.
            my $with-name := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-str',
                $capture, 0, 'CALL-ME');
            my $with-type := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                $with-name, 0, $code.WHAT);
            my $delegate := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                $with-type, 0, $call-me);
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-meth-call-resolved',
                $delegate);
        }
        elsif !nqp::isconcrete($code) && nqp::captureposelems($capture) >= 2 {
            # Looks like a coercion. In the best case we just have one argument
            # and things will be straightforward. Failing that, we'll have to
            # form a list and take the slow-bound path.
            if nqp::captureposelems($capture) == 2 {
                # Work out what we have to coerce.
                my $arg-type;
                my int $could-not-guard;
                my int $prim := nqp::captureposprimspec($capture, 1);
                if $prim == 1    { $arg-type := Int }
                elsif $prim == 2 { $arg-type := Num }
                elsif $prim == 3 { $arg-type := Str }
                else {
                    # Object argument, so type guard.
                    my $arg := nqp::captureposarg($capture, 1);
                    $arg-type := $arg.WHAT;
                    my $track-arg := nqp::dispatch('boot-syscall', 'dispatcher-track-arg',
                        $capture, 1);
                    nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $track-arg);
                    if nqp::isconcrete_nd($arg) && nqp::iscont($arg) {
                        # Containerized. If it's a Scalar, we can deref and guard
                        # on that. If not, we'll have to thunk it and figure it
                        # out each time.
                        if nqp::istype_nd($arg, Scalar) {
                            nqp::dispatch('boot-syscall', 'dispatcher-guard-type',
                                nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                                    $track-arg, Scalar, '$!value'));
                        }
                        else {
                            $could-not-guard := 1;
                        }
                    }
                }

                # Ensure there's no nameds.
                if nqp::capturehasnameds($capture) {
                    Perl6::Metamodel::Configuration.throw_or_die(
                        'X::Coerce::Impossible',
                        "Cannot coerce to " ~ $code.HOW.name($code) ~ " with named arguments",
                        :target-type($code.WHAT),
                        :from-type($arg-type), :hint("named arguments passed")
                    );
                }

                # If we could not guard need to delegate to a late-bound
                # handler.
                if $could-not-guard {
                    my $delegate := nqp::dispatch('boot-syscall',
                        'dispatcher-insert-arg-literal-obj', $capture,
                        0, $late-coerce);
                    nqp::dispatch('boot-syscall', 'dispatcher-delegate',
                        'boot-code-constant', $delegate);
                }

                # Otherwise, can rewrite the callsite directly to do the
                # coercion.
                else {
                    # Form the coercion type.
                    my $how := $code.HOW;
                    my $coercion-type := Perl6::Metamodel::CoercionHOW.new_type(
                        (nqp::istype($how, Perl6::Metamodel::ClassHOW) && $how.is_pun($code)
                            ?? $how.pun_source($code)
                            !! $code.WHAT),
                        $arg-type);

                    # Call $coercion-type.HOW.coerce($coercion-type, $val). We
                    # know that there was only one item, so we can drop the
                    # callee, prepend the coercion type, the HOW, and then the
                    # name and type as raku-meth-call wants.
                    my $coercee-only := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg',
                        $capture, 0);
                    my $with-coercion-type := nqp::dispatch('boot-syscall',
                        'dispatcher-insert-arg-literal-obj', $coercee-only, 0, $coercion-type);
                    my $coerce-how := $coercion-type.HOW;
                    my $with-how := nqp::dispatch('boot-syscall',
                        'dispatcher-insert-arg-literal-obj', $with-coercion-type, 0, $coerce-how);
                    my $with-name := nqp::dispatch('boot-syscall',
                        'dispatcher-insert-arg-literal-str', $with-how, 0, 'coerce');
                    my $delegate := nqp::dispatch('boot-syscall',
                        'dispatcher-insert-arg-literal-obj', $with-name, 0, $coerce-how);
                    nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-meth-call',
                        $delegate);
                }
            }
            else {
                # List formation is too complex for a dispatch program, so we
                # form a coercion type, prepend it, do the args that form the
                # list to be coerced, and then delegate to a code object to
                # do the rest of the work.
                if nqp::capturehasnameds($capture) {
                    Perl6::Metamodel::Configuration.throw_or_die(
                        'X::Coerce::Impossible',
                        "Cannot coerce to " ~ $code.HOW.name($code) ~ " with named arguments",
                        :target-type($code.WHAT),
                        :from-type(List), :hint("named arguments passed")
                    );
                }
                my $how := $code.HOW;
                my $coercion-type := Perl6::Metamodel::CoercionHOW.new_type(
                    (nqp::istype($how, Perl6::Metamodel::ClassHOW) && $how.is_pun($code)
                        ?? $how.pun_source($code)
                        !! $code.WHAT),
                    List);
                my $list-elems-only := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg',
                    $capture, 0);
                my $with-coercion-type := nqp::dispatch('boot-syscall',
                    'dispatcher-insert-arg-literal-obj', $list-elems-only, 0, $coercion-type);
                my $delegate := nqp::dispatch('boot-syscall',
                    'dispatcher-insert-arg-literal-obj', $with-coercion-type, 0, $listy-coercion);
                nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-code-constant',
                    $delegate);
            }
        }
        else {
            my $typename := $code.HOW.name($code);
            Perl6::Metamodel::Configuration.throw_or_die(
                'X::Method::NotFound',
                "No such method 'CALL-ME' for invocant of type '$typename'",
                :invocant($code), :method(nqp::hllizefor('CALL-ME', "Raku")),
                :typename(nqp::hllizefor($typename, "Raku"))
            );
        }
    }
});

# Entrypoint for dispatch to a wrapped routine. Builds the chain and delegates
# to another dispatcher that will handle the walking through it via resumption.
nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-invoke-wrapped', -> $capture {
    # Guard on the current set of wrappers (the array of them is immutable,
    # so we can rely on its identity).
    my $routine := nqp::captureposarg($capture, 0);
    my $wrapper_type := $routine.WRAPPER-TYPE;
    my $track_routine := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0);
    my $track-wrappers := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
            $track_routine, $wrapper_type, '$!wrappers');
    nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track-wrappers);

    # With wrappers, we pretty much know we'll be traversing them, so we
    # build the deferral chain up front, unlike in other dispatchers.
    my @all_callees := nqp::clone($routine.WRAPPERS);
    nqp::push(@all_callees, nqp::getattr($routine, Code, '$!do'));
    my $chain := Exhausted;
    while nqp::elems(@all_callees) {
        $chain := DeferralChain.new(nqp::pop(@all_callees), $chain);
    }

    # Delegate to the wrap deferral dispatcher with the arguments to call
    # the initial wrapper with, preceded by the calculated chain.
    my $without_routine := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 0);
    nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-wrapper-deferral',
        nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
            $without_routine, 0, $chain));
});

# The wrapper deferral dispatcher that moves through wrappers.
nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-wrapper-deferral',
    # Initial dispatch, called with the chain to walk through along with the
    # arguments. This is used either in the case we are just starting to walk
    # through the dispatchers or in the event of a callwith.
    -> $capture {
        # Obtain and guard on the first wrapper callee.
        my $cur_deferral := nqp::captureposarg($capture, 0);
        my $track_cur_deferral := nqp::dispatch('boot-syscall', 'dispatcher-track-arg',
            $capture, 0);
        my $track_code := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
            $track_cur_deferral, DeferralChain, '$!code');
        nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track_code);

        # Extract the arguments and set the resume init args to be the next item
        # in the chain prepended to the arguments.
        my $args := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 0);
        nqp::dispatch('boot-syscall', 'dispatcher-set-resume-init-args',
            nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                $args, 0, $cur_deferral.next));

        # Invoke the first wrapper.
        my $delegate_capture := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
            $args, 0, $cur_deferral.code);
        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-invoke', $delegate_capture);
    },
    # Resumption.
    -> $capture {
        # Guard on the kind of resume we're doing, and get that flag.
        my $track_kind := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0);
        nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track_kind);
        my int $kind := nqp::captureposarg_i($capture, 0);

        # Work out which wrapper we'll call next.
        my $init := nqp::dispatch('boot-syscall', 'dispatcher-get-resume-init-args');
        my $state := nqp::dispatch('boot-syscall', 'dispatcher-get-resume-state');
        my $track_cur_deferral;
        my $cur_deferral;
        if $kind == nqp::const::DISP_LASTCALL {
            # It's lastcall; just update the state to Exhausted.
            nqp::dispatch('boot-syscall', 'dispatcher-set-resume-state-literal', Exhausted);
        }
        elsif nqp::isnull($state) {
            # No state, so the initial resumption. Guard on there being no
            # dispatch state.
            my $track_state := nqp::dispatch('boot-syscall', 'dispatcher-track-resume-state');
            nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track_state);

            # The current deferral is obtained from the initialization state.
            $track_cur_deferral := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $init, 0);
            $cur_deferral := nqp::captureposarg($init, 0);
        }
        elsif !nqp::istype($state, Exhausted) {
            # Already working through a chain of wrappers deferrals. Thus the
            # current deferral is the current state;
            $track_cur_deferral := nqp::dispatch('boot-syscall', 'dispatcher-track-resume-state');
            $cur_deferral := $state;
        }
        else {
            # Dispatch already exhausted; guard on that and fall through to returning
            # Nil.
            my $track_state := nqp::dispatch('boot-syscall', 'dispatcher-track-resume-state');
            nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track_state);
        }

        # If we have a current deferral...
        if $cur_deferral {
            if $kind == nqp::const::DISP_CALLWITH {
                # Mark this dispatcher exhausted since we're moving on from it,
                # and then re-enter the dispatcher with the remaining wrappers
                # and the args given to us.
                nqp::dispatch('boot-syscall', 'dispatcher-set-resume-state-literal',
                    Exhausted);
                my $args := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 0);
                my $with_chain := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg',
                    $args, 0, $track_cur_deferral);
                nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-wrapper-deferral',
                    $with_chain);
            }
            else {
                # Not callwith, so we keep walking the list. Update state to
                # move to the next wrapper in the list and then put into effect
                # the resumption.
                my $track_code := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                    $track_cur_deferral, DeferralChain, '$!code');
                nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track_code);
                my $track_next := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                    $track_cur_deferral, DeferralChain, '$!next');
                nqp::dispatch('boot-syscall', 'dispatcher-set-resume-state', $track_next);
                if $kind == nqp::const::DISP_CALLSAME {
                    # Invoke the next bit of code.
                    my $delegate_capture := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                        nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $init, 0),
                        0, $cur_deferral.code);
                    nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-invoke', $delegate_capture);
                }
                elsif $kind == nqp::const::DISP_NEXTCALLEE {
                    # We just want the code itself, not to invoke it.
                    nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-constant',
                        nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                            $capture, 0, $cur_deferral.code));
                }
                else {
                    nqp::die('Unimplemented resumption kind in wrap dispatch');
                }
            }
        }
        else {
            # This dispatcher is exhausted. However, there may be another one
            # we can try (for example, in a wrapped method). Only do this if
            # it's not lastcall. Failing that, give back Nil.
            if $kind == nqp::const::DISP_LASTCALL || !nqp::dispatch('boot-syscall', 'dispatcher-next-resumption') {
                nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-constant',
                    nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                        $capture, 0, Nil));
            }
        }
    });

# The dispatcher backing p6capturelex. If we are passed a code object, then
# extracts the underlying handle and causes it to be captured.
nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-capture-lex', -> $capture {
    my $code := nqp::captureposarg($capture, 0);
    my $track-code := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0);
    nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $track-code);
    if nqp::istype($code, Code) {
        my $do := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
            $track-code, Code, '$!do');
        my $with-do := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg',
            nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 0),
            0, $do);
        my $delegate := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-str',
            $with-do, 0, 'try-capture-lex');
        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-syscall',
            $delegate);
    }
    else {
        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-value', $capture);
    }
});

# The dispatcher backing p6capturelexwhere. If we are passed a code object, then
# extracts the underlying handle and looks down the callstack for a caller that
# matches the outer, and captures it.
nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-capture-lex-callers', -> $capture {
    my $code := nqp::captureposarg($capture, 0);
    my $track-code := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0);
    nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $track-code);
    if nqp::istype($code, Code) {
        my $do := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
            $track-code, Code, '$!do');
        my $with-do := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg',
            nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 0),
            0, $do);
        my $delegate := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-str',
            $with-do, 0, 'try-capture-lex-callers');
        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-syscall',
            $delegate);
    }
    else {
        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-value', $capture);
    }
});

# Resumption error reporting dispatcher.
nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-resume-error', -> $capture {
    my str $redispatcher := nqp::getcodename(nqp::callercode());
    Perl6::Metamodel::Configuration.throw_or_die(
        'X::NoDispatcher',
        "$redispatcher is not in the dynamic scope of a dispatcher",
        :$redispatcher
    );
});
