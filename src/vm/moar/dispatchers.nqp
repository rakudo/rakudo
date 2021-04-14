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
        my %ex := nqp::gethllsym('Raku', 'P6EX');
        if nqp::isnull(%ex) || !nqp::existskey(%ex, 'X::TypeCheck::Return') {
            nqp::die("Type check failed for return value; expected '" ~
                $wanted.HOW.name($wanted) ~ "' but got '" ~
                $got.HOW.name($got) ~ "'");
        }
        else {
            nqp::atkey(%ex, 'X::TypeCheck::Return')($got, $wanted)
        }
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
        my %x := nqp::gethllsym('Raku', 'P6EX');
        if nqp::ishash(%x) {
            %x<X::TypeCheck::Assignment>($desc.name, $value, $desc.of);
        }
        else {
            nqp::die("Type check failed in assignment");
        }
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
    my $meth := $obj.HOW.find_method($obj, $name);

    # Report an error if there is no such method.
    unless nqp::isconcrete($meth) {
        my $class := nqp::getlexcaller('$?CLASS');
        if nqp::gethllsym('Raku', 'P6EX') -> %ex {
            if $name eq 'STORE' {
                if nqp::atkey(%ex,'X::Assignment::RO') -> $thrower {
                    $thrower($obj);
                }
            }
            elsif nqp::atkey(%ex,'X::Method::NotFound') -> $thrower {
                $thrower($obj, $name, $obj.HOW.name($obj), :in-class-call(nqp::eqaddr(nqp::what($obj), $class)));
            }
        }
        else {
            nqp::die("Method '$name' not found for invocant of class '{$obj.HOW.name($obj)}'");
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
        my %ex := nqp::gethllsym('Raku', 'P6EX');
        if nqp::isnull(%ex) || !nqp::existskey(%ex, 'X::Method::InvalidQualifier') {
            nqp::die("Cannot dispatch to method $name on " ~ $type.HOW.name($type) ~
                " because it is not inherited or done by " ~ $obj.HOW.name($obj));
        }
        else {
            nqp::atkey(%ex, 'X::Method::InvalidQualifier')($name, $obj, $type)
        }
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
    # and the method name respectively.
    -> $capture {
        # Guard on the kind of resume we're doing, and get that flag.
        my $track_kind := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0);
        nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track_kind);
        my int $kind := nqp::captureposarg_i($capture, 0);

        # Work out the next method to call, if any. This depends on if we have
        # an existing dispatch state (that is, a method deferral is already in
        # progress).
        my $init := nqp::dispatch('boot-syscall', 'dispatcher-get-resume-init-args');
        my $state := nqp::dispatch('boot-syscall', 'dispatcher-get-resume-state');
        my $next_method;
        if $kind == 2 {
            # It's lastcall; just update the state to Exhausted.
            nqp::dispatch('boot-syscall', 'dispatcher-set-resume-state-literal', Exhausted);
        }
        elsif nqp::isnull($state) {
            # No state, so just starting the resumption. Guard on the
            # invocant type and name.
            my $track_start_type := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $init, 0);
            nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $track_start_type);
            my $track_name := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $init, 1);
            nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track_name);

            # Also guard on there being no dispatch state.
            my $track_state := nqp::dispatch('boot-syscall', 'dispatcher-track-resume-state');
            nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track_state);

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

            # If there's nothing to defer to, we'll evaluate to Nil (just don't set
            # the next method, and it happens below).
            if nqp::elems(@methods) >= 2 {
                # We can defer. Populate next method.
                @methods.shift; # Discard the first one, which we initially called
                $next_method := @methods.shift; # The immediate next one

                # Build chain of further methods and set it as the state.
                my $chain := Exhausted;
                while @methods {
                    $chain := DeferralChain.new(@methods.pop, $chain);
                }
                nqp::dispatch('boot-syscall', 'dispatcher-set-resume-state-literal', $chain);
            }
        }
        elsif !nqp::istype($state, Exhausted) {
            # Already working through a chain of method deferrals. Obtain
            # the tracking object for the dispatch state, and guard against
            # the next code object to run.
            my $track_state := nqp::dispatch('boot-syscall', 'dispatcher-track-resume-state');
            my $track_method := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                $track_state, DeferralChain, '$!code');
            nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track_method);

            # Update dispatch state to point to next method.
            my $track_next := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                $track_state, DeferralChain, '$!next');
            nqp::dispatch('boot-syscall', 'dispatcher-set-resume-state', $track_next);

            # Set next method, which we shall defer to.
            $next_method := $state.code;
        }
        else {
            # Dispatch already exhausted; guard on that and fall through to returning
            # Nil.
            my $track_state := nqp::dispatch('boot-syscall', 'dispatcher-track-resume-state');
            nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track_state);
        }

        # If we found a next method...
        if nqp::isconcrete($next_method) {
            # Check the kind of deferral requested.
            if $kind == 0 {
                # Call with same (that is, original) arguments. Invoke with those.
                # We drop the first two arguments (which are only there for the
                # resumption), add the code object to invoke, and then leave it
                # to the invoke dispatcher.
                my $just_args := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg',
                    nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $init, 0),
                    0);
                my $delegate_capture := nqp::dispatch('boot-syscall',
                    'dispatcher-insert-arg-literal-obj', $just_args, 0, $next_method);
                if nqp::istype($next_method, Routine) && $next_method.is_dispatcher {
                    nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-multi',
                            $delegate_capture);
                }
                else {
                    nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-invoke',
                            $delegate_capture);
                }
            }
            elsif $kind == 3 {
                # We just want method itself, not to invoke it.
                nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-constant',
                    nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                        $capture, 0, $next_method));
            }
            else {
                nqp::die('Unimplemented resumption kind in method dispatch');
            }
        }
        else {
            # No method, so evaluate to Nil (boot-constant disregards all but
            # the first argument).
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-constant',
                nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                    $capture, 0, Nil));
        }
    });

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
        if $onlystar {
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
        if $kind == 5 {
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-multi-core',
                nqp::dispatch('boot-syscall', 'dispatcher-get-resume-init-args'));
        }
        elsif !nqp::dispatch('boot-syscall', 'dispatcher-next-resumption') {
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-constant',
                nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                    $capture, 0, Nil));
        }
    });

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
        my $obj := nqp::create(MultiDispatchCall);
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
# * Not actually used in a plan, just a sentinel to convey we have the
#   container but not Scalar (such as Proxy) case. The guards in this
#   case will just be on the arguments that are such containers.
my class MultiDispatchNonScalar {
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
sub raku-multi-plan(@candidates, $capture, int $stop-at-trivial) {
    # First check there's no non-Scalar containers in the positional arguments.
    # If there are, establish guards relating to those and we're done.
    my int $num_args := nqp::captureposelems($capture);
    my int $i;
    my $non-scalar := nqp::list_i();
    while $i < $num_args {
        my int $got_prim := nqp::captureposprimspec($capture, $i);
        if $got_prim == 0 {
            my $value := nqp::captureposarg($capture, $i);
            if nqp::iscont($value) && !nqp::istype_nd($value, Scalar) {
                nqp::push_i($non-scalar, $i);
            }
        }
        $i++;
    }
    if nqp::elems($non-scalar) {
        # TODO guards
        return MultiDispatchNonScalar;
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
                    if $got_prim == 0 {
                        # It's an object type. Obtain the value, and go by if it's a
                        # container or not.
                        my $value := nqp::captureposarg($capture, $i);
                        nqp::bindpos_i($need_type_guard, $i, 1);
                        if nqp::iscont($value) {
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
                            else {
                                nqp::die('multi disp on native references NYI');
                            }
                        }
                        else {
                            # If we need an rw argument and didn't get a container,
                            # we're out of luck.
                            if $rwness {
                                $rwness_mismatch := 1;
                            }
                        }

                        # Ensure the value meets the required type constraints.
                        unless nqp::eqaddr($type, Mu) || nqp::istype_nd($value, $type) {
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
                            my int $got := nqp::isconcrete_nd($value);
                            if ($got && $definedness == $DEFCON_UNDEFINED) ||
                                    (!$got && $definedness == $DEFCON_DEFINED) {
                                $type_mismatch := 1;
                            }
                            nqp::bindpos_i($need_conc_guard, $i, 1);
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
                        elsif (($type_flags +& $TYPE_NATIVE_INT) && $got_prim != $BIND_VAL_INT) ||
                               (($type_flags +& $TYPE_NATIVE_NUM) && $got_prim != $BIND_VAL_NUM) ||
                               (($type_flags +& $TYPE_NATIVE_STR) && $got_prim != $BIND_VAL_STR) {
                            # Mismatch.
                            $type_mismatch := 1;
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
                # Build a linked list of them, but filtering out any with
                # required named parameters that we don't have. Also track if
                # any need a bind check (so are thus not pure type and arity
                # based results).
                my $new-head;
                my $new-tail;
                my int $i;
                my int $n := nqp::elems(@possibles);
                my int $match;
                my int $need-bind-check;
                while $i < $n {
                    my %info := @possibles[$i];
                    unless nqp::existskey(%info, 'req_named') &&
                            !nqp::captureexistsnamed($capture, nqp::atkey(%info, 'req_named')) {
                        my $node;
                        if nqp::existskey(%info, 'bind_check') {
                            $node := MultiDispatchTry.new(%info<sub>);
                            $need-bind-check++;
                        }
                        else {
                            $node := MultiDispatchCall.new(%info<sub>);
                        }
                        if $new-head {
                            $new-tail.set-next($node);
                        }
                        else {
                            $new-head := $node;
                        }
                        $new-tail := $node;
                        $match++;
                    }
                    $i++;
                }

                # If there are multiple results that don't need a bind check,
                # add the ambiguity marker.
                my int $first-group := nqp::isnull($current-head);
                if $need-bind-check == 0 && $match > 1 {
                    my $node := MultiDispatchAmbiguous.new();
                    if nqp::isnull($current-head) {
                        $current-head := $node;
                    }
                    else {
                        $current-tail.set-next($node);
                    }
                    $current-tail := $node;
                }

                # Add the candidates.
                if $match > 0 {
                    if nqp::isnull($current-head) {
                        $current-head := $new-head;
                    }
                    else {
                        $current-tail.set-next($new-head);
                    }
                    $current-tail := $new-tail;
                }

                # If we are to stop at a trivial match and nothing needs a
                # bind check, and we've no results before now, we're done.
                if $stop-at-trivial && $first-group && $need-bind-check == 0 {
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
            nqp::die('Handling of Proxy arguments in multiple dispatch NYI');
        }
        elsif nqp::istype($dispatch-plan, MultiDispatchAmbiguous) &&
                nqp::istype($dispatch-plan.next, MultiDispatchCall) {
            nqp::die('Ambiguous multi candidates; better error NYI');
        }
        elsif nqp::istype($dispatch-plan, MultiDispatchEnd) {
            nqp::die('No applicable multi candidates; better error NYI');
        }
        else {
            # It's a non-trivial multi dispatch. Prefix the arguments with
            # the dispatch plan, and also a zero to indicate this is not a
            # resumption of any kind. The delegate to the non-trivial multi
            # dispatcher.
            my $capture-with-plan := nqp::dispatch('boot-syscall',
                'dispatcher-insert-arg-literal-obj', $arg-capture, 0,
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
                'dispatcher-insert-arg-literal-obj', $arg-capture, 0,
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

        # Drop the leading two arguments to get the argument capture.
        my $arg-capture := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg',
            nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 0), 0);

        # Perform the step.
        raku-multi-non-trivial-step($kind, $track-cur-state, $cur-state, $arg-capture, 0);
    },
    -> $capture {
        # Extract and guard on the kind (first argument).
        my $track-kind := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0);
        nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track-kind);
        my int $kind := nqp::captureposarg_i($capture, 0);

        # Have dispatch state already, or first resume?
        my $track-state := nqp::dispatch('boot-syscall', 'dispatcher-track-resume-state');
        my $state := nqp::dispatch('boot-syscall', 'dispatcher-get-resume-state');
        if nqp::isnull($state) {
            # First resumption. Guard that it is so.
            nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track-state);

            # Obtain plan and args from init state.
            my $init := nqp::dispatch('boot-syscall', 'dispatcher-get-resume-init-args');
            my $track-cur-state := nqp::dispatch('boot-syscall', 'dispatcher-track-arg',
                $init, 0);
            my $cur-state := nqp::captureposarg($init, 0);

            # Drop the leading argument to get the argument capture, and
            # then perform the step.
            my $arg-capture := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg',
                $init, 0);
            raku-multi-non-trivial-step($kind, $track-cur-state, $cur-state, $arg-capture, 1);
        }
        else {
            # Drop the leading argument to get the argument capture, and
            # then perform the step.
            my $init := nqp::dispatch('boot-syscall', 'dispatcher-get-resume-init-args');
            my $arg-capture := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg',
                $init, 0);
            raku-multi-non-trivial-step($kind, $track-state, $state, $arg-capture, 1);
        }
    });
sub raku-multi-non-trivial-step(int $kind, $track-cur-state, $cur-state, $arg-capture,
        $is-resume) {
    if nqp::istype($cur-state, MultiDispatchTry) {
        nqp::die('Multi-dispatch with bind checks NYI');
    }
    elsif nqp::istype($cur-state, MultiDispatchCall) {
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
                $arg-capture, 0, $next);
            nqp::dispatch('boot-syscall', 'dispatcher-set-resume-init-args', $init-state);
        }

        # Set up the call.
        my $capture-delegate := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg',
            $arg-capture, 0, $track-candidate);
        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-invoke', $capture-delegate);
    }
    else {
        nqp::die('Non-trivial multi dispatch step NYI for ' ~ $cur-state.HOW.name($cur-state));
    }
}

# This is where invocation bottoms out, however we reach it. By this point, we
# just have something to invoke, which is either a code object (potentially with
# wrappers)  or something that hopefully has a CALL-ME.
nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-invoke', -> $capture {
    # Guard type and concreteness of code object. This is a no-op in the case
    # that it's been determined a constant by an upper dispatcher.
    my $code := nqp::captureposarg($capture, 0);
    my $code_arg := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0);
    nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $code_arg);
    nqp::dispatch('boot-syscall', 'dispatcher-guard-concreteness', $code_arg);

    # If it's already a VM-level code reference, just invoke it.
    if nqp::reprname($code) eq 'MVMCode' {
        # TODO probably boot-code, not boot-code-constant
        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-code-constant',
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
        # TODO probably boot-code, not boot-code-constant
        if nqp::isconcrete($code) {
            my $do_attr := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                $code_arg, Code, '$!do');
            my $delegate_capture := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg',
                nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 0),
                0, $do_attr);
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-code-constant',
                $delegate_capture);
        }

        # Invoking non-concrete code object is an error.
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
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-code-constant',
                $delegate_capture);
        }
        else {
            nqp::die('Cannot invoke a ' ~ $code.HOW.name($code) ~ ' type object');
        }
    }

    # Otherwise, try the CALL-ME path.
    else {
        nqp::die('CALL-ME handling NYI in new dispatcher; got ' ~ $code.HOW.name($code));
    }
});

# Dispatch to a wrapped routine, providing a resumption handler to deal with
# moving through the various levels of wrapper.
nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-invoke-wrapped',
    # Dispatch
    -> $capture {
        # Guard on the current set of wrappers (the array of them is immutable,
        # so we can rely on its identity).
        my $routine := nqp::captureposarg($capture, 0);
        my $wrapper_type := $routine.WRAPPER-TYPE;
        my $track_routine := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0);
        my $track-wrappers := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                $track_routine, $wrapper_type, '$!wrappers');
        nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track-wrappers);

        # With wrappers, we pretty much know we'll be traversing them, so we
        # build the deferral chain up front, unlike in other dispatchers. It
        # goes into the resume init state.
        my @all_callees := nqp::clone($routine.WRAPPERS);
        nqp::push(@all_callees, nqp::getattr($routine, Code, '$!do'));
        my $first_callee := nqp::shift(@all_callees);
        my $chain := Exhausted;
        while nqp::elems(@all_callees) {
            $chain := DeferralChain.new(nqp::pop(@all_callees), $chain);
        }
        my $without_routine := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 0);
        nqp::dispatch('boot-syscall', 'dispatcher-set-resume-init-args',
            nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
            $without_routine, 0, $chain));

        # Invoke the first wrapper.
        my $delegate_capture := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
            $without_routine, 0, $first_callee);
        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-invoke', $delegate_capture);
    },
    # Resumption
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
        if $kind == 2 {
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

        # If we have a current deferral, then update state to move to the next
        # wrapper in the list and then put into effect the resumption.
        if $cur_deferral {
            my $track_code := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                $track_cur_deferral, DeferralChain, '$!code');
            nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track_code);
            my $track_next := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                $track_cur_deferral, DeferralChain, '$!next');
            nqp::dispatch('boot-syscall', 'dispatcher-set-resume-state', $track_next);
            if $kind == 0 {
                # Invoke the next bit of code.
                my $delegate_capture := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                    nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $init, 0),
                    0, $cur_deferral.code);
                nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-invoke', $delegate_capture);
            }
            elsif $kind == 3 {
                # We just want the code itself, not to invoke it.
                nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-constant',
                    nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                        $capture, 0, $cur_deferral.code));
            }
            else {
                nqp::die('Unimplemented resumption kind in wrap dispatch');
            }
        }
        else {
            # This dispatcher is exhausted. However, there may be another one
            # we can try (for example, in a wrapped method). Only do this if
            # it's not lastcall. Failing that, give back Nil.
            if $kind == 2 || !nqp::dispatch('boot-syscall', 'dispatcher-next-resumption') {
                nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-constant',
                    nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                        $capture, 0, Nil));
            }
        }
    });
