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
        # TODO proxy hack
        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-rv-decont', $capture);
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
                    nqp::die('NYI return value case (checked coerce)')
                }
            }
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
            # the arg of the coercion type, then insert two args: the resolved
            # callee and the name.
            my $meth_capture := nqp::dispatch('boot-syscall',
                'dispatcher-insert-arg-literal-obj',
                nqp::dispatch('boot-syscall',
                    'dispatcher-insert-arg-literal-str',
                        nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 1),
                        0, $to_name),
                    0, $coerce_method);
            nqp::dispatch('boot-syscall', 'dispatcher-delegate',
                    'raku-meth-call-resolved', $meth_capture);
        }
        else {
            coercion_error($to_coerce.HOW.name($to_coerce), $to_name);
        }
    });
}

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
            last unless nqp::isconcrete($meth);
        }
    }

    # If it's resolved, drop the invocant and type arguments targetted at
    # resolution, and then insert the resolved method as a constant.
    if nqp::isconcrete($meth) {
        my $capture_simplified := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg',
            nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 2),
            0);
        my $capture_delegate := nqp::dispatch('boot-syscall',
            'dispatcher-insert-arg-literal-obj', $capture_simplified, 0, $meth);
        nqp::dispatch('boot-syscall', 'dispatcher-delegate',
                'raku-meth-call-resolved', $capture_delegate);
    }

    # Otherwise, exception. TODO proper one
    else {
        nqp::die(nqp::isconcrete($meth) ?? 'qual meth found' !! 'qual meth not found');
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
        # Found it. Drop decont'd invocant, put in resolved method.
        my $capture_delegate := nqp::dispatch('boot-syscall',
            'dispatcher-insert-arg-literal-obj',
            nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 0),
            0, $meth);
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

# Resolved method call dispatcher. This is used to call a method, once we have
# already resolved it to a callee. Its first arg is the callee, the second is
# the method name (used in a continued dispatch), and the rest are the args to
# the method.
nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-meth-call-resolved', -> $capture {
    # TODO Set dispatch state for resumption

    # Drop the name, and delegate to multi-dispatch or just invoke if it's
    # single dispatch.
    my $without_name := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 1);
    my $method := nqp::captureposarg($capture, 0);
    if nqp::istype($method, Routine) && $method.is_dispatcher {
        nqp::die('multi meth disp in new dispatcher NYI');
    }
    else {
        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-invoke', $without_name);
    }
});

# This is where invocation bottoms out, however we reach it. By this point, we
# just have something to invoke, which is either a code object or something that
# hopefully has a CALL-ME.
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

    # If it's a code object...
    elsif nqp::istype($code, Code) {
        # Concrete code object: extract the $!do, replace the code object,
        # and delegate to boot-code.
        # TODO probably boot-code, not boot-code-constant
        if nqp::isconcrete($code) {
            my $do := nqp::getattr($code, Code, '$!do');
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

    # Otherwise, try the CALL-ME path.
    else {
        nqp::die('CALL-ME handling NYI in new dispatcher; got ' ~ $code.HOW.name($code));
    }
});
