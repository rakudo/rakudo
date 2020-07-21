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

    nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-assign', -> $capture {
        # Whatever we do, we'll guard on the type of the container and its
        # concreteness.
        my $cont_arg := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0);
        nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $cont_arg);
        nqp::dispatch('boot-syscall', 'dispatcher-guard-concreteness', $cont_arg);

        # TODO optimized cases

        # Otherwise, nothing we can optimize, so go for the fallback.
        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-code-constant',
            nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                $capture, 0, $assign-fallback));
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
            last if nqp::isconcrete($meth);
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
        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-multi', $without_name);
    }
    else {
        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-invoke', $without_name);
    }
});

# Multi-dispatch dispatcher, used for both multi sub and multi method dispatch.
# There are a number of cases that we need to deal with here.
# 1. Everything about the dispatch is possible to express with guards, so we
#    need do no more than add guards as needed and then delegate to raku-invoke
#    to dispatch to the chosen result.
# 2. Even if the properties above are true, we can only narrow down the list
#    of candidates to (hopefully) some subset of them, guarding against them.
#    Those will need further checks per dispatch. This is also the case when
#    we have to do things like reading from Proxy, which we cannot express via
#    the guard mechanism.
# 3. The dispatch fails, but then we do the Junction failover. In this case,
#    we can delegate to an invoke of the auto-threader, conditional on there
#    being a Junction in that argument spot.
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
my int $REQUIRE_CHECK_NONE        := 0; # Candidate can be chosen with no checking
my int $REQUIRE_CHECK_ALL         := 1; # Candidate needs checking in full
my int $REQUIRE_CHECK_BINDABILITY := 2; # Candidate needs only bindability checking
sub raku-multi-filter(@candidates, $capture, int $all, @filtered_candidates,
        $filtered_check_requirements) {
    # Look through all candidates. Eliminate those that can be ruled out by
    # setting guards on the incoming arguments OR by the shape of the
    # callsite. That callsite shape includes argument count, which named
    # arguments are present, and which arguments are natively typed.
    my int $num_args := nqp::captureposelems($capture);
    my int $cur_idx := 0;
    my int $done := 0;
    my $need_scalar_read := nqp::list_i();
    my $need_scalar_rw_check := nqp::list_i();
    my $need_type_guard := nqp::list_i();
    my $need_conc_guard := nqp::list_i();
    my int $group_has_unguardable := 0;
    my @possibles;
    my $possibles_unguardable := nqp::list_i();
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
                my int $unguardable := 0;
                my int $i := 0;
                while $i < $type_check_count && !($unguardable +| $type_mismatch +| $rwness_mismatch) {
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
                                nqp::die('multi disp on non-Scalar container NYI');
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
                        unless nqp::istype_nd($value, $type) {
                            # XXX various failovers
                            $type_mismatch := 1;
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

                # If it's unguardable, we must always add it to the list of
                # possibles. Otherwise, it's guardable and we only add it to
                # the list if there's no mismatch.
                if $unguardable || !($type_mismatch || $rwness_mismatch) {
                    nqp::push(@possibles, $cur_candidate);
                    nqp::push_i($possibles_unguardable, $unguardable);
                }
                $group_has_unguardable := 1 if $unguardable;
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
                if nqp::elems(@possibles) == 1 && !$group_has_unguardable {
                    # Exactly one guardable result, so we just take it and we
                    # are done.
                    nqp::push(@filtered_candidates, @possibles[0]);
                    nqp::push_i($filtered_check_requirements, $REQUIRE_CHECK_NONE);
                    $done := 1;
                }
                else {
                    # We have multiple candidates. We will visit them and see
                    # if any drop out by lack of required named argument; if
                    # that is not the case, then they survive into the filtered
                    # set.
                    my int $i := 0;
                    my int $n := nqp::elems(@possibles);
                    while $i < $n {
                        my %info := @possibles[$i];
                        unless nqp::existskey(%info, 'req_named') &&
                                !nqp::captureexistsnamed($capture, nqp::atkey(%info, 'req_named')) {
                            # Will need disambiguation later.
                            nqp::push(@filtered_candidates, %info);
                            nqp::push_i($filtered_check_requirements,
                                nqp::atpos_i($possibles_unguardable, $i) ?? $REQUIRE_CHECK_ALL !!
                                nqp::existskey(%info, 'bind_check') ?? $REQUIRE_CHECK_BINDABILITY !!
                                $REQUIRE_CHECK_NONE);
                        }
                        $i++;
                    }

                    # Done if we're ran out of candidates to consider.
                    unless nqp::isconcrete(nqp::atpos(@candidates, $cur_idx)) {
                        $done := 1;
                    }
                }

                # Mark end of group.
                nqp::push(@filtered_candidates, Mu);

                # Clear state for next group.
                nqp::setelems(@possibles, 0);
                nqp::setelems($possibles_unguardable, 0);
                $group_has_unguardable := 0;
            }

            # If we're really and the end of the list, we're done.
            unless nqp::isconcrete(nqp::atpos(@candidates, $cur_idx)) {
                $done := 1;
            }
        }
    }

    # Install guards as required.
    my int $i := 0;
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
    0
}
nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-multi', -> $capture {
    # Obtain the candidate list, producing it if it doesn't already exist.
    my $target := nqp::captureposarg($capture, 0);
    my @candidates := nqp::getattr($target, Routine, '@!dispatch_order');
    if nqp::isnull(@candidates) {
        nqp::scwbdisable();
        @candidates := $target.'!sort_dispatchees_internal'();
        nqp::bindattr($target, Routine, '@!dispatch_order', @candidates);
        nqp::scwbenable();
    }

    # TODO Set dispatch state for resumption (we only need consider those that
    # might match)

    # Drop the first argument, to get just the arguments to dispatch on, and
    # then pre-filter the candidate list.
    my $arg-capture := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 0);
    my @filtered_candidates;
    my $filtered_check_requirements := nqp::list_i();
    raku-multi-filter(@candidates, $arg-capture, 0, @filtered_candidates,
        $filtered_check_requirements);

    # See what we're left with after filtering. In the best case, it's only one
    # result.
    my int $onlystar := nqp::getattr_i($target, Routine, '$!onlystar');
    if nqp::elems(@filtered_candidates) == 2 &&
            nqp::atpos_i($filtered_check_requirements, 0) == $REQUIRE_CHECK_NONE {
        # Simple case where we have a single group with a single filtered
        # candidate, which was chosen based on guardable properties and has
        # no late-bound checks to do.
        if $onlystar {
            # Add this resolved target to the argument capture and delegate to
            # raku-invoke to run it.
            my $capture_delegate := nqp::dispatch('boot-syscall',
                'dispatcher-insert-arg-literal-obj', $arg-capture, 0,
                @filtered_candidates[0]<sub>);
            nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-invoke',
                $capture_delegate);
        }
        else {
            nqp::die('non-onlystar case of multi dispatch NYI in new dispatcher');
        }
    }
    else {
        # We need to perform disambiguation per-dispatch, but we hopefully have
        # saved ourselves some of the up-front work.
        nqp::die('new multi disp requiring per-dispatch disambiguation NYI');
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
