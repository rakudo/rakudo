# Some maximum cache sizes we allow at a callsite before we switch to a
# megamorphic strategy.
my int $MEGA-TYPE-CALLSITE-SIZE := 16;
my int $MEGA-METH-CALLSITE-SIZE := 16;

#- General Helper Subs ---------------------------------------------------------
# Helper sub to delegate to return given constant
sub delegate-constant($capture, $value) {

    # Insert the literal value at the start (boot-constant ignores
    # the rest of the args) and delegate
    nqp::delegate('boot-constant',
      nqp::syscall('dispatcher-insert-arg-literal-obj',
        $capture, 0, $value
      )
    );
}

# Helper sub to delegate to return Nil
sub delegate-constant-Nil($capture) { delegate-constant($capture, Nil) }

# Helper sub to delegate to return given value
sub delegate-value($capture, $value) {

    # Insert the value at the start (boot-value ignores the rest
    # of the args) and delegate
    nqp::delegate('boot-value',
      nqp::syscall('dispatcher-insert-arg', $capture, 0, $value)
    );
}

# Helper sub to delegate to a syscall for a code object as the first
# argument in a capture
sub delegate-code-syscall($capture, str $dispatcher) {
    my $Tcode := nqp::track('arg', $capture, 0);
    nqp::guard('type', $Tcode);

    nqp::istype(nqp::captureposarg($capture, 0), Code)
      # Delegate to calling try-capture-lex-callers function
      ?? nqp::delegate('boot-syscall',
           nqp::syscall('dispatcher-insert-arg-literal-str',
             nqp::syscall('dispatcher-insert-arg',
               nqp::syscall('dispatcher-drop-arg', $capture, 0),
               0, nqp::track('attr', $Tcode, Code, '$!do')
             ),
             0, $dispatcher
           )
         )

      # Produce whatever we were given
      !! nqp::delegate('boot-value', $capture);
}

# Guard the given tracker on type and concreteness
sub guard-type-concreteness($tracker) {
    nqp::guard('type', $tracker);
    nqp::guard('concreteness', $tracker);
}

#- raku-rv-decont --------------------------------------------------------------
# Return value decontainerization dispatcher. Often we have nothing at all
# to do, in which case we can make it identity. Other times, we need a
# decont. In a few, we need to re-wrap it.
;{
    # Simple re-containerization logic
    my $recont := -> $obj {
        my $rc := nqp::create(Scalar);
        nqp::bindattr($rc, Scalar, '$!value', nqp::decont($obj));
        $rc
    }

    # Default decontainerize logic, with check for Iterables
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

    # Dispatcher for decontainerization of return values in Raku.
    # Expects a capture with the value as its first argument.
    nqp::register('raku-rv-decont', -> $capture {

        # If it's heading megamorphic, then we'll install the fallback,
        # without any conditions, which is faster than over-filling the
        # cache and running this dispatch logic every time.
        my int $cache-size := nqp::syscall('dispatcher-inline-cache-size');
        if $cache-size >= $MEGA-TYPE-CALLSITE-SIZE {
            nqp::delegate('boot-code-constant',
              nqp::syscall('dispatcher-insert-arg-literal-obj',
                nqp::syscall('dispatcher-insert-arg-literal-obj',
                  $capture, 0, nqp::gethllsym('Raku', 'Iterable')
                ),
                0, $container-fallback
              )
            );
        }
        else {
            # We always need to guard on type and concreteness.
            my $rv  := nqp::captureposarg($capture, 0);
            my $Trv := nqp::track('arg',  $capture, 0);
            guard-type-concreteness($Trv);

            # Is it a container?
            if nqp::isconcrete_nd($rv) && nqp::iscont($rv) {

                # It's a container. We have special cases for Scalar.
                if nqp::istype_nd($rv, Scalar) {

                    # Check if the descriptor is undefined, in which case
                    # it's read-only.
                    my $desc := nqp::getattr($rv, Scalar, '$!descriptor');
                    my $Tdesc :=
                      nqp::track('attr', $Trv, Scalar, '$!descriptor');
                    nqp::guard('concreteness', $Tdesc);
                    if nqp::isconcrete($desc) {

                        # Writeable, so we may need to recontainerize the
                        # value if the type is iterable, otherwise we can
                        # decont it.
                        my $value := nqp::getattr($rv, Scalar, '$!value');
                        my $Tvalue :=
                           nqp::track('attr', $Trv, Scalar, '$!value');
                        nqp::guard('type', $Tvalue);
                        if nqp::istype_nd(
                             $value, nqp::gethllsym('Raku', 'Iterable')
                           ) {

                            # Need to recont in order to preserve item nature.
                            # Shuffle in the recont code to invoke. We already
                            # read the deconted value, so we insert that as the
                            # arg so it needn't be dereferenced again.
                            nqp::delegate('boot-code-constant',
                              nqp::syscall('dispatcher-insert-arg-literal-obj',
                                nqp::syscall('dispatcher-replace-arg',
                                  $capture, 0, $Tvalue
                                ),
                                0, $recont
                              )
                            );
                        }

                        # Not an Iterable
                        else {
                            # Decont, so just evaluate to the read attr
                            # (boot-value ignores all but the first argument)
                            delegate-value($capture, $Tvalue);
                        }
                    }

                    # No descriptor, so read-only: identity will do
                    else {
                        nqp::delegate('boot-value', $capture);
                    }
                }
                else {
                    # Delegate to non-Scalar container fallback.
                    nqp::delegate('boot-code-constant',
                      nqp::syscall('dispatcher-insert-arg-literal-obj',
                        nqp::syscall('dispatcher-insert-arg-literal-obj',
                          $capture, 0, nqp::gethllsym('Raku', 'Iterable')
                        ),
                        0, $container-fallback
                      )
                    );
                }
            }
            else {
                # Not containerized, so identity shall do.
                # Unless it is null, then we map it to Mu.
                nqp::isnull($rv)
                  ?? delegate-constant($capture, Mu)
                  !! nqp::delegate('boot-value', $capture);
            }
        }
    });

    # This emulates a bug where Proxy was never decontainerized no
    # matter what. The ecosystem came to depend on that, so we will
    # accept it for now. We need to revisit this in the future.
    # Expects the value to be in the first argument like raku-rv-decont.
    nqp::register('raku-rv-decont-6c', -> $capture {
        my $rv := nqp::captureposarg($capture, 0);
        if nqp::eqaddr(nqp::what_nd($rv), Proxy) && nqp::isconcrete_nd($rv) {
            guard-type-concreteness(nqp::track('arg', $capture, 0));
            nqp::delegate('boot-value', $capture);
        }
        else {
            nqp::delegate('raku-rv-decont', $capture);
        }
    });
}

#- raku-assign -----------------------------------------------------------------
# Assignment dispatcher, which case-analyzes assignments and provides
# optimized paths for a range of common situations, and typically lifting
# type checks to be guards.
{
    # Shortcut for throwing type errors on assignment
    sub assign-type-error($desc, $got) {
        Perl6::Metamodel::Configuration.throw_or_die(
          'X::TypeCheck::Assignment', "Type check failed in assignment",
          :symbol($desc.name), :$desc, :$got, :expected($desc.of)
        );
    }

    # Handler for when there is no optimization possible, moving all
    # type checks to runtime
    my $assign-fallback := -> $cont, $value {
        nqp::assign($cont, $value)
    }

    # Handler for simplest container assignment that is not part of
    # an array or hash (just bind value)
    my $assign-scalar-no-whence-no-typecheck := -> $cont, $value {
        nqp::bindattr($cont, Scalar, '$!value', $value)
    }

    # Handler for assigning Nil to a container that is not part of
    # an array or hash, which will set the default value from the
    # descriptor
    my $assign-scalar-nil-no-whence := -> $cont, $value {
        nqp::bindattr($cont, Scalar, '$!value',
          nqp::getattr(
            nqp::getattr($cont, Scalar, '$!descriptor'),
            ContainerDescriptor,
            '$!default'
          )
        )
    }

    # Handler for assigning a value to a container that is not part
    # of an array or hash, with type check done at runtime.
    my $assign-scalar-no-whence := -> $cont, $value {
        my $desc := nqp::getattr($cont, Scalar, '$!descriptor');
        my $type := nqp::getattr($desc, ContainerDescriptor, '$!of');

        nqp::istype($value, $type)
          ?? nqp::bindattr($cont, Scalar, '$!value',
               nqp::how_nd($type).archetypes($type).coercive
                 ?? nqp::dispatch('raku-coercion', $type, $value)
                 !! $value
             )
          !! assign-type-error($desc, $value);
    }

    # Handler for assigning a value to a container that is to be
    # part of an array.  Binds the container to the array, normalizes
    # the descriptor and sets the value without any typechecking.
    my $assign-scalar-bindpos-no-typecheck := -> $cont, $value {
        my $desc := nqp::getattr($cont, Scalar, '$!descriptor');

        # Install the container in the array
        nqp::bindpos(
          nqp::getattr(  $desc, ContainerDescriptor::BindArrayPos, '$!target'),
          nqp::getattr_i($desc, ContainerDescriptor::BindArrayPos, '$!pos'),
          $cont
        );

        # Initialization done, install "normal" descriptor
        nqp::bindattr($cont, Scalar, '$!descriptor',
          nqp::getattr(
            $desc, ContainerDescriptor::BindArrayPos, '$!next-descriptor'
          )
        );

        # Set the value
        nqp::bindattr($cont, Scalar, '$!value', $value);
    }

    # Handler for assigning a value to a container that is to be
    # part of an array with typechecking.  Binds the container to
    # the array, normalizes the descriptor and sets the value.
    my $assign-scalar-bindpos := -> $cont, $value {
        my $desc := nqp::getattr($cont, Scalar, '$!descriptor');
        my $next := nqp::getattr(
          $desc, ContainerDescriptor::BindArrayPos, '$!next-descriptor'
        );

        # Quit if typecheck failed
        my $type := nqp::getattr($next, ContainerDescriptor, '$!of');
        assign-type-error($next, $value) unless nqp::istype($value, $type);

        # Install the container in the array
        nqp::bindpos(
          nqp::getattr($desc, ContainerDescriptor::BindArrayPos, '$!target'),
          nqp::getattr_i($desc, ContainerDescriptor::BindArrayPos, '$!pos'),
          $cont
        );

        # Install "normal" descriptor
        nqp::bindattr($cont, Scalar, '$!descriptor', $next);

        # Set the value
        nqp::bindattr($cont, Scalar, '$!value',
          nqp::how_nd($type).archetypes($type).coercive
            ?? nqp::dispatch('raku-coercion', $type, $value)
            !! $value
        );
    }

    # Handler for assigning a value to a container that is to be
    # part of a two-dimensional array.  Binds the container to the
    # array, normalizes the descriptor and sets the value without
    # any typechecking.
    my $assign-scalar-bindpos2d-no-typecheck := -> $cont, $value {
        my $desc := nqp::getattr($cont, Scalar, '$!descriptor');

        # Install the container in the array
        nqp::bindpos2d(
          nqp::getattr(  $desc, ContainerDescriptor::BindArrayPos2D, '$!target'),
          nqp::getattr_i($desc, ContainerDescriptor::BindArrayPos2D, '$!one'),
          nqp::getattr_i($desc, ContainerDescriptor::BindArrayPos2D, '$!two'),
          $cont
        );

        # Initialization done, install "normal" descriptor
        nqp::bindattr($cont, Scalar, '$!descriptor',
          nqp::getattr(
            $desc, ContainerDescriptor::BindArrayPos2D, '$!next-descriptor'
          )
        );

        # Set the value
        nqp::bindattr($cont, Scalar, '$!value', $value);
    }

    # Handler for assigning a value to a container that is to be
    # part of a two-dimensional array with typechecking.  Binds the
    # container to the array, normalizes the descriptor and sets the
    # value.
    my $assign-scalar-bindpos2d := -> $cont, $value {
        my $desc := nqp::getattr($cont, Scalar, '$!descriptor');
        my $next := nqp::getattr(
          $desc, ContainerDescriptor::BindArrayPos2D, '$!next-descriptor'
        );

        # Quit if typecheck failed
        my $type := nqp::getattr($next, ContainerDescriptor, '$!of');
        assign-type-error($next, $value) unless nqp::istype($value, $type);

        # Install the container in the array
        nqp::bindpos2d(
          nqp::getattr($desc, ContainerDescriptor::BindArrayPos2D, '$!target'),
          nqp::getattr_i($desc, ContainerDescriptor::BindArrayPos2D, '$!one'),
          nqp::getattr_i($desc, ContainerDescriptor::BindArrayPos2D, '$!two'),
          $cont
        );

        # Install "normal" descriptor
        nqp::bindattr($cont, Scalar, '$!descriptor', $next);

        # Set the value
        nqp::bindattr($cont, Scalar, '$!value',
          nqp::how_nd($type).archetypes($type).coercive
            ?? nqp::dispatch('raku-coercion', $type, $value)
            !! $value
        );
    }

    # Handler for assigning a value to a container that is to be
    # part of a three-dimensional array.  Binds the container to the
    # array, normalizes the descriptor and sets the value without
    # any typechecking.
    my $assign-scalar-bindpos3d-no-typecheck := -> $cont, $value {
        my $desc := nqp::getattr($cont, Scalar, '$!descriptor');

        # Install the container in the array
        nqp::bindpos3d(
          nqp::getattr(  $desc, ContainerDescriptor::BindArrayPos3D, '$!target'),
          nqp::getattr_i($desc, ContainerDescriptor::BindArrayPos3D, '$!one'),
          nqp::getattr_i($desc, ContainerDescriptor::BindArrayPos3D, '$!two'),
          nqp::getattr_i($desc, ContainerDescriptor::BindArrayPos3D, '$!three'),
          $cont
        );

        # Initialization done, install "normal" descriptor
        nqp::bindattr($cont, Scalar, '$!descriptor',
          nqp::getattr(
            $desc, ContainerDescriptor::BindArrayPos3D, '$!next-descriptor'
          )
        );

        # Set the value
        nqp::bindattr($cont, Scalar, '$!value', $value);
    }

    # Handler for assigning a value to a container that is to be
    # part of a three-dimensional array with typechecking.  Binds the
    # container to the array, normalizes the descriptor and sets the
    # value.
    my $assign-scalar-bindpos3d := -> $cont, $value {
        my $desc := nqp::getattr($cont, Scalar, '$!descriptor');
        my $next := nqp::getattr(
          $desc, ContainerDescriptor::BindArrayPos3D, '$!next-descriptor'
        );

        # Quit if typecheck failed
        my $type := nqp::getattr($next, ContainerDescriptor, '$!of');
        assign-type-error($next, $value) unless nqp::istype($value, $type);

        # Install the container in the array
        nqp::bindpos3d(
          nqp::getattr($desc, ContainerDescriptor::BindArrayPos3D, '$!target'),
          nqp::getattr_i($desc, ContainerDescriptor::BindArrayPos3D, '$!one'),
          nqp::getattr_i($desc, ContainerDescriptor::BindArrayPos3D, '$!two'),
          nqp::getattr_i($desc, ContainerDescriptor::BindArrayPos3D, '$!three'),
          $cont
        );

        # Install "normal" descriptor
        nqp::bindattr($cont, Scalar, '$!descriptor', $next);

        # Set the value
        nqp::bindattr($cont, Scalar, '$!value',
          nqp::how_nd($type).archetypes($type).coercive
            ?? nqp::dispatch('raku-coercion', $type, $value)
            !! $value
        );
    }

    # Handler for assigning a value to a container that is to be
    # part of an N-dimensional array.  Binds the container to the
    # array, normalizes the descriptor and sets the value without
    # any typechecking.
    my $assign-scalar-bindposnd-no-typecheck := -> $cont, $value {
        my $desc := nqp::getattr($cont, Scalar, '$!descriptor');

        # Install the container in the array
        nqp::bindposnd(
          nqp::getattr(  $desc, ContainerDescriptor::BindArrayPosND, '$!target'),
          nqp::getattr($desc, ContainerDescriptor::BindArrayPosND, '$!idxs'),
          $cont
        );

        # Initialization done, install "normal" descriptor
        nqp::bindattr($cont, Scalar, '$!descriptor',
          nqp::getattr(
            $desc, ContainerDescriptor::BindArrayPosND, '$!next-descriptor'
          )
        );

        # Set the value
        nqp::bindattr($cont, Scalar, '$!value', $value);
    }

    # Handler for assigning a value to a container that is to be
    # part of an N-dimensional array with typechecking.  Binds the
    # container to the array, normalizes the descriptor and sets the
    # value.
    my $assign-scalar-bindposnd := -> $cont, $value {
        my $desc := nqp::getattr($cont, Scalar, '$!descriptor');
        my $next := nqp::getattr(
          $desc, ContainerDescriptor::BindArrayPosND, '$!next-descriptor'
        );

        # Quit if typecheck failed
        my $type := nqp::getattr($next, ContainerDescriptor, '$!of');
        assign-type-error($next, $value) unless nqp::istype($value, $type);

        # Install the container in the array
        nqp::bindposnd(
          nqp::getattr($desc, ContainerDescriptor::BindArrayPosND, '$!target'),
          nqp::getattr($desc, ContainerDescriptor::BindArrayPosND, '$!idxs'),
          $cont
        );

        # Install "normal" descriptor
        nqp::bindattr($cont, Scalar, '$!descriptor', $next);

        # Set the value
        nqp::bindattr($cont, Scalar, '$!value',
          nqp::how_nd($type).archetypes($type).coercive
            ?? nqp::dispatch('raku-coercion', $type, $value)
            !! $value
        );
    }

    # Handler for assigning a value to a container that is to be
    # part of a hash.  Binds the container to the hash, normalizes
    # the descriptor and sets the value without any typechecking.
    my $assign-scalar-bindkey-no-typecheck := -> $cont, $value {
        my $desc := nqp::getattr($cont, Scalar, '$!descriptor');

        # Install the container in the hash
        nqp::bindkey(
          nqp::getattr($desc, ContainerDescriptor::BindHashKey, '$!target'),
          nqp::getattr($desc, ContainerDescriptor::BindHashKey, '$!key'),
          $cont
        );

        # Initialization done, install "normal" descriptor
        nqp::bindattr($cont, Scalar, '$!descriptor',
          nqp::getattr(
            $desc, ContainerDescriptor::BindHashKey, '$!next-descriptor'
          )
        );

        # Set the value
        nqp::bindattr($cont, Scalar, '$!value', $value);
    }

    # Handler for assigning a value to a container that is to be
    # part of a hash with typechecking.  Binds the container to
    # the hash, normalizes the descriptor and sets the value.
    my $assign-scalar-bindkey := -> $cont, $value {
        my $desc := nqp::getattr($cont, Scalar, '$!descriptor');
        my $next := nqp::getattr(
          $desc, ContainerDescriptor::BindHashKey, '$!next-descriptor'
        );

        # Quit if typecheck failed
        my $type := nqp::getattr($next, ContainerDescriptor, '$!of');
        assign-type-error($next, $value) unless nqp::istype($value, $type);

        # Install the container in the hash
        nqp::bindkey(
          nqp::getattr($desc, ContainerDescriptor::BindHashKey, '$!target'),
          nqp::getattr($desc, ContainerDescriptor::BindHashKey, '$!key'),
          $cont
        );

        # Install "normal" descriptor
        nqp::bindattr($cont, Scalar, '$!descriptor', $next);

        # Set the value
        nqp::bindattr($cont, Scalar, '$!value',
          nqp::how_nd($type).archetypes($type).coercive
            ?? nqp::dispatch('raku-coercion', $type, $value)
            !! $value
        );
    }

    # Handler for assigning a value to a container that is to be
    # part of an object hash.  Binds the container to the hash,
    # normalizes the descriptor and sets the value without any
    # typechecking.
    my $assign-scalar-obj-bindkey-no-typecheck := -> $cont, $value {
        my $desc := nqp::getattr($cont, Scalar, '$!descriptor');

        # Install the container in the object hash
        nqp::bindkey(
          nqp::getattr($desc, ContainerDescriptor::BindObjHashKey, '$!target'),
          nqp::getattr($desc, ContainerDescriptor::BindObjHashKey, '$!which'),
          nqp::getattr(
            $desc, ContainerDescriptor::BindObjHashKey, '$!pair'
          ).new(
            nqp::getattr($desc, ContainerDescriptor::BindObjHashKey, '$!key'),
            $cont
          )
        );

        # Initialization done, install "normal" descriptor
        nqp::bindattr($cont, Scalar, '$!descriptor',
          nqp::getattr(
            $desc, ContainerDescriptor::BindObjHashKey, '$!next-descriptor'
          )
        );

        # Set the value
        nqp::bindattr($cont, Scalar, '$!value', $value);
    }

    # Handler for assigning a value to a container that is to be part
    # of an object hash with typechecking.  Binds the container to
    # the object hash, normalizes the descriptor and sets the value.
    my $assign-scalar-obj-bindkey := -> $cont, $value {
        my $desc := nqp::getattr($cont, Scalar, '$!descriptor');
        my $next := nqp::getattr(
          $desc, ContainerDescriptor::BindObjHashKey, '$!next-descriptor'
        );

        # Quit if typecheck failed
        my $type := nqp::getattr($next, ContainerDescriptor, '$!of');
        assign-type-error($next, $value) unless nqp::istype($value, $type);

        # Install the container in the object hash
        nqp::bindkey(
          nqp::getattr($desc, ContainerDescriptor::BindObjHashKey, '$!target'),
          nqp::getattr($desc, ContainerDescriptor::BindObjHashKey, '$!which'),
          nqp::getattr(
            $desc, ContainerDescriptor::BindObjHashKey, '$!pair'
          ).new(
            nqp::getattr($desc, ContainerDescriptor::BindObjHashKey, '$!key'),
            $cont
          )
        );

        # Install "normal" descriptor
        nqp::bindattr($cont, Scalar, '$!descriptor', $next);

        # Set the value
        nqp::bindattr($cont, Scalar, '$!value',
          nqp::how_nd($type).archetypes($type).coercive
            ?? nqp::dispatch('raku-coercion', $type, $value)
            !! $value
        );
    }

    # Handler for initializing an attribute.  The container is
    # assumed to have been bound to its object already.
    my $assign-scalar-uninit-no-typecheck := -> $cont, $value {

        # Install "normal" descriptor
        nqp::bindattr($cont, Scalar, '$!descriptor',
          nqp::getattr(
            nqp::getattr($cont, Scalar, '$!descriptor'),
            ContainerDescriptor::UninitializedAttribute,
            '$!next-descriptor'
          )
        );

        # Set the value
        nqp::bindattr($cont, Scalar, '$!value', $value);
    }

    # Handler for initializing an attribute with typecheck.  The
    # container is assumed to have been bound to its object already.
    my $assign-scalar-uninit := -> $cont, $value {
        my $desc := nqp::getattr($cont, Scalar, '$!descriptor');
        my $next := nqp::getattr(
          $desc,
          ContainerDescriptor::UninitializedAttribute,
          '$!next-descriptor'
        );

        # Quit if typecheck failed
        my $type := nqp::getattr($next, ContainerDescriptor, '$!of');
        assign-type-error($next, $value) unless nqp::istype($value, $type);

        # Install "normal" descriptor
        nqp::bindattr($cont, Scalar, '$!descriptor', $next);

        # Set the value
        nqp::bindattr($cont, Scalar, '$!value',
          nqp::how_nd($type).archetypes($type).coercive
            ?? nqp::dispatch('raku-coercion', $type, $value)
            !! $value
        );
    }

    # Dispatcher for assignment in Raku.  Expects a capture with the
    # container as its first argument, and the value as the second
    # argument.
    nqp::register('raku-assign', -> $capture {

        # Whatever we do, we'll guard on the type of the container
        # and its concreteness
        my $cont  := nqp::captureposarg($capture, 0);
        my $Tcont := nqp::track('arg',  $capture, 0);
        guard-type-concreteness($Tcont);

        # Final handler to be delegated to
        my $handler := $assign-fallback;

        if nqp::eqaddr(nqp::what_nd($cont), Scalar)   # it's a container
          && nqp::isconcrete_nd($cont)                # and it's concrete
          && nqp::isconcrete(                         # and descriptor ok
               my $desc := nqp::getattr($cont, Scalar, '$!descriptor')
             ) {
            my $desc-WHAT := $desc.WHAT;
            my $Tdesc := nqp::track('attr', $Tcont, Scalar, '$!descriptor');
            my $value  := nqp::captureposarg($capture, 1);
            my $Tvalue := nqp::track('arg',  $capture, 1);

            # Helper sub for handling container descriptors that have a
            # whence, aka that handle containers in an array or a hash
            sub delegate-whence($no-typecheck, $typecheck) {
                nqp::guard('type', $Tdesc);
                my $next := nqp::getattr($desc, $desc-WHAT, '$!next-descriptor');
                if nqp::eqaddr($next.WHAT, ContainerDescriptor) ||
                    nqp::eqaddr($next.WHAT, ContainerDescriptor::Untyped) {
                    # Ensure we're not assigning Nil. (This would be very odd,
                    # as a Scalar starts off with its default value, and if we
                    # are vivifying we'll likely have a new container).
                    unless nqp::eqaddr($value.WHAT, Nil) {
                        # Go by whether we can type check the target.
                        nqp::guard('literal', nqp::track(
                          'attr', $Tdesc, $desc-WHAT, '$!next-descriptor'
                        ));
                        nqp::guard('type', $Tvalue);

                        my $of := $next.of;
                        $handler := $of.HOW.archetypes.nominal
                          && (nqp::eqaddr($of, Mu) || nqp::istype($value, $of))
                          ?? $no-typecheck
                          !! $typecheck;
                    }
                }
            }

            # A simple container assignment that contains type information
            if nqp::eqaddr($desc-WHAT, ContainerDescriptor) {
                my $of := nqp::getattr($desc, ContainerDescriptor, '$!of');
                my $nominal := $of.HOW.archetypes.nominal;

                # Need to set default value
                if nqp::eqaddr($value, Nil) {
                    # Copy in the default, provided we've a simple type.
                    if $nominal {
                        nqp::guard('type', $Tvalue);
                        nqp::guard('type', $Tdesc);
                        nqp::guard('literal',
                          nqp::track('attr',$Tdesc,ContainerDescriptor,'$!of')
                        );
                        $handler := $assign-scalar-nil-no-whence;
                    }
                }

                # No whence, no Nil. Is it a nominal type? If yes, we can
                # check it here.
                else {
                    nqp::guard('type', $Tdesc);
                    if $nominal && nqp::istype($value, $of) {
                        # Nominal and passes type check; stack up guards and
                        # delegate to simple bind.
                        nqp::guard('literal',
                          nqp::track('attr',$Tdesc,ContainerDescriptor,'$!of')
                        );
                        nqp::guard('type', $Tvalue);
                        $handler := $assign-scalar-no-whence-no-typecheck;
                    }
                    else {
                        # Non-nominal or type check error.
                        nqp::guard('not-literal-obj', $Tvalue, Nil);
                        $handler := $assign-scalar-no-whence;
                    }
                }
            }

            # A container assignment without type information
            elsif nqp::eqaddr($desc-WHAT, ContainerDescriptor::Untyped) {
                if nqp::eqaddr($value, Nil) {
                    # Nil case is NYI.
                }
                else {
                    # Assignment to an untyped container descriptor;
                    # no type check is required, just bind the value
                    # into place.
                    nqp::guard('type', $Tdesc);
                    nqp::guard('not-literal-obj', $Tvalue, Nil);
                    $handler := $assign-scalar-no-whence-no-typecheck;
                }
            }

            # An attribute initialization
            elsif nqp::eqaddr(
                    $desc-WHAT,
                    ContainerDescriptor::UninitializedAttribute
                  ) {
                delegate-whence(
                  $assign-scalar-uninit-no-typecheck,
                  $assign-scalar-uninit
                );
            }

            # An array element initialization
            elsif nqp::eqaddr($desc-WHAT, ContainerDescriptor::BindArrayPos) {
                delegate-whence(
                  $assign-scalar-bindpos-no-typecheck,
                  $assign-scalar-bindpos
                );
            }

            # A hash element initialization
            elsif nqp::eqaddr($desc-WHAT, ContainerDescriptor::BindHashKey) {
                delegate-whence(
                  $assign-scalar-bindkey-no-typecheck,
                  $assign-scalar-bindkey
                );
            }

            # An object hash element initialization
            elsif nqp::eqaddr($desc-WHAT, ContainerDescriptor::BindObjHashKey) {
                delegate-whence(
                  $assign-scalar-obj-bindkey-no-typecheck,
                  $assign-scalar-obj-bindkey
                );
            }

            # A two-dimensional array element initialization
            elsif nqp::eqaddr($desc-WHAT, ContainerDescriptor::BindArrayPos2D) {
                delegate-whence(
                  $assign-scalar-bindpos2d-no-typecheck,
                  $assign-scalar-bindpos2d
                );
            }

            # A three-dimensional array element initialization
            elsif nqp::eqaddr($desc-WHAT, ContainerDescriptor::BindArrayPos3D) {
                delegate-whence(
                  $assign-scalar-bindpos3d-no-typecheck,
                  $assign-scalar-bindpos3d
                );
            }

            # An N-dimensional array element initialization
            elsif nqp::eqaddr($desc-WHAT, ContainerDescriptor::BindArrayPosND) {
                delegate-whence(
                  $assign-scalar-bindposnd-no-typecheck,
                  $assign-scalar-bindposnd
                );
            }
        }

        # Do the delegation with the indicated handler
        nqp::delegate('boot-code-constant',
          nqp::syscall('dispatcher-insert-arg-literal-obj',
            $capture, 0, $handler
          )
        );
    });
}

#- raku-bind-assert ------------------------------------------------------------
# Binding type assertion dispatcher, used to do type checks of binds.
# Evaluates to the value itself when the type check passes, installing
# a guard along the way. Otherwise, throws.
{
    # Helper sub to throw type check error
    sub bind-error($got, $expected) {
        Perl6::Metamodel::Configuration.throw_or_die(
            'X::TypeCheck::Binding',
            "Type check failed in binding; expected '"
              ~ nqp::how_nd($expected).name($expected)
              ~ "' but got '"
              ~ nqp::how_nd($got).name($got)
              ~ "'",
            :$got, :$expected
        );
    }

    # The run-time checker
    my $bind-check := -> $value, $value-decont, $type {
        nqp::istype_nd($value-decont, $type)
          ?? $value
          !! bind-error($value, $type)
    }

    # Actual dispatcher Expects the original value as the first argument,
    # the deconted value as the second, and the type to be checked against
    # as the third argument.
    nqp::register('raku-bind-assert', -> $capture {
        my $value-decont := nqp::captureposarg($capture, 1);
        my $type := nqp::captureposarg($capture, 2);

        # Nominal, so a type guard on the decont'd value will suffice,
        # then produce the original value.
        if nqp::how_nd($type).archetypes.nominal {

            # Type is ok
            if nqp::istype_nd($value-decont, $type) {
                nqp::guard('type', nqp::track('arg', $capture, 1));
                nqp::guard('type', nqp::track('arg', $capture, 2));
                nqp::delegate('boot-value', $capture);
            }

            # Not ok
            else {
                bind-error(nqp::captureposarg($capture, 0), $type);
            }
        }

        # Not a nominal type, can't guard it, so set up a call to do the
        # check late-bound.
        else {
            nqp::guard('type', nqp::track('arg', $capture, 2));
            nqp::delegate('boot-code-constant',
              nqp::syscall('dispatcher-insert-arg-literal-obj',
                $capture, 0, $bind-check
              )
            );
        }
    });
}

#- raku-is-attr-inited ---------------------------------------------------------
# Object construction time checking of if a container is initialized. Done as
# a dispatcher primarily to intern .^mixin_type, but also for more compact
# bytecode size in generated BUILDALL.

# Array initialization checker
my $array-init-check := -> $arr {
    my $storage := nqp::getattr($arr, List, '$!reified');
    nqp::isconcrete($storage) && nqp::elems($storage)
}

# Hash initialization checker
my $hash-init-check := -> $hash {
    my $storage := nqp::getattr($hash, Map, '$!storage');
    nqp::isconcrete($storage) && nqp::elems($storage)
}

# Actual dispatcher taking the attribute and returning 1 if initialized,
# and 0 if not.
nqp::register('raku-is-attr-inited', -> $capture {
    # If there's a non-concrete object observed, then we bound a non-container
    # in place, so trivially initialized.
    my $attr  := nqp::captureposarg($capture, 0);
    my $Tattr := nqp::track('arg', $capture, 0);
    my int $inited;
    my $elem-check;

    # Might be a container that was assigned.
    if nqp::isconcrete_nd($attr) {
        # Just try and read a descriptor. Also see if we have an array or
        # hash, which needs an additional element check to handle an
        # assignment to an individual element during BUILD.
        my $base;
        my $desc;
        my $Tdesc;
        try {
            $base  := nqp::how_nd($attr).mixin_base($attr);
            $desc  := nqp::getattr($attr, $base, '$!descriptor');
            $Tdesc := nqp::track('attr', $Tattr, $base, '$!descriptor');
        }

        # If we managed to track a descriptor, then we have a container to
        # see if was uninitialized. The attribute tracking above will have
        # established type/concreteness guards on the attribute, so don't
        # repeat them.
        if $Tdesc {
            # Guard on the descriptor type, then outcome depends on if
            # it's an uninitialized attribute descriptor. If it is, then
            # for arrays and hashes we also need an extra check.
            nqp::guard('type', $Tdesc);
            $inited := !nqp::eqaddr(
              $desc.WHAT, ContainerDescriptor::UninitializedAttribute
            );
            if nqp::istype($base, Array) {
                $elem-check := $array-init-check;
            }
            elsif nqp::istype($base, Hash) {
                $elem-check := $hash-init-check;
            }
        }

        # Otherwise, bound concrete value. Guard on type and concreteness,
        # outcome is that it's initialized.
        else {
            guard-type-concreteness($Tattr);
            $inited := 1;
        }
    }

    # Nothing concrete, so no container yet
    else {
        nqp::guard('concreteness', $Tattr);
        $inited := 1;
    }

    $inited || !nqp::isconcrete($elem-check)
      # Initialized per the descriptor, or no additional check needed
      ?? nqp::delegate('boot-constant',
           nqp::syscall('dispatcher-insert-arg-literal-int',
             $capture, 0, $inited
           )
         )
      # The descriptor suggests it's not initialized by assignment to
      # the entire array/hash, but individual elements may have been.
      !! nqp::delegate('boot-code-constant',
           nqp::syscall('dispatcher-insert-arg-literal-obj',
             $capture, 0, $elem-check
           )
         );
});

#- raku-sink -------------------------------------------------------------------
# Sink dispatcher. Called in void context with the value to sink, possibly
# inside a container.
nqp::register('raku-sink', -> $capture {
    # Guard on the type and concreteness.
    my $sinkee := nqp::captureposarg($capture, 0);
    guard-type-concreteness(nqp::track('arg', $capture, 0));

    # Now consider what we're sinking.
    if nqp::isconcrete_nd($sinkee) {
        # Concrete. See if it has a `sink` method, and then also if it's not
        # Mu.sink, which is a no-op
        my $sink := nqp::decont(
          nqp::how_nd($sinkee).find_method($sinkee, 'sink')
        );

        # A non-standard .sink method
        if nqp::isconcrete($sink)
          && (
              !nqp::eqaddr($sink, Mu.HOW.find_method(Mu, 'sink'))
              || nqp::istype(nqp::how_nd($sinkee), Perl6::Metamodel::NativeRefHOW)
          ) {

            # Need to actually do a call to the sink method. Since sink
            # is a Raku thing, assume we can go straight for the Raku
            # method dispatcher, with the necessary args prefixed
            # (resolved method, type, name)
            nqp::delegate('raku-meth-call-resolved',
              nqp::syscall('dispatcher-insert-arg-literal-obj',
                nqp::syscall('dispatcher-insert-arg-literal-obj',
                  nqp::syscall('dispatcher-insert-arg-literal-str',
                    $capture, 0, 'sink'
                  ),
                  0, nqp::what_nd($sinkee)
                ),
                0, $sink
              )
            );
        }

        # Nothing to do (use boot-value and let void context take
        # care of discarding the value)
        else {
            nqp::delegate('boot-value', $capture);
        }
    }
    else {
        # Not concrete, nothing to do.
        nqp::delegate('boot-value', $capture);
    }
});

#- raku-call -------------------------------------------------------------------
# A standard call (such as `func($arg)`, `$obj($arg)`, etc.) It receives the
# decontainerized callee as the first argument, followed by the arguments. Its
# primary purpose is to deal with multi dispatch vs. single dispatch and then
# delegate on to the appropriate dispatcher. It also looks at if we are doing
# a call to a method (either because it was looked up separately, or because
# we# are here via lang-call), and in that case sends it via the resolved
# method call dispatcher. This means it will have a working nextsame etc.
nqp::register('raku-call', -> $capture {

    # Guard on the type and, if it's a routine, on the dispatchees. (We assume
    # that the set of dispatchees shall not change, even over closure clones -
    # this may not always be a good assumption - and so we guard on that. If
    # it's not a dispatcher, we'll be guarding on a literal type object.)
    my $callee  := nqp::captureposarg($capture, 0);
    my $Tcallee := nqp::track('arg', $capture, 0);
    nqp::guard('type', $Tcallee);

    # The actual delegation to be done
    my str $delegate := 'raku-invoke';

    # Looks like a resolved method
    if nqp::istype_nd($callee, Method)
      && (my str $meth-name := $callee.name)
      && (my $inv_param := try { $callee.signature.params.AT-POS(0) }) {
        nqp::guard('literal', $Tcallee);

        # Add the type and method name to capture as expected
        $capture := nqp::syscall('dispatcher-insert-arg-literal-str',
          nqp::syscall('dispatcher-insert-arg-literal-obj',
            $capture, 1, $inv_param.type
          ),
          2, $meth-name
        );

        $delegate := 'raku-meth-call-resolved';
    }

    # Looks like a sub, set appropriate delegator
    elsif nqp::istype_nd($callee, Routine) {

        # Need extra handling of wrappers
        if nqp::can($callee, 'WRAPPERS') {
            $delegate := 'raku-invoke-wrapped';
        }

        # Not something to just invoke
        elsif !nqp::can($callee, 'CALL-ME') {

            # Guard if not a literal callee
            nqp::guard('literal',
              nqp::track('attr', $Tcallee, Routine, '@!dispatchees')
            ) unless nqp::syscall('dispatcher-is-arg-literal', $capture, 0);

            $delegate := $callee.is_dispatcher
              ?? 'raku-multi'
              !! 'raku-invoke';
        }
    }

    nqp::delegate($delegate, $capture);
});

#- raku-meth-call --------------------------------------------------------------
# A standard method call of the form $obj.meth($arg); also used for the
# indirect form $obj."$name"($arg).

# Helper sub for error reporting.
sub report-method-not-found($obj, $name, $class, $how, $containerized) {
    my $message := "Method '$name' not found for invocant of class '"
      ~ $how.name($obj)
      ~ "'";

    $name eq 'STORE'
      ?? Perl6::Metamodel::Configuration.throw_or_die(
           'X::Assignment::RO', $message, :value($obj)
         )
      !! Perl6::Metamodel::Configuration.throw_or_die(
           'X::Method::NotFound',
           $message,
           :invocant($obj),
           :method($name),
           :typename($how.name($obj)),
           :private(nqp::hllboolfor(0, 'Raku')),
           :in-class-call(
              nqp::hllboolfor(nqp::eqaddr(nqp::what($obj), $class), 'Raku')
            ),
           :containerized(nqp::hllboolfor($containerized, 'Raku'))
         );
}
# The actual dispatcher of a method.  Expects to receive the decontainerized
# invocant, the method name, and the args (starting with the invocant
# including any container).
nqp::register('raku-meth-call', -> $capture {

    # See if this callsite is heading megamorphic due to loads of different
    # method names or types; if so, we'll try to cope with that.
    my $obj := nqp::captureposarg($capture, 0);
    my $how := nqp::how_nd($obj);
    if nqp::syscall('dispatcher-inline-cache-size') >= $MEGA-METH-CALLSITE-SIZE
      && nqp::istype($how, Perl6::Metamodel::ClassHOW) {
        nqp::delegate('raku-meth-call-mega', $capture);
    }

    # Not mega-morphic yet, look more closely
    else {

        # See if we're making the method lookup on a pun; if so,
        # rewrite the args to do the call on the pun.
        my str $name := nqp::captureposarg_s($capture, 1);
        if nqp::istype($how, Perl6::Metamodel::RolePunning) &&
          $how.is_method_call_punned($obj, $name) {
            nqp::guard('type', nqp::track('arg', $capture, 0));

            # Replace by pun
            $obj := $how.pun($obj);
            $how := $obj.HOW;
            $capture := nqp::syscall('dispatcher-insert-arg-literal-obj',
                nqp::syscall('dispatcher-drop-arg', $capture, 0),
                0, $obj);
            $capture := nqp::syscall('dispatcher-insert-arg-literal-obj',
                nqp::syscall('dispatcher-drop-arg', $capture, 2),
                2, $obj);
        }

        # TODO Assorted optimizations are possible here later on to speed
        # up some kinds of dispatch, including:
        # * Using the dispatcher to directly rewrite args and invoke
        #   FALLBACK if needed
        # * Handling some forms of delegation via the dispatcher mechanism

        # Try to resolve the method call, report an error if there is
        # no such method.
        my $meth := nqp::decont($how.find_method($obj, $name));
        report-method-not-found(
          $obj, $name, nqp::getlexcaller('$?CLASS'), $how, nqp::iscont($obj)
        ) unless nqp::isconcrete($meth);

        # Establish a guard on the invocant type and method name (however
        # the name may well be a literal, in which case this is free).
        nqp::guard('type',    nqp::track('arg', $capture, 0));
        nqp::guard('literal', nqp::track('arg', $capture, 1));

        # Add the resolved method, delegate to the resolved method dispatcher
        nqp::delegate('raku-meth-call-resolved',
          nqp::syscall('dispatcher-insert-arg-literal-obj', $capture, 0, $meth)
        );
    }
});

#- raku-meth-call-mega ---------------------------------------------------------
# Internal dispatcher to be run when the inline cache is too large.
nqp::register('raku-meth-call-mega', -> $capture {
    # We're megamorphic in both type and name. Make sure the type has a lookup
    # table, and ensure the method exists. If it does not have a method table
    # then we will not install this dispatcher; we may already have a type
    # megamorphic handler in place and only missed it because we temporarily
    # lacked the calculated flattened method table. This avoids us stacking
    # up the same program repeatedly at the callsite.
    my $obj := nqp::captureposarg($capture, 0);
    my $how := nqp::how_nd($obj);

    # Don't install if no method table
    nqp::syscall('dispatcher-do-not-install')
      unless nqp::isconcrete(nqp::getattr(
               $how,
               Perl6::Metamodel::ClassHOW,
               '$!cached_all_method_table'
             ));

    # Obtain lookup table and key to look for
    my %lookup := $how.all_method_table($obj);
    my str $name := nqp::captureposarg_s($capture, 1);

    # It exists. We'll set up a dispatch program that tracks the HOW of
    # the type, looks up the cached method table on it, and then tracks
    # the resolution of the method.
    if nqp::isconcrete(nqp::atkey(%lookup, $name)) {
        my $Tobj := nqp::track('arg', $capture, 0);
        my $Thow := nqp::track('how', $Tobj);
        my $Ttable := nqp::track('attr',
          $Thow, Perl6::Metamodel::ClassHOW, '$!cached_all_method_table');
        my $Tname := nqp::track('arg', $capture, 1);
        my $Tresolution := nqp::syscall(
          'dispatcher-index-tracked-lookup-table', $Ttable, $Tname
        );

        # This is only a valid dispatch program if the method is found.
        # (If not, we'll run this again to report the error)
        nqp::guard('concreteness', $Tresolution);

        # Add the resolved method and delegate to the resolved method dispatcher.
        nqp::delegate('raku-meth-call-resolved',
          nqp::syscall('dispatcher-insert-arg', $capture, 0, $Tresolution)
        );
    }

    # Probably method not found, but sometimes the cache ends up
    # missing entries
    else {
        my $slowpath := $how.find_method($obj, $name);
        if nqp::isconcrete($slowpath) {
            nqp::syscall('dispatcher-do-not-install');
            nqp::delegate('raku-meth-call-resolved',
              nqp::syscall('dispatcher-insert-arg-literal-obj',
                $capture, 0, $slowpath
              )
            );
        }
        else {
            report-method-not-found(
              $obj, $name, nqp::getlexcaller('$?CLASS'), $how, nqp::iscont($obj)
            );
        }
    }
});

#- raku-meth-call-qualified ----------------------------------------------------
# Qualified method call dispatcher. This is used for calls of the form
# $foo.Some::ClassOrRole::bar($arg).  It receives the decontainerized
# invocant, the method name, the type qualifier, and then the args
# (starting with the invocant including any container).
nqp::register('raku-meth-call-qualified', -> $capture {

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
    nqp::guard('type', nqp::track('arg', $capture, 0));
    nqp::guard('type', nqp::track('arg', $capture, 2));

    # Try to resolve the method.
    my $obj      := nqp::captureposarg($capture, 0);
    my str $name := nqp::captureposarg_s($capture, 1);
    my $type     := nqp::captureposarg($capture, 2);
    my $meth;
    for ($caller-type, $obj.WHAT) {
        last if nqp::istype($_, $type) && nqp::isconcrete(
          $meth := nqp::how_nd($_).find_method_qualified($_, $type, $name)
        );
    }

    # If it's resolved, then:
    nqp::isconcrete($meth)
      # 1. Drop the invocant and type arguments targetted at this resolution
      # 2. Insert the type we resolved the method against before those, for
      #    deferral (the name is retained ahead of this)
      # 3. Finally, prepend the resolved method, and delegate to the
      #    resolved method dispatcher
      ?? nqp::delegate('raku-meth-call-resolved',
           nqp::syscall('dispatcher-insert-arg-literal-obj',   # prepend method
             nqp::syscall('dispatcher-insert-arg-literal-obj', # prepend type
               nqp::syscall('dispatcher-drop-arg',             # drop args
                 nqp::syscall('dispatcher-drop-arg', $capture, 2),
                 0
               ),
               0, $type
             ),
             0, $meth
           )
         )
      # Otherwise, exception
      !! Perl6::Metamodel::Configuration.throw_or_die(
           'X::Method::InvalidQualifier',
           "Cannot dispatch to method $name on "
             ~ nqp::how_nd($type).name($type)
             ~ " because it is not inherited or done by "
             ~ nqp::how_nd($obj).name($obj),
           :method($name), :invocant($obj), :qualifier-type($type)
         );
});

#- raku-meth-call-me-maybe -----------------------------------------------------
# Maybe method dispatch, of the form $obj.?foo.  Expects the invocant and
# the method name, followed by any arguments.
nqp::register('raku-meth-call-me-maybe', -> $capture {

    # Establish a guard on the invocant type
    nqp::guard('type', nqp::track('arg', $capture, 0));

    my $invocant := nqp::captureposarg($capture, 0);
    my str $name := nqp::captureposarg_s($capture, 1);

    # Try to find the method
    my $method := nqp::decont(
      nqp::how_nd($invocant).find_method($invocant, $name)
    );
    nqp::isconcrete($method)
      # Found it. Put in resolved method and leave the rest to the
      # resolved method call dispatcher.
      ?? nqp::delegate('raku-meth-call-resolved',
           nqp::syscall('dispatcher-insert-arg-literal-obj',
             $capture, 0, $method
           )
         )
      # Not found. Return Nil
      !! delegate-constant-Nil($capture);
});

#- raku-meth-private -----------------------------------------------------------
# Private method dispatch. This is actually a fallback, since in the best
# case we can resolve the private method into a constant at code-gen time
# and just invoke that. This happens with private methods in roles.
nqp::register('raku-meth-private', -> $capture {
    # Find the private method.
    my $type     := nqp::captureposarg($capture, 0);
    my str $name := nqp::captureposarg_s($capture, 1);
    my $meth := nqp::how_nd($type).find_private_method($type, $name);

    # If it's found, then we drop the first two arguments, insert the
    # resolved callee, and invoke it. This goes directly to invoke, as
    # there's no deferral (due to no inheritance relationship) or multi
    # dispatch for private methods.
    if nqp::isconcrete($meth) {
        nqp::guard('type', nqp::track('arg', $capture, 0));
        nqp::delegate('raku-invoke',
          nqp::syscall('dispatcher-insert-arg-literal-obj',
            nqp::syscall('dispatcher-drop-n-args', $capture, 0, 2),
            0, $meth
          )
        );
    }

    # Not found
    else {
        # TODO typed exception
        # NOTE: did not find a way to fire this path yet
        nqp::die("No such private method '$name' on " ~ nqp::how_nd($type).name($type));
    }
});

#- raku-meth-call-resolved -----------------------------------------------------
# A linked list is used to model the state of a dispatch that is deferring
# through a set of methods or wrappers (a similar approach is used for
# multi dispatch, but it needs more types of node). The Exhausted class
# is used as a sentinel for the end of the chain (or for dispatchers in
# general that have nothing more to contribute). The current state of the
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
}
my class Exhausted {}

# We also need a strategy for `callwith`. The complication is that arguments
# given to `callwith` should trigger a resumption of the innermost dispatch
# (for example, a wrap), but should also influence the arguments of outer
# dispatches too (for example, a multi and/or method dispatch). The overall
# strategy for `callwith` is:
# 1. Dispatchers for resumable dispatches are always broken into two parts:
#    one that manages the initial dispatch when there is no resume, and
#    another that walks through the candidates.
# 2. When we do a `callwith`, the current innermost of those dispatchers
#    marks itself exhausted. It then re-enters itself, passing along a
#    resumption of kind PROPAGATE_CALLWITH. This includes the information
#    required to put the next immediate call of the innermost dispatch
#    into effect (more in a moment on this).
# 3. The re-entered dispatcher sets its resume init args up using the
#    arguments from the callwith propagation, so that a later `callsame`
#    will use the correct arguments.
# 4. It then tries to use next-resumption to pass the callwith propagation
#    to any enclosing dispatcher. This always uses a common protocol: the
#    arguments are a kind (the PROPAGATE_CALLWITH integer constant), a
#    dispatcher name (string argument), a flag for if the arguments include
#    the invocant (an integer), followed by (typically) some arguments to
#    the named dispatcher. This is the information required to send the
#    dispatch to the destination determined by the *innermost* dispatcher.
# 5. If there is no next resumption, then we do that call.
# 6. Any outer dispatcher, upon receiving a PROPAGATE_CALLWITH, will mark
#    itself exhausted and re-enter itself with the new arguments. It then
#    continues with the above process from step 4.
# Thus, a callwith in the case there is an active wrap, multi, and method
# dispatch, ends up with all the previous resumable dispatchers for these
# marked as exhausted, and new ones created, with the updated arguments.
# Once all of these updated dispatchers are in place, then finally the next
# candidate of the innermost dispatch is invoked.
sub nil-or-callwith-propagation-terminal($capture) {
    unless nqp::syscall('dispatcher-next-resumption', $capture) {
        nqp::captureposarg_i($capture,0) == nqp::const::DISP_PROPAGATE_CALLWITH
          ?? nqp::delegate(
               nqp::captureposarg_s($capture, 1),
               nqp::syscall('dispatcher-drop-n-args', $capture, 0, 3)
             )
          !! delegate-constant-Nil($capture);
    }
}

# Resolved method call dispatcher. This is used to call a method, once we have
# already resolved it to a callee. Its first arg is the callee, the second and
# third are the type and name (used in deferral), and the rest are the args to
# the method.
nqp::register('raku-meth-call-resolved',

    # Initial dispatch
    -> $capture {
        # Save dispatch state for resumption. We don't need the method that
        # will be called now, so drop it.
        nqp::syscall('dispatcher-set-resume-init-args',
          nqp::syscall('dispatcher-drop-arg', $capture, 0)
        );

        # Drop the dispatch start type and name, and delegate to
        # multi-dispatch or just invoke if it's single dispatch.
        my str $delegate := 'raku-invoke';
        $capture := nqp::syscall('dispatcher-drop-n-args', $capture, 1, 2);
        my $method := nqp::captureposarg($capture, 0);

        # Need at least a type guard on the callee, if it's not constant.
        my int $code-constant :=
          nqp::syscall('dispatcher-is-arg-literal', $capture, 0);
        nqp::guard('type', nqp::track('arg', $capture, 0))
          unless $code-constant;

        # Looks like a sub, set appropriate delegator
        if nqp::istype($method, Routine) {

            # Need extra handling of wrappers
            if nqp::can($method, 'WRAPPERS') {
                $delegate := 'raku-invoke-wrapped';
            }

            # Not something to just invoke
            elsif !nqp::can($method, 'CALL-ME') {
                # If it's not a constant, need a guard on whether it's a
                # dispatcher, and if so on the candidate list. (Will want
                # to move this when we have a megamorphic multi solution.)
                nqp::guard('literal',
                  nqp::track('attr',
                    nqp::track('arg', $capture, 0),
                    Routine,
                    '@!dispatchees'
                  )
                ) unless $code-constant;

                $delegate := $method.is_dispatcher
                  ?? 'raku-multi'
                  !! 'raku-invoke';
            }
        }

        nqp::delegate($delegate, $capture);
    },

    # Resumption. The capture itself has a first argument indicating the kind
    # of resumption operation we're doing. The resume init capture's first two
    # arguments are the type that we initially did a method dispatch against
    # and the method name respectively. In this resumption we calculate the
    # set of methods we will defer through. We then delegate to another
    # dispatcher to handle movement through that list (this structure helps
    # us to handle `callwith`).
    -> $capture {

        # We put a sentinel value into the resume state in the case that we
        # have already set up the method resumption. We always guard on that
        # as well.
        my $state := nqp::syscall('dispatcher-get-resume-state');
        my $Tstate := nqp::track('resume-state');
        nqp::guard('literal', $Tstate);

        # Mark that the deferral was already set up, so we don't do this
        # again.
        nqp::syscall('dispatcher-set-resume-state-literal', Exhausted);

        # Guard on the kind of resume we're doing, and get that flag.
        my $Tkind := nqp::track('arg', $capture, 0);
        nqp::guard('literal', $Tkind);
        my int $kind := nqp::captureposarg_i($capture, 0);

        # If the state is null, it's we are entering a walk through the
        # methods.  We can short-circuit this if it's a lastcall (and
        # the Exhausted above puts it into effect).
        if nqp::isnull($state) && $kind != nqp::const::DISP_LASTCALL {

            # No state, so just starting the resumption. Guard on the
            # invocant type and name.
            my $init := nqp::syscall('dispatcher-get-resume-init-args');
            my $Tstart_type := nqp::track('arg', $init, 0);
            nqp::guard('type', $Tstart_type);
            my $Tname := nqp::track('arg', $init, 1);
            nqp::guard('literal', $Tname);

            # Build up the list of methods to defer through.
            my $start_type := nqp::captureposarg($init, 0);
            my str $name := nqp::captureposarg_s($init, 1);
            my $start_type_how := nqp::how_nd($start_type);
            my @mro := nqp::can($start_type_how, 'mro_unhidden')
              ?? $start_type_how.mro_unhidden($start_type)
              !! nqp::can($start_type_how, 'mro')
                ?? $start_type_how.mro($start_type)
                !! [];

            my @methods;
            for @mro {
                my $method := nqp::atkey(
                  nqp::how_nd($_).method_table($_), $name
                );
                @methods.push($method) unless nqp::isnull($method);
            }

            # Turn it into a linked list if there is a chain, otherwise
            # mark as exhausted.
            my $chain := Exhausted;
            if nqp::elems(@methods) >= 2 {
                # Discard the first one, which we initially called
                @methods.shift;

                while @methods {
                    $chain := DeferralChain.new(@methods.pop, $chain);
                }
            }

            # Determine the args to pass to the method we defer to. If
            # it's a callwith, then the arguments are given to us here.
            # If it's a callwith propagration, we need to use those args
            # also. Otherwise, they are those from the initial capture.
            my $args_with_kind;
            if $kind == nqp::const::DISP_CALLWITH {

                # Rewrite the kind into callsame, since we've already accounted
                # for the callwith. We do need to insert the original invocant.
                my $args := nqp::syscall('dispatcher-drop-arg', $capture, 0);
                $args_with_kind := nqp::syscall(
                  'dispatcher-insert-arg-literal-int',
                  nqp::syscall('dispatcher-insert-arg',
                    $args, 0, nqp::track('arg', $init, 2)
                  ),
                  0, nqp::const::DISP_CALLSAME
                );
            }
            elsif $kind == nqp::const::DISP_PROPAGATE_CALLWITH {
                $args_with_kind := $capture;
            }
            else {
                $args_with_kind := nqp::syscall('dispatcher-insert-arg',
                  nqp::syscall('dispatcher-drop-n-args', $init, 0, 2),
                  0, $Tkind
                );
            }

            # Prepend the chain of methods we dispatch through and defer.
            nqp::delegate('raku-meth-deferral',
              nqp::syscall('dispatcher-insert-arg-literal-obj',
                $args_with_kind, 0, $chain
              )
            );
        }

        # Otherwise, we already set up - and presumably completed - walking
        # through the methods.
        else {
            delegate-constant-Nil($capture);
        }
    }
);

#- raku-meth-call-deferral -----------------------------------------------------
# Helper sub to determine the kind of deferral
sub method-deferral-step($chain-head, int $kind, $args) {

    # Look at the kind of deferral we have to decide what to do.
    my $code := $chain-head.code;

    # Call with same (that is, original) arguments. Invoke with those
    if $kind == nqp::const::DISP_CALLSAME {

        # Call with same (that is, original) arguments. Invoke with those.
        # We drop the first two arguments (which are only there for the
        # resumption), add the code object to invoke, and then leave it
        # to the invoke dispatcher
        my str $delegate := 'raku-invoke';
        my $capture :=
          nqp::syscall('dispatcher-insert-arg-literal-obj', $args, 0, $code);

        if nqp::istype($code, Routine) {
            if nqp::can($code, 'WRAPPERS') {
                $delegate := 'raku-invoke-wrapped';
            }
            elsif !nqp::can($code, 'CALL-ME') && $code.is_dispatcher {
                $delegate := 'raku-multi';
            }
        }

        nqp::delegate($delegate, $capture);
    }

    # We just want method itself, not to invoke it.
    elsif $kind == nqp::const::DISP_NEXTCALLEE {
        delegate-constant($args, $code);
    }
    else {
        nqp::die('Unexpected resumption kind in method dispatch');
    }
}

# Actual method deferral dispatcher.
nqp::register('raku-meth-deferral',

    # Entry to the deferral, with the chain of methods to walk through as the
    # first argument, the resumption kind as the second argument, and the
    # args to the method (including the invocant) coming next. We assume here
    # that we already established guards on kind and similar, and that if we
    # have a lastcall we'll have handled it rather than sending it here.
    -> $capture {

        # If the chain is Exhausted, then we will delegate to Nil or to
        # the propagate callwith terminal.
        my $chain := nqp::captureposarg($capture, 0);
        if nqp::eqaddr($chain, Exhausted) {
            nil-or-callwith-propagation-terminal(
              nqp::syscall('dispatcher-drop-arg', $capture, 0)
            );
        }

        # Not exhausted yet
        else {
            my int $kind := nqp::captureposarg_i($capture, 1);

            # If we're propagating a callwith then we need to set the
            # resume init args and then invoke the thing that is being
            # propagated
            if $kind == nqp::const::DISP_PROPAGATE_CALLWITH {
                nqp::syscall('dispatcher-set-resume-init-args',
                  # Drop kind, dispatcher name, invocant flag, invokee
                  nqp::syscall('dispatcher-drop-n-args', $capture, 1, 4)
                );

                nqp::delegate(
                  nqp::captureposarg_s($capture, 2),
                  nqp::syscall('dispatcher-drop-n-args', $capture, 0, 4)
                );
            }

            # Otherwise, need to do a dispatch step.
            else {

                # The resume init state for a multi-step deferral is the next
                # thing in the chain prepended to the dispatch arguments
                my $args := nqp::syscall('dispatcher-drop-n-args',
                  $capture, 0, 2  # drop kind, dispatcher name
                );
                nqp::syscall('dispatcher-set-resume-init-args',
                  nqp::syscall('dispatcher-insert-arg-literal-obj',
                    $args, 0, $chain.next
                  )
                );

                # Now perform the action needed based upon the kind of
                # resumption we have
                method-deferral-step($chain, $kind, $args);
            }
        }
    },

    # Resumption, wherein we walk another step through the chain.
    -> $capture {

        # Guard on the kind of resume we're doing, and get that flag.
        my $Tkind := nqp::track('arg', $capture, 0);
        nqp::guard('literal', $Tkind);
        my int $kind := nqp::captureposarg_i($capture, 0);

        # If we're doing a lastcall, set the state to exhausted and we're done.
        if $kind == nqp::const::DISP_LASTCALL {
            nqp::syscall('dispatcher-set-resume-state-literal', Exhausted);
            delegate-constant-Nil($capture);
        }

        else {
            # If there's no dispatch state yet, we're on our first round of
            # resumption for this dispatcher. Otherwise, look to the state to
            # find the next method.
            my $init := nqp::syscall('dispatcher-get-resume-init-args');
            my $state  := nqp::syscall('dispatcher-get-resume-state');
            my $Tstate := nqp::track('resume-state');
            my $chain;
            my $Tchain;
            if nqp::isnull($state) {
                # Guard that the resume state is null, and then extract
                # the chain from the init state.
                nqp::guard('literal', $Tstate);
                $chain  := nqp::captureposarg($init, 0);
                $Tchain := nqp::track('arg', $init, 0);
            }
            else {
                # The chain is the state.
                $chain  := $state;
                $Tchain := $Tstate;
            }

            # If we're exhausted already, then produce Nil.
            if nqp::istype($chain, Exhausted) {
                nqp::guard('literal', $Tchain);
                delegate-constant-Nil($capture);
            }

            # If we're propagating new callwith args then mark this dispatcher
            # exhausted and re-enter with the updated args and current chain.
            elsif $kind == nqp::const::DISP_PROPAGATE_CALLWITH {
                nqp::syscall('dispatcher-set-resume-state-literal', Exhausted);
                nqp::delegate('raku-meth-deferral',
                  nqp::syscall('dispatcher-insert-arg', $capture, 0, $Tchain)
                );
            }

            else {
                # Otherwise, guard on the candidate that we shall be invoking.
                my $Tmethod :=
                  nqp::track('attr', $Tchain, DeferralChain, '$!code');
                nqp::guard('literal', $Tmethod);

                # Now perform the action needed based upon the kind of
                # resumption we have.
                my $args := nqp::syscall('dispatcher-drop-arg', $init, 0);
                if $kind == nqp::const::DISP_CALLWITH {

                    # Set the state to exhausted as we're abandoning the
                    # walk through the methods with these args.
                    nqp::syscall('dispatcher-set-resume-state-literal',
                      Exhausted);

                    # Re-enter this dispatcher with the new args.
                    nqp::delegate('raku-meth-deferral',
                      nqp::syscall('dispatcher-insert-arg',
                        nqp::syscall('dispatcher-insert-arg-literal-int',
                          nqp::syscall('dispatcher-insert-arg',
                            nqp::syscall('dispatcher-drop-arg', $capture, 0),
                            0, nqp::track('arg', $args, 0)
                          ),
                          0, nqp::const::DISP_CALLSAME
                        ),
                        0, $Tchain
                      )
                    );
                }
                else {
                    # Update dispatch state to point to the next method.
                    my $Tnext :=
                      nqp::track('attr', $Tchain, DeferralChain, '$!next');
                    nqp::syscall('dispatcher-set-resume-state', $Tnext);

                    # It's a normal step.
                    method-deferral-step($chain, $kind, $args);
                }
            }
        }
    }
);

#- raku-multi ------------------------------------------------------------------
# We in principle could always call the proto and have correct behavior.
# It is, however, nice when we can go straight to a candidate. To see if
# that's the case, we have to introspect the signature.
sub is-simple-args-proto($callee, $capture) {

    # If we're compiling the setting, all sorts of things we need to do
    # the analysis are missing, so don't even try. Nothing invoked in the
    # CORE.setting build today that has an onlystar proto actually needs
    # its proto invoking today, so we also regard them all as simple, to
    # avoid a bunch of throwaway compilations.
    return 1 if $*COMPILING_CORE_SETTING;

    # If it's out of range so far as arity goes, we'll call the proto to
    # produce an error.
    my $signature := $callee.signature;

    # First element is the callee
    my int $got-args := nqp::captureposelems($capture) - 1;
    return 0 if nqp::islt_i(
      $got-args,
      nqp::getattr_i($signature, Signature, '$!arity')
    ) || nqp::isgt_n(
      $got-args,
      nqp::getattr($signature, Signature, '$!count')
    );

    # Otherwise, arity is alright. Look through the params.
    my int $accepts-any-named;
    for nqp::getattr($signature, Signature, '@!params') -> $param {
        # If there's a constraint or unpack, need the proto to be run.
        return 0 unless nqp::isnull(
          nqp::getattr($param, Parameter, '@!post_constraints')
        ) && nqp::isnull(
          nqp::getattr($param, Parameter, '$!sub_signature')
        );

        # Otherwise, go by kind of parameter.
        if $param.capture {
            $accepts-any-named := 1;
        }
        elsif $param.slurpy {
            $accepts-any-named := 1 if $param.named;
        }
        elsif $param.named {
            # Can be a bit smarter by diffing what nameds we have or
            # miss, but for now conservatively run the proto.
            return 0;
        }
    }

    # If we get here, it's OK to elide running the proto so long as
    # if we have any named args, it accepts them.
    nqp::capturehasnameds($capture) ?? $accepts-any-named !! 1;
}

# Multi-dispatch dispatcher, used for both multi sub and multi method dispatch.
# Assumes that we have already guarded on a literal code object (methods) or
# ensured consistency of routine (subs where closure cloning may take place).
# This does not do the heart of the dispatch itself, but rather determines if
# we have a simple or complex proto, and thus whether we need to invoke the
# proto at all. In the case of a complex proto, we use dispatch resumption to
# continue with the dispatch.
nqp::register('raku-multi',

    # Initial dispatch, only setting up resumption if we need to invoke the
    # proto.
    -> $capture {
        my $callee := nqp::captureposarg($capture, 0);
        my int $onlystar := $callee.onlystar;
        my int $simple := $onlystar && is-simple-args-proto($callee, $capture);
        my str $delegate := 'raku-multi-core';

        # Need to invoke the proto itself, so set resume init args and
        # make it run the proto.
        unless $simple {
            nqp::syscall('dispatcher-set-resume-init-args', $capture);
            $delegate := 'raku-invoke';
        }

        nqp::delegate($delegate, $capture);
    },
    # Resumption means that we have reached the {*} in the proto and so now
    # should go ahead and do the dispatch. Make sure we only do this if we
    # are signalled to that it's a resume for an onlystar.
    -> $capture {
        nqp::guard('literal', nqp::track('arg', $capture, 0));

        # A resume of an onlystar
        if nqp::captureposarg_i($capture, 0) == nqp::const::DISP_ONLYSTAR {

            # Put a guard on the dispatchee list, as a given proto may be
            # cloned and used for multiple candidate lists.
            $capture := nqp::syscall('dispatcher-get-resume-init-args');
            nqp::guard('literal',
              nqp::track('attr',
                nqp::track('arg', $capture, 0), Routine, '@!dispatchees'
              )
            );
            nqp::delegate('raku-multi-core', $capture);
        }

        # Nothing to resume to, so make it error out
        elsif !nqp::syscall('dispatcher-next-resumption', $capture) {
            nqp::delegate('raku-resume-error', $capture);
        }
    }
);

# If we invoke a multi with an argument that is a Proxy (or some other non-Scalar
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
                ++$i;
            }

            # If we don't already have a reader for this key, produce it.
            unless nqp::existskey(%!readers, $key) {
                %!readers{$key} := self.'!produce-reader'($num-args, $has-nameds, $indices);
            }

            %!readers{$key}
        }
    }

    method !produce-reader($num-args, $has-nameds, $indices) {
        # Create a block taking each positional arg required, adding a
        # slurpy named if needed.
        my $block := QAST::Block.new(:is_thunk);
        my int $i;
        while $i < $num-args {
            $block.push(QAST::Var.new( :name("a$i"), :decl<param>, :scope<local> ));
            ++$i;
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
                ++$decont-index
            }
            else {
                $dispatch.push($var);
            }
            ++$i;
        }
        if $has-nameds {
            $dispatch.push(QAST::Var.new( :name<n>, :scope<local>, :named, :flat ));
        }
        $block.push($dispatch);

        # Compile and return it.
        my $comp := nqp::getcomp('Raku');
        $comp.compile($block, :from($comp.exists_stage('optimize') ?? 'optimize' !! 'qast'))
    }
}
my $PROXY-READERS := ProxyReaderFactory.new;
nqp::bindhllsym('Raku', 'PROXY-READERS', $PROXY-READERS);

#- raku-multi-core -------------------------------------------------------------
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
# * The end of the candidates. Either a "no applicable candidates" error
#   if we are in the initial phase of dispatch, or a next resumption (or
#   Nil) otherwise.
my class MultiDispatchEnd {
    method debug() { "End" }
}

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
    method set-next($next) { $!next := $next }
    method candidate() { $!candidate }
    method next() { $!next }
    method trivial() { nqp::istype($!next,MultiDispatchEnd) }
    method debug() {
        "Candidate " ~ $!candidate.signature.raku ~ "\n" ~ $!next.debug
    }
}

# * A candidate to try and invoke; if there's a bind failure, it will be
#   mapped into a resumption
my class MultiDispatchTry is MultiDispatchCall {
    method trivial() { 0 }
    method debug() {
        "Try candidate " ~ self.candidate.signature.raku ~ "\n"
          ~ self.next.debug
    }
}

# * An ambiguity. It's only an error if it's reached during the initial
#   phase of dispatch, not during a callsame-alike, thus why the chain
#   continues beyond this point.
my class MultiDispatchAmbiguous {
    has $!next;
    method set-next($next) { $!next := $next }
    method next() { $!next }
    method more() { nqp::istype($!next,MultiDispatchCall) }
    method debug() { "Ambiguous\n" ~ $!next.debug }
}

# * Not actually used in a plan, but instead conveys that we have containers
#   that need complex (running code) removal. This is created with the indices
#   of the arguments that need it removing.
my class MultiDispatchNonScalar {
    has $!args;
    method new($capture, int $num_args, $args) {

        # Establish guards on types of all positionals, but not on the values
        # inside of them if they are Scalar containers; we just need to make
        # sure we have the appropriate tuple of Proxy vs non-Proxy for the
        # Proxy removal code we'll invoke.
        my int $i;
        while $i < $num_args {
            nqp::guard('type', nqp::track('arg', $capture, $i))
              unless nqp::captureposprimspec($capture, $i);
            ++$i;
        }

        my $obj := nqp::create(self);
        nqp::bindattr($obj, MultiDispatchNonScalar, '$!args', $args);
        $obj
    }
    method args() { $!args }
    method debug() { "Non-Scalar container dispatch" }
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
my @hll_type := (Nil, Int, Num, Str, Nil, Nil, Nil, Nil, Nil, Nil, Int);

# Helper sub, returning 1 if there is a mismatch in named arguments, else 0.
sub has-named-args-mismatch($capture, %info) {
    # First consider required nameds.
    my $required-name-sets := %info<required_names>;
    my $nameds-list := nqp::syscall('capture-names-list', $capture);

    if $required-name-sets {
        # Quick exit if we required names, but have none.
        return 1 unless $nameds-list;

        # Otherwise check that required nameds are present.
        my int $i;
        my int $n := nqp::elems($required-name-sets);
        while $i < $n {
            my int $found;
            my $names := nqp::atpos($required-name-sets, $i);
            my int $j;
            my int $m := nqp::elems($names);
            while $j < $m {
                if nqp::captureexistsnamed($capture, nqp::atpos_s($names, $j)) {
                    $found := 1;
                    last;
                }
                ++$j;
            }
            return 1 unless $found;
            ++$i;
        }
    }

    # If we don't accept all nameds, then check there are acceptable nameds
    if $nameds-list && !nqp::atkey(%info, 'allows_all_names') {
        # Quick exit if there are no allowed nameds
        return 1
          unless my $allowed-names := nqp::atkey(%info, 'allowed_names');

        # Go through the nameds and check they are allowed.
        my int $i;
        my int $n := nqp::elems($nameds-list);
        while $i < $n {
            return 1
              unless nqp::existskey(
                $allowed-names, nqp::atpos_s($nameds-list, $i)
            );
            ++$i;
        }
    }

    # Otherwise, no mismatch
    0
}

# HLL roles needed for checks.  Sadly these cannot be initialized in the
# mainline, because of bootstrapping issues, so these will be initialized
# when needed.
my $Positional             := nqp::null;
my $PositionalBindFailover := nqp::null;

my $Associative            := nqp::null;

# Helper sub to create the dispatch plan.
sub raku-multi-plan(
      @candidates,              # list of candidates to check
      $capture,                 # the arguments to work with
  int $stop-at-trivial,         # flag: stop at trivial problem
      $orig-capture = $capture  # original arguments, if any
) {
    my int $num_args := nqp::captureposelems($capture);

    # First check there's no non-Scalar containers in the positional arguments.
    # If there are, establish guards relating to those and we're done. Native
    # references don't count; we know the native types they shall match up with
    # and don't need to dereference them
    my $non-scalar := nqp::list_i();
    my int $i;
    while $i < $num_args {
        unless nqp::captureposprimspec($capture, $i) {
            my $value := nqp::captureposarg($capture, $i);
            nqp::push_i($non-scalar, $i)
              if nqp::isconcrete_nd($value)        # outside instantiated
              && nqp::iscont($value)               # and is a container
              && !(nqp::istype_nd($value, Scalar)  # but not a Scalar container
                    || nqp::iscont_s($value)  # nor a str container
                    || nqp::iscont_i($value)  # nor an int container
                    || nqp::iscont_u($value)  # nor an unsigned int container
                    || nqp::iscont_n($value)  # nor a num container
                  );
        }
        ++$i;
    }

    # Found non-scalar containers (proxies) so hand back the indices we
    # need to strip Proxy from
    return MultiDispatchNonScalar.new($capture, $num_args, $non-scalar)
      if nqp::elems($non-scalar);

    # We keep track of the head of the plan as well as the tail node of it,
    # so we know where to add the next step.
    my $current-head := nqp::null();
    my $current-tail := nqp::null();

    # Look through all candidates. Eliminate those that can be ruled out by
    # setting guards on the incoming arguments OR by the shape of the
    # callsite. That callsite shape includes argument count, which named
    # arguments are present, and which arguments are natively typed.
    my $need_scalar_read     := nqp::list_i;
    my $need_scalar_rw_check := nqp::list_i;
    my $need_type_guard      := nqp::list_i;
    my $need_conc_guard      := nqp::list_i;
    my @possibles;
    my int $candidates-with-itemizable-params;

    my int $done;
    my int $cur_idx;
    until $done {

        # The candidate list is broken into tied groups (that is, groups of
        # candidates that are equally narrow). Those are seperated by a
        # type object sentinel.
        my $cur_candidate := nqp::atpos(@candidates, $cur_idx++);

        # An actual candidate
        if nqp::isconcrete($cur_candidate) {

            # Mark this group for disambigation via is item traits on params
            if ! $candidates-with-itemizable-params
            && nqp::atkey($cur_candidate, 'item_disambiguation') {
                $candidates-with-itemizable-params := 1
            }

            # Candidate; does the arity fit? (If not, it drops out on callsite
            # shape.)
            if   $num_args >= nqp::atkey($cur_candidate, 'min_arity')
              && $num_args <= nqp::atkey($cur_candidate, 'max_arity') {

                # Arity OK; now go through the arguments and see if we can
                # eliminate any of them based on guardable properties.
                my int $type_check_count :=
                  nqp::atkey($cur_candidate, 'num_types');
                $type_check_count := $num_args
                  if $type_check_count > $num_args;

                my int $type_mismatch;
                my int $rwness_mismatch;
                my int $positional-params;
                my int $associative-params;
                my int $i;
                while $i < $type_check_count
                  && !($type_mismatch +| $rwness_mismatch) {

                    # Obtain parameter properties.
                    my $type := nqp::atpos(
                      nqp::atkey($cur_candidate, 'types'), $i
                    );
                    my int $type_flags := nqp::atpos_i(
                      nqp::atkey($cur_candidate, 'type_flags'), $i
                    );
                    my int $rwness := nqp::atpos_i(
                      nqp::atkey($cur_candidate, 'rwness'), $i
                    );

                    my int $definedness :=
                      $type_flags +& nqp::const::DEFCON_MASK;

                    # Get the primitive type of the argument, and go on
                    # whether it's an object or primitive type
                    my int $got_prim  := nqp::captureposprimspec($capture, $i);
                    my int $want_prim :=
                      $type_flags +& nqp::const::TYPE_NATIVE_MASK;

                    # It's a native
                    if $got_prim {

                        # Read/write and type object incompatible with natives
                          # want rw, we ain't got it
                        if $rwness
                          # type object
                          || $definedness == nqp::const::DEFCON_UNDEFINED {
                            $rwness_mismatch := 1;
                        }

                        # Wrong type of native is a mismatch.
                        elsif $want_prim {
                            $type_mismatch := 1
                              if (($type_flags +& nqp::const::TYPE_NATIVE_STR)
                                   && $got_prim != nqp::const::BIND_VAL_STR)
                              || (($type_flags +& nqp::const::TYPE_NATIVE_INT)
                                   && $got_prim != nqp::const::BIND_VAL_INT)
                              || (($type_flags +& nqp::const::TYPE_NATIVE_UINT)
                                   && $got_prim != nqp::const::BIND_VAL_UINT)
                              || (($type_flags +& nqp::const::TYPE_NATIVE_NUM)
                                   && $got_prim != nqp::const::BIND_VAL_NUM);
                        }

                        # Otherwise, we want an object type. Figure out the
                        # correct one that we shall box to and test that
                        else {
                            $type_mismatch := 1 unless nqp::istype(
                              nqp::atpos(@hll_type, $got_prim), $type
                            );
                        }
                    }

                    # Want a native from an opaque
                    elsif $want_prim {

                        # Mark a type guard for this argument
                        nqp::bindpos_i($need_type_guard, $i, 1);

                        # Make sure it's the expected kind of native container
                        my $contish := nqp::captureposarg($capture, $i);
                        $type_mismatch := 1
                          unless (($type_flags +& nqp::const::TYPE_NATIVE_STR)
                                   && nqp::iscont_s($contish))
                              || (($type_flags +& nqp::const::TYPE_NATIVE_INT)
                                   && nqp::iscont_i($contish))
                              || (($type_flags +& nqp::const::TYPE_NATIVE_UINT)
                                   && nqp::iscont_u($contish))
                              || (($type_flags +& nqp::const::TYPE_NATIVE_NUM)
                                   && nqp::iscont_n($contish));
                    }

                    # Got an opaque, want an opaque
                    else {
                        my int $is-mu := nqp::eqaddr($type, Mu);

                        # For a type that's exactly Mu we do not need a type
                        # guard, however if it's got a definedness constraint
                        # we do, since we might then wrongly accept a Scalar
                        # container that meets the definedness property
                        nqp::bindpos_i($need_type_guard, $i, 1)
                          if $definedness || !$is-mu;

                        # It's an object type. Obtain the value, and go by
                        # if it's a container or not.
                        my $value := nqp::captureposarg($capture, $i);
                        my int $promoted_primitive;

                        # Containerized
                        if nqp::iscont($value) && nqp::isconcrete_nd($value) {

                            # Scalar we handle specially
                            if nqp::istype_nd($value, Scalar) {
                                nqp::bindpos_i($need_scalar_read, $i, 1);

                                if $rwness {
                                    nqp::bindpos_i($need_scalar_rw_check,$i,1);

                                    # Only a real descriptor can write
                                    $rwness_mismatch := 1
                                      unless nqp::isconcrete(nqp::getattr(
                                        $value,Scalar,'$!descriptor'
                                      ));
                                }
                                $value := nqp::getattr($value,Scalar,'$!value');
                            }

                            # Otherwise, it should be a native reference.
                            # Promote these to their boxed type
                            else {
                                $value := nqp::iscont_s($value)
                                  ?? Str
                                  !! nqp::iscont_i($value)
                                       || nqp::iscont_u($value)
                                    ?? Int
                                    !! nqp::iscont_n($value)
                                      ?? Num
                                      !! nqp::die('Unknown kind of l-value in multiple dispatch');
                                $promoted_primitive := 1;
                            }
                        }

                        # Not containerized
                        else {
                            # If we need an rw argument and didn't get a
                            # container, we're out of luck. Before asserting
                            # this, we should make sure that the original
                            # container (which maybe was a Proxy that was
                            # removed in the args we're doing the dispatch
                            # over) was not itself rw
                            $rwness_mismatch := 1
                              if $rwness
                              && !nqp::iscont(
                                   nqp::captureposarg($orig-capture, $i)
                                 );
                        }

                        # Ensure the value meets the required type constraints
                        # by converting listy and hashy values to Positional
                        # and Associative HLL roles that can be checked
                        unless $is-mu || nqp::istype_nd(
                          nqp::hllizefor($value, 'Raku'), $type
                        ) {

                            # Type did not check out, but we got a listy thing
                            if nqp::eqaddr(
                                 $type,
                                 nqp::ifnull(
                                   $Positional,
                                   $Positional :=
                                     nqp::gethllsym('Raku', 'MD_Pos')
                                 )
                            ) {

                                # Things like Seq, which do the
                                # PositionalBindFailover role, can bind to
                                # an @ sigil, so then there's no mismatch
                                $type_mismatch := 1 unless nqp::istype_nd(
                                  $value,
                                  nqp::ifnull(
                                    $PositionalBindFailover,
                                    $PositionalBindFailover :=
                                      nqp::gethllsym('Raku', 'MD_PBF')
                                  )
                                );
                            }

                            # Not an listy thing, and type did not match
                            else {
                                $type_mismatch := 1;
                            }
                        }

                        # Also ensure any concreteness constraints are unheld.
                        if !$type_mismatch && $definedness {
                            my int $got := $promoted_primitive
                              || nqp::isconcrete_nd($value);
                            $type_mismatch := 1
                              if ( $got && $definedness ==
                                             nqp::const::DEFCON_UNDEFINED)
                              || (!$got && $definedness ==
                                             nqp::const::DEFCON_DEFINED  );
                            nqp::bindpos_i($need_conc_guard, $i, 1);
                        }
                    }
                    ++$i;
                }

                # Add it to the possibles list of this group.
                nqp::push(@possibles, $cur_candidate)
                  unless $type_mismatch || $rwness_mismatch;
            }
        }

        # End of tied group
        else {

            # If there's possibles...
            if nqp::elems(@possibles) {
                # Build a new list of filtered possibles by ruling out any
                # that have unaccepted of missing nameds. Track if we need
                # bind checks or if we have declarative candidates. Also
                # check for defaults and exact arity matches, which we can
                # use for tie-breaking if there are ambiguities.
                my int $need-bind-check;
                my int $first-group := nqp::isnull($current-head);
                my @filtered-possibles;
                my @defaults;
                my @exact-arity;

                my int $n := nqp::elems(@possibles);
                my int $i;
                while $i < $n {
                    my %info := @possibles[$i];
                    unless has-named-args-mismatch($capture, %info) {
                        nqp::push(@filtered-possibles, %info);
                        ++$need-bind-check
                          if nqp::existskey(%info, 'bind_check')
                          # bugs in callstatic and callmethod decont
                          # the value sent into ParamTypeCheck, meaning
                          # we can't use the VM binder for named args.
                          && !$candidates-with-itemizable-params;
                        my $sub := nqp::atkey(%info, 'sub');
                        nqp::push(@defaults, %info)
                          if nqp::can($sub, 'default')
                          && $sub.default;
                        nqp::push(@exact-arity, %info)
                          if nqp::atkey(%info,'min_arity') == $num_args
                          && nqp::atkey(%info,'max_arity') == $num_args;
                    }
                    ++$i;
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
                    elsif $candidates-with-itemizable-params
                        && 1 == nqp::elems(my @disambiguated
                            := itemized-disambiguation($capture, @filtered-possibles))
                    {
                        @filtered-possibles := @disambiguated;
                    }
                    else {
                        my $node := MultiDispatchAmbiguous.new();
                        nqp::isnull($current-head)
                          ?? ($current-head := $node)
                          !! $current-tail.set-next($node);
                        $current-tail := $node;
                    }
                }

                # Add the filtered possibles to the plan
                $i := 0;
                $n := nqp::elems(@filtered-possibles);
                while $i < $n {
                    my %info := nqp::atpos(@filtered-possibles, $i);
                    my $node;

                    if $need-bind-check {

                        # Ensure it's already compiled, otherwise we can have
                        # compiler frames obscuring the bind control record
                        # we use for trying the next candidate.
                        my $sub := nqp::atkey(%info, 'sub');
                        my $cs  := nqp::getattr($sub, Code, '@!compstuff');
                        unless nqp::isnull($cs) {
                            my $ctf := nqp::atpos($cs,1);
                            $ctf() if $ctf;
                        }
                        $node := MultiDispatchTry.new($sub);
                    }

                    # No bind check needed
                    else {
                        $node := MultiDispatchCall.new(nqp::atkey(%info,'sub'));
                    }

                    # Add the node at the right place
                    nqp::isnull($current-head)
                      ?? ($current-head := $node)
                      !! $current-tail.set-next($node);
                    $current-tail := $node;

                    ++$i;
                }

                # If we are to stop at a trivial match and nothing needs a
                # bind check, and we've no results before now, we're done.
                # Otherwise, clear the set of possibles for the next group
                $stop-at-trivial
                  && !$need-bind-check
                  && $first-group
                  && nqp::elems(@filtered-possibles) == 1
                  ?? ($done := 1)
                  !! nqp::setelems(@possibles, 0);
            }

            # reset for the next group
            $candidates-with-itemizable-params := 0;

            # If we're really at the end of the list, we're done
            $done := 1
              unless nqp::isconcrete(nqp::atpos(@candidates, $cur_idx));
        }
    }

    # Install guards as required
    $i := 0;
    while $i < $num_args {
        my $Tvalue;
        if nqp::atpos_i($need_scalar_read, $i) {
            my $Targ := nqp::track('arg', $capture, $i);

            nqp::guard('concreteness',
              nqp::track('attr', $Targ, Scalar, '$!descriptor')
            ) if nqp::atpos_i($need_scalar_rw_check, $i);

            $Tvalue := nqp::track('attr', $Targ, Scalar, '$!value');
        }
        else {
            $Tvalue := nqp::track('arg', $capture, $i);
        }
        nqp::guard('type', $Tvalue)
          if nqp::atpos_i($need_type_guard, $i);

        nqp::guard('concreteness', $Tvalue)
          if nqp::atpos_i($need_conc_guard, $i);

        ++$i;
    }

    # Return the dispatch plan, just an end marker if none so far
    if nqp::isnull($current-head) {
        MultiDispatchEnd
    }
    else {
        $current-tail.set-next(MultiDispatchEnd);
        $current-head
    }
}

# Helper sub to disambiguate candidates that may have itemization requirements
sub itemized-disambiguation($capture, $candidates) {
    my int $num-candidates := nqp::elems($candidates);

    my @capture-item-assoc := nqp::list_i;
    my @capture-item-pos   := nqp::list_i;
    my $num-capture-args   := nqp::captureposelems($capture);
    my int $x;
    while $x < $num-capture-args {
        my $arg := nqp::captureposarg($capture, $x);
        if nqp::iscont($arg) {
            if nqp::istype($arg,
                nqp::ifnull($Associative, $Associative := nqp::gethllsym('Raku', 'Associative')))
            {
                nqp::push_i(@capture-item-assoc, $x);
            } elsif nqp::istype($arg,
                nqp::ifnull($Positional, $Positional := nqp::gethllsym('Raku', 'MD_Pos')))
            {
                nqp::push_i(@capture-item-pos, $x);
            }
        }
        ++$x;
    }

    my %capture-named-item-assoc := nqp::hash;
    my %capture-named-item-pos   := nqp::hash;
    if my %nameds := nqp::capturenamedshash($capture) {
        my $iter := nqp::iterator(%nameds);
        while $iter {
            my $pair := nqp::shift($iter);
            my $arg-name := nqp::iterkey_s($pair);
            my $arg := nqp::iterval($pair);
            if nqp::iscont($arg) {
                if nqp::istype($arg,
                    nqp::ifnull($Associative, $Associative := nqp::gethllsym('Raku', 'Associative')))
                {
                    nqp::bindkey(
                        %capture-named-item-assoc,
                        $arg-name, 1
                    );
                } elsif nqp::istype($arg,
                    nqp::ifnull($Positional, $Positional := nqp::gethllsym('Raku', 'MD_Pos')))
                {
                    nqp::bindkey(
                        %capture-named-item-pos,
                        $arg-name, 1
                    );
                }
            }
         }
    }

    my int $z := 0;
    my $candidate;
    while $z < $num-candidates {
        $candidate := nqp::atpos($candidates, $z);
        my $signature := nqp::atkey($candidate, 'signature');
        my @params    := nqp::getattr($signature, Signature, '@!params');

        my %cand-named-item-assoc := nqp::hash;
        my %cand-named-item-pos   := nqp::hash;

        my @cand-item-assoc := nqp::list_i;
        my @cand-item-pos   := nqp::list_i;
        my $num-cand-params := nqp::elems(@params);
        my @cand-types      := nqp::atkey($candidate, 'types');
        my %named-types     := nqp::atkey($candidate, 'named_types');
        my int $y;
        while $y < $num-cand-params {
            my $param := nqp::atpos(@params, $y);
            my int $is-named-param := $param.named;
            my $type  := $is-named-param
                            ?? nqp::atkey(%named-types, $param.usage-name)
                            !! nqp::atpos(@cand-types, $y);
            if $param.is-item {
                if nqp::istype($type,
                    nqp::ifnull($Associative, $Associative := nqp::gethllsym('Raku', 'Associative')))
                {
                    $is-named-param
                        ?? nqp::bindkey(%cand-named-item-assoc, $param.usage-name, 1)
                        !! nqp::push_i(@cand-item-assoc, $y);
                } elsif nqp::istype($type,
                    nqp::ifnull($Positional, $Positional := nqp::gethllsym('Raku', 'MD_Pos')))
                {
                    $is-named-param
                        ?? nqp::bindkey(%cand-named-item-pos, $param.usage-name, 1)
                        !! nqp::push_i(@cand-item-pos, $y);
                }
            }
            ++$y;
        }

        # when all counts are 0, the perfect-match will be the non-itemized signature
        if     (my int $num-assoc   := nqp::elems(@capture-item-assoc))       == nqp::elems(@cand-item-assoc)
            && (my int $num-pos     := nqp::elems(@capture-item-pos))         == nqp::elems(@cand-item-pos)
            && (my int $named-assoc := nqp::elems(%capture-named-item-assoc)) == nqp::elems(%cand-named-item-assoc)
            && (my int $named-pos   := nqp::elems(%capture-named-item-pos))   == nqp::elems(%cand-named-item-pos)
        {
            my int $perfect-match := 1;
            my int $z;
            while $z < $num-assoc {
                $perfect-match := $perfect-match
                    && nqp::atpos_i(@capture-item-assoc, $z) == nqp::atpos_i(@cand-item-assoc, $z);
                ++$z;
            }

            $z := 0 if $z;
            while $perfect-match && $z < $num-pos {
                $perfect-match := $perfect-match
                    && nqp::atpos_i(@capture-item-pos, $z) == nqp::atpos_i(@cand-item-pos, $z);
                ++$z;
            }

            if $named-assoc > 0 {
                my $iter := nqp::iterator(%capture-named-item-assoc);
                while $iter {
                    $perfect-match := $perfect-match
                        && nqp::atkey(%cand-named-item-assoc, nqp::iterkey_s(nqp::shift($iter)));
                }
            }

            if $named-pos > 0 {
                my $iter := nqp::iterator(%capture-named-item-pos);
                while $iter {
                    $perfect-match := $perfect-match
                        && nqp::atkey(%cand-named-item-pos, nqp::iterkey_s(nqp::shift($iter)));
                }
            }

            # for a well constructed multi, there should only be one candidate that is a perfect match.
            return [ $candidate ] if $perfect-match;
        }
        ++$z;
    }
    []
}

# Helper sub to return a Raku Capture for the given VM capture
sub form-raku-capture($capture) {
    my $raku-capture := nqp::create(Capture);
    nqp::bindattr($raku-capture, Capture, '@!list',
      nqp::syscall('capture-pos-args', $capture)
    );
    nqp::bindattr($raku-capture, Capture, '%!hash',
      nqp::syscall('capture-named-args', $capture)
    );
    $raku-capture
}

# Helper sub to determine whether there's a Junction as one of the arguments.
# If so, guard all of the non-native arguments and return indicating a Junction
# was found.
sub multi-junction-failover($capture) {

    # Take a first pass to see if there's a Junction arg (we look at both
    # named and positional).
    my int $num-args := nqp::syscall('capture-num-args', $capture);
    my int $i;
    while $i < $num-args {
        unless nqp::syscall('capture-arg-prim-spec', $capture, $i) {
            my $value := nqp::syscall('capture-arg-value', $capture, $i);

            if nqp::isconcrete($value) && nqp::istype($value, Junction) {
                # If there is a Junction arg, then pass through all arguments
                # to put guards on all argument types if they're not natives
                $i := 0;
                while $i < $num-args {
                    unless nqp::syscall('capture-arg-prim-spec', $capture, $i) {
                        my $Targ := nqp::track('arg', $capture, $i);
                        guard-type-concreteness(nqp::istype_nd(
                          nqp::syscall('capture-arg-value', $capture, $i),
                          Scalar
                        ) ?? nqp::track('attr', $Targ, Scalar, '$!value')
                          !! $Targ
                        );
                    }
                    ++$i;
                }

                # Found a junction
                return 1;
            }
        }
        ++$i;
    }

    # Did not find a function
    0
}

# Helper sub if there is no apparent candidate for dispatch found.  Check
# for any Junction arguments, and if so, delegate to autothread.  If not,
# throw a no-match error.
sub multi-no-match-handler(
  $target,
  $dispatch-arg-capture,
  $orig-capture,
  $orig-arg-capture
) {
    # If no candidates are found but there is a Junction argument, we'll
    # dispatch to that.  Otherwise, it's just an error
    multi-junction-failover($dispatch-arg-capture)  # Guards added if needed
      ?? nqp::delegate('raku-invoke',
           nqp::syscall('dispatcher-insert-arg-literal-obj',
             nqp::syscall('dispatcher-insert-arg-literal-obj',
               $orig-capture, 0, Junction
             ), 0, Junction.HOW.find_method(Junction, 'AUTOTHREAD')
           )
         )
      !! Perl6::Metamodel::Configuration.throw_or_die(
           'X::Multi::NoMatch',
            "Cannot call " ~ $target.name() ~ "; no signatures match",
           :dispatcher($target),
           :capture(form-raku-capture($orig-arg-capture))
         )
}

# Helper sub to handle ambiguous dispatch: collect all of the ambiguous
# candidates and throw an error with them.
sub multi-ambiguous-handler($ambig-call, $target, $capture) {
    my @ambiguous;
    while nqp::istype(($ambig-call := $ambig-call.next), MultiDispatchCall) {
        nqp::push(@ambiguous, $ambig-call.candidate);
    }

    Perl6::Metamodel::Configuration.throw_or_die(
      'X::Multi::Ambiguous',
      "Ambiguous call to " ~ $target.name(),
      :dispatcher($target),
      :@ambiguous,
      :capture(form-raku-capture($capture))
    );
}

# Helper sub to filter out revision-gated multi-candidates
sub multi-filter-revision-gated-candidates($proto, $caller-revision) {
    my @candidates := $proto.dispatch_order;

    my int $idx := 0;
    my @allowed-candidates;
    while $idx < nqp::elems(@candidates) {
        my $candidate := nqp::atpos(@candidates, $idx++);
        my $required-revision := nqp::atkey($candidate, 'required_revision');
        if (! nqp::isconcrete($required-revision)) || $required-revision <= $caller-revision  {
            nqp::push(@allowed-candidates, $candidate);
        }
    }

    @allowed-candidates
}

# The actual dispatcher
nqp::register('raku-multi-core',

    # Initial dispatch. Tries to find an initial candidate.
    -> $capture {

        # Obtain the candidate list, producing it if it doesn't already exist
        my $target := nqp::captureposarg($capture, 0);

        my @candidates := nqp::bitand_i(nqp::getattr_i($target, Routine, '$!flags'), 0x08)
                                ?? (my $caller-revision := nqp::getlexcaller('$?LANGUAGE-REVISION')) && $target.REQUIRED-REVISION <= $caller-revision
                                    ?? multi-filter-revision-gated-candidates($target, $caller-revision)
                                    !! []
                                !! $target.dispatch_order;

        # Drop the first argument, to get just the arguments to dispatch on,
        # and then produce a multi-dispatch plan. Decide what to do based
        # upon it
        my $arg-capture := nqp::syscall('dispatcher-drop-arg', $capture, 0);
        my $dispatch-plan := raku-multi-plan(@candidates, $arg-capture, 1);

        # A trivial multi dispatch
        if nqp::istype($dispatch-plan, MultiDispatchCall)
          && $dispatch-plan.trivial {

            # Set dispatch state for resumption, and then delegate to
            # to the appropriate handler
            nqp::syscall('dispatcher-set-resume-init-args', $capture);
            my $candidate := $dispatch-plan.candidate;
            nqp::delegate(nqp::can($candidate, 'WRAPPERS')
              ?? 'raku-invoke-wrapped'
              !! 'raku-invoke',
              nqp::syscall('dispatcher-insert-arg-literal-obj',
                $arg-capture, 0, $candidate
              )
            );
        }

        # Proxies are involved in dispatch
        elsif nqp::istype($dispatch-plan, MultiDispatchNonScalar) {

            # Need to strip the Proxy arguments and then try again. Produce a
            # proxy reader code object to do so, insert it as the first arg,
            # and delegate to a dispatcher to manage reading the args and
            # then retrying with the outcome.
            nqp::delegate('raku-multi-remove-proxies',
              nqp::syscall('dispatcher-insert-arg-literal-obj',
                $capture, 0, $PROXY-READERS.reader-for(
                  $arg-capture, $dispatch-plan.args
                )
              )
            );
        }

        # More than a simple ambiguous dispatch
        elsif nqp::istype($dispatch-plan, MultiDispatchAmbiguous)
          && $dispatch-plan.more {
            multi-ambiguous-handler($dispatch-plan, $target, $arg-capture);
        }

        # Nothing to dispatch to
        elsif nqp::istype($dispatch-plan, MultiDispatchEnd) {
            multi-no-match-handler(
              $target, $arg-capture, $capture, $arg-capture
            );
        }

        # It's a non-trivial multi dispatch
        else {

            # Prefix the capture with the dispatch plan, and also a
            # DISP_NONE to indicate this is not a resumption of any kind.
            # Then delegate to the non-trivial multi dispatcher
            nqp::delegate('raku-multi-non-trivial',
              nqp::syscall('dispatcher-insert-arg-literal-int',
                nqp::syscall('dispatcher-insert-arg-literal-obj',
                  $capture, 0, $dispatch-plan
                ), 0, nqp::const::DISP_NONE
              )
            );
        }
    },

    # Resume of a trivial dispatch.
    -> $capture {

        # Obtain and guard on the kind of resume.
        my $Tkind := nqp::track('arg', $capture, 0);
        nqp::guard('literal', $Tkind);
        nqp::guard('literal', nqp::track('resume-state'));
        my $state := nqp::syscall('dispatcher-get-resume-state');

        # We'll delegate the hard work to the non-trivial dispatcher. We
        # only want to do that once, however, and so set the dispatch state
        # to exhausted if we've already done it. Check that's not so. Also
        # shortcut here if it's lastcall.

        my int $kind := nqp::captureposarg_i($capture, 0);
        if nqp::isnull($state) && $kind != nqp::const::DISP_LASTCALL {

            # First time. Set state to exhausted.
            nqp::syscall('dispatcher-set-resume-state-literal', Exhausted);

            # Obtain resume initialization arguments and form the plan.
            my $init-args  := nqp::syscall('dispatcher-get-resume-init-args');
            my $target     := nqp::captureposarg($init-args, 0);
            my @candidates := $target.dispatch_order;
            my $arg-capture := nqp::syscall('dispatcher-drop-arg',$init-args,0);
            my $dispatch-plan := raku-multi-plan(@candidates, $arg-capture, 0);

            # Put a guard on the dispatchees.
            my $Ttarget := nqp::track('arg', $init-args, 0);
            nqp::guard('literal',
              nqp::track('attr', $Ttarget, Routine, '@!dispatchees')
            );

            # We already called the first candidate in the trivial plan, so
            # drop it.  Then go by the kind of resumption we have
            $dispatch-plan := $dispatch-plan.next;

            # It's a callwith
            if $kind == nqp::const::DISP_CALLWITH {

                # We'll use the provided args (except for adding the invocant
                # if it's a multi method), and then delegate to the non-trivial
                # dispatch handler
                my $args := nqp::syscall('dispatcher-drop-arg', $capture, 0);
                $args := nqp::syscall('dispatcher-insert-arg',
                  $args, 0, nqp::track('arg', $arg-capture, 0)
                ) if nqp::istype($target, Method);

                nqp::delegate('raku-multi-non-trivial',
                  nqp::syscall('dispatcher-insert-arg-literal-int',
                    nqp::syscall('dispatcher-insert-arg-literal-obj',
                      nqp::syscall('dispatcher-insert-arg',
                        $args, 0, $Ttarget
                      ), 0, $dispatch-plan
                    ), 0, nqp::const::DISP_CALLWITH
                  )
                );
            }

            # We're propagating a callwith
            elsif $kind == nqp::const::DISP_PROPAGATE_CALLWITH {

                # Insert our multi dispatch plan and target after the kind
                # and then pass it along to the non-trivial dispatcher
                nqp::delegate('raku-multi-non-trivial',
                  nqp::syscall('dispatcher-insert-arg-literal-obj',
                    nqp::syscall('dispatcher-insert-arg',
                      $capture, 1, $Ttarget
                    ), 1, $dispatch-plan
                  )
                );
            }

            # Definitely non-trivial
            else {

                # Delegate to the non-trivial dispatcher, passing along the
                # kind of dispatch we're doing and the plan
                nqp::delegate('raku-multi-non-trivial',
                  nqp::syscall('dispatcher-insert-arg',
                    nqp::syscall('dispatcher-insert-arg-literal-obj',
                      $init-args, 0, $dispatch-plan
                    ), 0, $Tkind
                  )
                );
            }
        }
        else {
            # Mark dispatcher exhausted if it's lastcall
            nqp::syscall('dispatcher-set-resume-state-literal', Exhausted)
              if $kind == nqp::const::DISP_LASTCALL;

            # Resume next dispatcher, if any, otherwise hand back Nil
            nil-or-callwith-propagation-terminal($capture);
        }
    }
);

# The non-trivial multi dispatch has quite similar initial and resume steps,
# and thus the majority of the work is factored out into a subroutine.
nqp::register('raku-multi-non-trivial',
    # Initialization of a non-trivial dispatch. Receives a dispatch resumption
    # kind, which is zero if we're not resuming.
    -> $capture {
        # Extract and guard on the kind (first argument).
        my $track-kind := nqp::track('arg', $capture, 0);
        nqp::guard('literal', $track-kind);
        my int $kind := nqp::captureposarg_i($capture, 0);

        # If it's propagating callwith arguments, then we don't want to invoke
        # any multi candidate, but rather set up resume init args and then run
        # the thing propagating args to us.
        if $kind == nqp::const::DISP_PROPAGATE_CALLWITH {
            # The resume init capture should have plan, original target, args.
            # We have kind, plan, original target, 3 values relating to the
            # callwith propagation, and then the args.
            my $without-prop-info := nqp::syscall('dispatcher-drop-n-args',
                $capture, 3, 3);
            my $init-args := nqp::syscall('dispatcher-drop-arg',
                $without-prop-info, 0);
            nqp::syscall('dispatcher-set-resume-init-args', $init-args);

            # Drop the plan and original target to get the callwith envelope,
            # and then propagate further or invoke.
            my $callwith-prop := nqp::syscall('dispatcher-drop-n-args',
                $capture, 1, 2);
            nil-or-callwith-propagation-terminal($callwith-prop);
        }

        # Otherwise, we probably want to run a candidate.
        else {
            # Extract and track the current state.
            my $track-cur-state := nqp::track('arg',
                $capture, 1);
            my $cur-state := nqp::captureposarg($capture, 1);

            # Drop the leading two arguments to get the argument capture prefixed
            # with the original dispatch target, and one more to get the argument
            # capture.
            my $orig-capture := nqp::syscall('dispatcher-drop-n-args',
                $capture, 0, 2);
            my $arg-capture := nqp::syscall('dispatcher-drop-arg',
                $orig-capture, 0);

            # Perform the step.
            raku-multi-non-trivial-step($kind, $track-cur-state, $cur-state, $orig-capture,
                $arg-capture, 0);
        }
    },
    -> $capture {
        # Extract and guard on the kind (first argument).
        my $track-kind := nqp::track('arg', $capture, 0);
        nqp::guard('literal', $track-kind);
        my int $kind := nqp::captureposarg_i($capture, 0);

        # Obtain and track state.
        my $track-state := nqp::track('resume-state');
        my $state := nqp::syscall('dispatcher-get-resume-state');

        # If it's a lastcall, we'll poke an Exhausted into the state,
        # propagate it, and produce Nil.
        if $kind == nqp::const::DISP_LASTCALL {
            nqp::syscall('dispatcher-set-resume-state-literal', Exhausted);
            delegate-constant-Nil($capture)
              unless nqp::syscall('dispatcher-next-resumption');
        }

        # If we're already exhausted, try for the next dispatcher.
        elsif nqp::istype($state, Exhausted) {
            nqp::guard('type', $track-state);
            nil-or-callwith-propagation-terminal($capture);
        }

        # If it's a callwith, then we want to re-enter the non-trivial
        # multi dispatcher with the current position in the dispatch
        # and the new args.
        elsif $kind == nqp::const::DISP_CALLWITH {
            my $args := nqp::syscall('dispatcher-drop-arg', $capture, 0);
            my $init := nqp::syscall('dispatcher-get-resume-init-args');
            my $target := nqp::captureposarg($init, 1);
            if nqp::istype($target, Method) {
                my $track-invocant := nqp::track('arg',
                  $init, 2);
                $args := nqp::syscall('dispatcher-insert-arg',
                  $args, 0, $track-invocant);
            }
            my $track-cur-state;
            if nqp::isnull($state) {
                nqp::guard('literal', $track-state);
                $track-cur-state := nqp::track('arg', $init, 0);
            }
            else {
                nqp::guard('type', $track-state);
                $track-cur-state := $track-state;
            }
            nqp::syscall('dispatcher-set-resume-state-literal', Exhausted);
            my $track-target := nqp::track('arg',
              $init, 1);
            my $with-target := nqp::syscall('dispatcher-insert-arg',
              $args, 0, $track-target);
            my $capture-with-plan := nqp::syscall('dispatcher-insert-arg',
              $with-target, 0, $track-cur-state);
            my $capture-delegate := nqp::syscall(
              'dispatcher-insert-arg-literal-int', $capture-with-plan,
              0, nqp::const::DISP_CALLWITH);
            nqp::delegate('raku-multi-non-trivial', $capture-delegate);
        }

        # Otherwise, it's a "normal" step through the candidate list.
        else {
            # Drop the leading argument to get the argument capture prefixed
            # with the original target, one more to get the arguments.
            my $init := nqp::syscall('dispatcher-get-resume-init-args');
            my $orig-capture := nqp::syscall('dispatcher-drop-arg',
                $init, 0);
            my $arg-capture := nqp::syscall('dispatcher-drop-arg',
                $orig-capture, 0);

            # Have dispatch state already, or first resume?
            if nqp::isnull($state) {
                # First resumption. Guard that it is so.
                nqp::guard('literal', $track-state);

                # Obtain plan and args from init state.
                my $track-cur-state := nqp::track('arg', $init, 0);
                my $cur-state := nqp::captureposarg($init, 0);
                raku-multi-non-trivial-step(
                  $kind, $track-cur-state, $cur-state, $orig-capture,
                  $arg-capture, 1);
            }
            else {
                raku-multi-non-trivial-step($kind, $track-state, $state, $orig-capture,
                    $arg-capture, 1);
            }
        }
    });
sub raku-multi-non-trivial-step(int $kind, $track-cur-state, $cur-state, $orig-capture,
        $arg-capture, $is-resume) {
    if nqp::istype($cur-state, MultiDispatchCall) {
        # Guard on the current state and on the callee (the type guards are
        # implicitly established when we guard the callee).
        my $track-candidate := nqp::track('attr',
            $track-cur-state, MultiDispatchCall, '$!candidate');
        nqp::guard('literal', $track-candidate);

        # If it's a bind failure or success, the we were doing nextcallee
        # on a dispatch needing a bind check, and want to hand back the
        # candidate or keep walking.
        if $kind == nqp::const::DISP_BIND_SUCCESS {
            # Successful. Peel off the candidate and hand it back.
            peel-off-candidate($is-resume, $track-cur-state, $cur-state, $orig-capture);
            delegate-value($arg-capture, $track-candidate);
            return;
        }
        elsif $kind == nqp::const::DISP_BIND_FAILURE {
            # Unsuccessful. Skip over the candidate we already tried, and then
            # try again.
            $track-cur-state := nqp::track('attr',
                $track-cur-state, MultiDispatchCall, '$!next');
            $cur-state := $cur-state.next;
            return raku-multi-non-trivial-step(nqp::const::DISP_NEXTCALLEE,
                $track-cur-state, $cur-state, $orig-capture, $arg-capture, $is-resume);
        }

        # If it needs a bind check, then...
        my int $peel-off-candidate := 1;
        my int $try := nqp::istype($cur-state, MultiDispatchTry);
        if $try {
            # If it's nextcallee, we need to resume either on success or failure,
            # and hand back the candidate. This is the only case where we want to
            # not peel off a candidate now.
            if $kind == nqp::const::DISP_NEXTCALLEE {
                my int $failure := nqp::const::DISP_BIND_FAILURE;
                my int $success := nqp::const::DISP_BIND_SUCCESS;
                nqp::syscall('dispatcher-resume-after-bind',
                    $failure, $success);
                $peel-off-candidate := 0;
            }
            # Otherwise, we just want to call it and resume on bind failure.
            else {
                nqp::syscall('dispatcher-resume-on-bind-failure', $kind);
            }
        }

        # Peel off one candidate and use that as the next state, unless we
        # aren't meant to.
        if $peel-off-candidate {
            peel-off-candidate($is-resume, $track-cur-state, $cur-state, $orig-capture);
        }

        # Set up the call. If we are doing a callwith then we need to propagate
        # the args also, otherwise we just invoke it right off.
        my $capture-delegate := nqp::syscall('dispatcher-insert-arg',
            $arg-capture, 0, $track-candidate);
        my $candidate := $cur-state.candidate;
        my str $disp := $try || $kind != nqp::const::DISP_NEXTCALLEE
            ?? (nqp::can($candidate, 'WRAPPERS') ?? 'raku-invoke-wrapped' !! 'raku-invoke')
            !! 'boot-value';
        if $kind == nqp::const::DISP_CALLWITH {
            my $with-inv-flag := nqp::syscall('dispatcher-insert-arg-literal-int',
                $capture-delegate, 0, 1);
            my $with-disp-name := nqp::syscall('dispatcher-insert-arg-literal-str',
                $with-inv-flag, 0, $disp);
            my $with-propagate := nqp::syscall('dispatcher-insert-arg-literal-int',
                $with-disp-name, 0, nqp::const::DISP_PROPAGATE_CALLWITH);
            nil-or-callwith-propagation-terminal($with-propagate);
        }
        else {
            nqp::delegate($disp, $capture-delegate);
        }
    }
    elsif nqp::istype($cur-state, MultiDispatchEnd) || nqp::istype($cur-state, Exhausted) {
        # If this is the initial dispatch, then error, otherwise try the next
        # resumption (possibly method dispatch), and finally give up.
        if $kind == nqp::const::DISP_NONE {
            my $target := nqp::captureposarg($orig-capture, 0);
            multi-no-match-handler($target, $arg-capture, $orig-capture, $arg-capture);
        }
        else {
            nqp::guard('type', $track-cur-state);
            delegate-constant-Nil($arg-capture)
              unless nqp::syscall('dispatcher-next-resumption');
        }
    }
    elsif nqp::istype($cur-state, MultiDispatchAmbiguous) {
        # If this is the initial dispatch, then error.
        if $kind == nqp::const::DISP_NONE {
            my $target := nqp::captureposarg($orig-capture, 0);
            multi-ambiguous-handler($cur-state, $target, $arg-capture);
        }
        # Otherwise, step past it and do whatever comes next.
        else {
            nqp::guard('type', $track-cur-state);
            $track-cur-state := nqp::track('attr',
              $track-cur-state, MultiDispatchAmbiguous, '$!next');
            $cur-state := $cur-state.next;
            return raku-multi-non-trivial-step(
              $kind, $track-cur-state, $cur-state,
              $orig-capture, $arg-capture, $is-resume);
        }
    }
    else {
        nqp::die('Unexpected multi dispatch step ' ~ $cur-state.HOW.name($cur-state));
    }
}
sub peel-off-candidate($is-resume, $track-cur-state, $cur-state, $orig-capture) {
    if $is-resume {
        my $track-next := nqp::track('attr',
            $track-cur-state, MultiDispatchCall, '$!next');
        nqp::syscall('dispatcher-set-resume-state', $track-next);
    }
    else {
        my $next := $cur-state.next;
        my $init-state := nqp::syscall('dispatcher-insert-arg-literal-obj',
            $orig-capture, 0, $next);
        nqp::syscall('dispatcher-set-resume-init-args', $init-state);
    }
}

# Proxy removal for multiple dispatch.
nqp::register('raku-multi-remove-proxies',
    # The dispatch receives (remover, original invokee, args...).
    -> $capture {
        # The resume init state drops the remover.
        nqp::syscall('dispatcher-set-resume-init-args',
            nqp::syscall('dispatcher-drop-arg', $capture, 0));

        # We then invoke the remover with the arguments (so need to drop the
        # original invokee).
        nqp::delegate('boot-code-constant',
            nqp::syscall('dispatcher-drop-arg', $capture, 1));
    },
    # The resumption is done with the args with proxies stripped.
    -> $capture {
        # Make sure this really is the resume with the proxies stripped,
        # not some inner resume, which we should just pass along.
        my $track_kind := nqp::track('arg', $capture, 0);
        nqp::guard('literal', $track_kind);
        my int $kind := nqp::captureposarg_i($capture, 0);
        if $kind == nqp::const::DISP_DECONT {
            # Yes, it's the resume we're looking for. Locate the candidates by
            # using the resume init args.
            my $orig-capture := nqp::syscall('dispatcher-get-resume-init-args');
            my $target     := nqp::captureposarg($orig-capture, 0);
            my @candidates := $target.dispatch_order;

            # Put a guard on the dispatchees. (TODO This risks the callsite in
            # the generated removers becoming a polymorphic blow-up point; when
            # we can associate it with the dispatch program of the initial
            # dispatch, that will be rather better.)
            my $track_callee := nqp::track('arg',
                $orig-capture, 0);
            nqp::guard('literal',
              nqp::track('attr', $track_callee, Routine, '@!dispatchees'));

            # We now make the dispatch plan using the arguments with proxies
            # removed, put pass along the original arg capture too, for use
            # in `rw`-ness testing.
            my $no-proxy-arg-capture := nqp::syscall('dispatcher-drop-arg',
                $capture, 0);
            my $orig-arg-capture := nqp::syscall('dispatcher-drop-arg',
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
                my $capture-with-plan := nqp::syscall(
                  'dispatcher-insert-arg-literal-obj', $orig-capture, 0,
                  $dispatch-plan);
                my $capture-delegate := nqp::syscall(
                  'dispatcher-insert-arg-literal-int', $capture-with-plan, 0, 0);
                nqp::delegate('raku-multi-non-trivial', $capture-delegate);
            }
        }
        elsif !nqp::syscall('dispatcher-next-resumption') {
            delegate-constant-Nil($capture);
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
    nqp::dispatch('raku-coercion', $coercion-type, $val);
}
my $listy-coercion := -> $coercion-type, *@args {
    my $list := nqp::create(List);
    nqp::bindattr($list, List, '$!reified', @args);
    nqp::dispatch('raku-coercion', $coercion-type, $list);
}
nqp::register('raku-invoke', -> $capture {
    # Guard type and concreteness of code object, unless it is a literal one
    # (and also determine if it's a literal, as we can avoid some things in
    # that case).
    my $code := nqp::captureposarg($capture, 0);
    my int $code-constant := nqp::syscall('dispatcher-is-arg-literal',
        $capture, 0);
    my $code_arg := nqp::track('arg', $capture, 0);
    guard-type-concreteness($code_arg) unless $code-constant;

    # If it's already a VM-level code reference, just invoke it.
    if nqp::reprname($code) eq 'MVMCode' {
        nqp::syscall('dispatcher-delegate',
            $code-constant ?? 'boot-code-constant' !! 'boot-code',
            $capture);
    }

    elsif nqp::isconcrete(
        my $custom-dispatcher := nqp::decont(nqp::how_nd($code).find_method(nqp::decont($code), 'CUSTOM-DISPATCHER', :no_fallback))
    ) {
        nqp::syscall('dispatcher-delegate',
            nqp::unbox_s($custom-dispatcher($code)), $capture);
    }

    # If it has a CALL-ME method then always use that (this means it being a
    # Code object, even).
    elsif nqp::isconcrete(my $call-me := nqp::decont(nqp::how_nd($code).find_method($code, 'CALL-ME', :no_fallback))) {
        # A CALL-ME method is found; make a call to it. We use raku-call-simple
        # to avoid setting up any further deferrals, which may get us into the
        # situation where we set up multiple resumptions.
        my $delegate := nqp::syscall('dispatcher-insert-arg-literal-obj',
            $capture, 0, $call-me);
        nqp::delegate('raku-call-simple',
            $delegate);
    }

    # If it's a code object...
    elsif nqp::istype($code, Code) {
        # Concrete code object: extract the $!do, replace the code object,
        # and delegate to boot-code.
        if nqp::isconcrete($code) {

            # Check for a required revision
            if nqp::istype($code, Routine) && nqp::bitand_i(nqp::getattr_i($code, Routine, '$!flags'), 0x08)
            && nqp::getlexcaller('$?LANGUAGE-REVISION') < $code.REQUIRED-REVISION {
                my str $name := "<METHOD>";
                if nqp::can($code, 'name') {
                    $name := $code.name;
                }
                nqp::die("Cannot dispatch to method '" ~ $name ~ "' because it requires a higher language revision than available in the caller's compunit");
            }

            my $do := nqp::getattr($code, Code, '$!do');
            if $code-constant && !nqp::syscall('code-is-stub', $do) {
                my $args := pass-decontainerized($code,
                    nqp::syscall('dispatcher-drop-arg', $capture, 0));
                my $delegate_capture := nqp::syscall(
                  'dispatcher-insert-arg-literal-obj', $args, 0, $do);
                nqp::delegate('boot-code-constant', $delegate_capture);
            }
            else {
                my $do_attr := nqp::track('attr',
                    $code_arg, Code, '$!do');
                my $delegate_capture := nqp::syscall('dispatcher-insert-arg',
                    nqp::syscall('dispatcher-drop-arg', $capture, 0),
                    0, $do_attr);
                nqp::delegate('boot-code', $delegate_capture);
            }
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
            my $do_attr := nqp::track('attr',
                $code_arg, ForeignCode, '$!do');
            my $delegate_capture := nqp::syscall('dispatcher-insert-arg',
                nqp::syscall('dispatcher-drop-arg', $capture, 0),
                0, $do_attr);
            nqp::delegate('lang-call',
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
            if $code-constant {
                my $do := nqp::getattr($code, NQPRoutine, '$!do');
                my $delegate_capture := nqp::syscall(
                  'dispatcher-insert-arg-literal-obj',
                  nqp::syscall('dispatcher-drop-arg', $capture, 0),
                  0, $do);
                nqp::delegate('boot-code-constant', $delegate_capture);
            }
            else {
                my $do_attr := nqp::track('attr',
                  $code_arg, NQPRoutine, '$!do');
                my $delegate_capture := nqp::syscall('dispatcher-insert-arg',
                  nqp::syscall('dispatcher-drop-arg', $capture, 0),
                  0, $do_attr);
                nqp::delegate('boot-code', $delegate_capture);
            }
        }
        else {
            nqp::die('Cannot invoke a ' ~ nqp::how_nd($code).name($code) ~ ' type object');
        }
    }

    # Otherwise, try the coercion path.
    elsif !nqp::isconcrete($code) && nqp::captureposelems($capture) >= 2 {
        # Looks like a coercion. In the best case we just have one argument
        # and things will be straightforward. Failing that, we'll have to
        # form a list and take the slow-bound path.
        if nqp::captureposelems($capture) == 2 {
            # Work out what we have to coerce.
            my $arg-type;
            my int $could-not-guard;
            my int $prim := nqp::captureposprimspec($capture, 1);
            if $prim == nqp::const::BIND_VAL_INT
              || $prim == nqp::const::BIND_VAL_UINT {
                $arg-type := Int
            }
            elsif $prim == nqp::const::BIND_VAL_NUM {
                $arg-type := Num
            }
            elsif $prim == nqp::const::BIND_VAL_STR {
                $arg-type := Str
            }
            else {
                # Object argument, so type guard.
                my $arg := nqp::captureposarg($capture, 1);
                $arg-type := $arg.WHAT;
                my $track-arg := nqp::track('arg', $capture, 1);
                nqp::guard('type', $track-arg);
                if nqp::isconcrete_nd($arg) && nqp::iscont($arg) {
                    # Containerized. If it's a Scalar, we can deref and guard
                    # on that. If not, we'll have to thunk it and figure it
                    # out each time.
                    if nqp::istype_nd($arg, Scalar) {
                        nqp::guard('type',
                          nqp::track('attr', $track-arg, Scalar, '$!value'));
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
                    "Cannot coerce to " ~ nqp::how_nd($code).name($code) ~ " with named arguments",
                    :target-type($code.WHAT),
                    :from-type($arg-type), :hint("named arguments passed")
                );
            }

            # If we could not guard need to delegate to a late-bound
            # handler.
            if $could-not-guard {
                my $delegate := nqp::syscall(
                  'dispatcher-insert-arg-literal-obj', $capture,
                  0, $late-coerce);
                nqp::delegate('boot-code-constant', $delegate);
            }

            # Otherwise, can rewrite the callsite directly to do the
            # coercion.
            else {
                # Form the coercion type.
                my $how := nqp::how_nd($code);
                my $coercion-type := Perl6::Metamodel::CoercionHOW.new_type(
                    (nqp::istype($how, Perl6::Metamodel::ClassHOW)
                       && $how.is_pun($code)
                       ?? $how.pun_source($code)
                       !! $code.WHAT
                    ),
                    $arg-type);

                # Call $coercion-type.HOW.coerce($coercion-type, $val). We
                # know that there was only one item, so we can drop the
                # callee, prepend the coercion type, the HOW, and then the
                # name and type as raku-meth-call wants.
                my $coercee-only := nqp::syscall('dispatcher-drop-arg',
                    $capture, 0);
                my $with-coercion-type := nqp::syscall(
                  'dispatcher-insert-arg-literal-obj',
                  $coercee-only, 0, $coercion-type);
                my $coerce-how := nqp::how_nd($coercion-type);
                my $with-how := nqp::syscall(
                  'dispatcher-insert-arg-literal-obj',
                  $with-coercion-type, 0, $coerce-how);
                my $with-name := nqp::syscall(
                  'dispatcher-insert-arg-literal-str', $with-how, 0, 'coerce');
                my $delegate := nqp::syscall(
                  'dispatcher-insert-arg-literal-obj',
                  $with-name, 0, $coerce-how);
                nqp::delegate('raku-meth-call', $delegate);
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
                    "Cannot coerce to " ~ nqp::how_nd($code).name($code) ~ " with named arguments",
                    :target-type($code.WHAT),
                    :from-type(List), :hint("named arguments passed")
                );
            }
            my $how := nqp::how_nd($code);
            my $coercion-type := Perl6::Metamodel::CoercionHOW.new_type(
                (nqp::istype($how, Perl6::Metamodel::ClassHOW) && $how.is_pun($code)
                    ?? $how.pun_source($code)
                    !! $code.WHAT),
                List);
            my $list-elems-only := nqp::syscall('dispatcher-drop-arg',
              $capture, 0);
            my $with-coercion-type := nqp::syscall(
              'dispatcher-insert-arg-literal-obj',
              $list-elems-only, 0, $coercion-type);
            my $delegate := nqp::syscall(
              'dispatcher-insert-arg-literal-obj',
              $with-coercion-type, 0, $listy-coercion);
            nqp::delegate('boot-code-constant', $delegate);
        }
    }

    # We can't invoke it; complain.
    else {
        my $how := nqp::how_nd($code);
        my $class := nqp::getlexcaller('$?CLASS');
        report-method-not-found($code, 'CALL-ME', $class, $how, nqp::iscont($code));
    }
});

# When we see that the callee expects readonly arguments, and what we are
# going to pass is a Scalar container, we can unwrap them on the caller
# side. This has a number of advantages:
# * Since in a multiple dispatch we have to decontainerize in order to guard
#   on the values, this dispatch program will often already have the
#   values in temporaries. Thus we would be doing the work anyway, and don't
#   need to leave it to the callee.
# * The specializer may do specialization linking and inlining, and may stack
#   up extra guards, which include reading from containers. This would duplicate
#   work in the dispatch program. However, if invocation at the end of the
#   dispatch program is just on values, it can avoid that.
# * Since most calls to an operator like infix:<+> will now be two values, not
#   all permutations of scalar with value and value, we may produce less
#   specializations. Further, value + value case has by far the simplest code
#   to inline (and typically avoids further guards and container reads).
# * Even in the case we can't inline, removing the value from the container
#   caller side rather than callee side means that the container doesn't
#   escape, which - especially as PEA in MoarVM gets more powerful - means it
#   will be able to do a great deal more scalar replacements.
sub pass-decontainerized($code, $args) {
    my $signature := nqp::getattr($code, Code, '$!signature');
    my int $readonly := nqp::getattr_i($signature, Signature, '$!readonly');
    my int $pos-args := nqp::captureposelems($args);
    my int $i;
    while $i < $pos-args {
        # If it should be passed read only, and it's an object...
        if $readonly +& nqp::bitshiftl_i(1, $i) && nqp::captureposprimspec($args, $i) == 0 {
            # If it's in a Scalar container...
            my $arg := nqp::captureposarg($args, $i);
            if nqp::isconcrete_nd($arg) && nqp::what_nd($arg) =:= Scalar {
                # Read it from the container and pass it decontainerized.
                my $track-arg := nqp::track('arg', $args, $i);
                my $track-value := nqp::track('attr',
                    $track-arg, Scalar, '$!value');
               $args := nqp::syscall('dispatcher-insert-arg',
                    nqp::syscall('dispatcher-drop-arg', $args, $i),
                    $i, $track-value);
            }
        }
        ++$i;
    }
    $args
}

# Entrypoint for dispatch to a wrapped routine. Builds the chain and delegates
# to another dispatcher that will handle the walking through it via resumption.
nqp::register('raku-invoke-wrapped', -> $capture {
    # Guard on the current set of wrappers (the array of them is immutable,
    # so we can rely on its identity).
    my $routine := nqp::captureposarg($capture, 0);
    my $wrapper-type := $routine.WRAPPER-TYPE;
    my $track-routine := nqp::track('arg', $capture, 0);
    my $track-wrappers := nqp::track('attr',
            $track-routine, $wrapper-type, '$!wrappers');
    nqp::guard('literal', $track-wrappers);

    # With wrappers, we pretty much know we'll be traversing them, so we
    # build the deferral chain up front, unlike in other dispatchers.
    my @all-callees := nqp::clone($routine.WRAPPERS);
    my $chain := Exhausted;
    while nqp::elems(@all-callees) {
        $chain := DeferralChain.new(nqp::pop(@all-callees), $chain);
    }

    # Delegate to the wrap deferral dispatcher with the arguments to call
    # the initial wrapper with, preceded by the calculated chain.
    my $without-routine := nqp::syscall('dispatcher-drop-arg', $capture, 0);
    my $with-chain := nqp::syscall('dispatcher-insert-arg-literal-obj',
        $without-routine, 0, $chain);
    my $with-kind := nqp::syscall('dispatcher-insert-arg-literal-int',
        $with-chain, 0, nqp::const::DISP_NONE);
    nqp::delegate('raku-wrapper-deferral', $with-kind);
});

# The wrapper deferral dispatcher that moves through wrappers.
nqp::register('raku-wrapper-deferral',
    # Initial dispatch, called with the chain to walk through along with the
    # arguments. This is used either in the case we are just starting to walk
    # through the dispatchers or in the event of a callwith.
    -> $capture {
        # Obtain and guard on the first wrapper callee.
        my $cur_deferral := nqp::captureposarg($capture, 1);
        my $track_cur_deferral := nqp::track('arg',
            $capture, 1);
        my $track_code :=
          nqp::track('attr', $track_cur_deferral, DeferralChain, '$!code');
        nqp::guard('literal', $track_code);

        # Extract the arguments and set the resume init args to be the next item
        # in the chain prepended to the arguments, so long as there is a next
        # wrapper.
        my $next := $cur_deferral.next;
        my $args := nqp::syscall('dispatcher-drop-n-args', $capture, 0, 2);
        if nqp::isconcrete($next) {
            nqp::syscall('dispatcher-set-resume-init-args',
                nqp::syscall('dispatcher-insert-arg-literal-obj',
                    $args, 0, $next));
        }

        # Either invoke the wrapper directly, or via callwith propagation.
        my $code := $cur_deferral.code;
        my $code-capture := nqp::syscall('dispatcher-insert-arg-literal-obj',
            $args, 0, $code);
        my int $callwith := nqp::captureposarg_i($capture, 0) == nqp::const::DISP_PROPAGATE_CALLWITH;
        if $callwith {
            my $with-inv-flag := nqp::syscall('dispatcher-insert-arg-literal-int',
                $code-capture, 0, 1);
            my $with-disp := nqp::syscall('dispatcher-insert-arg-literal-str',
                $with-inv-flag, 0, 'raku-call-simple');
            my $with-kind := nqp::syscall('dispatcher-insert-arg-literal-int',
                $with-disp, 0, nqp::const::DISP_PROPAGATE_CALLWITH);
            unless nqp::syscall('dispatcher-next-resumption', $with-kind) {
                nqp::delegate('raku-call-simple',
                    $code-capture);
            }
        }
        else {
            nqp::delegate('raku-call-simple', $code-capture);
        }
    },
    # Resumption.
    -> $capture {
        # Guard on the kind of resume we're doing, and get that flag.
        my $track_kind := nqp::track('arg', $capture, 0);
        nqp::guard('literal', $track_kind);
        my int $kind := nqp::captureposarg_i($capture, 0);

        # Work out which wrapper we'll call next.
        my $init := nqp::syscall('dispatcher-get-resume-init-args');
        my $state := nqp::syscall('dispatcher-get-resume-state');
        my $track_cur_deferral;
        my $cur_deferral;
        if $kind == nqp::const::DISP_LASTCALL {
            # It's lastcall; just update the state to Exhausted.
            nqp::syscall('dispatcher-set-resume-state-literal', Exhausted);
        }
        elsif $kind == nqp::const::DISP_ONLYSTAR {
            nqp::syscall('dispatcher-next-resumption') ||
                nqp::die('Failed to dispatch to candidate from wrapped proto')
        }
        elsif nqp::isnull($state) {
            # No state, so the initial resumption. Guard on there being no
            # dispatch state.
            my $track_state := nqp::track('resume-state');
            nqp::guard('literal', $track_state);

            # The current deferral is obtained from the initialization state.
            $track_cur_deferral := nqp::track('arg', $init, 0);
            $cur_deferral := nqp::captureposarg($init, 0);
        }
        elsif !nqp::istype($state, Exhausted) {
            # Already working through a chain of wrappers deferrals. Thus the
            # current deferral is the current state;
            $track_cur_deferral := nqp::track('resume-state');
            $cur_deferral := $state;
        }
        else {
            # Dispatch already exhausted; guard on that and fall through to returning
            # Nil.
            my $track_state := nqp::track('resume-state');
            nqp::guard('literal', $track_state);
        }

        # If we have a current deferral...
        if $cur_deferral {
            if $kind == nqp::const::DISP_CALLWITH {
                # Mark this dispatcher exhausted since we're moving on from it,
                # and then re-enter the dispatcher with the remaining wrappers
                # and the args given to us. We guard on the next candidate and
                # insert it as a literal, since we may not depend on resume
                # init state if using next resumption with args.
                nqp::syscall('dispatcher-set-resume-state-literal',
                    Exhausted);
                my $args := nqp::syscall('dispatcher-drop-arg', $capture, 0);
                my $with-chain := nqp::syscall(
                  'dispatcher-insert-arg-literal-obj', $args, 0, $cur_deferral);
                my $with-kind := nqp::syscall(
                  'dispatcher-insert-arg-literal-int',
                  $with-chain, 0, nqp::const::DISP_PROPAGATE_CALLWITH);
                nqp::delegate('raku-wrapper-deferral', $with-kind);
            }
            else {
                # Not callwith, so we keep walking the list. Update state to
                # move to the next wrapper in the list and then put into effect
                # the resumption.
                my $track_code := nqp::track('attr',
                  $track_cur_deferral, DeferralChain, '$!code');
                nqp::guard('literal', $track_code);
                my $track_next := nqp::track('attr',
                  $track_cur_deferral, DeferralChain, '$!next');
                nqp::syscall('dispatcher-set-resume-state', $track_next);
                if $kind == nqp::const::DISP_CALLSAME {
                    # Invoke the next bit of code. We send the original dispatchee to
                    # boot-code, since it's an unwrapped code handle; the rest, we
                    # treat as Raku calls, since it's possible somebody decided to wrap
                    # some code up with a multi.
                    my $code := $cur_deferral.code;
                    my $delegate_capture := nqp::syscall('dispatcher-insert-arg-literal-obj',
                        nqp::syscall('dispatcher-drop-arg', $init, 0),
                        0, $code);
                    nqp::delegate('raku-call-simple',
                        $delegate_capture);
                }
                elsif $kind == nqp::const::DISP_NEXTCALLEE {
                    # We just want the code itself, not to invoke it.
                    delegate-constant($capture, $cur_deferral.code);
                }
                else {
                    nqp::die('Unimplemented resumption kind in wrap dispatch');
                }
            }
        }
        else {
            # This dispatcher is exhausted. However, there may be another one
            # we can try (for example, in a wrapped method).
            nil-or-callwith-propagation-terminal($capture);
        }
    });

#- raku-call-simple ------------------------------------------------------------
# Like raku-call, except assumes that any method call we see will already have
# been taken care of.
nqp::register('raku-call-simple', -> $capture {
    my $callee  := nqp::captureposarg($capture, 0);
    my $Tcallee := nqp::track('arg', $capture, 0);
    nqp::guard('type', $Tcallee);
    my str $delegate := 'raku-invoke';

    if nqp::istype_nd($callee, Routine) {
        nqp::guard('literal',
          nqp::track('attr', $Tcallee, Routine, '@!dispatchees')
        );
        $delegate := 'raku-multi' if $callee.is_dispatcher;
    }

    nqp::delegate($delegate, $capture);
});

#- raku-find-meth --------------------------------------------------------------
# Dispatcher to try to find a method, backing nqp::findmethod,
# nqp::tryfindmethod, and nqp::can.  Expects the invocant and
# method name as arguments, and an integer indicating whether
# to throw an exception if not found
nqp::register('raku-find-meth', -> $capture {

    # See if this callsite is going megamorphic and do a fallback if so.
    # We only do this in the non-exceptional case.
    my $obj := nqp::captureposarg($capture, 0);
    my $how := nqp::how_nd($obj);
    my int $throw    := nqp::captureposarg_i($capture, 2);

    # Looks like megamorphic
    if nqp::syscall('dispatcher-inline-cache-size') >= $MEGA-METH-CALLSITE-SIZE
      && !$throw
      && nqp::istype($how, Perl6::Metamodel::ClassHOW) {
        nqp::delegate('raku-find-meth-mega', $capture);
    }

    # Not megamorphic
    else {

        # Guard on the invocant type and method name.
        nqp::guard('type',    nqp::track('arg', $capture, 0));
        nqp::guard('literal', nqp::track('arg', $capture, 1));

        # Try to find the method.
        my $meth := $how.find_method($obj, nqp::captureposarg_s($capture, 1));

        # If it's found, set to evaluate to it.
        if nqp::isconcrete($meth) {
            delegate-constant($capture, $meth);
        }

        # Otherwise, depends on exceptional flag whether we report the
        # missing method or hand back a null (trymethod/can).
        else {
            nqp::guard('literal', nqp::track('arg', $capture, 2));
            $throw
              ?? nqp::delegate('lang-meth-not-found',
                   nqp::syscall('dispatcher-drop-arg', $capture, 0)
                 )
              !! delegate-constant($capture, nqp::null);
        }
    }
});

#- raku-find-meth-mega  --------------------------------------------------------
# Dispatcher to try to find a method in a mega-morphic situation.  Expects
# the same arguments as 'raku-find-meth'.
nqp::register('raku-find-meth-mega', -> $capture {

    # Always guard on the throw mode (which should always be false,
    # since we don't handle it here).
    nqp::guard('literal', nqp::track('arg', $capture, 2));

    # Make sure that we have a method table built for this type (but
    # we don't actually need the table itself).
    my $obj := nqp::captureposarg($capture, 0);
    my $how := nqp::how_nd($obj);

    # Don't install if we do't have a method table yet
    nqp::syscall('dispatcher-do-not-install')
      unless nqp::isconcrete(nqp::getattr(
        $how, Perl6::Metamodel::ClassHOW, '$!cached_all_method_table'
      ));

    # Make sure there's a method table from now on
    $how.all_method_table($obj);

    # Track the HOW and then the attribute holding the table.  Do the
    # lookup of the method in the table we found in the meta-object.
    # If it's not found, the outcome will be a null, which is exactly
    # what we want to indicate lookup failure
    delegate-value(
      $capture,
      nqp::syscall('dispatcher-index-tracked-lookup-table',
        nqp::track('attr',
          nqp::track('how', nqp::track('arg', $capture, 0)),
          Perl6::Metamodel::ClassHOW,
          '$!cached_all_method_table'
        ),
        nqp::track('arg', $capture, 1)
      )
    );
});

#- raku-capture-lex ------------------------------------------------------------
# The dispatcher backing p6capturelex. If we are passed a code object, then
# extracts the underlying handle and causes it to be captured.
nqp::register('raku-capture-lex', -> $capture {
    delegate-code-syscall($capture, 'try-capture-lex');
});

#- raku-capture-lex-callers ----------------------------------------------------
# The dispatcher backing p6capturelexwhere. If we are passed a code object,
# then extracts the underlying handle and looks down the callstack for a
# caller that matches the outer, and captures it.
nqp::register('raku-capture-lex-callers', -> $capture {
    delegate-code-syscall($capture, 'try-capture-lex-callers');
});

#- raku-get-code-outer-ctx -----------------------------------------------------
# The dispatcher backing p6getouterctx. Unwraps the code object, and then
# gets a context object for its enclosing scope.
nqp::register('raku-get-code-outer-ctx', -> $capture {
    if nqp::istype(nqp::captureposarg($capture, 0), Code) {
        my $Tcode := nqp::track('arg', $capture, 0);
        nqp::delegate('boot-syscall',
          nqp::syscall('dispatcher-insert-arg-literal-str',
            nqp::syscall('dispatcher-insert-arg',
              nqp::syscall('dispatcher-drop-arg', $capture, 0),
              0, nqp::track('attr', $Tcode, Code, '$!do')
            ),
            0, 'get-code-outer-ctx'
          )
        );
    }
    else {
        nqp::die('raku-get-code-outer-ctx requires a Code object');
    }
});

#- raku-resume-error -----------------------------------------------------------
# Resumption error reporting dispatcher.  Throws with the name of
# the name of the caller dispatcher.
nqp::register('raku-resume-error', -> $capture {
    my str $redispatcher := nqp::getcodename(nqp::callercode());
    Perl6::Metamodel::Configuration.throw_or_die(
      'X::NoDispatcher',
      "$redispatcher is not in the dynamic scope of a dispatcher",
      :$redispatcher
    );
});

#- raku-isinvokable ------------------------------------------------------------
# Delegate to returning 1 if given object is a Code, else 0
nqp::register('raku-isinvokable', -> $capture {

    # Guard on the type, then evaluate to a constant for if it's a Code type
    nqp::guard('type', nqp::track('arg', $capture, 0));
    nqp::delegate('boot-constant',
      nqp::syscall('dispatcher-insert-arg-literal-int',
        $capture, 0, nqp::istype(nqp::captureposarg($capture, 0), Code)
      )
    );
});

#- raku-boolify -------------------------------------------------------------
# Dispatcher for boolification of a value.

# Code for HLLizing to a Raku Bool
my $hllbool := nqp::getstaticcode(
  -> $prim { nqp::hllboolfor(nqp::istrue($prim), 'Raku') }
);
# Code for HLLizing to a Raku Bool, negating the value
my $hllbool_not := nqp::getstaticcode(
  -> $prim { nqp::hllboolfor(nqp::isfalse($prim), 'Raku') }
);

# Return whether all candidates of the given method name on the
# given type have an :U constraint on the invocant, and whether
# the method is part of the coe setting.
sub is-method-setting-only-U($type, str $method-name) {
    my $method := nqp::tryfindmethod($type, $method-name);
    nqp::defined($method) && nqp::istype($method, Routine)
      ?? $method.IS-SETTING-ONLY-U
      !! nqp::istype($method, Code)
        ?? nqp::istrue($method.file.starts-with('SETTING::'))
        !! 1 # Non-Raku code objects are considered coming from setting
}

# Return whether all candidates of the given method name on the
# given type have an :D constraint on the invocant, and whether
# the method is part of the core setting.
sub is-method-setting-only-D($type, str $method-name) {
    my $method := nqp::tryfindmethod($type, $method-name);
    nqp::defined($method) && nqp::istype($method, Routine)
      ?? $method.IS-SETTING-ONLY-D
      !! nqp::istype($method, Code)
        ?? nqp::istrue($method.file.starts-with('SETTING::'))
        !! 1 # Non-Raku code objects are considered coming from setting
}

# The actual boolification dispatcher: expects a value to boolify, and
# produces either True or False
nqp::register('raku-boolify', -> $capture {

    # Get the thing to boolify and track it
    my $arg-spec := nqp::captureposprimspec($capture, 0);
    my $arg := $arg-spec
      ?? $arg-spec == nqp::const::BIND_VAL_INT  # XXX UINT ??
        ?? nqp::captureposarg_i($capture, 0)
        !! $arg-spec == nqp::const::BIND_VAL_NUM
          ?? nqp::captureposarg_n($capture, 0)
          !! nqp::captureposarg_s($capture, 0)
      !! nqp::captureposarg($capture, 0);
    my $Targ := nqp::track('arg', $capture, 0);

    # There is no need to guard for type when fallback to method call
    sub fallback-type() {
        nqp::delegate('raku-meth-call',
          nqp::syscall('dispatcher-insert-arg-literal-str',
            nqp::syscall('dispatcher-insert-arg-literal-obj',
              $capture, 0, nqp::what($arg)
            ),
            1, 'Bool'
          )
        );
    }

    if nqp::isconcrete($arg) {
        if $arg-spec {
            nqp::guard('concreteness', $Targ);
            nqp::guard('type', $Targ);
            nqp::delegate('boot-code-constant',
              nqp::syscall('dispatcher-insert-arg-literal-obj',
                $capture, 0, $hllbool
              )
            );
        }
        elsif nqp::istype($arg, Bool) {
            nqp::guard('concreteness', $Targ);
            nqp::guard('type', $Targ);
            nqp::delegate('boot-value', $capture);
        }
        else {
            fallback-type();
        }
    }

    # For non-concrete objects default method Bool candidate would always
    # produce False if it's a setting object
    elsif is-method-setting-only-U($arg, 'Bool') {
        nqp::guard('concreteness', $Targ);
        nqp::guard('type', $Targ);
        nqp::delegate('boot-value',
          nqp::syscall('dispatcher-insert-arg-literal-obj',
            $capture, 0, $hllbool(0)
          )
        );
    }

    # Non-core types we're not so sure, so type.Bool
    else {
        fallback-type();
    }
});

#- raku-smartmatch -------------------------------------------------------------
# Smartmatch support
{
    my &smartmatch-code := nqp::getstaticcode(-> $topic, $rhs {
        nqp::dispatch('raku-boolify', $rhs.ACCEPTS($topic))
    });
    my &negate-smartmatch-code := nqp::getstaticcode(-> $topic, $rhs {
        nqp::hllizefor($rhs.ACCEPTS($topic), 'Raku').not
    });
    my &nominalizable-sm-code := nqp::getstaticcode(-> $topic, $rhs {
        nqp::hllboolfor(nqp::istype($topic, $rhs), 'Raku')
    });
    my &negate-nominalizable-sm-code := nqp::getstaticcode(-> $topic, $rhs {
        nqp::hllboolfor(nqp::not_i(nqp::istype($topic, $rhs)), 'Raku')
    });

    my sub find-core-symbol(str $sym, :$ctx, :$revision) {
        unless nqp::isconcrete($ctx) {
            $ctx := nqp::ctxcaller(nqp::ctx());
        }
        my $core-rev-sym := 'CORE-SETTING-REV';
        while nqp::isnull(nqp::getlexrel($ctx, $core-rev-sym)) {
            if nqp::isnull($ctx := nqp::ctxcaller($ctx)) {
                last
            }
        }
        until nqp::isnull($ctx) {
            my $lexpad := nqp::ctxlexpad($ctx);
            if nqp::existskey($lexpad, $core-rev-sym) {
                last unless nqp::isconcrete($revision)
                            && nqp::isne_s(nqp::atkey($lexpad, $core-rev-sym), $revision);
            }
            $ctx := nqp::ctxouterskipthunks($ctx);
        }
        nqp::die("No symbol '" ~ $sym ~ "' found in CORE" ~ ($revision ?? "." ~ $revision !! "")) if nqp::isnull($ctx);
        nqp::getlexrel($ctx, $sym)
    }

    nqp::register('raku-smartmatch', -> $capture {
        # The dispatch receives:
        # - lhs with containerization preserved
        # - rhs with containerization preserved
        # - boolification flag
        # boolification flag can either be -1 to negate, 0 to return as-is, 1 to boolify
        # Note that boolification flag is not guarded because it is expected to be invariant over call site.
        my $Match               := find-core-symbol('Match', :ctx(nqp::ctxcaller(nqp::ctx())));
        my $lhs                 := nqp::captureposarg($capture, 0);
        my $rhs                 := nqp::captureposarg($capture, 1);
        my $boolification       := nqp::captureposarg_i($capture, 2);
        my $track-lhs           := nqp::track('arg', $capture, 0);
        my $track-rhs           := nqp::track('arg', $capture, 1);

        if nqp::istype_nd($lhs, Scalar) {
            nqp::guard('type', $track-lhs );
            $track-lhs := nqp::track('attr', $track-lhs, Scalar, '$!value');
            $lhs := nqp::getattr($lhs, Scalar, '$!value');
        }

        if nqp::istype_nd($rhs, Scalar) {
            nqp::guard('type', $track-rhs );
            $track-rhs := nqp::track('attr', $track-rhs, Scalar, '$!value');
            $rhs := nqp::getattr($rhs, Scalar, '$!value');
        }

        my $explicit-accepts := 1;

        if $boolification == 0 {
            if nqp::isconcrete_nd($rhs) && nqp::istype_nd($rhs, Junction) {
                # Make sure to collapse a Junction.
                # nqp::guard('literal', $track-boolification);
                nqp::guard('concreteness', $track-rhs);
                nqp::guard('type', $track-rhs);
                nqp::delegate('raku-meth-call',
                    nqp::syscall('dispatcher-insert-arg-literal-str',
                        nqp::syscall('dispatcher-insert-arg-literal-obj',
                            nqp::syscall('dispatcher-drop-arg',
                                nqp::syscall('dispatcher-drop-arg', $capture, 2), # boolification flag
                                0),                                                                # LHS
                            0, nqp::what($rhs)),
                        1, 'Bool'));
                $explicit-accepts := 0;
            }
            elsif nqp::isconcrete_nd($rhs) && nqp::istype_nd($rhs, List) {
                # A list must be reified in order to fire up any code embedded into regexes.
                # nqp::guard('literal', $track-boolification);
                nqp::guard('concreteness', $track-rhs);
                nqp::guard('type', $track-rhs);
                nqp::delegate('raku-meth-call',
                    nqp::syscall('dispatcher-insert-arg-literal-str',
                        nqp::syscall('dispatcher-insert-arg-literal-obj',
                            nqp::syscall('dispatcher-drop-arg',
                                nqp::syscall('dispatcher-drop-arg', $capture, 2), # boolification flag
                                0),                                                                # LHS
                            0, nqp::what($rhs)),
                        1, 'eager'));
                $explicit-accepts := 0;
            }
            elsif nqp::istype_nd($rhs, Nil) {
                # nqp::guard('literal', $track-boolification);
                nqp::guard('type', $track-rhs);
                nqp::delegate('boot-value',
                    nqp::syscall('dispatcher-insert-arg-literal-obj',
                        $capture, 0, nqp::hllboolfor(0, 'Raku')));
                $explicit-accepts := 0;
            }
            else {
                # nqp::guard('literal', $track-boolification);
                nqp::guard('type', $track-rhs);
                # Bypass is normally used with Regex-kind of RHS and it is not specced wether the smartmatch result must
                # be deconted in this case. Therefore we better return what we've got on RHS as-is.
                nqp::delegate('boot-value',
                    nqp::syscall('dispatcher-drop-arg', $capture, 0));
                $explicit-accepts := 0;
            }
        }
        # Delegate to Junction.BOOLIFY-ACCEPTS if possible and makes sense.
        # - Junction type object on RHS is always a type match and we can pass it to the typematching branch
        # - when boolifying over a concrete Regex ACCEPTS fallback is required too
        # - BOOLIFY-ACCEPTS only makes sense when ACCEPTS doesn't handle junctions in a special way. For now this can
        #   only be guaranteed for CORE's ACCEPTS only.
        elsif nqp::istype_nd($lhs, Junction)
            && nqp::isconcrete_nd($lhs)
            && (nqp::isconcrete_nd($rhs) || !nqp::istype($rhs, Junction))
            && !(nqp::isconcrete_nd($rhs) && $boolification == 1 && nqp::istype_nd($rhs, Regex))
            && is-method-setting-only-D($rhs, 'ACCEPTS')
        {
            # nqp::guard('literal', $track-boolification);
            nqp::guard('type', $track-lhs);
            nqp::guard('concreteness', $track-lhs);
            nqp::guard('type', $track-rhs);
            nqp::guard('concreteness', $track-rhs);
            my $method-capture := nqp::syscall('dispatcher-drop-arg', $capture, 2);
            $method-capture := nqp::syscall('dispatcher-insert-arg-literal-obj',
                $method-capture, 2, nqp::hllboolfor($boolification == -1, 'Raku')); # boolification -> Raku's Bool negation flag
            $method-capture :=
                nqp::syscall('dispatcher-insert-arg-literal-obj',
                    $method-capture, 0, nqp::what($lhs));
            $method-capture :=
                nqp::syscall('dispatcher-insert-arg-literal-str',
                    $method-capture, 1, 'BOOLIFY-ACCEPTS');

            nqp::delegate('raku-meth-call', $method-capture);
            $explicit-accepts := 0;
        }
        else {
            if nqp::isconcrete_nd($rhs) {
                if $boolification < 0 {
                    if nqp::istype_nd($rhs, Bool) {
                        # nqp::guard('literal', $track-boolification);
                        nqp::guard('type', $track-rhs);
                        nqp::guard('literal', $track-rhs);
                        nqp::delegate('boot-value',
                            nqp::syscall('dispatcher-insert-arg-literal-obj',
                                nqp::syscall('dispatcher-drop-arg', $capture, 0),
                                0, $hllbool_not($rhs)));
                        $explicit-accepts := 0;
                    }
                    elsif nqp::istype_nd($rhs, $Match) {
                        # nqp::guard('literal', $track-boolification);
                        nqp::guard('concreteness', $track-rhs);
                        nqp::guard('type', $track-rhs);
                        nqp::delegate('raku-meth-call',
                            nqp::syscall('dispatcher-insert-arg-literal-str',
                                nqp::syscall('dispatcher-insert-arg-literal-obj',
                                    nqp::syscall('dispatcher-drop-arg',
                                        nqp::syscall('dispatcher-drop-arg', $capture, 2), # boolification flag
                                        0),                                                                # LHS
                                    0, nqp::what($rhs)),
                                1, 'not'));
                        $explicit-accepts := 0;
                    }
                }
                elsif nqp::istype_nd($rhs, Bool) || nqp::istype_nd($rhs, $Match) {
                    # nqp::guard('literal', $track-boolification);
                    nqp::guard('concreteness', $track-rhs);
                    nqp::guard('type', $track-rhs);
                    nqp::delegate('boot-value',
                        nqp::syscall('dispatcher-drop-arg', $capture, 0)); # drop LHS
                    $explicit-accepts := 0;
                }
            }
            elsif is-method-setting-only-U($rhs, 'ACCEPTS') { # Non-concrete RHS
                # A typeobject on RHS with default ACCEPTS can be reduced to nqp::istype, unless LHS is a concrete Junction.
                nqp::guard('concreteness', $track-rhs);
                nqp::guard('type', $track-rhs);
                my $rhs-how := $rhs.HOW;
                my $can-archetypes := nqp::can($rhs-how, 'archetypes');

                if !$can-archetypes
                    || !$rhs-how.archetypes($rhs).nominalizable
                    || nqp::isnull($rhs-how.wrappee-lookup($rhs, :subset))
                {
                    nqp::guard('type', $track-lhs);
                    if $can-archetypes && $rhs-how.archetypes($rhs).definite {
                        # If RHS is a definite then concreteness of LHS must be considered.
                        nqp::guard('concreteness', $track-lhs);
                    }

                    my $matches := try nqp::istype_nd($lhs, $rhs);
                    $matches := $boolification < 0 ?? $hllbool_not($matches) !! $hllbool($matches);
                    nqp::delegate('boot-value',
                        nqp::syscall('dispatcher-insert-arg-literal-obj',
                            nqp::syscall('dispatcher-drop-arg', $capture, 0),
                            0, $matches));
                }
                else {
                    # Subsets are usually using topic value to ensure matching. Normally it would mean full ACCEPTS+Bool
                    # fallback, but we can shortcut directly to HOW.accepts_type and then boolify.
                    # Note that LHS is not relevant for dispatching in this case.
                    my $sm-code := $boolification < 0 ?? &negate-nominalizable-sm-code !! &nominalizable-sm-code;
                    nqp::delegate('boot-code-constant',
                        nqp::syscall('dispatcher-insert-arg-literal-obj',
                            nqp::syscall('dispatcher-drop-arg', $capture, 2), # boolification flag
                            0, $sm-code));
                }

                $explicit-accepts := 0;
            }
        }

        if $explicit-accepts {
            nqp::guard('concreteness', $track-rhs);
            nqp::guard('type', $track-rhs);
            if $boolification == 0 || (nqp::isconcrete_nd($rhs) && $boolification > -1 && nqp::istype_nd($rhs, Regex)) {
                # Do not boolify over a Regex RHS.

                # First, drop everything except for LHS.
                my $method-capture :=
                        nqp::syscall('dispatcher-drop-arg', $capture, 2); # boolification flag
                $method-capture :=
                    nqp::syscall('dispatcher-insert-arg',
                        $method-capture, 0,
                        nqp::track('arg', $capture, 1)); # Move RHS to the start
                $method-capture :=
                    nqp::syscall('dispatcher-drop-arg', $method-capture, 2); # Old RHS
                # Then prepare for raku-meth-call: deconted RHS, method name, RHS, LHS.
                $method-capture :=
                    nqp::syscall('dispatcher-insert-arg-literal-obj',
                        $method-capture, 0, nqp::what($rhs));
                $method-capture :=
                    nqp::syscall('dispatcher-insert-arg-literal-str',
                        $method-capture, 1, 'ACCEPTS');

                nqp::delegate('raku-meth-call', $method-capture);
            }
            else {
                my $sm-code := $boolification < 0 ?? &negate-smartmatch-code !! &smartmatch-code;
                nqp::delegate('boot-code-constant',
                    nqp::syscall('dispatcher-insert-arg-literal-obj',
                        nqp::syscall('dispatcher-drop-arg', $capture, 2), # boolify flag
                        0, $sm-code));
            }
        }
    });
}

#- raku-coercion ---------------------------------------------------------------
# Coercion protocol handling

# Coerce by target method name. I.e. $value.TargetType.
my $coerce-by-type-method := nqp::getstaticcode(
  -> $coercion, $value, $method, $nominal_target, $target_type {
    nqp::istype((my $coerced_value := $method($value)), $target_type)
      || nqp::istype($coerced_value, nqp::gethllsym('Raku', 'Failure'))
      ?? $coerced_value
      !! nqp::how($coercion)."!invalid_coercion"(
           $value,
           nqp::how_nd($nominal_target).name($nominal_target),
           $coerced_value
         )
});

# Indirect methods are `COERCE` or `new` on the target type.
my $coerce-indirect-method := nqp::getstaticcode(
  -> $coercion, $value, $method, $nominal_target, $target_type {
    my $*COERCION-TYPE := $coercion;
    nqp::istype(
      (my $coerced_value := $method($nominal_target, $value)),
      $target_type
    ) || nqp::istype($coerced_value, nqp::gethllsym('Raku', 'Failure'))
      ?? $coerced_value
      !! nqp::how($coercion)."!invalid_coercion"(
           $value, $method.name, $coerced_value
         )
});

# The special case of `new` when we suspect it may throw
# X::Constructor::Positional which we interpret as "no coercion method found"
my $coerce-new := nqp::getstaticcode(
  -> $coercion, $value, $method, $nominal_target, $target_type {
    my $exception;
    my $coerced_value := nqp::null();
    try {
        CATCH {
            my $exception_obj := nqp::getpayload($!);
            $exception := $!
              if $exception_obj.HOW.name($exception_obj)
                   ne 'X::Constructor::Positional';
        }

        my $*COERCION-TYPE := $coercion;
        $coerced_value := $method($nominal_target, $value);
    }
    nqp::rethrow($exception) if nqp::defined($exception);

    nqp::isnull($coerced_value)
      ?? nqp::how($coercion)."!invalid"(
           $value, "no acceptable coercion method found"
         )
      !! nqp::istype($coerced_value, $target_type)
           || nqp::istype($coerced_value, nqp::gethllsym('Raku', 'Failure'))
           ?? $coerced_value
           !! nqp::how($coercion)."!invalid_coercion"(
                $value, 'new', $coerced_value
              )
});

# Perform coercion at runtime
my $coerce-runtime := nqp::getstaticcode(-> $coercion, $value, *@pos {
    nqp::how($coercion)."!coerce_TargetType"($coercion, $value)
});

# Perform dispatch through the value in a container
my $coerce-via-container := nqp::getstaticcode(-> $coercion, $value {
    nqp::dispatch('raku-coercion', $coercion, nqp::decont($value))
});

# Return a list with coercer, method object and nominal target for the
# given coercion object, value and optional force :with-runtime flag
sub select-coercer($coercion, $value, :$with-runtime = 0) {
    my $how             := $coercion.HOW;
    my $target_type     := $how.target_type($coercion);
    my $constraint_type := $how.constraint_type($coercion);
    my $nominal_target  := $how.nominal_target($coercion);
    my $coercer         := nqp::null();
    my $method          := nqp::null();

    # Routine's method `cando` doesn't work well with a Match object passed
    # into as the first positional.  Besides, it requires type conversions
    # on NQP/Raku language boundary. Therefore we use a truncated local
    # version of it.
    my sub method-cando($method, *@pos) {
        my $disp;
        if $method.is_dispatcher {
            $disp := $method;
        }
        else {
            $disp := nqp::create(nqp::what($method));
            nqp::bindattr($disp, Routine, '@!dispatchees', nqp::list($method));
        }
        -> *@_ { $disp.find_best_dispatchee( nqp::usecapture(), 1) }(|@pos)
    }

    # Make sure none of the coercion method candidates uses run-time
    # constraints.
    my sub method-is-optimizable($method) {
        # Can do no assumptions about a non-Routine.
        return 0 unless nqp::istype($method, Routine);

        my @cands := $method.is_dispatcher
          ?? nqp::getattr($method, Routine, '@!dispatchees')
          !! nqp::list($method);

        for @cands -> $cand {
            my $signature := nqp::decont($cand.signature);

            # Skip the candidate if it has too few positionals or
            # requires too many of them.
            next if nqp::islt_n($signature.count,2)
                 || nqp::isgt_n($signature.arity,2);

            my $vparam := nqp::atpos(
              nqp::getattr($cand.signature, Signature, '@!params'), 1
            );
            my $ptype    := $vparam.type;
            my $ptypeHOW := $ptype.HOW;

            return 0
              if $ptypeHOW.archetypes($ptype).nominalizable
              && !nqp::isnull(
                   my $subset := $ptypeHOW.wrappee-lookup($ptype, :subset)
                 )
              && nqp::isconcrete(nqp::how($subset).refinement($subset));

            # XXX Retreat on any parameter postconstraint present. Some
            # refinement is possible here, but to be done later.
            return 0
              if nqp::getattr($vparam, Parameter, '@!post_constraints');
        }
        1  # it's optimizable!
    }

    # Pun a role in $nominal_target into a class
    my sub pun-nominal-target() {
        CATCH {
            Perl6::Metamodel::Configuration.throw_or_die(
              'X::Coerce::Role',
              "Coercion from " ~ $value.HOW.name($value)
                ~ " into " ~ $nominal_target.HOW.name($nominal_target)
                ~ " died while tried to pun the target role",
              :target-type($target_type),
              :from-type($value.WHAT),
              :exception($!)
            )
        }
        $nominal_target := nqp::how_nd($nominal_target).pun($nominal_target);
    }

    # Runtime requested and the constraint type can coerce
    if $with-runtime
      && $constraint_type.HOW.archetypes($constraint_type).coercive {
        $coercer := $coerce-runtime;
    }

    # There is .TargetType method on the value, use it.
    elsif nqp::defined(
      $method := nqp::tryfindmethod(
        $value.WHAT,
        nqp::how_nd($nominal_target).name($nominal_target)
      )
    ) && method-cando($method, $value) {
        $coercer := $coerce-by-type-method;
    }

    # The target type can .COERCE.
    elsif nqp::defined(
      $method := nqp::tryfindmethod(
        (nqp::how_nd($nominal_target).archetypes.composable
          ?? pun-nominal-target()
          !! $nominal_target
        ),
        'COERCE'
      )
    ) && method-cando($method, $nominal_target, $value) {

        $coercer := $coerce-indirect-method
          if method-is-optimizable($method);
    }

    # We can TargetType.new($value)
    elsif nqp::defined(
      $method := nqp::tryfindmethod($nominal_target, 'new'))
        && (my @cands := method-cando($method, $nominal_target, $value)) {

        if method-is-optimizable($method) {
            if nqp::elems(@cands) == 1 {
                # The only .new candidate for a single positional arg call
                # comes from Mu. That one throws "only named arguments"
                # error which means no candidates are actually# found.
                # Simulate this by resetting $method and pretend it's never
                # been found.  Otherwise use the indirect method approach
                nqp::eqaddr(@cands[0].package, Mu)
                  ?? ($method  := nqp::null)
                  !! ($coercer := $coerce-indirect-method)
            }
            else {
                $coercer := $coerce-new;
            }
        }
    }

    # Want to check with runtime, and we have a method so go for runtime
    # coercion
    elsif $with-runtime && nqp::isconcrete($method) {
        $coercer := $coerce-runtime;
    }

    nqp::list($coercer, $method, $nominal_target)
}

# The actual coercion dispatcher. Expected to receive the coercion type object
# and the value to be coerced as values
nqp::register('raku-coercion', -> $capture {

    my $Tcoercion := nqp::track('arg', $capture, 0);
    my $Tvalue    := nqp::track('arg', $capture, 1);
    nqp::guard('type', $Tcoercion);
    nqp::guard('type', $Tvalue);

    # Got a container, need to track/guard its value
    my $value := nqp::captureposarg($capture, 1);
    if nqp::istype_nd($value, Scalar) {
        $Tvalue := nqp::track('attr', $Tvalue, Scalar, '$!value');
        nqp::guard('type', $Tvalue);
        $value := nqp::getattr($value, Scalar, '$!value');
    }
    nqp::guard('concreteness', $Tvalue);

    # If the type happens to be another nominalizable, like a subset wrapped
    # around a coercion, then pull out the actual coercion type we need.
    my $coercion    := nqp::captureposarg($capture, 0);
    my $coercionHOW := $coercion.HOW;
    unless nqp::istype($coercionHOW, Perl6::Metamodel::CoercionHOW) {
        $coercion    := $coercionHOW.wrappee($coercion, :coercion);
        $coercionHOW := nqp::how_nd($coercion);
    }

    # Helper sub to dispatch on the MOP coercion method
    my sub runtime-fallback() {
        nqp::delegate('raku-meth-call',
          nqp::syscall('dispatcher-insert-arg-literal-obj',
            nqp::syscall('dispatcher-insert-arg-literal-str',
              nqp::syscall('dispatcher-insert-arg-literal-obj',
                $capture, 0, $coercionHOW
              ), 0, '!coerce_TargetType'
            ), 0, $coercionHOW
          )
        );
    }

    # Some shortcuts
    my $target_type   := $coercionHOW.target_type($coercion);
    my $constraint    := $coercionHOW.constraint_type($coercion);
    my $constraintHOW := $constraint.HOW;

    # If despite our efforts the value is still a container then try
    # deconting it first and then re-dispatch.
    if nqp::iscont($value) {
        nqp::delegate('boot-code-constant',
          nqp::syscall('dispatcher-insert-arg-literal-obj',
            nqp::syscall('dispatcher-replace-arg-literal-obj',
              $capture, 1, $value
            ), 0, $coerce-via-container
          )
        );
    }

    # Just matches, use identity.
    elsif nqp::istype_nd($value, $target_type) {
        nqp::delegate('boot-value',
          nqp::syscall('dispatcher-drop-arg', $capture, 0)
        );
    }

    # The value doesn't match constraint type, throw by calling the MOP
    # error throwing method
    elsif !nqp::eqaddr($constraint, Mu)
      && !nqp::istype_nd($value, $constraint) {
        nqp::delegate('raku-meth-call',
          nqp::syscall('dispatcher-insert-arg-literal-obj',
            nqp::syscall('dispatcher-insert-arg-literal-str',
              nqp::syscall('dispatcher-insert-arg-literal-obj',
                nqp::syscall('dispatcher-drop-arg',
                  $capture, 0
                ), 0, $coercionHOW
              ), 0, '!invalid_type'
            ), 0, $coercionHOW
          )
        );
    }

    # The constraint type is a coercion on its own. This is not a
    # dispatchable case. Fallback to the metamodel method.
    elsif $constraintHOW.archetypes($constraint).coercive {
        runtime-fallback();
    }

    # Try finding one of the coercion methods.
    else {
        my @cdesc := select-coercer($coercion, $value);

        # We found an acceptable coercer, use it.
        if nqp::isconcrete(nqp::atpos(@cdesc,0)) {       # coercer
            nqp::delegate('boot-code-constant',
              nqp::syscall('dispatcher-insert-arg-literal-obj',
                nqp::syscall('dispatcher-insert-arg-literal-obj',
                  nqp::syscall('dispatcher-insert-arg-literal-obj',
                    nqp::syscall('dispatcher-insert-arg-literal-obj',
                      $capture, 0, nqp::atpos(@cdesc,0)  # coercer
                    ), 3, nqp::atpos(@cdesc,1)           # method
                  ), 4, nqp::atpos(@cdesc,2)             # nominal_target
                ), 5, $target_type
              )
            );
        }

        # We found a method but cannot reliably assume that it can be
        # optimized. Let the run-time handle it.
        elsif nqp::isconcrete(nqp::atpos(@cdesc,1))  {   # method
            runtime-fallback();
        }

        # There is no way we can coerce the value.  Let the MOP handle the
        # error reporting
        else {
            nqp::delegate('raku-meth-call',
              nqp::syscall('dispatcher-insert-arg-literal-obj',
                nqp::syscall('dispatcher-insert-arg-literal-str',
                  nqp::syscall('dispatcher-insert-arg-literal-obj',
                    nqp::syscall('dispatcher-insert-arg-literal-str',
                      nqp::syscall(
                        'dispatcher-drop-arg', $capture, 0
                      ), 1, "no acceptable coercion method found"
                    ), 0, $coercionHOW
                  ), 0, '!invalid'
                ), 0, $coercionHOW
              )
            );
        }
    }
});

#- raku-rv-typecheck-generic ---------------------------------------------------
# Return value type-check dispatcher for instantiated generic types. The
# first value is the return value, the second is the type that is expected,
# which may be a definiteness or coercion type.
nqp::register('raku-rv-typecheck-generic', -> $capture {
    nqp::guard('type', nqp::track('arg', $capture, 1));
    nqp::delegate('raku-rv-typecheck', $capture);
});

#- raku-rv-typecheck -----------------------------------------------------------
# Typechecking return values

# Helper sub to return a typecheck error
sub return_error($got, $expected) {
    Perl6::Metamodel::Configuration.throw_or_die(
      'X::TypeCheck::Return',
      "Type check failed for return value; expected '"
        ~ $expected.HOW.name($expected)
        ~ "' but got '"
        ~ $got.HOW.name($got)
        ~ "'",
      :$got, :$expected
    );
}

# Error out if type object didn't match or is an instantiated value,
# otherwise return value
my $check_type_typeobj := -> $ret, $type {
    !nqp::isconcrete($ret) && nqp::istype($ret, $type)
      ?? $ret
      !! return_error($ret, $type)
}

# Error out if value didn't match or is a type object, otherwise return value
my $check_type_concrete := -> $ret, $type {
    nqp::isconcrete($ret) && nqp::istype($ret, $type)
        ?? $ret
        !! return_error($ret, $type)
}

# Error out if type doesn't match, otherwise return value
my $check_type := -> $ret, $type {
    nqp::istype($ret, $type) ?? $ret !! return_error($ret, $type)
}

# Error out if a value or it is the wrong type, or coercion didn't work
# out. For @cdesc values see select-coercer sub.
my $check_type_typeobj_coerce := -> $ret, $type, $coercion, @cdesc {
    !nqp::isconcrete($ret) && nqp::istype($ret, $type)
      ?? nqp::atpos(@cdesc,0)(    # coercer code
           $coercion,
           $ret,
           nqp::atpos(@cdesc,1),  # coercing method
           nqp::atpos(@cdesc,2),  # nominal target
           nqp::how($coercion).target_type($coercion)
         )
      !! return_error($ret, $type)
}

# Error out if a type object or it is the wrong type, or coercion didn't work
# out. For @cdesc values see select-coercer sub.
my $check_type_concrete_coerce := -> $ret, $type, $coercion, @cdesc {
    nqp::isconcrete($ret) && nqp::istype($ret, $type)
      ?? nqp::atpos(@cdesc,0)(    # coercer code
           $coercion, $ret,
           nqp::atpos(@cdesc,1),  # coercing method
           nqp::atpos(@cdesc,2),  # nominal_target
           nqp::how($coercion).target_type($coercion)
         )
      !! return_error($ret, $type)
}

# Error out if it is the wrong type, or coercion didn't work out. For
# @cdesc values see select-coercer sub.
my $check_type_coerce := -> $ret, $type, $coercion, @cdesc {
    nqp::istype($ret, $type)
      ?? nqp::atpos(@cdesc,0)(    # coercer code
           $coercion,
           $ret,
           nqp::atpos(@cdesc,1),  # coercing method
           nqp::atpos(@cdesc,2),  # nominal_target
           nqp::how($coercion).target_type($coercion)
         )
      !! return_error($ret, $type)
}

# Return value type-check dispatcher. The first value is the return value,
# the second is the type that is expected, which may be a definiteness or
# coercion type.  The type is supposed to be guarded already if it is an
# instantiated generic type.
nqp::register('raku-rv-typecheck', -> $capture {

    # Preset most common case, which will return identity
    my str $delegate := 'boot-value';

    # Fast track Nil / Failures being returned
    my $rv     := nqp::captureposarg($capture, 0);
    my $Tvalue := nqp::track( 'arg', $capture, 0);
    if nqp::istype($rv, Nil)
      && (!nqp::iscont($rv) || nqp::istype_nd($rv,Scalar)) {
        nqp::guard('type', nqp::istype_nd($rv,Scalar)
          ?? nqp::track('attr', $Tvalue, Scalar, '$!value')
          !! $Tvalue
        );
    }

    # Not a simple type-checking bypass case
    else {

        # Helper sub to indicate whether a runtime check is needed
        my sub runtime-only($t) {
            nqp::isnull($t)
              ?? 0
              !! nqp::isconcrete(nqp::how_nd($t).refinement($t))
                   || nqp::how_nd(
                        my $refinee := nqp::how_nd($t).refinee($t)
                      ).archetypes($refinee).nominalizable
                   && runtime-only(
                        nqp::how_nd($refinee).wrappee-lookup($refinee, :subset)
                      )
        }

        # Make sure we guard on on the value and concreteness, even if
        # inside a container
        if nqp::istype_nd($rv, Scalar) {
            nqp::guard('type', $Tvalue);
            $Tvalue := nqp::track('attr', $Tvalue, Scalar, '$!value');
        }
        nqp::guard('type', $Tvalue);

        # Set up check flags
        my $type := nqp::captureposarg($capture, 1);
        my $how  := nqp::how_nd($type);
        my $coercion-type;
        my $constraint-type := nqp::null();
        my int $definite-check := -1;  # do we need to check on concreteness?
        my int $runtime-check;         # Is there a run-time constraint?

        if $how.archetypes($type).nominalizable {
            $runtime-check := runtime-only($how.wrappee-lookup($type, :subset));
            unless nqp::isnull($coercion-type := $how.wrappee-lookup($type, :coercion)) {
                unless nqp::isnull($constraint-type := nqp::how_nd($coercion-type).constraint_type($coercion-type)) {
                    $runtime-check := $runtime-check
                        || (nqp::how_nd($constraint-type).archetypes($constraint-type).nominalizable
                            && runtime-only(nqp::how_nd($constraint-type).wrappee-lookup($constraint-type, :subset)));
                }
            }
            if $how.archetypes($type).definite {
                my $dtype := $how.wrappee($type, :definite);
                $definite-check := nqp::how_nd($dtype).definite($dtype);
                nqp::guard('concreteness', $Tvalue);
            }
        }

        my $need-coercion := $how.archetypes($type).coercive
          && !nqp::istype(
                $rv,
                nqp::how_nd($coercion-type).target_type($coercion-type)
             );

        # Needs a runtime check
        if $runtime-check {
            $delegate := 'boot-code-constant';  # always need to run code

            # The most expensive path: subset with constraints and coercion.
            if $need-coercion {
                # First, we re-consider defininite check because now we're
                # going to use coercion constraint for that
                $definite-check :=
                  nqp::how_nd(
                    $constraint-type
                  ).archetypes($constraint-type).definite
                    ?? nqp::how_nd(
                         my $dt := nqp::how_nd($constraint-type).wrappee(
                           $constraint-type, :definite
                         )
                       ).definite($dt)
                    !! -1;

                $capture  := nqp::syscall('dispatcher-insert-arg-literal-obj',
                  nqp::syscall('dispatcher-insert-arg-literal-obj',
                    nqp::syscall('dispatcher-insert-arg-literal-obj',
                      $capture, 2, $coercion-type
                    ), 3, select-coercer($coercion-type, $rv, :with-runtime)
                  ), 0, $definite-check == 0
                          ?? $check_type_typeobj_coerce
                          !! $definite-check == 1
                            ?? $check_type_concrete_coerce
                            !! $check_type_coerce
                );
            }

            # Subset with run-time constraints, no coercion.
            else {
                $capture := nqp::syscall('dispatcher-insert-arg-literal-obj',
                  $capture, 0, $definite-check == 0
                    ?? $check_type_typeobj
                    !! $definite-check == 1
                      ?? $check_type_concrete
                      !! $check_type
                );
            }
        }

        # No runtime check, but needs coercion
        elsif $need-coercion {
            # Make sure we can coerce by matching the constraint type and
            # then dispatch directly to the coercion dispatcher.
            if nqp::eqaddr($constraint-type, Mu)
              || nqp::istype($rv, $constraint-type) {
                # Coercion dispatcher uses reverse order of arguments, same
                # as the metamodel method `coerce`.
                $delegate := 'raku-coercion';
                $capture  := nqp::syscall('dispatcher-insert-arg-literal-obj',
                  nqp::syscall('dispatcher-drop-arg', $capture, 1), 0, $type
                );
            }

            # Coercion is not possible.
            else {
                $delegate := 'boot-code-constant';
                $capture  := nqp::syscall('dispatcher-insert-arg-literal-obj',
                  $capture, 0, &return_error
                );
            }
        }

        # No runtime check, no coercion
        else {

            # Not passing the definedness check, otherwise identity
            unless nqp::istype($rv, $type)
              && ($definite-check == 0
                   ?? !nqp::isconcrete($rv)
                   !! $definite-check == 1
                     ?? nqp::isconcrete($rv)
                     !! 1
                 ) {
                $delegate := 'boot-code-constant';
                $capture  := nqp::syscall('dispatcher-insert-arg-literal-obj',
                  $capture, 0, &return_error
                );
            }
        }
    }

    # Do the actual delegation
    nqp::delegate($delegate, $capture);
});
