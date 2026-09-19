    ##
    ## Various utility subs to help us produce types that look Raku-like.
    ##

    # The metamodel reads Raku's bools, null and Scalar type from the HLL
    # when it composes a class, and the bootstrap only gives the HLL those
    # when it is loaded, so they are given here as well. The VM does not
    # keep the values in the configuration from moving and does not update
    # them when they do, so they must already be in the old generation
    # here. Nothing enforces that. It holds because this block runs after
    # the generator's type stubs, which precede it in the generated file,
    # have been compiled, and an early collection of the bools would show
    # as a stale pointer rather than an error.
    nqp::sethllconfig('Raku', nqp::hash(
      'null_value',  Mu,
      'true_value',  (Bool.WHO)<True>,
      'false_value', (Bool.WHO)<False>,
    ));
    nqp::bindhllsym('Raku', 'Scalar', Scalar);

    sub parent($class, $parent) {
        $class.HOW.add_parent($class, $parent);
    }

    sub does($type, $role) {
        $type.HOW.add_role($type, $role);
    }

    sub add-attribute($class, $type, $name) {
        my $attribute := Attribute.new(
            :$name, :$type, :package($class), :auto_viv_primitive($type)
        );
        # The generator makes the accessors, so there is nothing left to
        # compose. Saying so keeps the setting's Attribute.compose from
        # rebinding the package to a class that is augmented or that does
        # a role, which the layout and a role's methods key on.
        nqp::bindattr_i($attribute, Attribute, '$!composed', 1);
        $class.HOW.add_attribute($class, $attribute);
    }

    sub make-method($package, $name, @parameters, $do, $returns, $yada) {
        # Assemble a signature object for introspection purposes.
        my @params;
        my $first := 1;
        for @parameters -> $type, $name, $named, $optional {
            my $param := nqp::create(Parameter);
            nqp::bindattr($param, Parameter, '$!type', $type);
            nqp::bindattr_s($param, Parameter, '$!variable_name', $name);
            my int $flags := 128; # Multi-invocant
            $flags := $flags + 64 if $first; # Invocant
            $flags := $flags + 2048 if $optional;
            nqp::bindattr_i($param, Parameter, '$!flags', $flags);
            if $named {
                nqp::bindattr($param, Parameter, '@!named_names',
                    nqp::list_s(nqp::substr($name, 1)));
            }
            nqp::push(@params, $param);
            $first := 0;
        }
        my $signature := nqp::create(Signature);
        nqp::bindattr($signature, Signature, '@!params', @params);
        nqp::bindattr($signature, Signature, '$!returns',
            nqp::eqaddr($returns, NQPMu) ?? Mu !! $returns);

        # Wrap code up in a Method object.
        my $wrapper := nqp::create(Method);
        nqp::bindattr($wrapper, Code, '$!do', $do);
        nqp::bindattr($wrapper, Code, '$!signature', $signature);
        nqp::bindattr($wrapper, Routine, '$!package', $package);
        $wrapper.set_name($name);
        $wrapper.set_yada if $yada;
        $wrapper
    }

    sub add-method($package, $name, @parameters, $impl, $returns?, :$yada) {
        $package.HOW.add_method($package, $name,
            make-method($package, $name, @parameters, nqp::getstaticcode($impl), $returns, $yada));
    }

    # The checks the generator emits for a declared return type call these,
    # which keeps a checked method small enough to inline. They are methods
    # of a class because a node method runs as static code, which can reach
    # a type but not a sub of this block. MoarVM inlines a frame only below
    # a bytecode size limit, so each method here has to stay under it for a
    # checked method to cost what an unchecked one does. Merging two of them
    # goes over it. Running with MVM_SPESH_INLINE_LOG=1 says whether bool,
    # bool-from-int and a checked method such as sunk still inline.
    my class ReturnCheck {
        method failure($value, $type, $name) {
            Perl6::Metamodel::Configuration.throw_or_die('X::TypeCheck::Return',
                "Type check failed for return value of '$name'; expected "
                    ~ $type.HOW.name($type) ~ " but got "
                    ~ (nqp::isnull($value) ?? 'null' !! $value.HOW.name($value)),
                :got(nqp::isnull($value) ?? Mu !! $value), :expected($type));
        }

        # A VM integer becomes a Bool, as it does for a Bool parameter. The
        # other values are handled apart so that this stays small enough to
        # inline.
        method bool($value, $name) {
            nqp::isint($value)
              ?? self.bool-from-int(nqp::unbox_i($value))
              !! self.bool-object($value, $name)
        }

        # This reads the stash because a frame that uses nqp::hllboolfor
        # is never inlined into a Raku caller.
        method bool-from-int(int $value) {
            $value ?? (Bool.WHO)<True> !! (Bool.WHO)<False>
        }

        # The NQPMu that NQP code returns for an absent value becomes the
        # Bool type object.
        method bool-object($value, $name) {
            nqp::istype($value, Bool)
              ?? $value
              !! nqp::eqaddr($value, NQPMu)
                ?? Bool
                !! self.failure($value, Bool, $name)
        }
    }

    sub compose($type) {
        # A role's methods are static code with the role as their invocant
        # type, shared by every class doing the role, so its body only has
        # to say which class it is being composed into. The attributes it
        # composes stay keyed on the role in the class's layout.
        if $type.HOW.archetypes($type).parametric {
            $type.HOW.set_body_block($type, sub ($class, *@_, *%_) {
                [$type, nqp::hash('$?CLASS', $class)]
            });
        }
        # The node types are 6.c types. Left unset, the revision would be
        # taken from whatever Raku compiler is registered while this runs,
        # and role specialization refuses a type without one.
        $type.HOW.set_language_revision($type, 1);
        $type.HOW.compose($type);
    }
