# Consuming class has to implement IMPL-SIGIL-TYPE which returns the resolution
# for the lookup created by IMPL-SIGIL-LOOKUP.
role RakuAST::ContainerCreator {
    # The RakuAST::Type node for an explicit container base type (e.g. `is T`
    # or `is Array[Str]`). Stored as AST, not just its meta-object, so
    # callers that need the resolved lookup node (not just the type object)
    # can reuse it directly.
    has RakuAST::Type $!explicit-container-base-type-ast;
    has RakuAST::Type $!conflicting-base-type-ast;
    has Bool $.forced-dynamic;
    has Bool $!initialized;
    has Mu $.container-base-type;
    has Mu $.container-type;
    has ContainerDescriptor $.container-descriptor;
    has Mu $.bind-constraint;

    # 1 when the explicit base type is one of the immutable QuantHashes
    # that need special handling on creation
    has int $!explicit-base-bare-create;

    method IMPL-SET-FORCED-DYNAMIC(Bool $forced-dynamic) {
        nqp::bindattr(self, RakuAST::ContainerCreator, '$!forced-dynamic', $forced-dynamic);
    }

    method IMPL-SET-EXPLICIT-CONTAINER-BASE-TYPE(RakuAST::Type $ast) {
        if self.IMPL-HAS-EXPLICIT-CONTAINER-BASE-TYPE {
            nqp::bindattr(self, RakuAST::ContainerCreator, '$!conflicting-base-type-ast', $!explicit-container-base-type-ast);
        }
        nqp::bindattr(self, RakuAST::ContainerCreator, '$!explicit-container-base-type-ast', $ast);
        nqp::bindattr(self, RakuAST::ContainerCreator, '$!initialized', False);
    }

    method IMPL-EXPLICIT-CONTAINER-BASE-TYPE-AST() {
        $!explicit-container-base-type-ast
    }

    method IMPL-HAS-EXPLICIT-CONTAINER-BASE-TYPE() {
        nqp::isconcrete($!explicit-container-base-type-ast)
    }

    method IMPL-HAS-CONFLICTING-BASE-TYPE() {
        nqp::isconcrete($!conflicting-base-type-ast)
    }

    method IMPL-EXPLICIT-CONTAINER-BASE-TYPE() {
        # A container cannot be an instance of a definite type, so
        # `@a is List:D` uses List as the base. This holds in every
        # language version, unlike the definite semantics elsewhere,
        # so it is not version-gated.
        if self.IMPL-HAS-EXPLICIT-CONTAINER-BASE-TYPE {
            my $type := $!explicit-container-base-type-ast.meta-object;
            nqp::eqaddr($type.HOW, Metamodel::DefiniteHOW)
              ?? $type.HOW.base_type($type)
              !! $type
        }
        else {
            Mu
        }
    }

    method IMPL-CONFLICTING-BASE-TYPE() {
        self.IMPL-HAS-CONFLICTING-BASE-TYPE
            ?? $!conflicting-base-type-ast.meta-object
            !! Mu
    }

    method IMPL-SIGIL-LOOKUP() {
        my constant SIGIL-LOOKUP := nqp::hash(
          '@', 'Positional',
          '%', 'Associative',
          '&', 'Callable',
        );

        (my str $name := nqp::atkey(SIGIL-LOOKUP,self.sigil))
          ?? RakuAST::Type::Setting.from-identifier($name)
          !! nqp::null
    }

    # The key types of the hash a container creator makes, one per dimension
    # in the order of the shape, or NQPMu for a hash keyed by Str.
    method IMPL-CONTAINER-KEY-TYPES() { NQPMu }

    # The key type of the hash, or the list of key types for a hash of more
    # than one dimension.
    method IMPL-CONTAINER-KEY-TYPE() {
        my $key-types := self.IMPL-CONTAINER-KEY-TYPES;
        nqp::islist($key-types)
          ?? nqp::elems($key-types) > 1 ?? $key-types !! $key-types[0]
          !! NQPMu
    }

    # The value type of a hash declared with a key type but no value type.
    method IMPL-UNTYPED-HASH-VALUE-TYPE() {
        self.IMPL-LANGUAGE-REVISION >= 3 ?? Mu !! Any
    }

    method IMPL-CALCULATE-TYPES(Mu $of) {
        # Form the container type.
        my str $sigil := self.sigil;
        my $key-type := self.IMPL-CONTAINER-KEY-TYPE;
        my $container-base-type;
        my $container-type;
        my $default := Any;
        my $bind-constraint := Mu;
        my $explicit-base := self.IMPL-EXPLICIT-CONTAINER-BASE-TYPE;
        if self.IMPL-HAS-EXPLICIT-CONTAINER-BASE-TYPE {
            # $explicit-base is already the base type of any `is Type:D`.
            if $key-type =:= NQPMu {
                $container-type := self.type
                    ?? $explicit-base.HOW.parameterize($explicit-base, $of)
                    !! $explicit-base;
            }
            else {
                my $value-type := self.type
                    ?? $of
                    !! self.IMPL-UNTYPED-HASH-VALUE-TYPE;
                $container-type := $explicit-base.HOW.parameterize(
                    $explicit-base, $value-type, $key-type);
            }
            # An @ or % variable is bound to its container directly, so the
            # explicit base constrains what can be bound. An `is` type on a
            # scalar names its scalar container type, which the bound value
            # never is.
            if $sigil eq '@' || $sigil eq '%' {
                $bind-constraint := $container-type;
            }
        }
        elsif $sigil eq '@' {
            $bind-constraint := self.IMPL-SIGIL-TYPE;
            $container-base-type := nqp::objprimspec($of) ?? array !! Array;
            if self.type {
                {
                    $container-type := $container-base-type.HOW.parameterize($container-base-type, $of);
                    CATCH {
                        if $*COMPILING_CORE_SETTING == 1 {
                            $container-type := $container-base-type
                        }
                        else {
                            nqp::die($_);
                        }
                    }
                }
                $bind-constraint := $bind-constraint.HOW.parameterize($bind-constraint, $of);
            }
            else {
                $container-type := Array;
            }
        }
        elsif $sigil eq '%' {
            $container-base-type := Hash;
            $bind-constraint := self.IMPL-SIGIL-TYPE;
            if $key-type =:= NQPMu {
                if self.type {
                    $container-type := Hash.HOW.parameterize(Hash, $of);
                    $bind-constraint := $bind-constraint.HOW.parameterize($bind-constraint, $of);
                }
                else {
                    $container-type := Hash;
                }
            }
            else {
                my $value-type := self.type ?? $of !! self.IMPL-UNTYPED-HASH-VALUE-TYPE;
                $container-type := Hash.HOW.parameterize(Hash, $value-type, $key-type);
                # A coercive role argument matches any type its coercion
                # accepts, so a keyed hash whose value or key type coerces
                # binds only a hash of its own type, as does a shaped hash.
                $bind-constraint :=
                    nqp::islist($key-type)
                      || $value-type.HOW.archetypes($value-type).coercive
                      || $key-type.HOW.archetypes($key-type).coercive
                    ?? $container-type
                    !! $bind-constraint.HOW.parameterize(
                         $bind-constraint, $value-type, $key-type);
            }
        }
        elsif $sigil eq '&' {
            $container-base-type := Scalar;
            my $Callable := self.IMPL-SIGIL-TYPE;
            $container-type := self.type
                ?? $Callable.HOW.parameterize($Callable, $of)
                !! $Callable;
            $default := $Callable;
            $bind-constraint := $container-type;
        }
        else {
            $container-base-type := Scalar;
            $container-type := Scalar;
            $bind-constraint := $of;
        }

        nqp::bindattr(self, RakuAST::ContainerCreator, '$!initialized', True);
        nqp::bindattr(self, RakuAST::ContainerCreator, '$!container-base-type', $container-base-type);
        nqp::bindattr(self, RakuAST::ContainerCreator, '$!container-type', $container-type);
        nqp::bindattr(self, RakuAST::ContainerCreator, '$!bind-constraint', $bind-constraint);

        # Form container descriptor.

        # A definite-constrained variable (e.g. `my Int:U $a`) should default
        # to the nominalized base type, not the wrapped type itself. For a
        # coercive type, the default is the coercion's nominal target so that
        # an uninitialized `my Coerced(Source) $v` does not store the coercion
        # type itself as a value.
        $default := RakuAST::Type.IMPL-NOMINALIZE-FOR-DEFAULT($of) if self.type;
        my int $dynamic := self.twigil eq '*' ?? 1 !! self.forced-dynamic ?? 1 !! 0;
        my int $isolated-match := self.lexical-name eq '$/'
            && self.IMPL-LANGUAGE-REVISION >= 3;
        nqp::bindattr(self, RakuAST::ContainerCreator, '$!container-descriptor',
            RakuAST::IMPL::Containers.create-descriptor(
                :$of, :$default, :$dynamic, :name(self.lexical-name),
                :$isolated-match));

        Nil
    }

    method IMPL-CONTAINER-DESCRIPTOR(Mu $of) {
        self.IMPL-CALCULATE-TYPES($of) unless $!initialized;

        self.container-descriptor
    }

    method IMPL-CONTAINER-TYPE(Mu $of) {
        self.IMPL-CALCULATE-TYPES($of) unless $!initialized;

        self.container-type
    }

    method IMPL-BIND-CONSTRAINT(Mu $of) {
        self.IMPL-CALCULATE-TYPES($of) unless $!initialized;

        self.bind-constraint
    }

    method IMPL-CONTAINER(Mu $of, Mu $cont-desc, Bool :$attribute) {
        self.IMPL-CALCULATE-TYPES($of) unless $!initialized;

        my $container-type := self.container-type;
        # Form the container.
        my str $sigil := self.sigil;
        if self.IMPL-HAS-EXPLICIT-CONTAINER-BASE-TYPE {
            self.IMPL-EXPLICIT-CONTAINER-BASE-TYPE
        }
        else {
            if $sigil ne '@' && $sigil ne '%' {
                if nqp::objprimspec($of) {
                    nqp::die("Natively typed state variables not yet implemented") if self.scope eq 'state';
                    return nqp::null unless $attribute;
                }

                $container-type := Scalar;
            }
            my $container := nqp::create($container-type);
            try nqp::bindattr($container, $!container-base-type, '$!descriptor', $cont-desc);

            nqp::bindattr(
              $container, $!container-base-type, '$!value', $cont-desc.default
            ) unless $sigil eq '@' || $sigil eq '%';

            $container
        }
    }

    # Resolve, by identity against the setting's Set/Bag/Mix, whether the
    # explicit base type is one of the immutable QuantHashes that must be
    # bare-created. A resolver is only available at BEGIN time, so the result
    # is cached for code-gen to read. In-setting resolution finds the core
    # type even when a same-named lexical shadows it; the lexical fallback
    # covers compiling the setting itself, where the core type is not yet in
    # an outer setting.
    method IMPL-RESOLVE-CONTAINER-VIVIFY-MODE(RakuAST::Resolver $resolver) {
        my $base := self.IMPL-EXPLICIT-CONTAINER-BASE-TYPE;
        my int $bare;
        for nqp::list_s('Set', 'Bag', 'Mix') -> str $type-name {
            my $name  := RakuAST::Name.from-identifier($type-name);
            my $found := $resolver.resolve-name-constant-in-setting($name)
                      || $resolver.resolve-name-constant($name);
            if $found && nqp::eqaddr($base, $found.compile-time-value) {
                $bare := 1;
                last;
            }
        }
        nqp::bindattr_i(self, RakuAST::ContainerCreator, '$!explicit-base-bare-create', $bare);
    }

    # QAST that produces a fresh instance of an explicit container base type.
    # Set/Bag/Mix keep pristine empty sentinels, so bare-create them rather
    # than run their .new (see issue #6246).
    method IMPL-EXPLICIT-CONTAINER-VIVIFY-QAST(RakuAST::IMPL::QASTContext $context, Mu $of, Mu :$class-qast) {
        my $wval := $class-qast;
        unless nqp::isconcrete($wval) {
            my $class := self.IMPL-CONTAINER-TYPE($of);
            $context.ensure-sc($class);
            $wval := QAST::WVal.new(:value($class));
        }
        $!explicit-base-bare-create
            ?? QAST::Op.new(:op<create>, $wval)
            !! QAST::Op.new(:op<callmethod>, :name<new>, $wval)
    }
}
