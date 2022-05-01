class Perl6::Metamodel::DefiniteHOW
    does Perl6::Metamodel::Documenting
    does Perl6::Metamodel::Nominalizable
{
    has $!archetypes;

    method archetypes() { $!archetypes }

    my class Definite { }
    my class NotDefinite { }

    method new(*%named) {
        nqp::findmethod(NQPMu, 'BUILDALL')(nqp::create(self), |%named)
    }

    # To serve definites it is sufficient for the meta class to know two values: the base type a definitie is applied
    # to; and defined/undefined constraint. Both values can be obtained from parameterization parameters. But to provide
    # archetype information of the base type (to act transparently as other nominalizables do) the metaobject itself
    # needs to be parameterized over archetypes of the underlying base type. This way we can have just 4 metaobjects to
    # serve the full range of all possible definite types. This brings us to the implementation where we need two-level
    # parameterization process:
    # 1. The metaclass is parameterized over coercive and generic archetypes. Then for the parameterized metaclass
    #    we create its instance and attach a new parameterizable root definite type to it.
    # 2. When we have a parameterized metaclass we take the root type attached to it and parameterize it with the base
    #    type and with Definite/NotDefinite.
    method new_type(:$base_type!, :$definite!) {
        my $root := nqp::parameterizetype(
            (Perl6::Metamodel::DefiniteHOW.WHO)<rootHOW>,
            [$base_type.HOW.archetypes.coercive, $base_type.HOW.archetypes.generic]).WHO<root>;
        my $type := nqp::parameterizetype($root, [$base_type, $definite ?? Definite !! NotDefinite]);
        nqp::setdebugtypename($type, self.name($type));
    }

    method name($definite_type) {
        if nqp::isnull(nqp::typeparameterized($definite_type)) {
            '?:?'
        }
        else {
            my $base_type := nqp::typeparameterat($definite_type, 0);
            my $definite  := nqp::typeparameterat($definite_type, 1);
            $base_type.HOW.name($base_type) ~ ':' ~ (nqp::eqaddr($definite, Definite) ?? 'D' !! 'U')
        }
    }

    method shortname($definite_type) {
        if nqp::isnull(nqp::typeparameterized($definite_type)) {
            '?:?'
        }
        else {
            my $base_type := nqp::typeparameterat($definite_type, 0);
            my $definite  := nqp::typeparameterat($definite_type, 1);
            $base_type.HOW.shortname($base_type) ~ ':' ~ (nqp::eqaddr($definite, Definite) ?? 'D' !! 'U')
        }
    }

    sub check_instantiated($definite_type) {
        nqp::die('Cannot perform this operation on an uninstantiated definite type')
            if nqp::isnull(nqp::typeparameterized($definite_type));
    }

    method base_type($definite_type) {
        check_instantiated($definite_type);
        nqp::typeparameterat($definite_type, 0)
    }

    method definite($definite_type) {
        check_instantiated($definite_type);
        nqp::eqaddr(nqp::typeparameterat($definite_type, 1), Definite) ?? 1 !! 0
    }

    method nominalize($obj) {
        my $base_type := $obj.HOW.base_type($obj);
        $base_type.HOW.archetypes.nominalizable
            ?? $base_type.HOW.nominalize($base_type)
            !! $base_type
    }

    #~ # Should have the same methods of the base type that we refine.
    #~ # (For the performance win, work out a way to steal its method cache.)
    method find_method($definite_type, $name) {
        my $base_type := self.base_type($definite_type);
        $base_type.HOW.find_method($base_type, $name)
    }

    method find_method_qualified($definite_type, $qtype, $name) {
        my $base_type := self.base_type($definite_type);
        $base_type.HOW.find_method_qualified($base_type, $qtype, $name)
    }

    # Do check when we're on LHS of smartmatch (e.g. Even ~~ Int).
    method type_check($definite_type, $checkee) {
        my $base_type := self.base_type($definite_type);
        nqp::hllboolfor(nqp::istype($base_type, $checkee), "Raku")
    }

    # Here we check the value itself (when on RHS on smartmatch).
    method accepts_type($definite_type, $checkee) {
        my $base_type := self.base_type($definite_type);
        my $definite  := self.definite($definite_type);
        nqp::hllboolfor(
            nqp::istype($checkee, $base_type) &&
            nqp::isconcrete($checkee) == $definite,
            "Raku"
        )
    }

    method instantiate_generic($definite_type, $type_env) {
        my $base_type := self.base_type($definite_type);
        return $definite_type unless $!archetypes.generic;
        self.new_type(
            base_type => $base_type.HOW.instantiate_generic($base_type, $type_env),
            definite => self.definite($definite_type)
        )
    }

    # Methods needed by Perl6::Metamodel::Nominalizable
    method nominalizable_kind() { 'definite' }
    method !wrappee($obj) { self.base_type($obj) }
}

BEGIN {
    my $rootHOW := Perl6::Metamodel::DefiniteHOW.HOW.new_type(name => 'DefiniteHOW root', repr => 'Uninstantiable');
    nqp::settypehll($rootHOW, 'NQP');

    nqp::setparameterizer($rootHOW, sub ($typeHOW, $params) {
        my $metaclass := $typeHOW.HOW.new_type(name => 'Perl6::Metamodel::DefiniteHOW[' ~ $params[0] ~ "," ~ $params[1] ~ ']');
        $metaclass.HOW.add_parent($metaclass, Perl6::Metamodel::DefiniteHOW);
        $metaclass.HOW.compose($metaclass);
        my $metaobj := $metaclass.new;
        nqp::bindattr($metaobj, Perl6::Metamodel::DefiniteHOW, '$!archetypes',
            Perl6::Metamodel::Archetypes.new(:definite, :nominalizable, :coercive($params[0]), :generic($params[1])));
        my $root := nqp::newtype($metaobj, 'Uninstantiable');
        nqp::setdebugtypename(nqp::settypehll($root, 'Raku'), 'DefiniteHOW root');
        nqp::setparameterizer($root, sub ($type, $params) {
            my $definite := nqp::settypehll(nqp::newtype($type.HOW, 'Uninstantiable'), 'Raku');
            nqp::settypecheckmode($definite, 2)
        });
        $metaclass.WHO<root> := $root;
        $metaclass
    });
    (Perl6::Metamodel::DefiniteHOW.WHO)<rootHOW> := $rootHOW;
}

# vim: expandtab sw=4
