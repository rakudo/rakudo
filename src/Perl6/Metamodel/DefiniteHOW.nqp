class Perl6::Metamodel::DefiniteHOW
    #~ does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Documenting
    does Perl6::Metamodel::Nominalizable

    #~ does Perl6::Metamodel::MethodDelegation
    #~ does Perl6::Metamodel::TypePretense

    #~ does Perl6::Metamodel::Stashing
    #~ does Perl6::Metamodel::AttributeContainer
    #~ does Perl6::Metamodel::MethodContainer
    #~ does Perl6::Metamodel::MultiMethodContainer
    #~ does Perl6::Metamodel::RoleContainer
    #~ does Perl6::Metamodel::BaseType
    #~ does Perl6::Metamodel::MROBasedMethodDispatch
    #~ does Perl6::Metamodel::MROBasedTypeChecking
    #~ does Perl6::Metamodel::BUILDPLAN
    #~ does Perl6::Metamodel::BoolificationProtocol
    #~ does Perl6::Metamodel::REPRComposeProtocol
    #~ does Perl6::Metamodel::InvocationProtocol
{
    my $archetypes := Perl6::Metamodel::Archetypes.new(:definite, :nominalizable(1));
    method archetypes() {
        $archetypes
    }

    #~ has @!mro;

    my class Definite { }
    my class NotDefinite { }

    method new_type(:$base_type!, :$definite!) {
        my $root := nqp::parameterizetype((Perl6::Metamodel::DefiniteHOW.WHO)<root>,
            [$base_type, $definite ?? Definite !! NotDefinite]);
        nqp::setdebugtypename($root, self.name($root));
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

    #~ # Our MRO is just that of base type.
    #~ method mro($obj) {
        #~ unless @!mro {
            #~ @!mro[0] := $obj;
            #~ for $!base_type.HOW.mro($!base_type) {
                #~ @!mro.push($_);
            #~ }
        #~ }
        #~ @!mro
    #~ }

    #~ method parents($obj, :$local, :$excl, :$all) {
        #~ my @parents := [$!base_type];
        #~ unless $local {
            #~ for $!base_type.HOW.parents($!base_type, :excl($excl), :all($all)) {
                #~ @parents.push($_);
            #~ }
        #~ }
        #~ @parents
    #~ }

    method nominalize($obj) {
        my $base_type := $obj.HOW.base_type($obj);
        $base_type.HOW.archetypes.nominal ??
            $base_type !!
            $base_type.HOW.nominalize($base_type)
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

    # Methods needed by Perl6::Metamodel::Nominalizable
    method nominalizable_kind() { 'definite' }
    method !wrappee($obj) { self.base_type($obj) }
}

BEGIN {
    my $root := nqp::newtype(Perl6::Metamodel::DefiniteHOW, 'Uninstantiable');
    nqp::setdebugtypename(nqp::settypehll($root, 'Raku'), 'DefiniteHOW root');

    nqp::setparameterizer($root, sub ($type, $params) {
        # Re-use same HOW.
        my $thing := nqp::settypehll(nqp::newtype($type.HOW, 'Uninstantiable'), 'Raku');
        nqp::settypecheckmode($thing, 2)
    });
    (Perl6::Metamodel::DefiniteHOW.WHO)<root> := $root;
}

# vim: expandtab sw=4
