# Coercion types, of the form TargetType(ConstraintType), are implemented with
# 6model parametrics. We create a single BEGIN-time "root" for the coercion
# type family, and the target and constraint types are stored as parameters.
# This means we get cross-compilation-unit interning "for free", as well as
# avoiding a meta-object instance per coercion type created.
class Perl6::Metamodel::DefiniteHOW
    #~ does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Documenting
    
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
    my $archetypes := Perl6::Metamodel::Archetypes.new(:definite);
    method archetypes() {
        $archetypes
    }

    #~ has @!mro;

    method new_type(:$base_type!, :$definite!) {
        my $root := nqp::parameterizetype((Perl6::Metamodel::DefiniteHOW.WHO)<root>,
            [$base_type, $definite]);
    }

    method name($definite_type) {
        if nqp::isnull(nqp::typeparameterized($definite_type)) {
            '?:?'
        }
        else {
            my $base_type := nqp::typeparameterat($definite_type, 0);
            my $definite  := nqp::typeparameterat($definite_type, 1);
            $base_type.HOW.name($base_type) ~ ':' ~ ($definite ?? 'D' !! 'U')
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
        nqp::typeparameterat($definite_type, 1)
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

    #~ # Should have the same methods of the base type that we refine.
    #~ # (For the performance win, work out a way to steal its method cache.)
    method find_method($definite_type, $name) {
        my $base_type := self.base_type($definite_type);
        $base_type.HOW.find_method($base_type, $name)
    }

    # Do check when we're on LHS of smartmatch (e.g. Even ~~ Int).
    method type_check($definite_type, $checkee) {
        my $base_type := self.base_type($definite_type);
        nqp::p6bool(nqp::istype($base_type, $checkee))
    }

    # Here we check the value itself (when on RHS on smartmatch).
    method accepts_type($definite_type, $checkee) {
        my $base_type := self.base_type($definite_type);
        my $definite  := self.definite($definite_type);
        nqp::p6bool(
            nqp::istype($checkee, $base_type) &&
            nqp::isconcrete($checkee) == $definite
        )
    }
}

BEGIN {
    my $root := nqp::newtype(Perl6::Metamodel::DefiniteHOW, 'Uninstantiable');
    nqp::settypehll($root, 'perl6');
    
    nqp::setparameterizer($root, sub ($type, $params) {
        # Re-use same HOW.
        my $thing := nqp::settypehll(nqp::newtype($type.HOW, 'Uninstantiable'), 'perl6');
        nqp::settypecheckmode($thing, 2)
    });
    (Perl6::Metamodel::DefiniteHOW.WHO)<root> := $root;
}
