# Sometimes, we see references to roles that provide parameters but
# do not fully resolve them. For example, in:
# 
#   class C does R[T] { }
#
# We need to represent R[T], but we cannot yet fully specialize the
# role because we don't have the first parameter to hand. We may also
# run into the issue where we have things like:
# 
#   sub foo(R[T] $x) { ... }
#   if $x ~~ R[T] { ... }
# 
# Where we clearly want to talk about a partial parameterization of a
# role and actually want to do so in a way distinct from a particular
# instantiation of it. This meta-object represents those "partial types"
# as both a way to curry on your way to a full specialization, but also
# as a way to do type-checking or punning.
class Perl6::Metamodel::CurriedRoleHOW
    does Perl6::Metamodel::RolePunning
    does Perl6::Metamodel::TypePretence
{
    has $!curried_role;
    has @!pos_args;
    has %!named_args;

    my $archetypes := Perl6::Metamodel::Archetypes.new( :nominal(1), :composable(1), :inheritalizable(1), :parametric(1) );
    method archetypes() {
        $archetypes
    }
    
    method new_type($curried_role, *@pos_args, *%named_args) {
        my $meta := self.new(:curried_role($curried_role), :pos_args(@pos_args),
            :named_args(%named_args));
        pir::repr_type_object_for__PPS($meta, 'Uninstantiable');
    }
    
    method specialize($obj, $first_arg) {
        $!curried_role.HOW.specialize($!curried_role, $first_arg,
            |@!pos_args, |%!named_args);
    }
    
    method name($obj) {
        $!curried_role.HOW.name($!curried_role)
    }
}
