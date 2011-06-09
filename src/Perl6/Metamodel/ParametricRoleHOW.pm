class Perl6::Metamodel::ParametricRoleHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Versioning
    does Perl6::Metamodel::MethodContainer
    does Perl6::Metamodel::MultiMethodContainer
    does Perl6::Metamodel::AttributeContainer
    does Perl6::Metamodel::RoleContainer
    does Perl6::Metamodel::MultipleInheritance
    does Perl6::Metamodel::Stashing
    does Perl6::Metamodel::NonGeneric
{
    has $!composed;

    method new_type(:$name = '<anon>', :$ver, :$auth, :$repr) {
        my $metarole := self.new(:name($name), :ver($ver), :auth($auth));
        self.add_stash(pir::repr_type_object_for__PPS($metarole, 'Uninstantiable'));
    }
    
    method compose($obj) {
        $!composed := 1;
        $obj
    }
    
    method curry($obj, *@pos_args, *%named_args) {
        # XXX We really want to keep a cache here of previously
        # seen curryings.
        Perl6::Metamodel::CurriedRoleHOW.new_type(:curried_role($obj),
            :pos_args(@pos_args), |named_args(%named_args))
    }
    
    method specialize($obj, *@pos_args, *%named_args) {
        
    }
}
