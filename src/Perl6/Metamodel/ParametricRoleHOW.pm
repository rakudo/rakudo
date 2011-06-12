my $concrete := Perl6::Metamodel::ConcreteRoleHOW;
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
    has $!body_block;

    method new_type(:$name = '<anon>', :$ver, :$auth, :$repr) {
        my $metarole := self.new(:name($name), :ver($ver), :auth($auth));
        self.add_stash(pir::repr_type_object_for__PPS($metarole, 'Uninstantiable'));
    }
    
    method set_body_block($obj, $block) {
        $!body_block := $block
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
        # Run the body block to get the type environment.
        my $type_env;
        my $error;
        try {
            $type_env := $!body_block(|@pos_args, |%named_args);
            CATCH {
                $error := $!
            }
        }
        if $error {
            pir::die("Could not instantiate role '" ~ self.name($obj) ~ "':\n$error")
        }
        
        # Create a concrete role.
        my $conc := $concrete.new_type(:parametrics([$obj]), :name(self.name($obj)));
        
        # Go through attributes, reifying as needed and adding to
        # the concrete role.
        for self.attributes($obj, :local(1)) {
            $conc.HOW.add_attribute($conc,
                $_.is_generic ?? $_.instantiate_generic($type_env) !! $_);
        }
        
        # Go through methods and instantiate them; we always do this
        # unconditionally, since we need the clone anyway.
        for self.methods($obj, :local(1)) {
            $conc.HOW.add_method($conc, $_.name, $_.instantiate_generic($type_env))
        }
        for self.multi_methods_to_incorporate($obj) {
            $conc.HOW.add_multi_method($conc, $_.name, $_.code.instantiate_generic($type_env))
        }
        
        # Roles down by this role need fully specializing also; all
        # they'll be missing is the target class (e.g. our first arg).
        for self.roles_to_compose($obj) {
            $conc.HOW.add_role($conc, $_.HOW.specialize($_, @pos_args[0]));
        }
        
        # XXX More to copy/instantiate
        
        $conc.HOW.compose($conc);
        return $conc;
    }
}
