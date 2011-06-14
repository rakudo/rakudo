class Perl6::Metamodel::SubsetHOW
    does Perl6::Metamodel::Naming
{
    # The subset type or nominal type that we refine.
    has $!refinee;
    
    # The block implementing the refinement.
    has $!refinement;

    method new_type(:$name = '<anon>', :$refinee!, :$refinement!) {
        my $metasubset := self.new(:name($name), :refinee($refinee),
            :refinement($refinement));
        my $type := pir::repr_type_object_for__PPS($metasubset, 'Uninstantiable');
        pir::stable_set_type_check_mode__0PI($type, 2)
    }
    
    method set_of($obj, $refinee) {
        $!refinee := $refinee
    }
    
    method refinee($obj) {
        $!refinee
    }
    
    method refinement($obj) {
        $!refinement
    }
    
    # Should have the same methods of the (eventually nominal) type
    # that we refine. (For the performance win, work out a way to
    # steal its method cache.)
    method find_method($obj, $name) {
        pir::find_method__PPS($!refinee, $name)
    }
    
    # Do check when we're on LHS of smartmatch (e.g. Even ~~ Int).
    method type_check($obj, $checkee) {
        pir::perl6_booleanize__PI(pir::istrue($checkee.HOW =:= self) ||
            pir::type_check__IPP($checkee, $!refinee))
    }
    
    # Here we check the value itself (when on RHS on smartmatch).
    method accepts_type($obj, $checkee) {
        pir::perl6_booleanize__PI(
            pir::type_check__IPP($checkee, $!refinee) &&
            pir::istrue__IP($!refinement.ACCEPTS($checkee)))
    }
}
