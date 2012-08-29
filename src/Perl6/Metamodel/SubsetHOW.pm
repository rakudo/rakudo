class Perl6::Metamodel::SubsetHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Documenting
{
    # The subset type or nominal type that we refine.
    has $!refinee;
    
    # The block implementing the refinement.
    has $!refinement;

    my $archetypes := Perl6::Metamodel::Archetypes.new( :nominalizable(1) );
    method archetypes() {
        $archetypes
    }
    
    method new(*%named) {
        nqp::findmethod(NQPMu, 'BUILDALL')(nqp::create(self), |%named)
    }
    
    method BUILD(:$refinee, :$refinement) {
        $!refinee := $refinee;
        $!refinement := $refinement;
    }

    method new_type(:$name = '<anon>', :$refinee!, :$refinement!) {
        my $metasubset := self.new(:refinee($refinee), :refinement($refinement));
        my $type := pir::repr_type_object_for__PPS($metasubset, 'Uninstantiable');
        $metasubset.set_name($type, $name);
        pir::stable_set_type_check_mode__0PI($type, 2)
    }
    
    method set_of($obj, $refinee) {
        my $archetypes := $!refinee.HOW.archetypes;
        unless $archetypes.nominal || $archetypes.nominalizable {
            nqp::die("The 'of' type of a subset must either be a valid nominal " ~
                "type or a type that can provide one");
        }
        $!refinee := pir::nqp_decontainerize__PP($refinee);
    }
    
    method refinee($obj) {
        $!refinee
    }
    
    method refinement($obj) {
        $!refinement
    }
    
    method nominalize($obj) {
        $!refinee.HOW.archetypes.nominal ??
            $!refinee !!
            $!refinee.HOW.nominalize($!refinee)
    }
    
    # Should have the same methods of the (eventually nominal) type
    # that we refine. (For the performance win, work out a way to
    # steal its method cache.)
    method find_method($obj, $name) {
        $!refinee.HOW.find_method($!refinee, $name)
    }
    
    # Do check when we're on LHS of smartmatch (e.g. Even ~~ Int).
    method type_check($obj, $checkee) {
        pir::perl6_booleanize__PI(nqp::istrue($checkee.HOW =:= self) ||
            nqp::istype($!refinee, $checkee))
    }
    
    # Here we check the value itself (when on RHS on smartmatch).
    method accepts_type($obj, $checkee) {
        pir::perl6_booleanize__PI(
            nqp::istype($checkee, $!refinee) &&
            nqp::istrue($!refinement.ACCEPTS($checkee)))
    }
}
