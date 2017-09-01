class Perl6::Metamodel::SubsetHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Documenting
    does Perl6::Metamodel::Stashing
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
        my $type := nqp::settypehll(nqp::newtype($metasubset, 'Uninstantiable'), 'perl6');
        $metasubset.set_name($type, $name);
        nqp::settypecheckmode($type, 2);
        self.add_stash($type)
    }
    
    method set_of($obj, $refinee) {
        my $archetypes := $!refinee.HOW.archetypes;
        unless $archetypes.nominal || $archetypes.nominalizable {
            nqp::die("The 'of' type of a subset must either be a valid nominal " ~
                "type or a type that can provide one");
        }
        $!refinee := nqp::decont($refinee);
        if nqp::objprimspec($!refinee) {
            my %ex := nqp::gethllsym('perl6', 'P6EX');
            if nqp::existskey(%ex, 'X::NYI') {
                %ex{'X::NYI'}('Subsets of native types');
            }
            else {
                nqp::die("Subsets of native types NYI");
            }
        }
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
        nqp::p6bool(nqp::istrue($checkee.HOW =:= self) ||
            nqp::istype($!refinee, $checkee))
    }
    
    # Here we check the value itself (when on RHS on smartmatch).
    method accepts_type($obj, $checkee) {
        nqp::p6bool(
            nqp::istype($checkee, $!refinee) &&
            nqp::istrue($!refinement.ACCEPTS($checkee)))
    }
}
