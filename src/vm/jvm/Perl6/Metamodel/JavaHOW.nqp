class Perl6::Metamodel::JavaHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Stashing
    does Perl6::Metamodel::TypePretense
{
    my $archetypes := Perl6::Metamodel::Archetypes.new( );
    method archetypes() {
        $archetypes
    }
    
    method is_composed($obj) {
        1
    }
}
