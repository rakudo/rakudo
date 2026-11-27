class Metamodel::JavaHOW
    does Metamodel::Naming
    does Metamodel::Stashing
    does Metamodel::AttributeContainer
    does Metamodel::TypePretense
    does Metamodel::MethodContainer
{
    my $archetypes := Metamodel::Archetypes.new( :nominal );
    method archetypes($obj?) {
        $archetypes
    }

    method is_composed($obj) {
        1
    }

    method parents($obj, :$all, :$excl) { [] }

    # While we normally end up locating methods through the method cache,
    # this is here as a fallback.
    method find_method($obj, $name, :$no_fallback, *%adverbs) {
        if nqp::can($obj.HOW, 'submethod_table') {
            my %submethods := $obj.HOW.submethod_table($obj);
            if nqp::existskey(%submethods, $name) {
                return %submethods{$name}
            }
        }
        if nqp::can($obj.HOW, 'method_table') {
            my %methods := $obj.HOW.method_table($obj);
            if nqp::existskey(%methods, $name) {
                return %methods{$name}
            }
        }
        nqp::null()
    }
}

# vim: expandtab sw=4
