# A meta-object written in Raku rather than in NQP, in the shape Inline::Perl5
# uses: it composes Metamodel::Naming and names the type from its own new_type,
# whose signature boxes the string it is handed. Its other methods then read
# $!name back expecting a value they can call methods on.
class MetamodelX::RakuLevelNameHOW
    does Metamodel::Naming
    does Metamodel::Stashing
{
    my $archetypes := Metamodel::Archetypes.new(:nominal(1));
    method archetypes(Mu $?) { $archetypes }

    method new_type(:$name) {
        my $how = self.new;
        my $type := Metamodel::Primitives.create_type($how);
        $how.set_name($type, $name);
        $how.add_stash($type);
        Metamodel::Primitives.configure_type_checking($type, (Any, Mu), :authoritative);
        $type
    }

    method compose(Mu $type) { $type }

    method name-chars(Mu $type) { $!name.chars }
}

my package EXPORTHOW {
    package DECLARE {
        constant rakuname = MetamodelX::RakuLevelNameHOW;
    }
}

# vim: expandtab shiftwidth=4
