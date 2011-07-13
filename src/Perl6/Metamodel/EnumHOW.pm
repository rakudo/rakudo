# This is the meta-object for an enumeration (declared with enum).
# It keeps hold of the enumeration values in an EnumMap, which is
# created at composition time. It supports having roles composed in,
# one or two of which presumably provide the core enum-ish methods.
class Perl6::Metamodel::EnumHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Stashing
    does Perl6::Metamodel::AttributeContainer
    does Perl6::Metamodel::MethodContainer
    does Perl6::Metamodel::MultiMethodContainer
    does Perl6::Metamodel::RoleContainer
    does Perl6::Metamodel::BaseType
    does Perl6::Metamodel::NonGeneric
    does Perl6::Metamodel::ParrotInterop
{
    # Hash representing enumeration keys to values.
    has %!values;
    
    method new_type(:$name!, :$base_type!) {
        my $meta := self.new(:name($name));
        my $obj  := pir::repr_type_object_for__PPS($meta, 'P6opaque');
        $meta.set_base_type($meta, $obj);
        self.add_stash($obj);
    }
    
    method add_enum_value($obj, $key, $value) {
        %!values{$key} := $value;
    }
    
    method compose($obj) {
    }
}
