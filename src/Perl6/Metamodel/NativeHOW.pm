class Perl6::Metamodel::NativeHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Versioning
    does Perl6::Metamodel::Stashing
    does Perl6::Metamodel::MultipleInheritance
    does Perl6::Metamodel::C3MRO
{
    has $!composed;

    method new_type(:$name = '<anon>', :$repr = 'P6opaque', :$ver, :$auth) {
        my $metaclass := self.new(:name($name), :ver($ver), :auth($auth));
        self.add_stash(pir::repr_type_object_for__PPS($metaclass, $repr));
    }

    method compose($obj) {
        $!composed := 1;
    }
    
    method is_composed($obj) {
        $!composed
    }

    method type_check($obj, $checkee) {
        # The only time we end up in here is if the type check cache was
        # not yet published, which means the class isn't yet fully composed.
        # Just hunt through MRO.
        for self.mro($obj) {
            if $_ =:= $checkee {
                return 1;
            }
        }
        0
    }
}
