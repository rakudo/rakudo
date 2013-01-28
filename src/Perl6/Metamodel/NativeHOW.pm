class Perl6::Metamodel::NativeHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Documenting
    does Perl6::Metamodel::Versioning
    does Perl6::Metamodel::Stashing
    does Perl6::Metamodel::MultipleInheritance    
    does Perl6::Metamodel::C3MRO
    does Perl6::Metamodel::MROBasedMethodDispatch
    does Perl6::Metamodel::MROBasedTypeChecking
{
    has $!nativesize;
    has $!composed;

    my $archetypes := Perl6::Metamodel::Archetypes.new( :nominal(1), :inheritable(1) );
    method archetypes() {
        $archetypes
    }
    
    method new(*%named) {
        nqp::findmethod(NQPMu, 'BUILDALL')(nqp::create(self), |%named)
    }

    method new_type(:$name = '<anon>', :$repr = 'P6opaque', :$ver, :$auth) {
        my $metaclass := self.new(:nativesize(0));
        my $obj := pir::repr_type_object_for__PPS($metaclass, $repr);
        $metaclass.set_name($obj, $name);
        $metaclass.set_ver($obj, $ver) if $ver;
        $metaclass.set_auth($obj, $auth) if $auth;
        self.add_stash($obj);
    }

    method compose($obj) {
        self.compute_mro($obj);
        self.publish_method_cache($obj);
        self.publish_type_cache($obj);
        if !$!composed && $!nativesize {
            my $info := nqp::hash();
            $info<integer> := nqp::hash();
            $info<integer><bits> := nqp::unbox_i($!nativesize);
            nqp::composetype($obj, $info);
            #nqp::composetype($obj, hash(integer => hash(bits => $!nativesize)));
        }
        $!composed := 1;
    }
    
    method is_composed($obj) {
        $!composed
    }
    
    method set_nativesize($obj, $nativesize) {
        $!nativesize := $nativesize;
    }
    
    method nativesize($obj) {
        $!nativesize
    }
    
    method method_table($obj) { nqp::hash() }
    method submethod_table($obj) { nqp::hash() }
}
