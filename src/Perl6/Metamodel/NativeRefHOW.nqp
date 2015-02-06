class Perl6::Metamodel::NativeRefHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Documenting
    does Perl6::Metamodel::Versioning
    does Perl6::Metamodel::Stashing
    does Perl6::Metamodel::MultipleInheritance
    does Perl6::Metamodel::C3MRO
    does Perl6::Metamodel::MROBasedMethodDispatch
    does Perl6::Metamodel::MROBasedTypeChecking
{
    has $!type;
    has $!refkind;
    has $!composed;

    my $archetypes := Perl6::Metamodel::Archetypes.new( :nominal(1), :inheritable(1) );
    method archetypes() {
        $archetypes
    }

    method new(*%named) {
        nqp::findmethod(NQPMu, 'BUILDALL')(nqp::create(self), |%named)
    }

    method new_type(:$name = '<anon>', :$ver, :$auth) {
        my $metaclass := self.new();
        my $obj := nqp::settypehll(nqp::newtype($metaclass, 'NativeRef'), 'perl6');
        $metaclass.set_name($obj, $name);
        $metaclass.set_ver($obj, $ver) if $ver;
        $metaclass.set_auth($obj, $auth) if $auth;
        self.add_stash($obj);
    }

    method compose($obj) {
        self.compose_repr($obj);
        self.compute_mro($obj);
        self.publish_method_cache($obj);
        self.publish_type_cache($obj);
        $obj
    }

    method compose_repr($obj) {
        if !$!composed {
            my $info := nqp::hash();
            $info<nativeref> := nqp::hash();
            $info<nativeref><type> := nqp::decont($!type);
            $info<nativeref><refkind> := $!refkind // 'unknown';
            nqp::composetype($obj, $info);
        }
        $!composed := 1;
    }

    method is_composed($obj) {
        $!composed
    }

    method set_native_type($obj, $type) {
        $!type := $type;
    }

    method native_type($obj) {
        $!type
    }

    method set_ref_kind($obj, $refkind) {
        $!refkind := $refkind;
    }

    method ref_kind($obj) {
        $!refkind
    }

    method method_table($obj) { nqp::hash() }
    method submethod_table($obj) { nqp::hash() }
}
