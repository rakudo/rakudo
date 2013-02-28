class Perl6::Metamodel::VMArrayHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Versioning
    does Perl6::Metamodel::Stashing
    does Perl6::Metamodel::MethodContainer
    does Perl6::Metamodel::PrivateMethodContainer
{
    has $!element-type;
    has $!composed;

    method new(*%named) {
        nqp::findmethod(NQPMu, 'BUILDALL')(nqp::create(self), |%named)
    }

    method new_type(:$name = '<anon>', :$ver, :$auth) {
        my $metaclass := self.new();
        my $obj := pir::repr_type_object_for__PPS($metaclass, 'VMArray');
        self.add_stash($obj);
        $metaclass.set_name($obj, $name);
        $metaclass.set_ver($obj, $ver) if $ver;
        $metaclass.set_auth($obj, $auth) if $auth;
        $obj
    }
    
    method compose($obj) {
        if !$!composed && $!element-type {
            nqp::composetype($obj, hash(array => hash(type => $!element-type)));
            $!composed := 1;
        }
    }

    method parametrize($obj, $type) {
        $!element-type := $type;
    }
}
