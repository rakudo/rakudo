# A metaclass that creates a closure during compose and installs it as a
# method, the way OO::Monitors wraps methods with lock handling. The
# closure's outer frame is the compose invocation, whose slurpy named
# arguments hold the compiler services, so the frame chain must stay
# serializable once composition is done.
class MetamodelX::ComposeAddsMethodHOW is Metamodel::ClassHOW {
    method compose(Mu \type) {
        my $tag := 'composed';
        self.add_method(type, 'compose-tag', sub ($self) { $tag });
        self.Metamodel::ClassHOW::compose(type);
    }
}
my package EXPORTHOW {
    package DECLARE {
        constant addsmethod = MetamodelX::ComposeAddsMethodHOW;
    }
}
