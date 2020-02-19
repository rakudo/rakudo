# Anything doing RakuAST::Meta is capable of producing a meta-object.
class RakuAST::Meta is RakuAST::Node {
    has Mu $!cached-meta-object;
    has Bool $!meta-object-produced;

    method meta-object() {
        unless $!meta-object-produced {
            nqp::bindattr(self, RakuAST::Meta, '$!cached-meta-object',
                self.PRODUCE-META-OBJECT());
            nqp::bindattr(self, RakuAST::Meta, '$!meta-object-produced', True);
        }
        $!cached-meta-object
    }

    method has-meta-object() {
        $!cached-meta-object || False
    }
}
