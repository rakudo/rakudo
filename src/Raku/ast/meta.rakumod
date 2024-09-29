# Anything doing RakuAST::Meta is capable of producing a meta-object. Note
# that meta-object is used in a very general sense: it's any object that
# models a program element that still exists until runtime. The meta-object
# of a class is actually its type object, not its HOW.
class RakuAST::Meta
  is RakuAST::CompileTimeValue
{
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

    # For when the user would be fine with a stubbed meta-object but may also
    # deal with objects that don't stub and instead just have a fully initialized
    # meta-object available.
    method stubbed-meta-object() {
        self.meta-object
    }

    method has-meta-object() {
        $!meta-object-produced || False
    }

    method compile-time-value() {
        self.meta-object
    }
}

# Anything doing RakuAST::StubbyMeta is not only capable of producing a
# meta-object, but can also produce a stub of one. This is important for
# cases where we have circularities (for example, a class has attributes and
# needs them when it is composed, but furthermore an attribute needs to
# reference the class; in this case the attribute will just want the stubbed
# meta-object of the class.
class RakuAST::StubbyMeta
  is RakuAST::Meta
{
    has Mu $!cached-stubbed-meta-object;
    has Bool $!stubbed-meta-object-produced;

    method stubbed-meta-object() {
        unless $!stubbed-meta-object-produced {
            nqp::bindattr(self, RakuAST::StubbyMeta, '$!cached-stubbed-meta-object',
                self.PRODUCE-STUBBED-META-OBJECT());
            nqp::bindattr(self, RakuAST::StubbyMeta, '$!stubbed-meta-object-produced', True);
        }
        $!cached-stubbed-meta-object
    }

    method has-stubbed-meta-object() {
        $!cached-stubbed-meta-object || False
    }

    method meta-object() {
        # Ensure we have the stubbed meta-object first, then delegate to our
        # parent to produce the full meta-object.
        self.stubbed-meta-object();
        nqp::findmethod(RakuAST::Meta, 'meta-object')(self)
    }

    method compile-time-value() {
        self.stubbed-meta-object
    }
}
