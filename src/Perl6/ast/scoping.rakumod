# Done by anything that implies a lexical scope.
class RakuAST::LexicalScope is RakuAST::Node {
}

# Done by anything that is a declaration - that is, declares a symbol.
class RakuAST::Declaration is RakuAST::Node {
}

# Done by a declaration that installs a lexical symbol.
class RakuAST::Declaration::Lexical is RakuAST::Declaration {
    method lexical-name() {
        nqp::die("Lexical name not implemented for " ~ self.HOW.name(self))
    }
}

# A lexical declaration that comes from an external symbol (for example, the
# setting or an EVAL). XXX May break out the setting one.
class RakuAST::Declaration::External is RakuAST::Declaration::Lexical {
    has str $!lexical-name;

    method new(str $lexical-name) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration::External, '$!lexical-name', $lexical-name);
        $obj
    }

    method lexical-name() {
        nqp::getattr_s(self, RakuAST::Declaration::External, '$!lexical-name')
    }
}

# Done by anything that is a lookup of a symbol. May or may not need resolution
# at compile time.
class RakuAST::Lookup is RakuAST::Node {
    has RakuAST::Declaration $!resolution;

    method needs-resolution() { True }

    method is-resolved() {
        nqp::isconcrete($!resolution) ?? True !! False
    }

    method resolution() {
        nqp::isconcrete($!resolution)
            ?? $!resolution
            !! nqp::die('This element has not been resolved')
    }

    method set-resolution(RakuAST::Declaration $resolution) {
        nqp::bindattr(self, RakuAST::Lookup, '$!resolution', $resolution)
    }
}
