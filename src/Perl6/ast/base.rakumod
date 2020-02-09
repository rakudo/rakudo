class RakuAST::Node {
    method type { Mu }

    # XXX temporary attributes/methods while we refactor to the new AST
    has Str $!named;
    method annotate($key, $value) {
    }
    method ann($key) {
        Mu
    }
    method wanted($value?) {
        1
    }
    method sunk($value?) {
        Mu
    }
    method returns($value?) {
        self.type
    }
    method has_compile_time_value() {
        0
    }
    method compile_time_value() {
        self.value
    }
    method flat() {
        Mu
    }
    method okifnil($vvalue?) {
        Mu
    }
    method named($value?) {
       if nqp::isconcrete($value) {
            nqp::bindattr(self, RakuAST::Node, '$!named', $value);
            $value
        }
        else {
            $!named || ''
        }
    }
    # XXX end temporaries
}
