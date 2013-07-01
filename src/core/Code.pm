my class Code does Callable {
    multi method ACCEPTS(Code:D $self: Mu $topic) {
        $self.count ?? $self($topic) !! $self()
    }
    
    method arity(Code:D:) { $!signature.arity }
    
    method count(Code:D:) { $!signature.count }
    
    method signature(Code:D:) { $!signature }
    
    multi method Str(Code:D:) { self.name }

    method outer(Code:D:) {
        nqp::getcodeobj(nqp::findmethod($!do, 'get_outer')($!do))
    }

    # returns an identifier for this code object
    # that is the same even for cloned closures
    method static_id(Code:D:) {
        nqp::p6box_i(nqp::where(nqp::getstaticcode($!do)));
    }
}
