my class Code does Callable { # declared in BOOTSTRAP
    # class Code is Any {
    #     has $!do;                # Low level code object
    #     has $!signature;         # Signature object
    #     has $!compstuff;         # Place for the compiler to hang stuff

    multi method ACCEPTS(Code:D $self: Mu $topic) {
        $self.count ?? $self($topic) !! $self()
    }
    
    method arity(Code:D:) { $!signature.arity }
    
    method count(Code:D:) { $!signature.count }
    
    method signature(Code:D:) { $!signature }
    
    multi method Str(Code:D:) { self.name }

    method outer(Code:D:) {
        nqp::getcodeobj(nqp::p6staticouter($!do))
    }

    # returns an identifier for this code object
    # that is the same even for cloned closures
    method static_id(Code:D:) {
        nqp::p6box_i(nqp::where(nqp::getstaticcode($!do)));
    }
}
