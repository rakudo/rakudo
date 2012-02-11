my class Code does Callable {
    multi method ACCEPTS(Code:D $self: Mu $topic) {
        $self.count ?? $self($topic) !! $self()
    }
    
    method arity(Code:D:) { $!signature.arity }
    
    method count(Code:D:) { $!signature.count }
    
    method signature(Code:D:) { $!signature }
    
    multi method Str(Code:D:) { self.name }

    method outer(Code:D:) {
        pir::perl6_code_object_from_parrot_sub__PP($!do.get_outer())
    }

    # returns an identifier for this code object
    # that is the same even for cloned closures
    method static_id(Code:D:) {
        nqp::p6box_i(nqp::where($!do.get_lexinfo));
    }
}
