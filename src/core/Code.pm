my class Code does Callable {
    multi method ACCEPTS(Code:D $self: Mu $topic) {
        $self.count ?? $self($topic) !! $self()
    }
    
    method arity(Code:D:) { $!signature.arity }
    
    method count(Code:D:) { $!signature.count }
    
    method signature(Code:D:) { $!signature }
    
    multi method Str(Code:D:) { self.name }
}
