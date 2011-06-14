my class Code {
    multi method ACCEPTS(Code:D $self: $topic) {
        $self.count ?? $self($topic) !! $self()
    }
    
    method arity() { $!signature.arity }
    
    method count() { $!signature.count }
    
    method signature() { $!signature }
    
    multi method Str(Code:D:) { self.name }
}
