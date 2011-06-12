my class Code {
    method arity() { $!signature.arity }
    
    method count() { $!signature.count }
    
    method signature() { $!signature }
    
    multi method Str(Code:D:) { self.name }
}
