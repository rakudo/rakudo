my class Code {
    method signature() { $!signature }
    
    multi method Str(Code:D:) { self.name }
}
