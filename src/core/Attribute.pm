augment class Attribute {
    method has-accessor() {
        $!has_accessor ?? True !! False
    }
    method Str() { self.name }
}
