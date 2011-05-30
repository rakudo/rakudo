my class Str {
    # XXX Should be multi method Str(Str:D $self:) { ... } so we don't
    # screw up the type object stringification.
    method Str() { self }
}
