class Perl6::Compiler::Package;

# This is the name of the HOW this package is based on.
has $!how;

# Table of methods we're adding (name => PAST::Block).
has $!methods;

# Table of attributes meta-data hashes. Maps name to
has $!attributes;

# Initializes a new Perl6::Compiler::Package instance and sets the how.
method new_with_how($name) {
    my $result := self.new();
    $result.how($name);
    return $result;
}

# Accessor for how.
method how($how?) {
    if $how { $!how := $how }
    $!how
}

# Accessor for methods hash.
method methods() {
    unless $!methods { $!methods := Q:PIR { %r = new ['Hash'] } }
    $!methods
}

# Accessor for attributes hash.
method attributes() {
    unless $!attributes { $!attributes := Q:PIR { %r = new ['Hash'] } }
    $!attributes
}
