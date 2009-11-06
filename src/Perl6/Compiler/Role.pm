class Perl6::Compiler::Role is Perl6::Compiler::Package;

# Holds the signautre for this parametric role, if any.
has $!signature;

# Accessor for signature.
method signature($signature?) {
    if pir::defined__IP($signature) { $!signature := $signature }
    $!signature
}
