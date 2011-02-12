role Perl6::Metamodel::Versioning {
    has $!ver;
    has $!auth;

    method ver() { $!ver }
    method auth() { $!auth }
}
