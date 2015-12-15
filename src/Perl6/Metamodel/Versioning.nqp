role Perl6::Metamodel::Versioning {
    has $!ver;
    has $!auth;

    method ver($obj) { $!ver // nqp::null() }
    method auth($obj) { $!auth // '' }

    method set_ver($obj, $ver) { $!ver := $ver }
    method set_auth($obj, $auth) { $!auth := $auth }
}
