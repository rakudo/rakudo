role Perl6::Metamodel::Versioning {
    has $!ver;
    has $!auth;
    has $!api;

    method ver($obj) { $!ver // nqp::null() }
    method auth($obj) { $!auth // '' }
    method api($obj) { $!api // '' }

    method set_ver($obj, $ver) {
        $!ver := $ver if $ver
    }
    method set_auth($obj, $auth) { $!auth := $auth }
    method set_api($obj, $api) { $!api := $api }
}

# vim: expandtab sw=4
