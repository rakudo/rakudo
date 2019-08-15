role Perl6::Metamodel::Versioning {
    has $!ver;
    has $!auth;
    has $!api;

    method ver($obj) { $!ver // nqp::null() }
    method auth($obj) { $!auth // '' }
    method api($obj) { $!api // '' }

    method set_ver($obj, $ver) {
        if $*COMPILING_CORE_SETTING && !$ver {
            $ver := nqp::getcomp('perl6').language_version;
        }
        $!ver := $ver if $ver
    }
    method set_auth($obj, $auth) { $!auth := $auth }
    method set_api($obj, $api) { $!api := $api }
}
