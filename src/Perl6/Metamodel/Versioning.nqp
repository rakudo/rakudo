role Perl6::Metamodel::Versioning {
    has $!ver;
    has $!auth;
    has $!api;

    method ver( $XXX?) { $!ver  // nqp::null }
    method auth($XXX?) { $!auth // ''        }
    method api( $XXX?) { $!api  // ''        }

    method set_ver($XXX, $ver) {
        $!ver := $ver if $ver
    }
    method set_auth($XXX, $auth) { $!auth := $auth }
    method set_api( $XXX, $api ) { $!api  := $api  }
}

# vim: expandtab sw=4
