#- Metamodel::Versioning -------------------------------------------------------
role Perl6::Metamodel::Versioning {
    has     $!ver;
    has str $!auth;
    has str $!api;

    method ver( $XXX?) { $!ver  // nqp::null }
    method auth($XXX?) { $!auth // ''        }
    method api( $XXX?) { $!api  // ''        }

    method set_ver( $XXX, $ver)  { $!ver  := $ver  // nqp::null }
    method set_auth($XXX, $auth) { $!auth := $auth // ''        }
    method set_api( $XXX, $api ) { $!api  := $api  // ''        }

    # Make sure we handle anonymous classes threadsafely
    my $anon_id_lock := NQPLock.new;
    my int $anon_id;

    # Set full identity of class: name, ver, auth, api.  Take ver, auth, api
    # from the given named arguments hash, for convenience.
    method set_identity($target, %nameds) {
        self.set_name(
          $target,
          nqp::ifnull(
            nqp::atkey(%nameds, 'name'),
            "<anon|" ~ $anon_id_lock.protect({ ++$anon_id }) ~ ">"
          )
        );

        $!ver  :=             nqp::atkey(%nameds, 'ver' );
        $!auth := nqp::ifnull(nqp::atkey(%nameds, 'auth'), '');
        $!api  := nqp::ifnull(nqp::atkey(%nameds, 'api' ), '');
    }
}

# vim: expandtab sw=4
