my class Regex { # declared in BOOTSTRAP
    # class Regex is Method {
    #     has Mu $!caps;
    #     has Mu $!nfa;
    #     has Mu $!alt_nfas;
    #     has Mu $!source;

    proto method ACCEPTS(|) { * }
    multi method ACCEPTS(Regex:D: Mu:U \a) { False }
    multi method ACCEPTS(Regex:U: Any \topic) { # use of Any on topic to force autothreading
        nqp::p6bool(nqp::istype(topic, self)) # so that all(@foo) ~~ Type works as expected
    }
    multi method ACCEPTS(Regex:D \SELF: Any \topic) {
        my $dollar_slash := nqp::getlexrelcaller(
            nqp::ctxcallerskipthunks(nqp::ctx()),
            '$/');
        $dollar_slash = SELF.(Cursor."!cursor_init"(topic, :c(0))).MATCH_SAVE;
    }

    multi method ACCEPTS(Regex:D \SELF: @a) {
        my $dollar_slash := nqp::getlexrelcaller(
            nqp::ctxcallerskipthunks(nqp::ctx()),
            '$/');
        for flat @a {
            $dollar_slash = SELF.(Cursor.'!cursor_init'($_, :c(0))).MATCH_SAVE;
            return $dollar_slash if $dollar_slash;
        }
        Nil;
    }
    multi method ACCEPTS(Regex:D \SELF: %h) {
        my $dollar_slash := nqp::getlexrelcaller(
            nqp::ctxcallerskipthunks(nqp::ctx()),
            '$/');
        for %h.keys {
            $dollar_slash = SELF.(Cursor.'!cursor_init'($_, :c(0))).MATCH_SAVE;
            return $dollar_slash if $dollar_slash;
        }
        Nil;
    }

    multi method Bool(Regex:D:) {
        my $dollar_slash := nqp::getlexrelcaller(
            nqp::ctxcallerskipthunks(nqp::ctx()),
            '$/');
        my $dollar_underscore := nqp::getlexrelcaller(
            nqp::ctxcallerskipthunks(nqp::ctx()),
            '$_');
        $dollar_slash = $dollar_underscore.match(self);
        $dollar_slash.Bool()
    }

    multi method gist(Regex:D:) {
        nqp::isnull($!source) ?? '' !! $!source
    }

    multi method perl(Regex:D:) {
        nqp::isnull($!source) ?? '' !! $!source
    }
}

# vim: ft=perl6 expandtab sw=4
