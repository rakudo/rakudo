my class Regex { # declared in BOOTSTRAP
    # class Regex is Method {
    #     has Mu $!caps;
    #     has Mu $!nfa;
    #     has Mu $!alt_nfas;
    #     has Mu $!source;

    # cache cursor initialization lookup
    my $cursor-init := Cursor.^can("!cursor_init").AT-POS(0);

    proto method ACCEPTS(|) { * }
    multi method ACCEPTS(Regex:D: Mu:U \a) {
        False
    }

    # use of Any on topic to force autothreading
    # so that all(@foo) ~~ Type works as expected
    multi method ACCEPTS(Regex:U: Any \topic) {
        nqp::p6bool(nqp::istype(topic, self))
    }

    multi method ACCEPTS(Regex:D \SELF: Any \topic) {
        nqp::decont(
          nqp::getlexrelcaller(nqp::ctxcallerskipthunks(nqp::ctx()),'$/') =
          nqp::stmts(
            (my \cursor := SELF.($cursor-init(Cursor, topic, :c(0)))),
            nqp::if(
              nqp::isge_i(nqp::getattr_i(cursor,Cursor,'$!pos'),0),
              cursor.MATCH,
              Nil
            )
          )
        )
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
        nqp::stmts(
          (my $ctx := nqp::ctx),
          nqp::until(
            nqp::isnull($ctx := nqp::ctxcallerskipthunks($ctx))
              || (my $underscore := nqp::getlexrelcaller($ctx,'$_')).DEFINITE,
            nqp::null
          ),
          nqp::if(
            nqp::isnull($ctx),
            False,
            nqp::stmts(
              (my $slash := nqp::getlexrelcaller($ctx,'$/')),
              ($slash = $underscore.match(self)).Bool
            )
          )
        )
    }

    multi method gist(Regex:D:) {
        nqp::ifnull($!source,'')
    }

    multi method perl(Regex:D:) {
        nqp::ifnull($!source,'')
    }
}

multi sub infix:<~~>(Mu \topic, Regex:D \matcher) {
    $/ := nqp::getlexdyn('$/');
    matcher.ACCEPTS(topic)
}

# vim: ft=perl6 expandtab sw=4
