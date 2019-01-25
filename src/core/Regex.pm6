my class Regex { # declared in BOOTSTRAP
    # class Regex is Method
    #     has @!caps;
    #     has Mu $!nfa;
    #     has %!alt_nfas;
    #     has str $!source;
    #     has Mu $!topic;
    #     has Mu $!slash;

    proto method ACCEPTS(|) {*}
    multi method ACCEPTS(Regex:D: Mu:U \a) {
        False
    }

    # use of Any on topic to force autothreading
    # so that all(@foo) ~~ Type works as expected
    multi method ACCEPTS(Regex:U: Any \topic) {
        nqp::hllbool(nqp::istype(topic, self))
    }

    # Create a braid and fail cursor that we can use with all the normal,
    # "boring", regex matches that are on the Regex type. This saves them
    # being created every single time.
    my $cursor := Match.'!cursor_init'('');
    my $braid := $cursor.braid;
    my $fail_cursor := $cursor.'!cursor_start_cur'();

    multi method ACCEPTS(Regex:D \SELF: Any \topic) {
        nqp::decont(
          nqp::getlexrelcaller(nqp::ctxcallerskipthunks(nqp::ctx()),'$/') =
          nqp::stmts(
            (my \cursor := SELF.(Match.'!cursor_init'(topic, :c(0), :$braid, :$fail_cursor))),
            nqp::if(
              nqp::isge_i(nqp::getattr_i(cursor,Match,'$!pos'),0),
              cursor.MATCH,
              Nil
            )
          )
        )
    }

#?if !jvm
    multi method ACCEPTS(Regex:D \SELF: Uni:D \uni) {
        $/ := nqp::getlexrelcaller(nqp::ctxcallerskipthunks(nqp::ctx()),'$/');
        self.ACCEPTS(uni.Str)
    }
#?endif

    multi method ACCEPTS(Regex:D \SELF: @a) {
        SELF!ACCEPT-ITERATOR(
          nqp::getlexrelcaller(nqp::ctxcallerskipthunks(nqp::ctx()),'$/'),
          @a.iterator
        )
    }

    multi method ACCEPTS(Regex:D \SELF: %h) {
        SELF!ACCEPT-ITERATOR(
          nqp::getlexrelcaller(nqp::ctxcallerskipthunks(nqp::ctx()),'$/'),
          %h.keys.iterator
        )
    }

    method !ACCEPT-ITERATOR(Regex:D \SELF: \slash, Iterator:D \iter) {
        nqp::decont(slash =
          nqp::stmts(
            nqp::until(
              nqp::eqaddr(                                 # nothing to check?
                (my $pulled := iter.pull-one),IterationEnd)
                || nqp::isge_i(                            # valid match?
                     nqp::getattr_i(
                       (my \cursor := SELF.(Match.'!cursor_init'($pulled,:0c,:$braid,:$fail_cursor))),
                       Match,'$!pos'),
                   0),
              nqp::null
            ),
            nqp::if(
              nqp::eqaddr($pulled,IterationEnd),
              Nil,               # no match found
              cursor.MATCH       # found it!
            )
          )
        )
    }

    multi method Bool(Regex:D:) {
        nqp::isconcrete($!topic)
            ?? ($!slash = $!topic.match(self)).Bool
            !! False
    }

    multi method gist(Regex:D:) {
        nqp::ifnull($!source,'')
    }

    multi method perl(Regex:D:) {
        nqp::ifnull($!source,'')
    }

    method clone(Mu :$topic is raw, Mu :$slash is raw --> Regex) {
        nqp::p6bindattrinvres(
            nqp::p6bindattrinvres(self.Method::clone, Regex, '$!topic', $topic),
            Regex, '$!slash', $slash)
    }
}

multi sub infix:<~~>(Mu \topic, Regex:D \matcher) {
    $/ := nqp::getlexrelcaller(nqp::ctxcallerskipthunks(nqp::ctx()),'$/');
    matcher.ACCEPTS(topic)
}

# vim: ft=perl6 expandtab sw=4
