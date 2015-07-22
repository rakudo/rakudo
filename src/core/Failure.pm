my class Failure {
    has $.exception;
    has $.backtrace;
    has $!handled;

    method new(Exception $exception) {
         self.bless(:$exception);
    }

    submethod BUILD (:$!exception) {
        $!backtrace = $!exception.backtrace() || Backtrace.new(9);
    }

    # "Shouldn't happen."  We use note here because the dynamic scope in GC is likely meaningless.
    submethod DESTROY () { if not $!handled { note "WARNING: unhandled Failure detected in DESTROY:\n" ~ self.mess } }

    # Marks the Failure has handled (since we're now fatalizing it) and throws.
    method !throw() {
        $!handled = 1;
        $!exception.throw($!backtrace);
    }

    # Turns out multidimensional lookups are one way to leak unhandled failures, so
    # we'll just propagate the initial failure much as we propagate Nil on methods.
    method AT-POS(|) { self }
    method AT-KEY(|) { self }

    # TODO: should be Failure:D: multi just like method Bool,
    # but obscure problems prevent us from making Mu.defined
    # a multi. See http://irclog.perlgeek.de/perl6/2011-06-28#i_4016747
    method defined() {
        $!handled = 1 if nqp::isconcrete(self);
        Bool::False;
    }
    multi method Bool(Failure:D:) { $!handled = 1; Bool::False; }

    method Int(Failure:D:)        { $!handled ?? 0   !! self!throw(); }
    method Num(Failure:D:)        { $!handled ?? 0e0 !! self!throw(); }
    method Numeric(Failure:D:)    { $!handled ?? 0e0 !! self!throw(); }
    multi method Str(Failure:D:)  { $!handled ?? ''  !! self!throw(); }
    multi method gist(Failure:D:) { $!handled ?? $.mess !! self!throw(); }
    method mess (Failure:D:) {
        self.exception.message ~ "\n" ~ self.backtrace;
    }

    method sink() {
        self!throw() unless $!handled
    }
    method CALL-ME(|) {
        self!throw()
    }
    method FALLBACK(*@) {
        self!throw()
    }
}

proto sub fail(|) {*};
multi sub fail($payload =
    (CALLER::CALLER::.EXISTS-KEY('$!') and CALLER::CALLER::('$!').DEFINITE)
     ?? CALLER::CALLER::('$!') !! 'Failed') {

    my Mu $return := nqp::getlexcaller('RETURN');

    # Strange behavior alert:
    # If you try to if(...) { $fail := ... } else { $fail := ... }
    # here it behaves sort of as if "use fatal" were in effect even
    # when it is not, except that's not what is going on because
    # "die" does not get hit AFAICT.  That took me 4 hours of
    # fumbling around to figure out what was wrong.
    if ($payload ~~ Exception) {
        my $fail := Failure.new($payload);
        $return($fail) unless nqp::isnull($return);
        $fail
    }
    else {
        my $fail := Failure.new(X::AdHoc.new(:$payload));
        $return($fail) unless nqp::isnull($return);
        $fail
    }
}
multi sub fail(|cap (*@msg)) {
    my $fail := Failure.new(X::AdHoc.from-slurpy(|cap));
    my Mu $return := nqp::getlexcaller('RETURN');
    $return($fail) unless nqp::isnull($return);
    $fail
}

multi sub die(Failure:D $failure) {
    $failure.exception.throw
}
multi sub die(Failure:U) {
    X::AdHoc('Failure').throw
}

# vim: ft=perl6 expandtab sw=4
