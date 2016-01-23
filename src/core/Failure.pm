my class Failure is Nil {
    has $.exception;
    has $.backtrace;
#?if moar
    has int $!handled;
#?endif
#?if jvm
    has $!handled;
#?endif

    multi method new() {
        my $stash := CALLER::;
        my $payload = $stash<$!>.DEFINITE ?? $stash<$!> !! "Failed";
        self.bless(:exception( $payload ~~ Exception
          ?? $payload !! X::AdHoc.new(:$payload)
        ))
    }
    multi method new(Exception $exception) {
        self.bless(:$exception);
    }
    multi method new($payload) {
        self.bless(:exception( $payload ~~ Exception
          ?? $payload !! X::AdHoc.new(:$payload)
        ))
    }
    multi method new(|cap (*@msg)) {
         self.bless(:exception(X::AdHoc.from-slurpy(|cap)));
    }

    submethod BUILD (:$!exception) {
        $!backtrace = $!exception.backtrace() || Backtrace.new(8);
        $!exception.reset-backtrace;
    }

    # "Shouldn't happen."  We use note here because the dynamic scope in GC is likely meaningless.
    submethod DESTROY () { if not $!handled { note "WARNING: unhandled Failure detected in DESTROY:\n" ~ self.mess } }

    # Marks the Failure has handled (since we're now fatalizing it) and throws.
    method !throw(Failure:D:) {
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


#?if moar
    method Int(Failure:D:)        { $!handled ?? Int !! self!throw(); }
#?endif
#?if jvm
    method Int(Failure:D:)        { $!handled ?? 0   !! self!throw(); }
#?endif

    method Num(Failure:D:)        { $!handled ?? NaN !! self!throw(); }
    method Numeric(Failure:D:)    { $!handled ?? NaN !! self!throw(); }
    multi method Str(Failure:D:)  { $!handled ?? $.mess !! self!throw(); }
    multi method gist(Failure:D:) { $!handled ?? $.mess !! self!throw(); }
    multi method gist(Failure:U:) { '(' ~ self.^name ~ ')' }
    multi method perl(Failure:D:) { self.Mu::perl() }
    multi method perl(Failure:U:) { self.^name }
    method mess (Failure:D:) {
        "(HANDLED) " x $!handled ~ self.exception.message ~ "\n" ~ self.backtrace;
    }

    method sink(Failure:D:) {
        self!throw() unless $!handled
    }
    method CALL-ME(Failure:D: |) {
        self!throw()
    }
    method FALLBACK(Failure:D: *@) {
        self!throw()
    }
    method STORE(Failure:D: *@) {
        self!throw()
    }
}

proto sub fail(|) {*};
multi sub fail() {
    my $stash := CALLER::;
    my $payload = $stash<$!>.DEFINITE ?? $stash<$!> !! "Failed";

    my $fail := Failure.new( $payload ~~ Exception
      ?? $payload !! X::AdHoc.new(:$payload));

    my Mu $return := nqp::getlexrel(nqp::ctxcallerskipthunks(nqp::ctx()), 'RETURN');
    $return($fail) unless nqp::isnull($return);

    $fail.exception.throw
}
multi sub fail(Exception:U $e) {
    my $fail := Failure.new(
        X::AdHoc.new(:payload("Failed with undefined " ~ $e.^name))
    );
    my Mu $return := nqp::getlexrel(nqp::ctxcallerskipthunks(nqp::ctx()), 'RETURN');
    $return($fail) unless nqp::isnull($return);
    $fail.exception.throw
}
multi sub fail($payload) {
    my $fail := Failure.new( $payload ~~ Exception
      ?? $payload
      !! X::AdHoc.new(:$payload)
    );

    my Mu $return := nqp::getlexrel(nqp::ctxcallerskipthunks(nqp::ctx()), 'RETURN');
    $return($fail) unless nqp::isnull($return);

    $fail.exception.throw
}
multi sub fail(|cap (*@msg)) {
    my $fail := Failure.new(X::AdHoc.from-slurpy(|cap));
    my Mu $return := nqp::getlexrel(nqp::ctxcallerskipthunks(nqp::ctx()), 'RETURN');
    $return($fail) unless nqp::isnull($return);
    $fail.exception.throw
}
multi sub fail(Failure:U $f) {
    my $fail := Failure.new(
        X::AdHoc.new(:payload("Failed with undefined " ~ $f.^name))
    );
    my Mu $return := nqp::getlexrel(nqp::ctxcallerskipthunks(nqp::ctx()), 'RETURN');
    $return($fail) unless nqp::isnull($return);
    $fail.exception.throw
}
multi sub fail(Failure:D $fail) {
    my Mu $return := nqp::getlexrel(nqp::ctxcallerskipthunks(nqp::ctx()), 'RETURN');
    $return($fail) unless nqp::isnull($return);
    $fail.exception.throw
}

multi sub die(Failure:D $f) {
    $f.exception.throw
}
multi sub die(Failure:U $f) {
    X::AdHoc.new(:payload("Died with undefined " ~ $f.^name)).throw;
}

# vim: ft=perl6 expandtab sw=4
