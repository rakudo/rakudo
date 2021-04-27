my class Failure is Nil {
    has $.exception;
    has $.backtrace;
#?if !jvm
    has int $!handled;
#?endif
#?if jvm
    has Int $!handled;   # alas, native int breaks on the JVM
#?endif

    method !SET-SELF(\exception) {
        $!exception := exception;
        $!backtrace := exception.backtrace || Backtrace.new(
#?if !js
            4
#?endif
#?if js
            5
#?endif
        );
        exception.reset-backtrace;
        self
    }

    multi method new(Failure:D:) { self!throw }
    multi method new(Failure:U:) {
        my $stash   := CALLER::LEXICAL::;
        my $payload := nqp::existskey($stash,'$!')
          ?? nqp::atkey($stash,'$!')
          !! "Failed";
        nqp::create(self)!SET-SELF(
          nqp::isconcrete($payload)
            ?? nqp::istype($payload,Exception)
              ?? $payload
              !! X::AdHoc.new(:$payload)
            !! X::AdHoc.new(:payload<Failed>)
        )
    }
    multi method new(Failure:U: Exception:D \exception) {
        nqp::create(self)!SET-SELF(exception)
    }
    multi method new(Failure:U: $payload) {
        nqp::create(self)!SET-SELF(X::AdHoc.new(:$payload))
    }
    multi method new(Failure:U: |cap (*@msg)) {
        nqp::create(self)!SET-SELF(X::AdHoc.from-slurpy(|cap))
    }

    submethod DESTROY () {
        note "WARNING: unhandled Failure detected in DESTROY. If you meant "
            ~ "to ignore it, you can mark it as handled by calling .Bool, "
            ~ ".so, .not, or .defined methods. The Failure was:\n" ~ self.mess
        unless $!handled;
    }

    # allow Failures to throw when they replace an Iterable
    multi method iterator(Failure:D:) { self!throw }
    multi method list(Failure:D:)     { self!throw }

    # Marks the Failure has handled (since we're now fatalizing it) and throws.
    method !throw(Failure:D:) {
        $!handled = 1;
        $!exception.throw($!backtrace);
    }

    # Turns out multidimensional lookups are one way to leak unhandled failures, so
    # we'll just propagate the initial failure much as we propagate Nil on methods.
    method AT-POS(|) { self }
    method AT-KEY(|) { self }

    multi method defined(Failure:D: --> False) { $!handled = 1 }

    multi method Bool(Failure:D: --> False) { $!handled = 1 }
    method handled() is rw {
        Proxy.new(
          FETCH => {
#?if !jvm
              nqp::hllbool($!handled)
#?endif
#?if jvm
              $!handled.Bool
#?endif
          },
          STORE => -> $, $value { $!handled = $value.Bool.Numeric }
       )
    }

    method Capture() {
        self.DEFINITE.not || $!handled
            ?? X::Cannot::Capture.new(what => self).throw
            !! self!throw
    }
    method Int(Failure:D:)        { $!handled ?? Int !! self!throw(); }
    method Num(Failure:D:)        { $!handled ?? NaN !! self!throw(); }
    method Numeric(Failure:D:)    { $!handled ?? NaN !! self!throw(); }

    method Set(Failure:D:)     { $!handled ?? Set.new(self)     !! self!throw }
    method SetHash(Failure:D:) { $!handled ?? SetHash.new(self) !! self!throw }
    method Bag(Failure:D:)     { $!handled ?? Bag.new(self)     !! self!throw }
    method BagHash(Failure:D:) { $!handled ?? BagHash.new(self) !! self!throw }
    method Mix(Failure:D:)     { $!handled ?? Mix.new(self)     !! self!throw }
    method MixHash(Failure:D:) { $!handled ?? MixHash.new(self) !! self!throw }

    multi method Str(Failure:D:)  { $!handled ?? $.mess !! self!throw(); }
    multi method gist(Failure:D:) { $!handled ?? $.mess !! self!throw(); }
    multi method gist(Failure:U:) { '(' ~ self.^name ~ ')' }
    multi method raku(Failure:D:) {
        $!handled ?? '&CORE::infix:<orelse>(' ~ self.Mu::raku ~ ', *.self)'
                  !! self.Mu::raku
    }
    multi method raku(Failure:U:) { self.^name }
    method mess (Failure:D:) {
        my $message = (try self.exception.message) // self.exception.^name ~ ' with no message';
        "(HANDLED) " x $!handled ~ "$message\n" ~ self.backtrace;
    }

    method sink(Failure:D:) {
        self!throw() unless $!handled
    }
    method self(Failure:D:) {
        self!throw() unless $!handled;
        self
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
multi sub fail(--> Nil) {
    my $stash := CALLER::LEXICAL::;
    my $payload = ($stash<$!>:exists && $stash<$!>.DEFINITE) ?? $stash<$!> !! "Failed";

    my $fail := Failure.new( $payload ~~ Exception
      ?? $payload !! X::AdHoc.new(:$payload));

    nqp::throwpayloadlexcaller(nqp::const::CONTROL_RETURN, $fail);
    CATCH { $fail.exception.throw }
}
multi sub fail(Exception:U $e --> Nil) {
    my $fail := Failure.new(
        X::AdHoc.new(:payload("Failed with undefined " ~ $e.^name))
    );
    nqp::throwpayloadlexcaller(nqp::const::CONTROL_RETURN, $fail);
    CATCH { $fail.exception.throw }
}
multi sub fail($payload --> Nil) {
    my $fail := Failure.new( $payload ~~ Exception
      ?? $payload
      !! X::AdHoc.new(:$payload)
    );
    nqp::throwpayloadlexcaller(nqp::const::CONTROL_RETURN, $fail);
    CATCH { $fail.exception.throw }
}
multi sub fail(|cap (*@msg) --> Nil) {
    my $fail := Failure.new(X::AdHoc.from-slurpy(|cap));
    nqp::throwpayloadlexcaller(nqp::const::CONTROL_RETURN, $fail);
    CATCH { $fail.exception.throw }
}
multi sub fail(Failure:U $f --> Nil) {
    my $fail := Failure.new(
        X::AdHoc.new(:payload("Failed with undefined " ~ $f.^name))
    );
    nqp::throwpayloadlexcaller(nqp::const::CONTROL_RETURN, $fail);
    CATCH { $fail.exception.throw }
}
multi sub fail(Failure:D $fail --> Nil) {
    $fail.handled = 0;
    nqp::throwpayloadlexcaller(nqp::const::CONTROL_RETURN, $fail);
    CATCH { $fail.exception.throw }
}

multi sub die(Failure:D $f --> Nil) {
    $f.exception.throw
}
multi sub die(Failure:U $f --> Nil) {
    X::AdHoc.new(:payload("Died with undefined " ~ $f.^name)).throw;
}

# vim: expandtab shiftwidth=4
