my class Failure is Nil {
    has $.exception;
    has $.backtrace;
#?if moar
    has int $!handled;
#?endif
#?if jvm
    has Int $!handled;   # alas, native int breaks on the JVM
#?endif

    method !SET-SELF($!exception) {
        $!backtrace = $!exception.backtrace || Backtrace.new(5);
        $!exception.reset-backtrace;
        self
    }

    multi method new() {
        my $stash := CALLER::;
        my $payload = $stash<$!>.DEFINITE ?? $stash<$!> !! "Failed";
        nqp::create(self)!SET-SELF(
          $payload ~~ Exception ?? $payload !! X::AdHoc.new(:$payload)
        )
    }
    multi method new(Exception:D \exception) {
        nqp::create(self)!SET-SELF(exception)
    }
    multi method new($payload) {
        nqp::create(self)!SET-SELF(X::AdHoc.new(:$payload))
    }
    multi method new(|cap (*@msg)) {
        nqp::create(self)!SET-SELF(X::AdHoc.from-slurpy(|cap))
    }

    submethod DESTROY () {
        note "WARNING: unhandled Failure detected in DESTROY. If you meant "
            ~ "to ignore it, you can mark it as handled by calling .Bool, "
            ~ ".so, .not, or .defined methods. The Failure was:\n" ~ self.mess
        unless $!handled;
    }

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
    method handled() is rw {
        Proxy.new(
          FETCH => {
#?if moar
              nqp::p6bool($!handled)
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
    multi method perl(Failure:D:) {
        $!handled ?? '&CORE::infix:<orelse>(' ~ self.Mu::perl ~ ', *.self)'
                  !! self.Mu::perl
    }
    multi method perl(Failure:U:) { self.^name }
    method mess (Failure:D:) {
        "(HANDLED) " x $!handled ~ self.exception.message ~ "\n" ~ self.backtrace;
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
    my $stash := CALLER::;
    my $payload = $stash<$!>.DEFINITE ?? $stash<$!> !! "Failed";

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

# vim: ft=perl6 expandtab sw=4
