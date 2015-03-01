my class Failure {
    has $.exception;
    has $!handled;

    method new($exception) { self.bless(:$exception) }

    # TODO: should be Failure:D: multi just like method Bool,
    # but obscure problems prevent us from making Mu.defined
    # a multi. See http://irclog.perlgeek.de/perl6/2011-06-28#i_4016747
    method defined() {
        $!handled =1 if nqp::isconcrete(self);
        Bool::False;
    }
    multi method Bool(Failure:D:) { $!handled = 1; Bool::False; }

    method Int(Failure:D:)        { $!handled ?? 0   !! $!exception.throw; }
    method Num(Failure:D:)        { $!handled ?? 0e0 !! $!exception.throw; }
    method Numeric(Failure:D:)    { $!handled ?? 0e0 !! $!exception.throw; }
    multi method Str(Failure:D:)  { $!handled ?? ''  !! $!exception.throw; }
    multi method gist(Failure:D:) { $!handled ?? $.perl !! $!exception.throw; }

    Failure.^add_fallback(
        -> $, $ { True },
        method ($name) {
            $!exception.throw;
        }
    );
    method sink() is hidden_from_backtrace {
        $!exception.throw unless $!handled
    }
}

proto sub fail(|) is hidden_from_backtrace {*};
multi sub fail(Exception $e) is hidden_from_backtrace {
    die $e if $*FATAL;
    my $fail := Failure.new($e);
    my Mu $return := nqp::getlexcaller('RETURN');
    $return($fail) unless nqp::isnull($return);
    $fail
}
multi sub fail($payload) is hidden_from_backtrace {
    die $payload if $*FATAL;
    my $fail := Failure.new(X::AdHoc.new(:$payload));
    my Mu $return := nqp::getlexcaller('RETURN');
    $return($fail) unless nqp::isnull($return);
    $fail
}
multi sub fail(*@msg) is hidden_from_backtrace {
    my $payload = @msg == 1 ?? @msg[0] !! @msg.join;
    die $payload if $*FATAL;
    my $fail := Failure.new(X::AdHoc.new(:$payload));
    my Mu $return := nqp::getlexcaller('RETURN');
    $return($fail) unless nqp::isnull($return);
    $fail
}

multi sub die(Failure:D $failure) is hidden_from_backtrace {
    $failure.exception.throw
}
multi sub die(Failure:U) is hidden_from_backtrace {
    X::AdHoc('Failure').throw
}

# vim: ft=perl6 expandtab sw=4
