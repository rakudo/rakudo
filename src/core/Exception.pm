my class Exception {
    has $!ex;

    method backtrace() { Backtrace.new(self) }

    method Str() {
        nqp::p6box_s(nqp::atkey($!ex, 'message'))
    }

    multi method Numeric(Exception:D:) {
        self.Str.Numeric()
    }

    method throw() is hidden_from_backtrace {
        nqp::bindattr(self, Exception, '$!ex', pir::new('Exception'))
            unless pir::defined($!ex);
        pir::setattribute__vPsP($!ex, 'payload', nqp::p6decont(self));
        pir::throw__0P($!ex)
    }
    method rethrow() is hidden_from_backtrace {
        pir::rethrow__0P($!ex)
    }

    method Bool() { False }
}

sub EXCEPTION(|$) {
    my Mu $parrot_ex := nqp::shift(pir::perl6_current_args_rpa__P());
    my Mu $payload   := nqp::atkey($parrot_ex, 'payload');
    if nqp::p6bool(pir::type_check__IPP($payload, Exception)) {
        nqp::bindattr($payload, Exception, '$!ex', $parrot_ex);
        $payload;
    } else {
        my $ex := nqp::create(Exception);
        nqp::bindattr($ex, Exception, '$!ex', $parrot_ex);
        $ex;
    }
}


do {
    sub is_runtime($bt) {
        for $bt.keys {
            try {
                return True if nqp::iseq_s($bt[$_]<sub>, 'eval')
                    && nqp::iseq_s(
                            pir::join(';', $bt[$_]<sub>.get_namespace.get_name),
                            'nqp;HLL;Compiler'
                    );
                return False if nqp::iseq_s($bt[$_]<sub>, 'compile')
                    && nqp::iseq_s(
                            pir::join(';', $bt[$_]<sub>.get_namespace.get_name),
                            'nqp;HLL;Compiler'
                    );
            }
        }
        return False;
    }

    my Mu $comp := pir::compreg__Ps('perl6');
    $comp.HOW.add_method($comp, 'handle-exception',
        method (|$) {
            my Mu $ex := nqp::atpos(
                pir::perl6_current_args_rpa__P(),
                1
            );
            if is_runtime($ex.backtrace) {
                my $e := EXCEPTION($ex);
                my Mu $err := pir::getstderr__P();
                $err.print: $e;
                $err.print: "\n";
                $err.print: Backtrace.new($e);
            } else {
                my Mu $err := pir::getstderr__P();
                $err.print: "===SORRY!===\n";
                $err.print: $ex;
                $err.print: "\n";
            }
            pir::exit(1);
            0;
        }
    );
}
