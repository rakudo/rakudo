my class Exception {
    has $!ex;

    method Str() {
        nqp::p6box_s(nqp::atkey($!ex, 'message'))
    }

    method throw() {
        pir::throw__0P($!ex)
    }
    method rethrow() {
        pir::rethrow__0P($!ex)
    }
}

sub EXCEPTION(|$) {
    my $ex := nqp::create(Exception);
    nqp::bindattr($ex, Exception, '$!ex', 
        nqp::shift(pir::perl6_current_args_rpa__P()));
    $ex;
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
                say $e;
                say Backtrace.new($e);
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
