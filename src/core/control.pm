my class Nil { ... }
my class X::Eval::NoSuchLang { ... }
my class PseudoStash { ... }

my &THROW :=
    -> | {
        Q:PIR {
            .local pmc args, payload, type, severity, ex
            args = perl6_current_args_rpa
            payload  = args[0]
            type     = args[1]
            severity = args[2]
            unless null severity goto have_severity
            severity = box .EXCEPT_NORMAL
          have_severity:
            ex = root_new ['parrot';'Exception']
            setattribute ex, 'payload', payload
            setattribute ex, 'type', type
            setattribute ex, 'severity', severity
            throw ex
        };
        0
    };

my &RETURN-PARCEL := -> Mu \parcel {
    my Mu $storage := nqp::getattr(parcel, Parcel, '$!storage');
    nqp::iseq_i(nqp::elems($storage), 0)
      ?? Nil
      !! (nqp::iseq_i(nqp::elems($storage), 1)
            ?? nqp::shift($storage)
            !! parcel)
}

my &return-rw := -> | { 
    my $parcel := 
        &RETURN-PARCEL(nqp::p6parcel(nqp::p6argvmarray(), Nil));
    nqp::p6routinereturn($parcel);
    $parcel
};
my &return := -> | {
    my $parcel := 
        &RETURN-PARCEL(nqp::p6parcel(nqp::p6argvmarray(), Nil));
    nqp::p6routinereturn(nqp::p6recont_ro($parcel));
    $parcel
};

my &take-rw := -> | { 
    my $parcel := 
        &RETURN-PARCEL(nqp::p6parcel(nqp::p6argvmarray(), Nil));
    THROW($parcel, pir::const::CONTROL_TAKE);
    $parcel
};
my &take := -> | { 
    my $parcel := 
        &RETURN-PARCEL(nqp::p6parcel(nqp::p6argvmarray(), Nil));
    THROW(nqp::p6recont_ro($parcel), pir::const::CONTROL_TAKE);
    $parcel
};

my &last := -> | { 
    my $parcel := 
        &RETURN-PARCEL(nqp::p6parcel(nqp::p6argvmarray(), Nil));
    THROW(nqp::decont($parcel), 
          pir::const::CONTROL_LOOP_LAST) 
};

my &next := -> | { 
    my $parcel := 
        &RETURN-PARCEL(nqp::p6parcel(nqp::p6argvmarray(), Nil));
    THROW(nqp::decont($parcel), 
          pir::const::CONTROL_LOOP_NEXT) 
};

my &redo := -> | { 
    my $parcel := 
        &RETURN-PARCEL(nqp::p6parcel(nqp::p6argvmarray(), Nil));
    THROW(nqp::decont($parcel), 
          pir::const::CONTROL_LOOP_REDO) 
};

my &succeed := -> | { 
    my $parcel := 
        &RETURN-PARCEL(nqp::p6parcel(nqp::p6argvmarray(), Nil));
    THROW(nqp::decont($parcel), 
          pir::const::CONTROL_BREAK) 
};

my &proceed := -> {
    THROW(Nil, pir::const::CONTROL_CONTINUE)
}

my &callwith := -> *@pos, *%named {
    my Mu $dispatcher := nqp::p6finddispatcher('callwith');
    $dispatcher.exhausted ?? Nil !!
        $dispatcher.call_with_args(|@pos, |%named)
};

my &nextwith := -> *@pos, *%named {
    my Mu $dispatcher := nqp::p6finddispatcher('nextwith');
    unless $dispatcher.exhausted {
        nqp::p6routinereturn(nqp::p6recont_ro(
            $dispatcher.call_with_args(|@pos, |%named)))
    }
    Nil
};

my &callsame := -> {
    my Mu $dispatcher := nqp::p6finddispatcher('callsame');
    $dispatcher.exhausted ?? Nil !!
        $dispatcher.call_with_capture(
            nqp::p6argsfordispatcher($dispatcher))
};

my &nextsame := -> {
    my Mu $dispatcher := nqp::p6finddispatcher('nextsame');
    unless $dispatcher.exhausted {
        nqp::p6routinereturn(nqp::p6recont_ro(
            $dispatcher.call_with_capture(
                nqp::p6argsfordispatcher($dispatcher))))
    }
    Nil
};

my &lastcall := -> {
    nqp::p6finddispatcher('lastcall').last();
    True
};

proto sub die(|) is hidden_from_backtrace {*};
multi sub die(Exception $e) is hidden_from_backtrace { $e.throw }
multi sub die($payload) is hidden_from_backtrace {
    X::AdHoc.new(:$payload).throw
}
multi sub die(*@msg) is hidden_from_backtrace {
    X::AdHoc.new(payload => @msg.join).throw
}

multi sub warn(*@msg) is hidden_from_backtrace {
    my $ex := nqp::newexception();
    nqp::bindattr($ex, Exception, 'message', @msg.join(''));
    nqp::bindattr($ex, Exception, 'type', nqp::p6box_i(pir::const::CONTROL_OK));
    nqp::bindattr($ex, Exception, 'severity', nqp::p6box_i(pir::const::EXCEPT_WARNING));
    nqp::throw($ex);
    0;
}

proto sub eval($, *%) {*}
multi sub eval(Str $code, :$lang = 'perl6', PseudoStash :$context) {
    my $eval_ctx := nqp::getattr(nqp::decont($context // CALLER::CALLER::), PseudoStash, '$!ctx');
    my $?FILES   := 'eval_' ~ (state $no)++;
    my $compiler := nqp::getcomp($lang);
    X::Eval::NoSuchLang.new(:$lang).throw
        if nqp::isnull($compiler);
    my $compiled := $compiler.compile($code, :outer_ctx($eval_ctx), :global(GLOBAL));
    nqp::forceouterctx(nqp::getattr($compiled, ForeignCode, '$!do'), $eval_ctx);
    $compiled();
}


sub exit($status = 0) {
    $_() for nqp::hllize(@*END_PHASERS);
    nqp::exit(nqp::unbox_i($status.Int));
    $status;
}

sub run(*@args ($, *@)) {
    my $error_code;
    try {
        $error_code = nqp::p6box_i(
            pir::spawnw__IP(
                nqp::getattr(
                    @args.eager,
                    List,
                    '$!items'
                )
            )
        ) +> 8;
        CATCH {
            default {
                $error_code = 1;
            }
        }
    }
    $error_code but !$error_code;
}

sub shell($cmd) {
    my $status = 255;
    try {
        $status = 
            nqp::p6box_i(
                nqp::bitshiftr_i(
                    pir::spawnw__Is(nqp::unbox_s($cmd)),
                    8));
    }
    $status;
}

# XXX: Temporary definition of $Inf and $NaN until we have constants ava
# need to come pretty early, because we use it in lots of setting files
# constant Inf = ...
# constant NaN = ...
my $Inf = nqp::p6box_n(pir::set__Ns('Inf'));
my $NaN = nqp::p6box_n(pir::set__Ns('NaN'));


sub sleep($seconds = $Inf) {         # fractional seconds also allowed
    my $time1 = time;
    if $seconds ~~ $Inf {
        nqp::sleep(1e16) while True;
    } else {
        nqp::sleep($seconds.Num);
    }
    my $time2 = time;
    return $time2 - $time1;
}

sub QX($cmd) {
    my Mu $pio := nqp::open(nqp::unbox_s($cmd), 'rp');
    fail "Unable to execute '$cmd'" unless $pio;
    $pio.encoding('utf8');
    my $result = nqp::p6box_s($pio.readall());
    $pio.close();
    $result;
}

sub EXHAUST(|) {
    X::ControlFlow::Return.new.throw();
}
