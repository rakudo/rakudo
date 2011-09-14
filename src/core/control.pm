my class Nil { ... }

my &THROW :=
    -> |$ {
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

my &RETURN-PARCEL := -> Mu \$parcel {
    my Mu $storage := nqp::getattr($parcel, Parcel, '$!storage');
    nqp::iseq_i(nqp::elems($storage), 0)
      ?? Nil
      !! (nqp::iseq_i(nqp::elems($storage), 1)
            ?? nqp::shift($storage)
            !! $parcel)
}

my &return-rw := -> |$ { 
    my $parcel := 
        &RETURN-PARCEL(nqp::p6parcel(pir::perl6_current_args_rpa__PP(), Nil));
    my Mu $return := pir::find_caller_lex__Ps('RETURN');
    nqp::isnull($return)
        ?? die "Attempt to return outside of any Routine"
        !! $return($parcel);
    $parcel
};
my &return := -> |$ {
    my $parcel := 
        &RETURN-PARCEL(nqp::p6parcel(pir::perl6_current_args_rpa__PP(), Nil));
    my Mu $return := pir::find_caller_lex__Ps('RETURN');
    nqp::isnull($return)
        ?? die "Attempt to return outside of any Routine"
        !! $return(pir::perl6_decontainerize__PP($parcel));
    $parcel
};

my &take-rw := -> |$ { 
    my $parcel := 
        &RETURN-PARCEL(nqp::p6parcel(pir::perl6_current_args_rpa__PP(), Nil));
    THROW($parcel, pir::const::CONTROL_TAKE) 
};
my &take := -> |$ { 
    my $parcel := 
        &RETURN-PARCEL(nqp::p6parcel(pir::perl6_current_args_rpa__PP(), Nil));
    THROW(pir::perl6_decontainerize__PP($parcel), 
          pir::const::CONTROL_TAKE) 
};

my &last := -> |$ { 
    my $parcel := 
        &RETURN-PARCEL(nqp::p6parcel(pir::perl6_current_args_rpa__PP(), Nil));
    THROW(pir::perl6_decontainerize__PP($parcel), 
          pir::const::CONTROL_LOOP_LAST) 
};

my &next := -> |$ { 
    my $parcel := 
        &RETURN-PARCEL(nqp::p6parcel(pir::perl6_current_args_rpa__PP(), Nil));
    THROW(pir::perl6_decontainerize__PP($parcel), 
          pir::const::CONTROL_LOOP_NEXT) 
};

my &redo := -> |$ { 
    my $parcel := 
        &RETURN-PARCEL(nqp::p6parcel(pir::perl6_current_args_rpa__PP(), Nil));
    THROW(pir::perl6_decontainerize__PP($parcel), 
          pir::const::CONTROL_LOOP_REDO) 
};

my &succeed := -> |$ { 
    my $parcel := 
        &RETURN-PARCEL(nqp::p6parcel(pir::perl6_current_args_rpa__PP(), Nil));
    THROW(pir::perl6_decontainerize__PP($parcel), 
          pir::const::CONTROL_BREAK) 
};

my &proceed := -> {
    THROW(Nil, pir::const::CONTROL_CONTINUE)
}

my &callwith := -> *@pos, *%named {
    my Mu $dispatcher := pir::perl6_find_dispatcher__P();
    $dispatcher.exhausted ?? Nil !!
        $dispatcher.call_with_args(|@pos, |%named)
};

my &nextwith := -> *@pos, *%named {
    my Mu $dispatcher := pir::perl6_find_dispatcher__P();
    my Mu $return     := pir::find_caller_lex__Ps('RETURN');
    unless $dispatcher.exhausted {
        nqp::isnull($return)
            ?? die "Attempt to return outside of any Routine"
            !! $return(pir::perl6_decontainerize__PP(
                $dispatcher.call_with_args(|@pos, |%named)))
    }
    Nil
};

my &callsame := -> {
    my Mu $dispatcher := pir::perl6_find_dispatcher__P();
    $dispatcher.exhausted ?? Nil !!
        $dispatcher.call_with_capture(
            pir::perl6_args_for_dispatcher__PP($dispatcher))
};

my &nextsame := -> {
    my Mu $dispatcher := pir::perl6_find_dispatcher__P();
    my Mu $return     := pir::find_caller_lex__Ps('RETURN');
    unless $dispatcher.exhausted {
        nqp::isnull($return)
            ?? die "Attempt to return outside of any Routine"
            !! $return(pir::perl6_decontainerize__PP(
                $dispatcher.call_with_capture(
                    pir::perl6_args_for_dispatcher__PP($dispatcher))))
    
    }
    Nil
};

my &lastcall := -> {
    pir::perl6_find_dispatcher__P().last();
    True
};

sub die(*@msg) is hidden_from_backtrace { pir::die__0P(@msg.join('')) }

sub eval(Str $code, :$lang = 'perl6') {
    my $caller_ctx := Q:PIR {
        $P0 = getinterp
        %r = $P0['context';1]
    };
    my $caller_lexpad := Q:PIR {
        $P0 = getinterp
        %r = $P0['lexpad';1]
    };
    my $result;
    my $success;
    try {
        my $compiler := pir::compreg__PS($lang);
        my $pbc      := $compiler.compile($code, :outer_ctx($caller_ctx));
        nqp::atpos($pbc, 0).set_outer_ctx($caller_ctx);
        $result := $pbc();
        $success = 1;
    }
    nqp::bindkey($caller_lexpad, '$!', $!);
    $success ?? $result !! Any # XXX fail($!)
}


sub exit($status = 0) {
    nqp::exit($status.Int);
    $status;
}

sub run(*@) {
    die 'run() is not yet implemented, please use shell() for now';
}

sub shell($cmd) {
    my $status = 255;
    try {
        $status = 
            nqp::p6box_i(
                pir::shr__0II(
                    pir::spawnw__Is(nqp::unbox_s($cmd)),
                    8));
    }
    $status;
}

sub sleep($seconds = $Inf) {         # fractional seconds also allowed
    my $time1 = time;
    if $seconds ~~ $Inf {
        pir::sleep__vN(1e16) while True;
    } else {
        pir::sleep__vN($seconds);
    }
    my $time2 = time;
    return $time2 - $time1;
}

sub QX($cmd) {
    my Mu $pio := pir::open__Pss(nqp::unbox_s($cmd), 'rp');
    fail "Unable to execute '$cmd'" unless $pio;
    $pio.encoding('utf8');
    my $result = nqp::p6box_s($pio.readall());
    $pio.close();
    $result;
}

sub EXHAUST(|$) {
    die "Attempt to return from exhausted Routine"
}
