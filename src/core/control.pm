my class X::Eval::NoSuchLang { ... }
my class PseudoStash { ... }
my class Label { ... }

my &THROW :=
    -> | {
        my Mu $args := nqp::p6argvmarray();
        my Mu $ex   := nqp::newexception();
        nqp::setpayload($ex, nqp::atpos($args, 0));
        nqp::setextype($ex, nqp::atpos($args, 1));
#?if parrot
        pir::setattribute__vPsP($ex, 'severity', pir::const::EXCEPT_NORMAL);
#?endif
        nqp::throw($ex);
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
    THROW($parcel, nqp::const::CONTROL_TAKE);
    $parcel
};
my &take := -> | { 
    my $parcel := 
        &RETURN-PARCEL(nqp::p6parcel(nqp::p6argvmarray(), Nil));
    THROW(nqp::p6recont_ro($parcel), nqp::const::CONTROL_TAKE);
    $parcel
};

my &last := -> | {
    my Mu $args := nqp::p6argvmarray();
    if nqp::islist($args) && nqp::istype(nqp::atpos($args, 0), Label) {
        nqp::atpos($args, 0).last()
    }
    else {
        my $parcel := nqp::decont(&RETURN-PARCEL(nqp::p6parcel($args, Nil)));
        THROW($parcel, nqp::const::CONTROL_LAST)
    }
};

my &next := -> | { 
    my Mu $args := nqp::p6argvmarray();
    if nqp::islist($args) && nqp::istype(nqp::atpos($args, 0), Label) {
        nqp::atpos($args, 0).next()
    }
    else {
        my $parcel := nqp::decont(&RETURN-PARCEL(nqp::p6parcel($args, Nil)));
        THROW($parcel, nqp::const::CONTROL_NEXT)
    }
};

my &redo := -> | { 
    my Mu $args := nqp::p6argvmarray();
    if nqp::islist($args) && nqp::istype(nqp::atpos($args, 0), Label) {
        nqp::atpos($args, 0).redo()
    }
    else {
        my $parcel := nqp::decont(&RETURN-PARCEL(nqp::p6parcel($args, Nil)));
        THROW($parcel, nqp::const::CONTROL_REDO)
    }
};

my &succeed := -> | { 
    my $parcel := 
        &RETURN-PARCEL(nqp::p6parcel(nqp::p6argvmarray(), Nil));
    THROW(nqp::decont($parcel), 
          nqp::const::CONTROL_SUCCEED)
};

my &proceed := -> {
    THROW(Nil, nqp::const::CONTROL_PROCEED)
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

my &samewith := -> *@pos, *%named {
    my $my   = callframe(1).my;
    my $self = $my<self>;
    die "Could not find 'self'" if !$self.DEFINITE;
    my $dispatcher = $my<&?ROUTINE>.dispatcher
      || die "Could not find dispatcher";
    $dispatcher( $self, |@pos, |%named );
}

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
    nqp::setmessage($ex, nqp::unbox_s(@msg.join('')));
    nqp::setextype($ex, nqp::const::CONTROL_WARN);
#?if parrot
    nqp::bindattr($ex, Exception, 'severity', nqp::p6box_i(pir::const::EXCEPT_WARNING));
#?endif
    nqp::throw($ex);
    0;
}

proto sub eval($, *%) {*}  # is DEPRECATED doesn't work in settings
multi sub eval(Str $code, :$lang = 'perl6', PseudoStash :$context) {
    DEPRECATED("'EVAL'");
    my $eval_ctx := nqp::getattr(nqp::decont($context // CALLER::), PseudoStash, '$!ctx');
    my $?FILES   := 'eval_' ~ (state $no)++;
    my $compiler := nqp::getcomp($lang);
    X::Eval::NoSuchLang.new(:$lang).throw
        if nqp::isnull($compiler);
    my $compiled := $compiler.compile($code, :outer_ctx($eval_ctx), :global(GLOBAL));
    nqp::forceouterctx(nqp::getattr($compiled, ForeignCode, '$!do'), $eval_ctx);
    $compiled();
}
proto sub EVAL($, *%) {*}
multi sub EVAL(Str $code, :$lang = 'perl6', PseudoStash :$context) {
    my $eval_ctx := nqp::getattr(nqp::decont($context // CALLER::), PseudoStash, '$!ctx');
    my $?FILES   := 'eval_' ~ (state $no)++;
    my $compiler := nqp::getcomp($lang);
    X::Eval::NoSuchLang.new(:$lang).throw
        if nqp::isnull($compiler);
    my $compiled := $compiler.compile($code, :outer_ctx($eval_ctx), :global(GLOBAL));
    nqp::forceouterctx(nqp::getattr($compiled, ForeignCode, '$!do'), $eval_ctx);
    $compiled();
}


sub exit($status = 0) {
    $_() for nqp::hllize(nqp::getcurhllsym('@END_PHASERS'));
    nqp::exit(nqp::unbox_i($status.Int));
    $status;
}

my class Proc::Status { ... }

sub run(*@args ($, *@)) {
    my $status = Proc::Status.new( :exit(255) );
    my Mu $hash-with-containers := nqp::getattr(%*ENV, EnumMap, '$!storage');
    my Mu $hash-without         := nqp::hash();
    my Mu $enviter := nqp::iterator($hash-with-containers);
    my $envelem;
    while $enviter {
        $envelem := nqp::shift($enviter);
        nqp::bindkey($hash-without, nqp::iterkey_s($envelem), nqp::decont(nqp::iterval($envelem)))
    }
    my Mu $args-without := nqp::list();
    for @args.eager {
        nqp::push($args-without, nqp::decont($_));
    }
    try {
        $status.status( nqp::p6box_i( nqp::spawn($args-without, $*CWD.Str, $hash-without) ) );
    }
    $status
}

sub shell($cmd) {
    my $status = Proc::Status.new( :exit(255) );
    my Mu $hash-with-containers := nqp::getattr(%*ENV, EnumMap, '$!storage');
    my Mu $hash-without         := nqp::hash();
    my Mu $enviter := nqp::iterator($hash-with-containers);
    my $envelem;
    while $enviter {
        $envelem := nqp::shift($enviter);
        nqp::bindkey($hash-without, nqp::iterkey_s($envelem), nqp::decont(nqp::iterval($envelem)))
    }
    try {
        $status.status( nqp::p6box_i(nqp::shell($cmd, $*CWD.Str, $hash-without)) );
    }
    $status
}

# XXX: Temporary definition of $Inf and $NaN until we have constants ava
# need to come pretty early, because we use it in lots of setting files
# constant Inf = ...
# constant NaN = ...
my $Inf = nqp::p6box_n(nqp::inf());
my $NaN = nqp::p6box_n(nqp::nan());
# EM 20130627 attempt at using constants failed during optimizing phase

sub QX($cmd) {
#?if parrot    
    nqp::chdir($*CWD);
    my Mu $pio := nqp::open(nqp::unbox_s($cmd), 'rp');    
    fail "Unable to execute '$cmd'" unless $pio;
    $pio.encoding('utf8');
    my $result = nqp::p6box_s($pio.readall());
    $pio.close();
    $result;
#?endif
#?if !parrot
    my Mu $hash-with-containers := nqp::getattr(%*ENV, EnumMap, '$!storage');
    my Mu $hash-without         := nqp::hash();
    my Mu $enviter := nqp::iterator($hash-with-containers);
    my $envelem;
    while $enviter {
        $envelem := nqp::shift($enviter);
        nqp::bindkey($hash-without, nqp::iterkey_s($envelem), nqp::decont(nqp::iterval($envelem)))
    }
    my Mu $pio := nqp::openpipe(nqp::unbox_s($cmd), $*CWD.Str, $hash-without, '');
    fail "Unable to execute '$cmd'" unless $pio;
    my $result = nqp::p6box_s(nqp::readallfh($pio));
    nqp::closefh($pio);
    $result;
#?endif    
}

sub EXHAUST(|) {
    X::ControlFlow::Return.new.throw();
}

# vim: ft=perl6 expandtab sw=4
