my class X::Eval::NoSuchLang { ... }
my class PseudoStash { ... }
my class Label { ... }

sub THROW(Mu \arg, int $type) {
    my Mu $ex := nqp::newexception();
    nqp::setpayload($ex, arg);
    nqp::setextype($ex, $type);
#?if parrot
    pir::setattribute__vPsP($ex, 'severity', pir::const::EXCEPT_NORMAL);
#?endif
    nqp::throw($ex);
    0
}

sub RETURN-PARCEL(Mu \parcel) is rw {
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

# RT #122732 - control operator crossed continuation barrier
#?if jvm
my &take-rw := -> | {
    my $parcel := &RETURN-PARCEL(nqp::p6parcel(nqp::p6argvmarray(), Nil));
    THROW($parcel, nqp::const::CONTROL_TAKE);
    $parcel
}
#?endif
#?if !jvm
proto sub take-rw(|) { * }
multi sub take-rw() {
    THROW(Nil, nqp::const::CONTROL_TAKE);
    Nil
}
multi sub take-rw(\x) {
    THROW(x, nqp::const::CONTROL_TAKE);
    x
}
multi sub take-rw(|) {
    my $parcel := &RETURN-PARCEL(nqp::p6parcel(nqp::p6argvmarray(), Nil));
    THROW($parcel, nqp::const::CONTROL_TAKE);
    $parcel
}
#?endif

# RT #122732 - control operator crossed continuation barrier
#?if jvm
my &take := -> | {
    my $parcel := &RETURN-PARCEL(nqp::p6parcel(nqp::p6argvmarray(), Nil));
    THROW(nqp::p6recont_ro($parcel), nqp::const::CONTROL_TAKE);
    $parcel
}
#?endif
#?if !jvm
proto sub take(|) { * }
multi sub take() {
    THROW(Nil, nqp::const::CONTROL_TAKE);
    Nil
}
multi sub take(\x) {
    THROW(nqp::p6recont_ro(x), nqp::const::CONTROL_TAKE);
    x
}
multi sub take(|) {
    my $parcel := &RETURN-PARCEL(nqp::p6parcel(nqp::p6argvmarray(), Nil));
    THROW(nqp::p6recont_ro($parcel), nqp::const::CONTROL_TAKE);
    $parcel
}
#?endif

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

proto sub succeed(|) { * }
multi sub succeed() {
    THROW(Nil, nqp::const::CONTROL_SUCCEED)
}
multi sub succeed(\x) {
    THROW(nqp::decont(x), nqp::const::CONTROL_SUCCEED)
}
multi sub succeed(|) {
    my $parcel := &RETURN-PARCEL(nqp::p6parcel(nqp::p6argvmarray(), Nil));
    THROW(nqp::decont($parcel), nqp::const::CONTROL_SUCCEED)
}

sub proceed() {
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
multi sub die($payload = "Died") is hidden_from_backtrace {
    X::AdHoc.new(:$payload).throw
}
multi sub die(*@msg) is hidden_from_backtrace {
    X::AdHoc.new(payload => @msg.join).throw
}

multi sub warn(*@msg) is hidden_from_backtrace {
    my $ex := nqp::newexception();
    nqp::setmessage($ex, nqp::unbox_s(@msg.join));
    nqp::setextype($ex, nqp::const::CONTROL_WARN);
#?if parrot
    nqp::bindattr($ex, Exception, 'severity', nqp::p6box_i(pir::const::EXCEPT_WARNING));
#?endif
    nqp::throw($ex);
    0;
}

proto sub EVAL($, *%) {*}
multi sub EVAL(Cool $code, :$lang = 'perl6', PseudoStash :$context) {
    my $eval_ctx := nqp::getattr(nqp::decont($context // CALLER::), PseudoStash, '$!ctx');
    my $?FILES   := 'EVAL_' ~ (state $no)++;
    my $compiler := nqp::getcomp($lang);
    X::Eval::NoSuchLang.new(:$lang).throw
        if nqp::isnull($compiler);
    my $compiled := $compiler.compile($code.Stringy, :outer_ctx($eval_ctx), :global(GLOBAL));
    nqp::forceouterctx(nqp::getattr($compiled, ForeignCode, '$!do'), $eval_ctx);
    $compiled();
}

sub exit($status = 0) {
    state $exit;
    $exit = $status;

    return if $++;  # already exiting

    THE_END();
    nqp::exit(nqp::unbox_i($exit.Int));
    $exit;
}

sub THE_END {
    my @END := nqp::p6list(nqp::getcurhllsym("@END_PHASERS"), Array, Mu);
    while @END.shift -> $end { $end() };
}

my class Proc::Status { ... }

sub run(*@args ($, *@)) {
    my $status = Proc::Status.new( :exit(255) );
    try {
        $status.status(nqp::p6box_i(nqp::spawn(
          CLONE-LIST-DECONTAINERIZED(@args),
          $*CWD.Str,
          CLONE-HASH-DECONTAINERIZED(%*ENV),
        )));
    }
    $status
}

sub shell($cmd) {
    my $status = Proc::Status.new( :exit(255) );
    try {
        $status.status(nqp::p6box_i(nqp::shell(
          $cmd,
          $*CWD.Str,
          CLONE-HASH-DECONTAINERIZED(%*ENV),
        )));
    }
    $status
}

constant Inf = nqp::p6box_n(nqp::inf());
constant NaN = nqp::p6box_n(nqp::nan());

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
    my Mu $pio := nqp::openpipe(
      nqp::unbox_s($cmd), $*CWD.Str, CLONE-HASH-DECONTAINERIZED(%*ENV), ''
    );
    fail "Unable to execute '$cmd'" unless $pio;
    my $result = nqp::p6box_s(nqp::readallfh($pio));
    nqp::closefh($pio);
    $result;
#?endif
}

sub EXHAUST(|) {
    X::ControlFlow::Return.new.throw();
}

# True if given array does not just contain defined objects of given type
sub NOT_ALL_DEFINED_TYPE(\values,\type) {
    for values {
        return True unless nqp::defined($_) && nqp::istype($_,type);
    }
    False;
}

sub CLONE-HASH-DECONTAINERIZED(\hash) {
    my Mu $clone := nqp::hash();
    my Mu $iter  := nqp::iterator(nqp::getattr(hash,EnumMap,'$!storage'));
    my $e;
    while $iter {
        $e := nqp::shift($iter);
        nqp::bindkey($clone, nqp::iterkey_s($e), nqp::decont(nqp::iterval($e)));
    }
    $clone;
}

sub CLONE-LIST-DECONTAINERIZED(*@list) {
    my Mu $list-without := nqp::list();
    nqp::push($list-without, nqp::decont(~$_)) for @list.eager;
    $list-without;
}

# vim: ft=perl6 expandtab sw=4
