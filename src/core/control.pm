my class X::ControlFlow::Return { ... }
my class X::Eval::NoSuchLang { ... }
my class X::Multi::NoMatch { ... }
my class X::NYI { ... }
my class PseudoStash { ... }
my class Label { ... }

sub THROW(int $type, Mu \arg) {
    my Mu $ex := nqp::newexception();
    nqp::setpayload($ex, arg);
    nqp::setextype($ex, $type);
    nqp::throw($ex);
    arg;
}
sub THROW-NIL(int $type) {
    my Mu $ex := nqp::newexception();
#    nqp::setpayload($ex, Nil);
    nqp::setextype($ex, $type);
    nqp::throw($ex);
    Nil
}

sub RETURN-LIST(Mu \list) is raw {
    my Mu $storage := nqp::getattr(list, List, '$!reified');
    nqp::iseq_i(nqp::elems($storage), 0)
      ?? Nil
      !! (nqp::iseq_i(nqp::elems($storage), 1)
            ?? nqp::shift($storage)
            !! list)
}

my &return-rw := -> | {
    my $list := RETURN-LIST(nqp::p6argvmarray());
    nqp::p6routinereturn($list);
    $list;
};
my &return := -> | {
    my $list := RETURN-LIST(nqp::p6argvmarray());
    nqp::p6routinereturn(nqp::p6recont_ro($list));
    $list;
};

# RT #122732 - control operator crossed continuation barrier
#?if jvm
my &take-rw := -> | {
    my $list := RETURN-LIST(nqp::p6argvmarray());
    THROW(nqp::const::CONTROL_TAKE, $list);
}
#?endif
#?if !jvm
proto sub take-rw(|) { * }
multi sub take-rw()   { die "take-rw without parameters doesn't make sense" }
multi sub take-rw(\x) { THROW(nqp::const::CONTROL_TAKE, x) }
multi sub take-rw(|) {
    my $list := RETURN-LIST(nqp::p6argvmarray());
    THROW(nqp::const::CONTROL_TAKE, $list);
}
#?endif

# RT #122732 - control operator crossed continuation barrier
#?if jvm
my &take := -> | {
    my $list := RETURN-LIST(nqp::p6argvmarray());
    THROW( nqp::const::CONTROL_TAKE, nqp::p6recont_ro($list) );
    $list;
}
#?endif
#?if !jvm
proto sub take(|) { * }
multi sub take()   { die "take without parameters doesn't make sense" }
multi sub take(\x) {
    my $ = THROW(nqp::const::CONTROL_TAKE, nqp::p6recont_ro(x));
    x
}
multi sub take(|) {
    my $list := RETURN-LIST(nqp::p6argvmarray());
    THROW( nqp::const::CONTROL_TAKE, nqp::p6recont_ro($list) );
    $list;
}
#?endif

proto sub goto(|) { * }
multi sub goto(Label:D \x) { x.goto }

proto sub last(|) { * }
multi sub last()           { THROW-NIL(nqp::const::CONTROL_LAST) }
multi sub last(Label:D \x) { x.last }

proto sub next(|) { * }
multi sub next()           { THROW-NIL(nqp::const::CONTROL_NEXT) }
multi sub next(Label:D \x) { x.next }

proto sub redo(|) { * }
multi sub redo()           { THROW-NIL(nqp::const::CONTROL_REDO) }
multi sub redo(Label:D \x) { x.redo }

proto sub succeed(|) { * }
multi sub succeed()   { THROW-NIL(nqp::const::CONTROL_SUCCEED) }
multi sub succeed(\x) { THROW(nqp::const::CONTROL_SUCCEED, nqp::decont(x)) }
multi sub succeed(|) {
    my $list := RETURN-LIST(nqp::p6argvmarray());
    THROW( nqp::const::CONTROL_SUCCEED, nqp::decont($list));
}

sub proceed() { THROW-NIL(nqp::const::CONTROL_PROCEED) }

my &callwith := -> |c {
    my Mu $dispatcher := nqp::p6finddispatcher('callwith');
    $dispatcher.exhausted ?? Nil !!
        $dispatcher.call_with_args(|c)
};

my &nextwith := -> |c {
    my Mu $dispatcher := nqp::p6finddispatcher('nextwith');
    unless $dispatcher.exhausted {
        nqp::p6routinereturn(nqp::p6recont_ro(
            $dispatcher.call_with_args(|c)))
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

sub samewith(|c) {
    my Mu $ctx := nqp::ctxcaller(nqp::ctx());
    until nqp::isnull($ctx) {
        my $caller := nqp::getcodeobj(nqp::ctxcode($ctx));
        if nqp::istype($caller, Routine) {
            my $dispatcher := $caller.?dispatcher || die "Could not find dispatcher";
            return nqp::istype($caller, Method)
              ?? $dispatcher(nqp::atkey($ctx, 'self') // $caller.package, |c)
              !! $dispatcher(|c);
        }
        $ctx := nqp::ctxouter($ctx);
    }
    die "Cannot use samewith outside of a routine";
}

sub leave(|) { X::NYI.new(feature => 'leave').throw }

sub emit(\value) {
    THROW(nqp::const::CONTROL_EMIT, nqp::p6recont_ro(value));
    value
}
sub done() {
    THROW(nqp::const::CONTROL_DONE, Nil);
    Nil
}

proto sub die(|) {*};
multi sub die() {
    my $stash  := CALLER::CALLER::;
    my $payload = $stash<$!>.DEFINITE ?? $stash<$!> !! "Died";
    $payload ~~ Exception
      ?? $payload.throw
      !! X::AdHoc.new(:$payload).throw
}
multi sub die(Exception:U $e) {
    X::AdHoc.new(:payload("Died with undefined " ~ $e.^name)).throw;
}
multi sub die($payload) {
    $payload ~~ Exception
      ?? $payload.throw
      !! X::AdHoc.new(:$payload).throw
}
multi sub die(|cap ( *@msg )) {
    X::AdHoc.from-slurpy(|cap).throw
}

multi sub warn(*@msg) {
    my $msg = @msg.join || "Warning: something's wrong";
    my $ex := nqp::newexception();
    nqp::setmessage($ex, nqp::unbox_s($msg));
    nqp::setextype($ex, nqp::const::CONTROL_WARN);
    nqp::throw($ex);
    0;
}

proto sub EVAL(Cool $code, :$lang = 'perl6', PseudoStash :$context) {
    # First look in compiler registry.
    my $compiler := nqp::getcomp($lang);
    if nqp::isnull($compiler) {
        # Try a multi-dispatch to another EVAL candidate. If that fails to
        # dispatch, map it to a typed exception.
        CATCH {
            when X::Multi::NoMatch {
                X::Eval::NoSuchLang.new(:$lang).throw
            }
        }
        return {*};
    }
    my $eval_ctx := nqp::getattr(nqp::decont($context // CALLER::), PseudoStash, '$!ctx');
    my $?FILES   := 'EVAL_' ~ (state $no)++;
    my $compiled := $compiler.compile($code.Stringy, :outer_ctx($eval_ctx), :global(GLOBAL));
    nqp::forceouterctx(nqp::getattr($compiled, ForeignCode, '$!do'), $eval_ctx);
    $compiled();
}
multi sub EVAL(Cool $code, Str :$lang where { ($lang // '') eq 'Perl5' }, PseudoStash :$context) {
    my $eval_ctx := nqp::getattr(nqp::decont($context // CALLER::), PseudoStash, '$!ctx');
    my $?FILES   := 'EVAL_' ~ (state $no)++;
    state $p5;
    unless $p5 {
        {
            require Inline::Perl5;
            CATCH {
                X::Eval::NoSuchLang.new(:$lang).throw;
            }
        }
        $p5 = ::("Inline::Perl5").default_perl5;
    }
    $p5.run($code);
}

proto sub EVALFILE($, *%) {*}
multi sub EVALFILE($filename, :$lang = 'perl6') {
    EVAL slurp($filename), :$lang, :context(CALLER::);
}

sub exit($status = 0) {
    state $exit;
    $exit = $status;

    once {
        Rakudo::Internals::THE_END();
        nqp::exit(nqp::unbox_i($exit.Int));
    }
    $exit;
}

constant Inf = nqp::p6box_n(nqp::inf());
constant NaN = nqp::p6box_n(nqp::nan());

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
    my Mu $iter  := nqp::iterator(nqp::getattr(hash,Map,'$!storage'));
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
