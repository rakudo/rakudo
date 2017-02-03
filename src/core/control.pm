my class X::ControlFlow::Return { ... }
my class X::Eval::NoSuchLang { ... }
my class X::Multi::NoMatch { ... }
my class X::NYI { ... }
my class PseudoStash { ... }
my class Label { ... }
class CompUnit::DependencySpecification { ... }

sub THROW(int $type, Mu \arg) {
    my Mu $ex := nqp::newexception();
    nqp::setpayload($ex, arg);
    nqp::setextype($ex, $type);
    nqp::throw($ex);
    arg;
}
sub THROW-NIL(int $type --> Nil) {
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

proto sub return-rw(|) {*}
multi sub return-rw(--> Nil) {
    nqp::throwpayloadlexcaller(nqp::const::CONTROL_RETURN, Nil);
}
multi sub return-rw(Mu \x --> Nil) {
    nqp::throwpayloadlexcaller(nqp::const::CONTROL_RETURN, x);
}
multi sub return-rw(**@x is raw --> Nil) {
    nqp::throwpayloadlexcaller(nqp::const::CONTROL_RETURN, @x);
}
proto sub return(|) {*}
multi sub return(--> Nil) {
    nqp::throwpayloadlexcaller(nqp::const::CONTROL_RETURN, Nil);
}
multi sub return(Mu \x --> Nil) {
    nqp::throwpayloadlexcaller(nqp::const::CONTROL_RETURN, nqp::p6recont_ro(x));
}
multi sub return(**@x is raw --> Nil) {
    nqp::throwpayloadlexcaller(nqp::const::CONTROL_RETURN, @x);
}

proto sub take-rw(|) { * }
multi sub take-rw()   { die "take-rw without parameters doesn't make sense" }
multi sub take-rw(\x) { THROW(nqp::const::CONTROL_TAKE, x) }
multi sub take-rw(|) {
    THROW(nqp::const::CONTROL_TAKE,RETURN-LIST(nqp::p6argvmarray))
}

proto sub take(|) { * }
multi sub take()   { die "take without parameters doesn't make sense" }
multi sub take(\x) {
    THROW(nqp::const::CONTROL_TAKE, nqp::p6recont_ro(x))
}
multi sub take(|) {
    THROW(
      nqp::const::CONTROL_TAKE,
      nqp::p6recont_ro(RETURN-LIST(nqp::p6argvmarray))
    )
}

proto sub goto(|) { * }
multi sub goto(Label:D \x --> Nil) { x.goto }

proto sub last(|) { * }
multi sub last(--> Nil) { nqp::throwextype(nqp::const::CONTROL_LAST); Nil }
multi sub last(Label:D \x --> Nil) { x.last }

proto sub next(|) { * }
multi sub next(--> Nil) { nqp::throwextype(nqp::const::CONTROL_NEXT); Nil }
multi sub next(Label:D \x --> Nil) { x.next }

proto sub redo(|) { * }
multi sub redo(--> Nil) { nqp::throwextype(nqp::const::CONTROL_REDO); Nil }
multi sub redo(Label:D \x --> Nil) { x.redo }

proto sub succeed(|) { * }
multi sub succeed(--> Nil) { THROW-NIL(nqp::const::CONTROL_SUCCEED) }
multi sub succeed(\x --> Nil) { THROW(nqp::const::CONTROL_SUCCEED, x) }
multi sub succeed(| --> Nil) {
    THROW(nqp::const::CONTROL_SUCCEED,RETURN-LIST(nqp::p6argvmarray))
}

sub proceed(--> Nil) { THROW-NIL(nqp::const::CONTROL_PROCEED) }

sub callwith(|c) is raw {
    $/ := nqp::getlexcaller('$/');
    my Mu $dispatcher := nqp::p6finddispatcher('callwith');
    $dispatcher.exhausted ?? Nil !!
        $dispatcher.call_with_args(|c)
}

sub nextwith(|c) is raw {
    $/ := nqp::getlexcaller('$/');
    my Mu $dispatcher := nqp::p6finddispatcher('nextwith');
    nqp::throwpayloadlexcaller(nqp::const::CONTROL_RETURN, $dispatcher.exhausted
        ?? Nil
        !! $dispatcher.call_with_args(|c))
}

sub callsame() is raw {
    $/ := nqp::getlexcaller('$/');
    my Mu $dispatcher := nqp::p6finddispatcher('callsame');
    $dispatcher.exhausted ?? Nil !!
        $dispatcher.call_with_capture(
            nqp::p6argsfordispatcher($dispatcher))
}

sub nextsame() is raw {
    $/ := nqp::getlexcaller('$/');
    my Mu $dispatcher := nqp::p6finddispatcher('nextsame');
    nqp::throwpayloadlexcaller(nqp::const::CONTROL_RETURN, $dispatcher.exhausted
        ?? Nil
        !! $dispatcher.call_with_capture(nqp::p6argsfordispatcher($dispatcher)))
}

sub lastcall(--> True) {
    nqp::p6finddispatcher('lastcall').last();
}

sub nextcallee() {
    my Mu $dispatcher := nqp::p6finddispatcher('nextsame');
    $dispatcher.exhausted ?? Nil !! $dispatcher.shift_callee()
}

sub samewith(|c) {
    $/ := nqp::getlexcaller('$/');
    my Mu $ctx := nqp::ctxcaller(nqp::ctx());
    until nqp::isnull($ctx) {
        my $caller := nqp::getcodeobj(nqp::ctxcode($ctx));
        if nqp::istype($caller, Routine) {
            if $caller.multi {
                my $dispatcher := $caller.?dispatcher || die "Could not find dispatcher";
                return nqp::istype($caller, Method)
                  ?? $dispatcher(nqp::atkey($ctx, 'self') // $caller.package,|c)
                  !! $dispatcher(|c);
            }
            else {
                return $caller(|c);
            }
        }
        $ctx := nqp::ctxouter($ctx);
    }
    die "Cannot use samewith outside of a routine";
}

sub leave(|) { X::NYI.new(feature => 'leave').throw }

sub emit(\value --> Nil) {
    THROW(nqp::const::CONTROL_EMIT, nqp::p6recont_ro(value));
}
sub done(--> Nil) {
    THROW-NIL(nqp::const::CONTROL_DONE);
}

proto sub die(|) {*};
multi sub die(--> Nil) {
    my $stash  := CALLER::;
    my $payload = $stash<$!>.DEFINITE ?? $stash<$!> !! "Died";
    $payload ~~ Exception
      ?? $payload.throw
      !! X::AdHoc.new(:$payload).throw
}
multi sub die(Exception:U $e --> Nil) {
    X::AdHoc.new(:payload("Died with undefined " ~ $e.^name)).throw;
}
multi sub die($payload --> Nil) {
    $payload ~~ Exception
      ?? $payload.throw
      !! X::AdHoc.new(:$payload).throw
}
multi sub die(|cap ( *@msg ) --> Nil) {
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

proto sub EVAL(Cool $code, Str() :$lang = 'perl6', PseudoStash :$context, *%n) {
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
    my \mast_frames := nqp::hash();
    my $*CTXSAVE; # make sure we don't use the EVAL's MAIN context for the currently compiling compilation unit
    my $compiled := $compiler.compile(
        $code.Stringy,
        :outer_ctx($eval_ctx),
        :global(GLOBAL),
        :mast_frames(mast_frames),
    );
    if $*W and $*W.is_precompilation_mode() { # we are still compiling
        $*W.add_additional_frames(mast_frames);
    }
    nqp::forceouterctx(nqp::getattr($compiled, ForeignCode, '$!do'), $eval_ctx);
    $compiled();
}

multi sub EVAL(Cool $code, Str :$lang where { ($lang // '') eq 'Perl5' }, PseudoStash :$context) {
    my $eval_ctx := nqp::getattr(nqp::decont($context // CALLER::), PseudoStash, '$!ctx');
    my $?FILES   := 'EVAL_' ~ (state $no)++;
    state $p5;
    unless $p5 {
        {
            my $compunit := $*REPO.need(CompUnit::DependencySpecification.new(:short-name<Inline::Perl5>));
            GLOBAL.WHO.merge-symbols($compunit.handle.globalish-package);
            CATCH {
                #X::Eval::NoSuchLang.new(:$lang).throw;
                note $_;
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

constant Inf = nqp::p6box_n(nqp::inf());
constant NaN = nqp::p6box_n(nqp::nan());

sub CLONE-HASH-DECONTAINERIZED(\hash) {
    nqp::if(
      nqp::getattr(hash,Map,'$!storage').DEFINITE,
      nqp::stmts(
        (my $clone := nqp::hash),
        (my $iter  := nqp::iterator(nqp::getattr(hash,Map,'$!storage'))),
        nqp::while(
          $iter,
          nqp::bindkey($clone,
            nqp::iterkey_s(my $e := nqp::shift($iter)),
            nqp::if(
              nqp::defined(nqp::iterval($e)),
              nqp::decont(nqp::iterval($e)).Str,
              ''
            )
          )
        ),
        $clone
      ),
      nqp::hash
    )
}

sub CLONE-LIST-DECONTAINERIZED(*@list) {
    my Mu $list-without := nqp::list();
    nqp::push($list-without, nqp::decont(~$_)) for @list.eager;
    $list-without;
}

# vim: ft=perl6 expandtab sw=4
