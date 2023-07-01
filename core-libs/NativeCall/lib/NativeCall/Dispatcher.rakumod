use NativeCall::Types;

use nqp;

my sub raku-nativecall-deproxy(Mu $capture is raw) {
    my $callee := nqp::captureposarg($capture, 1);
    # The resume init state drops the remover.
    nqp::dispatch('boot-syscall', 'dispatcher-set-resume-init-args',
        nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 0));

    # We then invoke the remover with the arguments (so need to drop the
    # original invokee).
    nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-code-constant',
        nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 1));
}

my sub raku-nativecall-deproxy-resume(Mu $capture is raw) {
    my $track_kind := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0);
    nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track_kind);
    my int $kind = nqp::captureposarg_i($capture, 0);

    if $kind == nqp::const::DISP_DECONT {
        my $orig-capture := nqp::dispatch('boot-syscall', 'dispatcher-get-resume-init-args');
        my $track_callee := nqp::dispatch('boot-syscall', 'dispatcher-track-arg',
            $orig-capture, 0);
        nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track_callee);
        my $callee := nqp::captureposarg($orig-capture, 0);
        my $capture-delegate := nqp::dispatch('boot-syscall',
            'dispatcher-insert-arg-literal-obj',
            nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 0), 0, $callee);
        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-nativecall-core', $capture-delegate);
    }
}

my $do := nqp::getattr(&raku-nativecall-deproxy, Code, '$!do');
nqp::forceouterctx($do, nqp::getattr(MY::, PseudoStash, '$!ctx'));
my $do-resume := nqp::getattr(&raku-nativecall-deproxy-resume, Code, '$!do');
nqp::forceouterctx($do-resume, nqp::getattr(MY::, PseudoStash, '$!ctx'));
nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-nativecall-deproxy', $do, $do-resume);

my $PROXY-READERS := nqp::gethllsym('Raku', 'PROXY-READERS');
my sub raku-nativecall(Mu $capture is raw) {
    my $track_callee := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0);
    nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track_callee);
    my $callee := nqp::captureposarg($capture, 0);
    $callee.setup;

    my Mu $args := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 0);
    my int $pos-args = nqp::captureposelems($args);
    my int $i = 0;

    my $non-scalar := nqp::list_i();
    while $i < $pos-args {
        if nqp::captureposprimspec($args, $i) == 0 {
            my $value := nqp::captureposarg($args, $i);
            if nqp::isconcrete_nd($value) &&
                nqp::iscont($value) && !nqp::istype_nd($value, Scalar) &&
                !(nqp::iscont_i($value) || nqp::iscont_u($value) || nqp::iscont_n($value) || nqp::iscont_s($value))
            {
                nqp::push_i($non-scalar, nqp::unbox_i($i));
            }
        }
        $i++;
    }
    if nqp::elems($non-scalar) {
        # Establish guards on types of all positionals, but not on the values
        # inside of them if they are Scalar containers; we just need to make
        # sure we have the appropriate tuple of Proxy vs non-Proxy for the
        # Proxy removal code we'll invoke.
        $i = 0;
        while $i < $pos-args {
            if nqp::captureposprimspec($args, $i) == 0 {
                my $track-arg := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $args, nqp::unbox_i($i));
                nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $track-arg);
            }
            $i++;
        }

        # Need to strip the Proxy arguments and then try again. Produce a
        # proxy reader code object to do so, insert it as the first arg,
        # and delegate to a dispatcher to manage reading the args and
        # then retrying with the outcome.
        my $reader := $PROXY-READERS.reader-for($args, $non-scalar);
        my $capture-delegate := nqp::dispatch('boot-syscall',
            'dispatcher-insert-arg-literal-obj', $capture, 0, nqp::getattr($reader, Code, '$!do'));
        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-nativecall-deproxy',
            $capture-delegate);
        return;
    }

    nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-nativecall-core', $capture);
}
$do := nqp::getattr(&raku-nativecall, Code, '$!do');
nqp::forceouterctx($do, nqp::getattr(MY::, PseudoStash, '$!ctx'));
nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-nativecall', $do);

my sub raku-nativecall-core(Mu $capture is raw) {
    my $callee := nqp::captureposarg($capture, 0);

    my Mu $args := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 0);
    my int $pos-args = nqp::captureposelems($args);
    my int $i = 0;
    while $i < $pos-args {
        # If it should be passed read only, and it's an object...
        if nqp::captureposprimspec($args, $i) == 0 {
            # If it's in a Scalar container...
            my $track-arg := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $args, nqp::unbox_i($i));
            nqp::dispatch('boot-syscall', 'dispatcher-guard-type', $track-arg);
            nqp::dispatch('boot-syscall', 'dispatcher-guard-concreteness', $track-arg);
            my $arg := nqp::captureposarg($args, $i);
            my $track-value;
            my $cstr = False;
            if nqp::isconcrete_nd($arg) && nqp::istype_nd($arg, Scalar) {
                # Read it from the container and pass it decontainerized.
                $track-value := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                    $track-arg, Scalar, '$!value');
                $args := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg',
                    nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $args, nqp::unbox_i($i)),
                    nqp::unbox_i($i), $track-value);
                $arg := nqp::decont($arg);
            }
            else {
                $track-value := $track-arg;
            }
            if nqp::isconcrete_nd($arg) && nqp::istype_nd($arg, Code) {
                $track-value := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                    $track-value, Code, '$!do');
                $args := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg',
                    nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $args, nqp::unbox_i($i)),
                    nqp::unbox_i($i), $track-value);
            }
            if nqp::isconcrete_nd($arg) && $arg.does(NativeCall::Types::ExplicitlyManagedString) {
                $cstr = True;
                $track-value := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                    $track-value, $arg.WHAT, '$!cstr');
                $args := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg',
                    nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $args, nqp::unbox_i($i)),
                    nqp::unbox_i($i), $track-value);
                $arg := nqp::getattr($arg, $arg.WHAT, '$!cstr');
                if nqp::isconcrete_nd($arg) && nqp::what_nd($arg) =:= Scalar {
                    $track-value := nqp::dispatch('boot-syscall', 'dispatcher-track-attr',
                        $track-value, Scalar, '$!value');
                    $args := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg',
                        nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $args, nqp::unbox_i($i)),
                        nqp::unbox_i($i), $track-value);
                    $arg := nqp::decont($arg);
                }
            }

            my $param = $callee.signature.params[$i];
            unless $param.rw or nqp::isrwcont($arg) {
                if $param.type ~~ Int or $param.type.REPR eq 'CPointer' {
                    if nqp::isconcrete_nd($arg) {
                        $track-value := nqp::dispatch('boot-syscall', 'dispatcher-track-unbox-int',
                            $track-value);
                        $args := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg',
                            nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $args, nqp::unbox_i($i)),
                            nqp::unbox_i($i), $track-value);
                    }
                    else {
                        $args := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-int',
                            nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $args, nqp::unbox_i($i)),
                            nqp::unbox_i($i), 0); # 0 or NULL for undefined args
                    }
                }
                elsif $param.type ~~ Num {
                    if nqp::isconcrete_nd($arg) {
                        $track-value := nqp::dispatch('boot-syscall', 'dispatcher-track-unbox-num',
                            $track-value);
                        $args := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg',
                            nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $args, nqp::unbox_i($i)),
                            nqp::unbox_i($i), $track-value);
                    }
                    else {
                        $args := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-num',
                            nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $args, nqp::unbox_i($i)),
                            nqp::unbox_i($i), NaN);
                    }
                }
                elsif $param.type ~~ Str and not $cstr {
                    if nqp::isconcrete_nd($arg) {
                        $track-value := nqp::dispatch('boot-syscall', 'dispatcher-track-unbox-str',
                            $track-value);
                        $args := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg',
                            nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $args, nqp::unbox_i($i)),
                            nqp::unbox_i($i), $track-value);
                    }
                    else {
                        $args := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-int',
                            nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $args, nqp::unbox_i($i)),
                            nqp::unbox_i($i), 0); # NULL for undefined args
                    }
                }
            }
        }
        $i++;
    }

    my $new_capture := nqp::dispatch('boot-syscall',
        'dispatcher-insert-arg-literal-obj',
        $args, 0, nqp::decont($callee.rettype));
    my $delegate_capture := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
        $new_capture, 0, $callee.call);
    nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-foreign-code', $delegate_capture);
};

$do := nqp::getattr(&raku-nativecall-core, Code, '$!do');
nqp::forceouterctx($do, nqp::getattr(MY::, PseudoStash, '$!ctx'));
nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-nativecall-core', $do);
