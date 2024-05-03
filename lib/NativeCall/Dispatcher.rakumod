use NativeCall::Types;

use nqp;

#- raku-nativecall-deproxy -----------------------------------------------------
my sub raku-nativecall-deproxy(Mu $capture is raw) {
    my $callee := nqp::captureposarg($capture, 1);
    # The resume init state drops the remover.
    nqp::syscall('dispatcher-set-resume-init-args',
      nqp::syscall('dispatcher-drop-arg', $capture, 0)
    );

    # We then invoke the remover with the arguments (so need to drop the
    # original invokee).
    nqp::delegate('boot-code-constant',
      nqp::syscall('dispatcher-drop-arg', $capture, 1)
    );
}

my sub raku-nativecall-deproxy-resume(Mu $capture is raw) {
    my $Tkind := nqp::track('arg', $capture, 0);
    nqp::guard('literal', $Tkind);

    if nqp::captureposarg_i($capture, 0) == nqp::const::DISP_DECONT {
        my $orig-capture := nqp::syscall('dispatcher-get-resume-init-args');
        nqp::guard('literal', nqp::track('arg', $orig-capture, 0));
        nqp::delegate('raku-nativecall-core',
          nqp::syscall('dispatcher-insert-arg-literal-obj',
            nqp::syscall('dispatcher-drop-arg', $capture, 0),
            0,
            nqp::captureposarg($orig-capture, 0)
          )
        );
    }
}

my $do := nqp::getattr(&raku-nativecall-deproxy, Code, '$!do');
nqp::forceouterctx($do, nqp::getattr(MY::, PseudoStash, '$!ctx'));
my $do-resume := nqp::getattr(&raku-nativecall-deproxy-resume, Code, '$!do');
nqp::forceouterctx($do-resume, nqp::getattr(MY::, PseudoStash, '$!ctx'));
nqp::register('raku-nativecall-deproxy', $do, $do-resume);

#- raku-nativecall -------------------------------------------------------------

my $PROXY-READERS := nqp::gethllsym('Raku', 'PROXY-READERS');
my sub raku-nativecall(Mu $capture is raw) {
    nqp::guard('literal', nqp::track('arg', $capture, 0));
    my $callee := nqp::captureposarg($capture, 0);
    $callee.setup;

    my Mu $args := nqp::syscall('dispatcher-drop-arg', $capture, 0);
    my int $pos-args = nqp::captureposelems($args);
    my int $i;

    my $non-scalar := nqp::list_i;
    while $i < $pos-args {

        # Not a native value
        unless nqp::captureposprimspec($args, $i) {
            my $value := nqp::captureposarg($args, $i);
            nqp::push_i($non-scalar, $i)
              if nqp::isconcrete_nd($value)
              && nqp::iscont($value)
              && nqp::not_i(nqp::istype_nd($value, Scalar))
              && nqp::not_i(nqp::iscont_s($value)
                   || nqp::iscont_i($value)
                   || nqp::iscont_u($value)
                   || nqp::iscont_n($value)
                 );
        }
        ++$i;
    }

    if nqp::elems($non-scalar) {

        # Establish guards on types of all positionals, but not on the values
        # inside of them if they are Scalar containers; we just need to make
        # sure we have the appropriate tuple of Proxy vs non-Proxy for the
        # Proxy removal code we'll invoke.
        $i = 0;
        while $i < $pos-args {
            nqp::guard('type', nqp::track('arg', $args, nqp::unbox_i($i)))
              unless nqp::captureposprimspec($args, $i);
            ++$i;
        }

        # Need to strip the Proxy arguments and then try again. Produce a
        # proxy reader code object to do so, insert it as the first arg,
        # and delegate to a dispatcher to manage reading the args and
        # then retrying with the outcome.
        my $reader := $PROXY-READERS.reader-for($args, $non-scalar);
        nqp::delegate('raku-nativecall-deproxy',
          nqp::syscall('dispatcher-insert-arg-literal-obj',
            $capture, 0, nqp::getattr($reader, $reader.WHAT, '$!do')
          )
        );
    }
    else {
        nqp::delegate('raku-nativecall-core', $capture);
    }
}
$do := nqp::getattr(&raku-nativecall, Code, '$!do');
nqp::forceouterctx($do, nqp::getattr(MY::, PseudoStash, '$!ctx'));
nqp::register('raku-nativecall', $do);

#- raku-nativecall-core --------------------------------------------------------

my sub raku-nativecall-core(Mu $capture is raw) {
    my $callee := nqp::captureposarg($capture, 0);

    my Mu $args := nqp::syscall('dispatcher-drop-arg', $capture, 0);
    my int $pos-args = nqp::captureposelems($args);
    my int $i;

    while $i < $pos-args {
        # If it should be passed read only, and it's an object...
        if nqp::captureposprimspec($args, $i) == 0 {
            # If it's in a Scalar container...
            my $Targ := nqp::track('arg', $args, nqp::unbox_i($i));
            nqp::guard('type', $Targ);
            nqp::guard('concreteness', $Targ);
            my $arg := nqp::captureposarg($args, $i);
            my $Tvalue;
            my $cstr = False;
            if nqp::isconcrete_nd($arg) && nqp::istype_nd($arg, Scalar) {
                # Read it from the container and pass it decontainerized.
                $Tvalue := nqp::track('attr', $Targ, Scalar, '$!value');
                $args := nqp::syscall('dispatcher-insert-arg',
                    nqp::syscall('dispatcher-drop-arg', $args, nqp::unbox_i($i)),
                    nqp::unbox_i($i), $Tvalue);
                $arg := nqp::decont($arg);
            }
            else {
                $Tvalue := $Targ;
            }
            if nqp::isconcrete_nd($arg) && nqp::istype_nd($arg, Code) {
                $Tvalue := nqp::track('attr', $Tvalue, Code, '$!do');
                $args := nqp::syscall('dispatcher-insert-arg',
                    nqp::syscall('dispatcher-drop-arg', $args, nqp::unbox_i($i)),
                    nqp::unbox_i($i), $Tvalue);
            }
            if nqp::isconcrete_nd($arg) && $arg.does(NativeCall::Types::ExplicitlyManagedString) {
                $cstr = True;
                $Tvalue := nqp::track('attr', $Tvalue, $arg.WHAT, '$!cstr');
                $args := nqp::syscall('dispatcher-insert-arg',
                    nqp::syscall('dispatcher-drop-arg', $args, nqp::unbox_i($i)),
                    nqp::unbox_i($i), $Tvalue);
                $arg := nqp::getattr($arg, $arg.WHAT, '$!cstr');
                if nqp::isconcrete_nd($arg) && nqp::what_nd($arg) =:= Scalar {
                    $Tvalue := nqp::track('attr', $Tvalue, Scalar, '$!value');
                    $args := nqp::syscall('dispatcher-insert-arg',
                        nqp::syscall('dispatcher-drop-arg', $args, nqp::unbox_i($i)),
                        nqp::unbox_i($i), $Tvalue);
                    $arg := nqp::decont($arg);
                }
            }

            my $param = $callee.signature.params[$i];
            unless $param.rw or nqp::isrwcont($arg) {
                if $param.type ~~ Int or $param.type.REPR eq 'CPointer' {
                    if nqp::isconcrete_nd($arg) {
                        $Tvalue := nqp::track('unbox-int', $Tvalue);
                        $args := nqp::syscall('dispatcher-insert-arg',
                            nqp::syscall('dispatcher-drop-arg', $args, nqp::unbox_i($i)),
                            nqp::unbox_i($i), $Tvalue);
                    }
                    else {
                        $args := nqp::syscall('dispatcher-insert-arg-literal-int',
                            nqp::syscall('dispatcher-drop-arg', $args, nqp::unbox_i($i)),
                            nqp::unbox_i($i), 0); # 0 or NULL for undefined args
                    }
                }
                elsif $param.type ~~ Num {
                    if nqp::isconcrete_nd($arg) {
                        $Tvalue := nqp::track('unbox-num', $Tvalue);
                        $args := nqp::syscall('dispatcher-insert-arg',
                            nqp::syscall('dispatcher-drop-arg', $args, nqp::unbox_i($i)),
                            nqp::unbox_i($i), $Tvalue);
                    }
                    else {
                        $args := nqp::syscall('dispatcher-insert-arg-literal-num',
                            nqp::syscall('dispatcher-drop-arg', $args, nqp::unbox_i($i)),
                            nqp::unbox_i($i), NaN);
                    }
                }
                elsif $param.type ~~ Str and not $cstr {
                    if nqp::isconcrete_nd($arg) {
                        $Tvalue := nqp::track('unbox-str', $Tvalue);
                        $args := nqp::syscall('dispatcher-insert-arg',
                            nqp::syscall('dispatcher-drop-arg', $args, nqp::unbox_i($i)),
                            nqp::unbox_i($i), $Tvalue);
                    }
                    else {
                        $args := nqp::syscall('dispatcher-insert-arg-literal-int',
                            nqp::syscall('dispatcher-drop-arg', $args, nqp::unbox_i($i)),
                            nqp::unbox_i($i), 0); # NULL for undefined args
                    }
                }
            }
        }
        ++$i;
    }

    my $new_capture := nqp::syscall('dispatcher-insert-arg-literal-obj',
      $args, 0, nqp::decont($callee.rettype)
    );
    my $delegate_capture := nqp::syscall('dispatcher-insert-arg-literal-obj',
       $new_capture, 0, $callee.call
    );
    nqp::delegate('boot-foreign-code', $delegate_capture);
}

$do := nqp::getattr(&raku-nativecall-core, Code, '$!do');
nqp::forceouterctx($do, nqp::getattr(MY::, PseudoStash, '$!ctx'));
nqp::register('raku-nativecall-core', $do);

# vim: expandtab shiftwidth=4
