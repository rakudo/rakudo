use NativeCall::Types;

use nqp;
sub raku-native-dispatch-deproxy($callee, **@args) {
    $callee(|@args) # implicitly fetches values from proxies
}
my &raku-nativecall := -> $capture is raw {
    my $track_callee := nqp::dispatch('boot-syscall', 'dispatcher-track-arg', $capture, 0);
    nqp::dispatch('boot-syscall', 'dispatcher-guard-literal', $track_callee);
    my $callee := nqp::captureposarg($capture, 0);
    $callee.setup;

    my Mu $args := nqp::dispatch('boot-syscall', 'dispatcher-drop-arg', $capture, 0);
    my $signature := nqp::getattr($callee, Code, '$!signature');
    my int $readonly = nqp::getattr_i($signature, Signature, '$!readonly');
    my int $pos-args = nqp::captureposelems($args);
    my int $i = 0;
    my int $continue = 1;
    while $i < $pos-args and $continue {
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
            elsif nqp::isconcrete_nd($arg) && nqp::istype_nd($arg, Proxy) {
                $args := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                    $args, 0, $callee);
                $args := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
                    $args, 0, &raku-native-dispatch-deproxy);
                nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'raku-invoke', $args);
                $continue = 0;
            }

            if $continue {
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
        }
        $i++;
    }
    if $continue {
        my $new_capture := nqp::dispatch('boot-syscall',
            'dispatcher-insert-arg-literal-obj',
            $args, 0, nqp::decont($callee.rettype));
        my $delegate_capture := nqp::dispatch('boot-syscall', 'dispatcher-insert-arg-literal-obj',
            $new_capture, 0, $callee.call);
        nqp::dispatch('boot-syscall', 'dispatcher-delegate', 'boot-foreign-code', $delegate_capture);
    }
};
my $do := nqp::getattr(&raku-nativecall, Code, '$!do');
nqp::forceouterctx($do, nqp::getattr(MY::, PseudoStash, '$!ctx'));
nqp::dispatch('boot-syscall', 'dispatcher-register', 'raku-nativecall', $do);
