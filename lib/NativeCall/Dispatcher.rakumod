use v6.d;
use NativeCall::Types;

use nqp;

#- raku-nativecall-deproxy -----------------------------------------------------
my sub raku-nativecall-deproxy(Mu $capture is raw) {
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
    nqp::guard('literal', nqp::track('arg', $capture, 0));

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

# Set up actual dispatcher and its context
my $do := nqp::getattr(&raku-nativecall-deproxy, Code, '$!do');
nqp::forceouterctx($do, nqp::getattr(MY::, PseudoStash, '$!ctx'));
my $do-resume := nqp::getattr(&raku-nativecall-deproxy-resume, Code, '$!do');
nqp::forceouterctx($do-resume, nqp::getattr(MY::, PseudoStash, '$!ctx'));
nqp::register('raku-nativecall-deproxy', $do, $do-resume);

#- raku-nativecall -------------------------------------------------------------

my $PROXY-READERS := nqp::gethllsym('Raku', 'PROXY-READERS');
my sub raku-nativecall(Mu $capture is raw) {
    nqp::guard('literal', nqp::track('arg', $capture, 0));

    # Make sure the callee is ready
    nqp::captureposarg($capture, 0).setup;

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

    # No non-scalars
    else {
        nqp::delegate('raku-nativecall-core', $capture);
    }
}

# Set up actual dispatcher and its context
$do := nqp::getattr(&raku-nativecall, Code, '$!do');
nqp::forceouterctx($do, nqp::getattr(MY::, PseudoStash, '$!ctx'));
nqp::register('raku-nativecall', $do);

#- raku-nativecall-core --------------------------------------------------------

my sub raku-nativecall-core(Mu $capture is raw) {
    my $callee      := nqp::captureposarg($capture, 0);
    my $params      := nqp::getattr($callee.signature.params, List, '$!reified');
    my $param-elems := nqp::elems($params);

    my Mu $args := nqp::syscall('dispatcher-drop-arg', $capture, 0);
    my int $pos-args = nqp::captureposelems($args);
    my Bool $variadic;
    my Int $variadic-rw-bitfield = 0;
    my int $i;

    # Helper sub to replace the i-th positional argument with a value
    # and update the $args capture accordingly
    my sub set-arg-i-value(Mu $value is raw) {
        $args := nqp::syscall('dispatcher-insert-arg',
          nqp::syscall('dispatcher-drop-arg', $args, nqp::unbox_i($i)),
          nqp::unbox_i($i),
          $value
        );
    }

    # Helper sub to replace the i-th positional argument with a literal 0
    # and update the $args capture accordingly
    my sub set-arg-i-zero() {
        $args := nqp::syscall("dispatcher-insert-arg-literal-int",
          nqp::syscall('dispatcher-drop-arg', $args, nqp::unbox_i($i)),
          nqp::unbox_i($i),
          0
        );
    }

    # Helper sub to replace the i-th positional argument with a literal NaN
    # and update the $args capture accordingly
    my sub set-arg-i-NaN() {
        $args := nqp::syscall("dispatcher-insert-arg-literal-num",
          nqp::syscall('dispatcher-drop-arg', $args, nqp::unbox_i($i)),
          nqp::unbox_i($i),
          NaN
        );
    }

    if $pos-args > 64 {
        # Since the Moar code to process the variadic-rw-bitfield can currently
        # only process uint64 (bitint is NYI), we are limited to 64 args.
        X::TooManyVarArgs.new(:num($pos-args)).throw
    }

    while $i < $pos-args {

        # If it should be passed read only, and it's an object...
        unless nqp::captureposprimspec($args, $i) {

            # With variadic C functions / a **@vararg slurpy parameter, the
            # $params list ends early.
            my $param;
            unless $variadic {
                $param := nqp::atpos($params, $i);
                # Detect the **@vararg param. If found the var args start and
                # we have to work without params from now on.
                if $param.slurpy && nqp::istype($param.type, Positional) {
                    $variadic = True;
                }
            }

            # If it's in a Scalar container...
            my $Tvalue := nqp::track('arg', $args, nqp::unbox_i($i));
            nqp::guard('type', $Tvalue);
            nqp::guard('concreteness', $Tvalue);

            my $arg    := nqp::captureposarg($args, $i);
            my int $cstr;

            # If it's a vararg that should be passed as Pointer, remove the
            # sentinel wrapper and set the respective bit in the bit field.
            if nqp::istype_nd($arg, NativeCall::Types::VarArgPointerSentinel) {
                if $variadic {
                    set-arg-i-value(
                      $Tvalue := nqp::track('attr', $Tvalue, NativeCall::Types::VarArgPointerSentinel, '$!target')
                    );
                    $arg := nqp::captureposarg($args, $i);
                    $variadic-rw-bitfield +|= 1 +< $i;
                }
                else {
                    X::PointerToOutsideVarArg.new.throw
                }
            }

            # Read it from the container and pass it decontainerized.
            if nqp::isconcrete_nd($arg) && nqp::istype_nd($arg, Scalar) {
                set-arg-i-value(
                  $Tvalue := nqp::track('attr', $Tvalue, Scalar, '$!value')
                );
                $arg := nqp::decont($arg);
            }

            # Get to the actual low-level code if Code
            set-arg-i-value(
              $Tvalue := nqp::track('attr', $Tvalue, Code, '$!do')
            ) if nqp::isconcrete_nd($arg) && nqp::istype_nd($arg, Code);

            # Handle explicitely managed strings
            if nqp::isconcrete_nd($arg)
              && $arg.does(NativeCall::Types::ExplicitlyManagedString) {
                $cstr = 1;  # mark explicitely managed
                set-arg-i-value(
                  $Tvalue := nqp::track('attr', $Tvalue, $arg.WHAT, '$!cstr')
                );
                $arg := nqp::getattr($arg, $arg.WHAT, '$!cstr');

                # Decontainerize if possible
                if nqp::isconcrete_nd($arg)
                  && nqp::eqaddr(nqp::what_nd($arg),Scalar) {
                    set-arg-i-value(
                      $Tvalue := nqp::track('attr', $Tvalue, Scalar, '$!value')
                    );
                    $arg := nqp::decont($arg);
                }
            }

            if $variadic {
                if !nqp::isrwcont($arg) {
                    if nqp::istype($arg, Int) || nqp::reprname($arg) eq 'CPointer' {
                        nqp::isconcrete_nd($arg)
                          ?? set-arg-i-value(nqp::track('unbox-int', $Tvalue))
                          !! set-arg-i-zero;
                    }
                    elsif nqp::istype($arg, Str) && nqp::not_i($cstr) {
                        nqp::isconcrete_nd($arg)
                          ?? set-arg-i-value(nqp::track('unbox-str', $Tvalue))
                          !! set-arg-i-zero;
                    }
                    elsif nqp::istype($arg, Num) {
                        nqp::isconcrete_nd($arg)
                          ?? set-arg-i-value(nqp::track('unbox-num', $Tvalue))
                          !! set-arg-i-NaN;
                    }
                }
            }
            else {
                # Done with argument checking, check on the associated parameter
                unless nqp::isrwcont($arg) || $param.rw {
                    my $type := $param.type;

                    if nqp::istype($type, Int) || $type.REPR eq 'CPointer' {
                        nqp::isconcrete_nd($arg)
                          ?? set-arg-i-value(nqp::track('unbox-int', $Tvalue))
                          !! set-arg-i-zero;
                    }
                    elsif nqp::istype($type, Str) && nqp::not_i($cstr) {
                        nqp::isconcrete_nd($arg)
                          ?? set-arg-i-value(nqp::track('unbox-str', $Tvalue))
                          !! set-arg-i-zero;
                    }
                    elsif nqp::istype($type, Num) {
                        nqp::isconcrete_nd($arg)
                          ?? set-arg-i-value(nqp::track('unbox-num', $Tvalue))
                          !! set-arg-i-NaN;
                    }
                }
            }
        }
        ++$i;
    }

    if $variadic {
        $args := nqp::syscall('dispatcher-insert-arg-literal-obj',
          $args, nqp::unbox_i($pos-args), nqp::decont($variadic-rw-bitfield)
        );
    }

    nqp::delegate('boot-foreign-code',
      nqp::syscall('dispatcher-insert-arg-literal-obj',
        nqp::syscall('dispatcher-insert-arg-literal-obj',
          $args, 0, nqp::decont($callee.rettype)
        ),
        0,
        $callee.call
      )
    );
}

# Set up actual dispatcher and its context
$do := nqp::getattr(&raku-nativecall-core, Code, '$!do');
nqp::forceouterctx($do, nqp::getattr(MY::, PseudoStash, '$!ctx'));
nqp::register('raku-nativecall-core', $do);

# vim: expandtab shiftwidth=4
