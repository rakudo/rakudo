# TABLE OF CONTENTS
- [Rakudo Opcodes]
    - [p6argsfordispatcher](#p6argsfordispatcher)
    - [p6argvmarray](#p6argvmarray)
    - [p6bindassert](#p6bindassert)
    - [p6bindattrinvres](#p6bindattrinvres)
    - [p6bindcaptosig](#p6bindcaptosig)
    - [p6bindsig](#p6bindsig)
    - [p6bool](#p6bool)
    - [p6box_i](#p6box_i)
    - [p6box_n](#p6box_n)
    - [p6box_s](#p6box_s)
    - [p6box_u](#p6box_u)
    - [p6capturelex](#p6capturelex)
    - [p6capturelexwhere](#p6capturelexwhere)
    - [p6captureouters2](#p6captureouters2)
    - [p6clearpre](#p6clearpre)
    - [p6configposbindfailover](#p6configposbindfailover)
    - [p6decodelocaltime](#p6decodelocaltime)
    - [p6decontrv](#p6decontrv)
    - [p6definite](#p6definite)
    - [p6finddispatcher](#p6finddispatcher)
    - [p6getouterctx](#p6getouterctx)
    - [p6init](#p6init)
    - [p6inpre](#p6inpre)
    - [p6invokeflat](#p6invokeflat)
    - [p6invokehandler](#p6invokehandler)
    - [p6invokeunder](#p6invokeunder)
    - [p6isbindable](#p6isbindable)
    - [p6recont_ro](#p6recont_ro)
    - [p6reprname](#p6reprname)
    - [p6return](#p6return)
    - [p6scalarfromdesc](#p6scalarfromdesc)
    - [p6setautothreader](#p6setautothreader)
    - [p6setbinder](#p6setbinder)
    - [p6setfirstflag](#p6setfirstflag)
    - [p6setpre](#p6setpre)
    - [p6settypes](#p6settypes)
    - [p6sink](#p6sink)
    - [p6sort](#p6sort)
    - [p6stateinit](#p6stateinit)
    - [p6staticouter](#p6staticouter)
    - [p6store](#p6store)
    - [p6takefirstflag](#p6takefirstflag)
    - [p6trialbind](#p6trialbind)
    - [p6typecheckrv](#p6typecheckrv)
    - [p6var](#p6var)


## p6argsfordispatcher
* p6argsfordispatcher(Sub $dispatcher)

## p6argvmarray
* p6argvmarray()

## p6bindassert
* p6bindassert(Mu $obj, Mu $target)

Check if $obj can bind into a container typed with type $target and return $obj.

## p6bindattrinvres
* p6bindattrinvres(Mu $obj, Mu $type, str $attr-name, Mu $value)

Bind $value into Attribute $attr-name of object $obj of type $type and return $obj.
This desugars to:

    {
        bindattr($obj, $type, $attr-name, $value);
        $obj;
    }

## p6bindcaptosig
* p6bindcaptosig(Mu $signature, Mu $capture)

## p6bindsig
* p6bindsig()

## p6bool
* p6bool(Mu $value)

Create a Perl 6 Bool from $value.

## p6box_i
* p6box_i(int $value)

Box a native int into a Perl 6 Int.

## p6box_n
* p6box_n(num $value)

Box a native num into a Perl 6 Num.

## p6box_s
* p6box_s(str $value)

Box a native str into a Perl 6 Str.

## p6box_u
* p6box_u(uint $value)

Box a native uint into a Perl 6 UInt.

## p6capturelex
* p6capturelex(Mu $closure)

## p6capturelexwhere
* p6capturelexwhere(Mu $closure)

## p6captureouters2
* p6captureouters2(Mu $coderef)

## p6clearpre
* p6clearpre()

Clears the "pre" flag in the current frame.

## p6configposbindfailover
* p6configposbindfailover(Mu $type, Mu $failover-type)

Configers the Binder to allow $failover-type to bind to $type in subroutine invocation.

## p6decodelocaltime
* p6decodelocaltime(int $epoch)

Decodes the unix timestamp $epoch into a native int array with six fields containing second, minute, hour, day, month, year in that order.

## p6decontrv
* p6decontrv(Mu $type, Mu $value)

## p6definite
* p6definite(Mu $obj)

## p6finddispatcher
* p6finddispatcher(str $value)

## p6getouterctx
* p6getouterctx(Mu $closure)

## p6init
* p6init()

Initializes the GlobalContext extensions for Perl 6.

## p6inpre
* p6inpre()

Checks for the "pre" flag on the current frame, returns it, and clears it if it was set.

## p6invokeflat
* p6invokeflat(Mu $block, Mu $value-buffer)

## p6invokehandler
* p6invokehandler(Mu $handler-name, Mu $exception)

Invokes handler $handler-name to handle Exception $exception.

## p6invokeunder
* p6invokeunder(Mu $code, Mu $closure)

Invokes $code under $closure.

## p6isbindable
* p6isbindable(Mu $signature, Mu $capture)

Checks if Capture $capture can bind to Signature $signature.

## p6recont_ro
* p6recont_ro(Mu $value)

Recontainerizes $value into a read-only container.

## p6reprname
* p6reprname(Mu $obj)

Returns the name of the REPR underlying $obj.

## p6return
* p6return(Mu $value)

## p6scalarfromdesc
* p6scalarfromdesc(Mu $container-descriptor)

## p6setautothreader
* p6setautothreader(Mu $auto-threader)

Registers a callable that handles the case where a call didn't succeed because it contained Junction arguments.

## p6setbinder
* p6setbinder(Mu $binder)

Register the class that handles binding. Its methods `bind`, `bind_sig`, `is_bindable`, and `trial_bind` are used throughout the rakudo codebase.

## p6setfirstflag
* p6setfirstflag(Mu $coderef)

Sets the "first" flag on a code object, then returns that code object. Used to handle FIRST phaser blocks.

## p6setpre
* p6setpre()

Sets the "pre" flag on the current frame.

## p6settypes
* p6settypes(Mu $stash)

## p6sink
* p6sink(Mu $past)

## p6sort
* p6sort(Mu @data, Mu &comparator)

## p6stateinit
* p6stateinit()

## p6staticouter
* p6staticouter(Mu $coderef)

## p6store
* p6store(Mu $container, Mu $value)

## p6takefirstflag
* p6takefirstflag(Mu $coderef)

Returns the value of the "first" flag of a code object and clears it. Used to handle FIRST phaser blocks.

## p6trialbind
* p6trialbind(Mu $signature, Mu @types, Mu @flags)

Tries a compile-time signature bind against @types with @flags

## p6typecheckrv
* p6typecheckrv(Mu $return-value, Mu $routine, Mu $bypass-type)

Checks if $return-value satisifies the declared return type of $routine, letting $bypass-type bypass the typecheck.

## p6var
* p6var(Mu $variable)
