# Dispatchers available in Rakudo on MoarVM

## MoarVM

Dispatchers provided by MoarVM.

Recommended reading:
- [Towards a new general dispatch mechanism in MoarVM](https://6guts.wordpress.com/2021/03/15/towards-a-new-general-dispatch-mechanism-in-moarvm/)
- [Raku multiple dispatch with the new MoarVM dispatcher](https://6guts.wordpress.com/2021/04/15/raku-multiple-dispatch-with-the-new-moarvm-dispatcher/)
- [The new MoarVM dispatch mechanism is here!](https://6guts.wordpress.com/2021/09/29/the-new-moarvm-dispatch-mechanism-is-here/)

---

### boot-code
```
nqp::dispatch('boot-code', $vmhandle, …);
```
Take the first argument, which must be a VM bytecode handle, and run that
bytecode, passing the rest of the arguments as its parameters; evaluate to
the return value of the bytecode.

### boot-code-constant
```
nqp::dispatch('boot-code-constant', $vmhandle, …);
```
Take the first argument, which must be a VM bytecode handle, and run that
bytecode, passing the rest of the arguments as its parameters; evaluate to
the return value of the bytecode.  Treat the VM bytecode handle as
immutable, thus providing the optimizer with options.

### boot-constant
```
nqp::dispatch('boot-constant', $value);
```
Take the first argument and produce it as the result, but also treat it as
a constant value that will always be produced (thus meaning the optimizer
could consider any pure code used to calculate the value as dead).

### boot-resume
```
nqp::dispatch('boot-resume');
```
Resume the topmost ongoing dispatch.

### boot-syscall
```
nqp::dispatch('boot-syscall', $name, …);
nqp::syscall($name, …);                   # same, using shortcut NQP op
```
Treat the first argument as the name of a VM-provided built-in operation
(or operation added later with `'dispatcher-register'`) and call it,
providing the remaining arguments as its parameters

### boot-value
```
nqp::dispatch('boot-value', $value, …);
```
Take the first argument and use it as the result (the identity function,
except discarding any further arguments).

### can-unbox-to-int
```
nqp::syscall("can-unbox-to-int",42.Int);  # 1
nqp::syscall("can-unbox-to-int",42.Str);  # 0
```
Takes the argument (a non-native object) and returns non-zero if it can be
unboxed to a native integer, and 0 otherwise.

### can-unbox-to-num
```
nqp::syscall("can-unbox-to-num",42.Num);  # 2
nqp::syscall("can-unbox-to-num",42.Str);  # 0
```
Takes the argument (a non-native object) and returns non-zero if it can be
unboxed to a native num, and 0 otherwise.

### can-unbox-to-str
```
nqp::syscall("can-unbox-to-str",42.Int);  # 0
nqp::syscall("can-unbox-to-str",42.Str);  # 4
```
Takes the argument (a non-native object) and returns non-zero if it can be
unboxed to a native integer, and 0 otherwise.

### capture-is-literal-arg
```
nqp::syscall("capture-is-literal-arg",$capture);
```
Takes the argument (made with nqp::savecapture) and returns non-zero if it
consists of a single literal argument, and 0 otherwise.

### code-is-stub
```
nqp::syscall("code-is-stub",$mvmcode);
```
Takes the argument (a low-level MoarVM code object) and returns non-zero if
it it is a stub, and 0 otherwise.

### dispatcher-delegate
```
nqp::dispatch('boot-syscall', 'dispatcher-delegate', $name, …);
nqp::delegate($name, …);              # same, using shortcut NQP op
```
Delegate control to the given dispatcher by name (either pre-defined, or
user-defined) and pass any given additional arguments to it.

### dispatcher-do-not-install
```
nqp::syscall("dispatcher-do-not-install");
```
Marks the current dispatch program as to not be installed upon completion.
Returns VMNull.

### dispatcher-drop-arg
### dispatcher-drop-n-args
### dispatcher-get-resume-init-args
### dispatcher-get-resume-state
### dispatcher-guard-concreteness
### dispatcher-guard-literal
### dispatcher-guard-type
### dispatcher-index-lookup-table
### dispatcher-index-tracked-lookup-table
### dispatcher-inline-cache-size
### dispatcher-insert-arg
### dispatcher-insert-arg-literal-int
### dispatcher-insert-arg-literal-num
### dispatcher-insert-arg-literal-obj
### dispatcher-insert-arg-literal-str
### dispatcher-is-arg-literal
### dispatcher-next-resumption
### dispatcher-register
```
nqp::dispatch('boot-syscall', 'dispatcher-register', $name, -> $capture {
    …
});
nqp::register($name, -> $capture {    # same, using shortcut NQP op
    …
});
```
Register a dispatcher.  Takes a name for the dispatcher along with a closure,
which will be called each time we need to handle the dispatch (the
"dispatch callback"). It receives a single argument, which is a capture of
arguments (see
[Captures](https://github.com/Raku/nqp/blob/main/docs/ops.markdown#captures)).

### dispatcher-replace-arg
### dispatcher-replace-arg-literal-obj
### dispatcher-resume-after-bind
### dispatcher-resume-on-bind-failure
### dispatcher-set-resume-init-args
### dispatcher-set-resume-state
### dispatcher-set-resume-state-literal
### dispatcher-track-arg
### dispatcher-track-attr
### dispatcher-track-how
### dispatcher-track-resume-state
### dispatcher-track-unbox-int
### dispatcher-track-unbox-num
### dispatcher-track-unbox-str
### has-type-check-cache
```
nqp::syscall("has-type-check-cache",$object);
```
Takes the argument (a HLL object) and returns non-zero if the class of the
object has a cache for type checking, and 0 if not.

### lang-call
### lang-meth-call
### lang-meth-not-found
### set-cur-hll-config-key
```
nqp::syscall("set-cur-hll-config-key",$key,$value);
```
Takes two arguments: a string key and a value, and sets that in the
configuration information of the current HLL language.  Returns VMNull.

### type-check-mode-flags
```
nqp::syscall("type-check-mode-flags",$object);
```
Returns the type check mode flags of the given object.  Possible values are:
- 0 no typecheck (?)
- 1 check type using "type_check" method
- 2 check type using "accepts-type" method

---

## NQP

These dispatcher are provided by the NQP bootstrap.

---

### nqp-call
### nqp-find-meth
### nqp-find-meth-mega-name
### nqp-find-meth-mega-type
### nqp-hllize
### nqp-intify
### nqp-isinvokable
### nqp-istype
### nqp-meth-call
### nqp-meth-call-mega-name
### nqp-meth-call-mega-type
### nqp-multi
### nqp-multi-core
### nqp-numify
### nqp-stringify
### nqp-uintify

---

## Rakudo

These dispatcher are provided by the Rakudo bootstrap.

---

### raku-assign
### raku-bind-assert
### raku-boolify
### raku-call
### raku-call-simple
### raku-capture-lex
### raku-capture-lex-callers
### raku-class-archetypes
### raku-coercion
### raku-find-meth
### raku-find-meth-mega
### raku-get-code-outer-ctx
### raku-hllize
### raku-invoke
### raku-invoke-wrapped
### raku-is-attr-inited
### raku-isinvokable
### raku-meth-call
### raku-meth-call-me-maybe
### raku-meth-call-mega
### raku-meth-call-qualified
### raku-meth-call-resolved
### raku-meth-deferral
### raku-meth-private
### raku-multi
### raku-multi-core
### raku-multi-non-trivial
### raku-multi-remove-proxies
### raku-nativecall
### raku-nativecall-core
### raku-nativecall-deproxy
### raku-resume-error
### raku-rv-decont
### raku-rv-decont-6c
### raku-rv-typecheck
### raku-sink
### raku-smartmatch
### raku-wrapper-deferral
