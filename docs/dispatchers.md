# Dispatchers available in Rakudo on MoarVM

Recommended reading to understand what dispatchers are and why they exist:
- [Towards a new general dispatch mechanism in MoarVM](https://6guts.wordpress.com/2021/03/15/towards-a-new-general-dispatch-mechanism-in-moarvm/)
- [Raku multiple dispatch with the new MoarVM dispatcher](https://6guts.wordpress.com/2021/04/15/raku-multiple-dispatch-with-the-new-moarvm-dispatcher/)
- [The new MoarVM dispatch mechanism is here!](https://6guts.wordpress.com/2021/09/29/the-new-moarvm-dispatch-mechanism-is-here/)

## MoarVM

Dispatchers provided by MoarVM.

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
nqp::dispatch('boot-constant', $value, …);
```
Take the first argument and produce it as the result, but also treat it as
a constant value that will always be produced (thus meaning the optimizer
could consider any pure code used to calculate the value as dead).

Ignores all other arguments: this allows calls to this dispatcher to be
using an existing capture by just inserting a value as the first argument,
thus removing the need to remove any superfluous arguments.

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

### Notes on dispatcher- calls

The `dispatcher-` prefix means that it's part of specifying the dispatch
program.  So you're not only getting a return value, but the dispatch
recording has tracked the operation.  For instance, in case of these calls
taking and returning a capture, it also internally builds up a tree of
relationships between the various captures.  And so knows what they are
made up of.

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
```
nqp::syscall("dispatcher-drop-arg", $capture, int $index);
```
Takes the specified capture and returns a capture with the positional
argument at the given $index removed.

### dispatcher-drop-n-args
```
nqp::syscall("dispatcher-drop-n-args", $capture, int $index, int $count);
```
Takes the specified capture and returns a capture with the $count positional
argument at the given $index removed.

### dispatcher-get-resume-init-args
```
nqp::syscall("dispatcher-get-resume-init-args");
```
Returns the capture that was previously saved with
`nqp::syscall("dispatcher-set-resume-init-args", $capture)`.

### dispatcher-get-resume-state
```
nqp::syscall("dispatcher-get-resume-state");
```
Returns the resume state that was previously saved with
`nqp::syscall("dispatcher-set-resume-state", $state)` or 
`nqp::syscall("dispatcher-set-resume-state-literal", $state)`.

### dispatcher-guard-concreteness
```
nqp::syscall("dispatcher-guard-concreteness", $tracker);
nqp::guard('concreteness", $tracker);
```
Record a guard for the concreteness of the given $tracker in the current
dispatch program.  Returns the given $tracker.

### dispatcher-guard-literal
```
nqp::syscall("dispatcher-guard-literal", $tracker);
nqp::guard("literal", $tracker);
```
Record a guard for the given $tracker in the current dispatch program on
whether the tracked objects is a literal or not.  Returns the given $tracker.

### dispatcher-guard-type
```
nqp::syscall("dispatcher-guard-type", $tracker);
nqp::guard("type", $tracker);
```
Record a guard for the type of the given $tracker in the current dispatch
program.  Returns the given $tracker.

### dispatcher-index-lookup-table
```
nqp::syscall("dispatcher-index-lookup-table", %lookup, $tracker);
```
Adds a (mega-morphic) %lookup hash table to the current dispatch program,
tracked by the given tracker.

### dispatcher-index-tracked-lookup-table
```
nqp::syscall("dispatcher-index-tracked-lookup-table", $tracked-lookup, $tracker);
```
Adds a (mega-morphic) tracked lookup hash table to the current dispatch
program, tracked by the given tracker.

### dispatcher-inline-cache-size
```
nqp::syscall("dispatcher-inline-cache-size");
```
Returns a native integer with the number of entries that are being held in
the inline cache.

### dispatcher-insert-arg
```
nqp::syscall("dispatcher-insert-arg", $capture, int $index, $value);
```
Returns a capture based on the given $capture, but with the $value (regardless
of being a native type or not) inserted at position $index.

### dispatcher-insert-arg-literal-int
```
nqp::syscall("dispatcher-insert-arg-literal-int", $capture, int $index, int $value);
```
Returns a capture based on the given $capture, but with a native integer
$value inserted at position $index.

### dispatcher-insert-arg-literal-num
```
nqp::syscall("dispatcher-insert-arg-literal-num", $capture, int $index, num $value);
```
Returns a capture based on the given $capture, but with a native num
$value inserted at position $index.

### dispatcher-insert-arg-literal-obj
```
nqp::syscall("dispatcher-insert-arg-literal-obj", $capture, int $index, $object);
```
Returns a capture based on the given $capture, but with an object $object
inserted at position $index.

### dispatcher-insert-arg-literal-str
```
nqp::syscall("dispatcher-insert-arg-literal-str", $capture, int $index, str $value);
```
Returns a capture based on the given $capture, but with a native string
$value inserted at position $index.

### dispatcher-is-arg-literal
```
nqp::syscall("dispatcher-is-arg-literal", $capture, int $index);
```
Returns 1 if the argument at position $index in the given $capture, is a
literal value.  0 if not.

### dispatcher-next-resumption
```
nqp::syscall("dispatcher-is-arg-literal");
nqp::syscall("dispatcher-is-arg-literal", $capture);
```
Returns 1 if there is a next resumption for the current dispatcher, optionally
for the given $capture.  0 if not.

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
```
nqp::syscall("dispatcher-replace-arg", $capture, int $index, $value);
```
Returns a capture based on the given $capture, but with the positional
argument at position $index replaced by $value (regardless of being a
native type or not).

### dispatcher-replace-arg-literal-obj
```
nqp::syscall("dispatcher-replace-arg-literal-obj", $capture, int $index, $object);
```
Returns a capture based on the given $capture, but with the positional
argument at position $index replaced by $object.

### dispatcher-resume-after-bind
```
nqp::syscall("dispatcher-resume-after-bind", int $success, int $failure);
```
Mark the current dispatch program to resume after a successful bind with the
given $success kind of dispatch, otherwise with the $failure kind of dispatch.
(both are nqp::const::DISP_xxx values).

### dispatcher-resume-on-bind-failure
```
nqp::syscall("dispatcher-resume-on-bind-failure", int $kind);
```
Mark the current dispatch program to resume after a successful bind with the
given kind of dispatch (an nqp::const::DISP_xxx value).

### dispatcher-set-resume-init-args
```
nqp::syscall("dispatcher-set-resume-init-args", $capture);
```
Set the capture to be returned with
`nqp::syscall("dispatcher-get-resume-init-args")`.

### dispatcher-set-resume-state
```
nqp::syscall("dispatcher-set-resume-state", $state);
```
Set the resume $state that to be returned by
`nqp::syscall("dispatcher-get-resume-state")`.

### dispatcher-set-resume-state-literal
```
nqp::syscall("dispatcher-set-resume-state-literal", $state);
```
Set the resume $state that to be returned by
`nqp::syscall("dispatcher-get-resume-state")`.  This is usually used to mark
the dispatch as exhausted.

### dispatcher-track-arg
```
nqp::syscall("dispatcher-track-arg", $capture, int $index);
nqp::track("arg", $capture, int $index);
```
Returns a tracker object for the positional argument in the given $capture
at the given $index.

### dispatcher-track-attr
```
nqp::syscall("dispatcher-track-attr", $tracker, $type, $name);
nqp::track("attr", $tracker, $type, $name);
```
Returns a tracker object for the attribute in the object of the given
$tracker object for the given $type and $name.

### dispatcher-track-how
```
nqp::syscall("dispatcher-track-how", $tracker);
nqp::track("how", $tracker);
```
Returns a tracker object for the HOW of the object of the given $tracker
object.

### dispatcher-track-resume-state
```
nqp::syscall("dispatcher-track-resume-state");
nqp::track("resume-state");
```
Returns a tracker object for the current resume state as set with
`nqp::syscall("dispatcher-set-resume-state", $state)`.

### dispatcher-track-unbox-int
```
nqp::syscall("dispatcher-track-unbox-int", $tracker);
nqp::track("unbox-int", $tracker);
```
Returns a tracker object for the unboxed integer for the given tracker of a
a boxed integer.

### dispatcher-track-unbox-num
```
nqp::syscall("dispatcher-track-unbox-num", $tracker);
nqp::track("unbox-num", $tracker);
```
Returns a tracker object for the unboxed num for the given tracker of a
a boxed num.

### dispatcher-track-unbox-str
```
nqp::syscall("dispatcher-track-unbox-str", $tracker);
nqp::track("unbox-str", $tracker);
```
Returns a tracker object for the unboxed string for the given tracker of a
a boxed string.

### has-type-check-cache
```
nqp::syscall("has-type-check-cache", $object);
```
Takes the argument (a HLL object) and returns non-zero if the class of the
object has a cache for type checking, and 0 if not.

### lang-call
```
nqp::delegate("lang-call", $capture);
```
Call the code given by the first argument in the $capture, and pass any other
arguments to that code.

### lang-meth-call
```
nqp::delegate("lang-meth-call", $capture);
```
Call the method given of which the name is given by the second argument in
the $capture, on the invocant in the first argument of the capture, and pass
any other arguments to that method.

### lang-meth-not-found
```
nqp::delegate("lang-meth-not-found", $capture);
```
Delegate as if a method was not found.

### set-cur-hll-config-key
```
nqp::syscall("set-cur-hll-config-key", $key, $value);
```
Takes two arguments: a string key and a value, and sets that in the
configuration information of the current HLL language.  Returns VMNull.

### type-check-mode-flags
```
nqp::syscall("type-check-mode-flags", $object);
```
Returns the type check mode flags of the given object.  Possible values are:
- 0 no typecheck (?)
- 1 check type using "type_check" method
- 2 check type using "accepts-type" method

---

## Rakudo

These dispatcher are provided by the Rakudo bootstrap.

---

### raku-assign
```
nqp::dispatch("raku-assign", $container, $value);
```
Assigns the given $value to the given $container, and produces the $container.

### raku-bind-assert
```
nqp::dispatch("raku-bind-assert", $value, $deconted, $type);
```
Returns the given $value if the $deconted value type matches against $type.
Otherwise throws.

### raku-boolify
```
nqp::dispatch("raku-boolify", $value);
```
Returns `True` if the given $value is true, else returns `False`.

### raku-call
```
nqp::dispatch("raku-call", $callee, …);
```
Calls the given $callee with the given arguments, and returns the result.
The $callee can be a sub / method / code object.  

### raku-capture-lex
```
nqp::dispatch("raku-capture-lex", $code);
```
Returns a capture of the lexical environment of the given $code object.
Workhorse of `nqp::p6capturelex`.

### raku-capture-lex-callers
```
nqp::dispatch("raku-capture-lex-callers", $code);
```
Returns a capture of the environment of the caller matching the outer of
the given $code object.  Workhorse of `nqp::p6capturelexwhere`.

### raku-class-archetypes
```
nqp::dispatch("raku-class-archetypes", $HOW, $object);
```
Returns a list of archetypes of the given $HOW and $object.

### raku-coercion
```
nqp::dispatch("raku-coercion", $coercion-type, $value);
```
Returns a coerced value for the given $coercion-type and $value.

### raku-find-meth
```
nqp::dispatch("raku-find-meth", $invocant, str $method, int $throw);
```
Returns the `Method` object for the given $invocant and $method name if
it can be found.  If it is not found, the $throw determines whether this
will cause a "method not found" dispatch (if 1) or `nqp::null` (if 0).

### raku-get-code-outer-ctx
```
nqp::dispatch("raku-get-outer-ctx", $code);
```
Returns the context of the outer of the given $code object.  Workhorse of
`nqp::p6getouterctx`.

### raku-hllize
```
nqp::dispatch("raku-hllize", $value);
```
Returns a Raku HLLized version of the given value, if it needs HLLizing..

### raku-is-attr-inited
```
nqp::dispatch("raku-is-attr-inited", $attribute);
```
Returns 1 if the given $attribute is initialized, 0 if not.

### raku-isinvokable
```
nqp::dispatch("raku-isinvokable", $object);
```
Returns 1 if the given $object can be invoked (aka, is a `Code` object).
Returns 0 if not.

### raku-meth-call
```
nqp::dispatch("raku-meth-call", $invocant, str $method, …);
```
Calls the given $method (by name) on the given $invocant with the given
arguments, and return the result.

### raku-meth-call-me-maybe
```
nqp::dispatch("raku-meth-call-me-maybe", $invocant, str $method, …);
```
Calls the given $method (by name) on the given $invocant with the given
arguments if the method can be found, and return the result.  Otherwise
returns `Nil`.

### raku-meth-call-qualified
```
nqp::dispatch("raku-meth-callqualified", $invocant, str $method, $type, …);
```
Calls the given $method (by name) on the given $invocant but starting method
lookup with the given $type, then call that resolved method with the rest of
the arguments, and return the result.

### raku-meth-private
```
nqp::dispatch("raku-meth-private", $invocant, str $method, …);
```
Calls the given private $method (by name) on the given $invocant with the
given arguments, and return the result.

### raku-nativecall
### raku-nativecall-core
### raku-nativecall-deproxy
### raku-resume-error
```
nqp::dispatch("raku-resume-error");
```
Throws an `X::NoDispatcher` error with the name of the calling dispatcher.

### raku-rv-decont
```
nqp::dispatch("raku-rv-decont", $value);
```
Returns the given $value without any container.

### raku-rv-decont-6c
```
nqp::dispatch("raku-rv-decont-6c", $value);
```
Returns the given $value without any container using 6.c semantics, which
implies that any `Proxy` object will **not** be decontainerized.  This was
an error in the 6.c implementation on which some modules in the ecosystem
depend.

### raku-rv-typecheck-generic
```
nqp::dispatch("raku-rv-typecheck", $value, $generic-type);
```
Returns the given $value if the typecheck with the **generic** type-value
is successful: assumes the actual type value can vary due to its
genericness.  Otherwise throw an error.

### raku-rv-typecheck
```
nqp::dispatch("raku-rv-typecheck", $value, $type);
```
Returns the given $value if the typecheck with the type-value is successful
assuming the type at the call-site is a constant.  Otherwise throw an error.

### raku-sink
```
nqp::dispatch("raku-sink", $value);
```
Perform any non-standard sink logic on the potentially containerized value,
or don't do anything otherwise.

### raku-smartmatch
### raku-wrapper-deferral
