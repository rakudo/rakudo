use Test;

plan 63;

# A boxed value reaches a native-only multi candidate by unboxing, the
# same way it reaches the equivalent non-multi routine. The dispatcher
# only considers such candidates when no candidate matches otherwise.

multi sub str-only(str $s) { "str:$s" }
is str-only("f"), "str:f",
    "a string literal dispatches to a lone str candidate";
my Str $typed = "g";
is str-only($typed), "str:g",
    "a typed Str variable dispatches to a lone str candidate";
my $untyped = "h";
is str-only($untyped), "str:h",
    "an untyped variable holding a Str dispatches to a lone str candidate";
my $bound := "i";
is str-only($bound), "str:i",
    "a bound Str value dispatches to a lone str candidate";
is str-only(<5>), "str:5",
    "an allomorph dispatches to a lone str candidate";

multi sub int-only(int $i) { "int:$i" }
is int-only(42), "int:42",
    "an integer literal dispatches to a lone int candidate";
my $int-val = 43;
is int-only($int-val), "int:43",
    "a variable holding an Int dispatches to a lone int candidate";

multi sub uint-only(uint $u) { "uint:$u" }
is uint-only(44), "uint:44",
    "an integer literal dispatches to a lone uint candidate";

multi sub num-only(num $n) { "num:$n" }
is num-only(4.5e0), "num:4.5",
    "a num literal dispatches to a lone num candidate";
my $num-val = 5.5e0;
is num-only($num-val), "num:5.5",
    "a variable holding a Num dispatches to a lone num candidate";

class WithNativeMethod {
    multi method m(str $s) { "method:$s" }
}
is WithNativeMethod.m("f"), "method:f",
    "a string literal dispatches to a lone str method candidate";
is WithNativeMethod.m($typed), "method:g",
    "a Str variable dispatches to a lone str method candidate";

# The unbox path must not change any dispatch that already resolves.
multi sub tie-break(int $i) { "int" }
multi sub tie-break(Int $i) { "Int" }
is tie-break(1), "Int",
    "a boxed integer keeps choosing the Int candidate over the int one";
my int $native-int = 1;
is tie-break($native-int), "int",
    "a native int container keeps choosing the int candidate";

multi sub str-tie(str $s) { "str" }
multi sub str-tie(Str $s) { "Str" }
is str-tie("x"), "Str",
    "a boxed string keeps choosing the Str candidate over the str one";
my str $native-str = "x";
is str-tie($native-str), "str",
    "a native str container keeps choosing the str candidate";

# Constraints on the candidate still apply through the bind check.
multi sub constrained(str $s where *.chars == 1) { "short:$s" }
is constrained("f"), "short:f",
    "a where constraint on a native parameter passes a fitting boxed value";
throws-like { constrained("toolong") }, X::Multi::NoMatch,
    "a where constraint on a native parameter rejects an unfitting boxed value";

# What cannot unbox still fails to dispatch.
dies-ok { EVAL q[multi sub int-target(int $i) { }; int-target("nope")] },
    "a Str value does not dispatch to a lone int candidate";
dies-ok { EVAL q[multi sub num-target(num $n) { }; num-target(42)] },
    "an Int value does not dispatch to a lone num candidate";
multi sub st-target(str $s) { $s }
throws-like { st-target(Str) }, X::Multi::NoMatch,
    "a type object does not dispatch to a lone str candidate";
dies-ok { EVAL q[multi sub big-target(int $i) { }; big-target(2 ** 70)] },
    "an Int too wide for a native int does not dispatch to a lone int candidate";

# A parameter needing write access is only served by a native container.
multi sub rw-target(str $s is rw) { $s = "set"; "bound" }
throws-like { rw-target($typed) }, X::Multi::NoMatch,
    "a boxed value does not dispatch to a lone native rw candidate";
my str $rw-native = "f";
is rw-target($rw-native), "bound",
    "a native str container binds a native rw parameter";
is $rw-native, "set",
    "the native rw parameter writes back to its container";

# Junction arguments still autothread over the candidates.
multi sub junct(str $s) { "j:$s" }
my $junction-result = junct("a" | "b");
isa-ok $junction-result, Junction,
    "a Junction argument autothreads over a lone str candidate";
ok so ($junction-result eq "j:a"),
    "the Junction result contains the first threaded value";
ok so ($junction-result eq "j:b"),
    "the Junction result contains the second threaded value";
nok so ($junction-result eq "j:c"),
    "the Junction result contains only threaded values";

# A named native parameter binds a boxed value through the binder,
# without the dispatcher's help.
multi sub named-nat(str :$s!) { "named:$s" }
is named-nat(s => "f"), "named:f",
    "a boxed value binds a named native parameter of a multi";

# Mixtures of native and boxed parameters and arguments.
multi sub mixed(str $a, Int $b) { "mixed:$a:$b" }
is mixed("x", 2), "mixed:x:2",
    "a boxed string beside a matching Int dispatches to a str, Int candidate";
multi sub two-native(str $a, int $b) { "two:$a:$b" }
is two-native("x", 2), "two:x:2",
    "two boxed values dispatch to a candidate with two native parameters";
is two-native($native-str, 2), "two:x:2",
    "a native container beside a boxed value dispatches to a two native candidate";

# Native uint and num containers keep their first pass match.
my uint $native-uint = 7;
is uint-only($native-uint), "uint:7",
    "a native uint container dispatches to a lone uint candidate";
my num $native-num = 1.5e0;
is num-only($native-num), "num:1.5",
    "a native num container dispatches to a lone num candidate";
multi sub uint-tie(uint $u) { "uint" }
multi sub uint-tie(Int $i) { "Int" }
is uint-tie(1), "Int",
    "a boxed integer keeps choosing the Int candidate over the uint one";
is uint-tie($native-uint), "uint",
    "a native uint container keeps choosing the uint candidate";

# One call site must re-dispatch when the argument changes kind.
my @replay;
for "a", "toolong", "b" -> $s {
    @replay.push((try constrained($s)) // "no-match");
}
is-deeply @replay, ["short:a", "no-match", "short:b"],
    "one call site re-evaluates the bind check per argument";
sub via-capture(|c) { tie-break(|c) }
my @kinds;
@kinds.push(via-capture(1));
@kinds.push(via-capture($native-int));
@kinds.push(via-capture(2));
@kinds.push(via-capture($native-int));
is-deeply @kinds, ["Int", "int", "Int", "int"],
    "one call site distinguishes native containers from boxed values";
sub via-value($x) { str-only($x) }
my @concreteness;
@concreteness.push(via-value("a"));
@concreteness.push((try via-value(Str)) // "no-match");
@concreteness.push(via-value("b"));
is-deeply @concreteness, ["str:a", "no-match", "str:b"],
    "one call site distinguishes concrete values from type objects";

# Candidates that can only match by unboxing are tried in narrowness
# order, and a failed bind check moves on to the next one.
multi sub ordered(str $s where *.chars == 1) { "first" }
multi sub ordered(str $s) { "second" }
is ordered("a"), "first",
    "the narrower candidate is tried first in the unbox retry";
is ordered("toolong"), "second",
    "a failed bind check moves on to the next candidate in the unbox retry";

# Any candidate that matches without unboxing wins, however wide.
multi sub prefer-match(str $s) { "str" }
multi sub prefer-match(Cool $s) { "Cool" }
is prefer-match("x"), "Cool",
    "a wider boxed candidate beats a narrower native candidate for a boxed value";

# A required named still filters candidates during the unbox retry.
multi sub named-skip(str $s, :$k!) { "with-k" }
multi sub named-skip(str $s) { "plain" }
is named-skip("x"), "plain",
    "a candidate missing its required named is skipped in the unbox retry";
is named-skip("x", :k), "with-k",
    "a candidate with its required named present wins the unbox retry";

# Method candidates keep the same tie behavior as subs.
class MethodTie {
    multi method m(int $i) { "int" }
    multi method m(Int $i) { "Int" }
}
is MethodTie.m(1), "Int",
    "a boxed integer keeps choosing the Int method candidate";

# Introspection agrees with the dispatcher.
multi sub cando-target(str $s) { $s }
is +&cando-target.cando(\("f")), 1,
    "cando finds a lone str candidate for a Str argument";
is +&cando-target.cando(\(42)), 0,
    "cando finds no candidate for an Int argument against a lone str candidate";
is +&tie-break.cando(\(1)), 1,
    "cando keeps reporting only the Int candidate for a boxed integer";
is +&cando-target.cando(\(Str)), 0,
    "cando finds no candidate for a type object against a lone str candidate";
is +&rw-target.cando(\("f")), 0,
    "cando finds no candidate for a boxed value against a lone native rw candidate";
lives-ok { &uint-only.cando(\(1)) },
    "cando survives a boxed integer against a lone uint candidate";
is +&uint-only.cando(\(1)), 0,
    "cando reports no uint candidate for a boxed integer while the trial binder cannot bind a uint lexical";
multi sub big-cando(int $i) { $i }
is +&big-cando.cando(\(2 ** 70)), 0,
    "cando counts a throwing trial bind as not bindable for a too-wide Int";

# A candidate that is in the running suppresses the unbox retry even
# when its bind check fails, so cando and dispatch stay in agreement.
multi sub gated(Int $i where * > 5) { "big" }
multi sub gated(int $i) { "native" }
is gated(9), "big",
    "a matchable constrained candidate wins over an unbox-only candidate";
throws-like { gated(3) }, X::Multi::NoMatch,
    "a failed bind check on a matchable candidate suppresses the unbox retry";
is +&gated.cando(\(3)), 0,
    "cando agrees the unbox retry is suppressed by a matchable candidate";
is +&gated.cando(\(9)), 1,
    "cando reports the matchable constrained candidate when its bind check passes";

# A candidate the named arguments rule out was never in the running,
# so it does not suppress the unbox retry.
multi sub alias-gate(Int $x, :aa(:$a)!) { "aliased" }
multi sub alias-gate(int $x) { "n:$x" }
is alias-gate(42), "n:42",
    "a candidate missing its aliased required named does not suppress the unbox retry";
is alias-gate(42, :aa), "aliased",
    "a candidate with its aliased required named present wins outright";
is +&alias-gate.cando(\(42)), 1,
    "cando agrees the unbox retry runs when an aliased required named is missing";
multi sub open-named(str $s, *%opts) { "native" }
multi sub open-named(Cool $c where *.chars > 99) { "Cool" }
is open-named("x", :k(1)), "native",
    "a candidate rejecting a passed named does not suppress the unbox retry";
is +&open-named.cando(\("x", :k(1))), 1,
    "cando agrees the unbox retry runs when a candidate rejects a passed named";

# vim: expandtab shiftwidth=4
