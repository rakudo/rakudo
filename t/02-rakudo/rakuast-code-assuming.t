use lib <t/02-rakudo/test-packages>;
use Test;
use AssumingExternalType;

plan 7;

# External (use-imported) typed named arg curried.
{
    sub transport(AssumingExternalType :$thing!, Str :$payload) {
        "thing.value=" ~ $thing.value ~ " payload=" ~ $payload
    }
    is &transport.assuming(thing => AssumingExternalType.new(42))(payload => "hi"),
       "thing.value=42 payload=hi",
       'assuming over a sub with an externally-defined typed parameter';
}

# Externally-typed named arg curried inside a class's BUILD.
{
    class C {
        has Code $!transport;
        submethod BUILD(AssumingExternalType :$thing!) {
            $!transport = &transport.assuming(thing => $thing);
        }
        sub transport(AssumingExternalType :$thing!, Str :$msg!) {
            "$thing.value()/$msg"
        }
        method call(Str $msg) { $!transport(:$msg) }
    }
    is C.new(thing => AssumingExternalType.new(99)).call("hello"),
       "99/hello",
       'assuming from a BUILD submethod, currying a typed named arg';
}

# Exercises the `RakuAST::PointyBlock` branch of `Code.assuming`,
# distinct from the `RakuAST::Sub` branch used by the named-sub
# tests above.
{
    my $b = -> AssumingExternalType $t, Str $msg { "$t.value()/$msg" };
    is $b.assuming(AssumingExternalType.new(7))("hi"),
       "7/hi",
       'assuming over a pointy block with an externally-defined typed parameter';
}

{
    sub f($x, $y, $z) { $x + $y + $z }
    is &f.assuming(1, 2)(10), 13, 'plain positional assuming still works';
}

# Containerised positional declared inline at the call site: the
# synthesised closure must not depend on `@positionals` resolving
# in the caller's lexical scope, since the caller has no such
# binding.
{
    class A { our method f(:$x) { "A.f($x)" } };
    my &g = &A::f.assuming(my $a = A.new);
    is g(x => 1), 'A.f(1)',
       'assuming a containerized positional declared at the call site';
}

# `is raw` propagation: a Scalar passed positionally must still
# resolve to the live container at invocation time, so assigning
# to the source variable between `.assuming(...)` and a call
# propagates.
{
    sub a($a is raw) { $a }
    my $b = 42;
    my &c = &a.assuming($b);
    is c(), 42, 'is raw curried call sees initial container value';
    $b = 666;
    is c(), 666, 'is raw curried call sees mutated container value';
}

# vim: expandtab shiftwidth=4
