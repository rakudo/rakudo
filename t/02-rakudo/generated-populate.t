use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 56;

# Classes composed by the compiler get a generated POPULATE
# submethod compiled from the BUILDALLPLAN, on both frontends, with
# a class declaring its own POPULATE keeping it. These
# tests pin the observable construction semantics that method must
# reproduce from the generic Mu.POPULATE interpretation, and the
# introspective shape of the generated method itself.

{
    my class Point { has $.x = 1; has @.tags }
    my $m = Point.^lookup('POPULATE');
    ok $m.defined, 'a class with attributes gets its own POPULATE';
    isa-ok $m, Submethod, 'the generated POPULATE is a submethod';
    is $m.package.^name, 'Point', 'the generated POPULATE belongs to its class';
    ok $m.?is-hidden-from-backtrace, 'the generated POPULATE hides from backtraces';
    my class Bare { }
    ok Bare.^lookup('POPULATE').defined, 'a class without attributes has a POPULATE';
}

{
    my class Sigils { has $.s; has @.a; has %.h; has &.c }
    my $o = Sigils.new(s => 1, a => [1, 2], h => { x => 1 }, c => { 42 });
    is $o.s, 1, 'a scalar attribute takes its named argument';
    is-deeply $o.a, [1, 2], 'an array attribute stores its named argument';
    is-deeply $o.h, { x => 1 }, 'a hash attribute stores its named argument';
    is $o.c.(), 42, 'a code attribute takes its named argument';
}

{
    my class Defaults {
        has $.plain = 5;
        has Int $.typed = 7;
        has @.list = 1, 2;
    }
    my $o = Defaults.new;
    is $o.plain, 5, 'a constant default applies when the named is absent';
    is $o.typed, 7, 'a typed attribute default applies';
    is-deeply $o.list, [1, 2], 'an array attribute default applies';
    is Defaults.new(plain => 9).plain, 9, 'a named argument beats the default';
}

{
    my class SelfDefault { has $.base = 10; has $.derived = self.base * 2 }
    is SelfDefault.new.derived, 20, 'a default closure reads self';
}

{
    my class Req { has $.r is required }
    throws-like { Req.new }, X::Attribute::Required,
        'an absent required attribute throws';
    my class ReqInt { has int $.i is required }
    throws-like { ReqInt.new }, X::Attribute::Required,
        'an absent required native int attribute throws';
    is Req.new(r => 1).r, 1, 'a supplied required attribute binds';
}

{
    my class Bound { has Int $.v is built(:bind) }
    is Bound.new(v => 3).v, 3, 'a bound attribute takes its value';
    throws-like { Bound.new(v => 'nope') }, X::TypeCheck::Binding,
        'a bound attribute type checks its value';
    my class Hidden { has $.h is built(False) }
    nok Hidden.new(h => 5).h.defined, 'is built(False) ignores the named argument';
}

{
    my class Nat { has int $.i = 3; has num $.n = 2e0; has str $.s = 'd'; has uint $.u = 9 }
    my $o = Nat.new(i => 1, n => 1e0, s => 'x', u => 2);
    is $o.i, 1, 'a native int attribute takes its named argument';
    is $o.n, 1e0, 'a native num attribute takes its named argument';
    is $o.s, 'x', 'a native str attribute takes its named argument';
    is $o.u, 2, 'a native uint attribute takes its named argument';
    my $d = Nat.new;
    is $d.i, 3, 'a native int attribute default applies';
    is $d.n, 2e0, 'a native num attribute default applies';
    is $d.s, 'd', 'a native str attribute default applies';
    is $d.u, 9, 'a native uint attribute default applies';
}

{
    my @src = 1, 2;
    my %src = a => 1;
    my class Cont { has @.a; has %.h }
    my $o = Cont.new(a => @src, h => %src);
    nok $o.a === @src, 'an array attribute is not the argument container itself';
    nok $o.h === %src, 'a hash attribute is not the argument container itself';
    @src.push(3);
    %src<b> = 2;
    is-deeply $o.a, [1, 2], 'an array attribute holds its own copy of the argument';
    is-deeply $o.h, { a => 1 }, 'a hash attribute holds its own copy of the argument';
}

{
    my class Dfl { has @.a is default(42) }
    is Dfl.new.a[0], 42, 'an array attribute keeps its is default value';
    my class Shaped { has @.s[2] }
    is-deeply Shaped.new.s.shape, (2,), 'a shaped array attribute keeps its shape';
}

{
    my @order;
    my role R { has $.r = 3 }
    my class P { has $.p = 1; submethod TWEAK { @order.push('P') } }
    my class C is P does R { has $.c = 2; submethod TWEAK { @order.push('C') } }
    my $o = C.new;
    is $o.p, 1, 'a parent attribute default applies in a compiled subclass';
    is $o.r, 3, 'a role attribute default applies in a compiled class';
    is $o.c, 2, 'an own attribute default applies alongside inherited ones';
    is-deeply @order, ['P', 'C'], 'a parent TWEAK runs before the child TWEAK';
    my $n = C.new(p => 9, r => 8, c => 7);
    is $n.p ~ $n.r ~ $n.c, '987', 'named arguments reach attributes at every level';
}

{
    my class BuildKeeps { has $.x = 5; submethod BUILD() { } }
    is BuildKeeps.new.x, 5, 'a default applies after a BUILD that leaves the attribute alone';
    my class BuildWrites { has $.x = 5; submethod BUILD() { $!x = 1 } }
    is BuildWrites.new.x, 1, 'a BUILD write suppresses the default';
}

{
    my class BoundDflt { has Int $.v is built(:bind) = 5; has $.u is built(:bind) = 6 }
    is BoundDflt.new.v, 5, 'a typed bound attribute default binds';
    is BoundDflt.new.u, 6, 'an untyped bound attribute default binds';
    my $b = BoundDflt.new(v => 1, u => 2);
    is $b.v + $b.u, 3, 'named arguments beat bound defaults';
}

{
    my class ReqNum { has num $.n is required }
    throws-like { ReqNum.new }, X::Attribute::Required,
        'an absent required native num attribute throws';
    my class ReqStr { has str $.s is required }
    throws-like { ReqStr.new }, X::Attribute::Required,
        'an absent required native str attribute throws';
    my class ReqUint { has uint $.u is required }
    throws-like { ReqUint.new }, X::Attribute::Required,
        'an absent required native uint attribute throws';
    is ReqStr.new(s => '').s, '', 'an empty string satisfies a required native str attribute';
}

{
    my class WithBuild { has $.x; submethod BUILD(:$x) { $!x = ($x // 0) * 2 } }
    is WithBuild.new(x => 21).x, 42, 'BUILD receives the constructor nameds';
    my class WithTweak { has $.x = 1; submethod TWEAK() { $!x++ } }
    is WithTweak.new.x, 2, 'TWEAK runs after attribute initialization';
    my class FailBuild { submethod BUILD() { fail 'nope' } }
    isa-ok FailBuild.new, Failure, 'a Failure from BUILD becomes the .new result';
}

{
    my class Custom { has $.x; method POPULATE(%h) { $!x = 123; self } }
    is Custom.new(x => 5).x, 123, 'a user written POPULATE is not replaced';
}

{
    my $seen-file;
    my class FrameProbe {
        has $.x;
        submethod TWEAK { $seen-file = callframe(1).file }
    }
    FrameProbe.new(x => 1);
    ok $seen-file.contains('generated-populate'),
        'the generated POPULATE frame carries the file of its class';
}

{
    my \RT = Metamodel::ClassHOW.new_type(name => 'RT');
    RT.^add_attribute(Attribute.new(
        name => '$!v', type => Mu, package => RT, has_accessor => 1));
    RT.^compose;
    is RT.new(v => 7).v, 7, 'a class composed at runtime still constructs';
    my class Parent { has $.p = 1 }
    my \Kid = Metamodel::ClassHOW.new_type(name => 'Kid');
    Kid.^add_parent(Parent);
    Kid.^add_attribute(Attribute.new(
        name => '$!k', type => Mu, package => Kid, has_accessor => 1));
    Kid.^compose;
    my $kid = Kid.new(p => 3, k => 4);
    is $kid.p + $kid.k, 7, 'a runtime subclass of a compiled class constructs';
}

{
    my $tmp = make-temp-dir;
    $tmp.add('PopulateRoundTrip.rakumod').spurt: q:to/EOF/;
    class RoundTrip is export {
        has Int $.v is built(:bind) = 5;
        has @.a is default(42);
        has str $.s is required;
    }
    EOF
    is-run 'use PopulateRoundTrip; my $o = RoundTrip.new(s => "x"); print $o.v, $o.a[0], $o.s',
        'a precompiled class constructs through its generated POPULATE',
        :compiler-args['-I', $tmp.absolute], :out<542x>;
}

# vim: expandtab shiftwidth=4
