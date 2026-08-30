use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 96;

# A call to a named setting routine compiles its callee lookup as a static
# one, which the VM may resolve a single time. So does a call to a routine
# bound once in the outermost scope of the compilation unit: a sub
# declaration, which cannot be rebound, or an import, which is installed
# once at load. A `my &f` variable keeps the plain lookup since it can be
# rebound at runtime, and so does a routine declared in a nested scope,
# whose enclosing frame may bind a fresh clone per entry.

sub qast-op-named (Mu $qast, Str:D $op, Str:D $name --> Bool:D) {
    if nqp::istype($qast, QAST::Op) && $qast.op eq $op && $qast.name eq $name {
        return True;
    }
    elsif qast-descendable $qast {
        for $qast.list {
            qast-op-named $_, $op, $name and return True;
        }
    }
    False
}

# These observe the emitted QAST.
qast-is 'my $x = 1; say $x', -> \v {
        qast-op-named(v, 'callstatic', '&say')
    and not qast-op-named(v, 'call', '&say')
}, 'a call to a setting routine compiles to a static callee lookup';

qast-is 'my $s = "hi"; chars $s', -> \v {
        qast-op-named(v, 'callstatic', '&chars')
    and not qast-op-named(v, 'call', '&chars')
}, 'a setting routine whose value is used compiles to a static callee lookup';

qast-is '{ my sub f($x) { return $x + 1 }; f(1) }', :full, -> \v {
        qast-op-named(v, 'call', '&f')
    and not qast-op-named(v, 'callstatic', '&f')
}, 'a call to a nested user routine keeps the plain callee lookup';

qast-is 'sub foo($x) { return $x }; foo(1)', -> \v {
        qast-op-named(v, 'callstatic', '&foo')
    and not qast-op-named(v, 'call', '&foo')
}, 'a call to a sub declared in the outermost scope compiles to a static callee lookup';

qast-is 'multi sub mf(Int $x) { return 1 }; multi sub mf(Str $x) { return 2 }; mf(1)', -> \v {
        qast-op-named(v, 'callstatic', '&mf')
    and not qast-op-named(v, 'call', '&mf')
}, 'a call to a multi declared in the outermost scope compiles to a static callee lookup';

# The recursive call inside the sub is not asserted on: the sub's own name
# is visible in its own scope, so the mark declines it there.
qast-is 'sub fact($n) { return 1 if $n < 2; fact($n - 1) * $n }; fact(5)', -> \v {
    qast-op-named(v, 'callstatic', '&fact')
}, 'the outer call to a recursive sub compiles to a static callee lookup';

qast-is 'use Test; plan 1', -> \v {
        qast-op-named(v, 'callstatic', '&plan')
    and not qast-op-named(v, 'call', '&plan')
}, 'a call to an imported routine compiles to a static callee lookup';

qast-is 'my &foo = sub { 1 }; foo()', -> \v {
        qast-op-named(v, 'call', '&foo')
    and not qast-op-named(v, 'callstatic', '&foo')
}, 'a call through a routine variable keeps the plain callee lookup';

# A single comparison is a static lookup under both frontends, though they
# differ in shape: the legacy optimizer first rewrites the one-link chain
# to a plain call.
qast-is 'my $a = 1; my $b = 2; $a == $b', -> \v {
    (qast-op-named(v, 'chainstatic', '&infix:<==>')
        or qast-op-named(v, 'callstatic', '&infix:<==>'))
    and not qast-op-named(v, 'chain', '&infix:<==>')
    and not qast-op-named(v, 'call', '&infix:<==>')
}, 'a comparison against a setting operator compiles to a static callee lookup';

qast-is 'my $a = 1; my $b = 2; my $c = 3; $a == $b == $c', -> \v {
        qast-op-named(v, 'chainstatic', '&infix:<==>')
    and not qast-op-named(v, 'chain', '&infix:<==>')
}, 'a chained comparison against a setting operator compiles to static chain links';

qast-is '{ my multi sub infix:<==>(\a, \b) { return 3 }; my $x = "a"; my $y = "b"; $x == $y }', :full, -> \v {
    not qast-op-named(v, 'chainstatic', '&infix:<==>')
}, 'a comparison against a nested user operator keeps the plain lookup';

qast-is 'my $x = 1; my $y = $x * 2', -> \v {
        qast-op-named(v, 'callstatic', '&infix:<*>')
    and not qast-op-named(v, 'call', '&infix:<*>')
}, 'a setting infix that does not chain compiles to a static callee lookup';

qast-is 'my $x = 5; my $y = -$x', -> \v {
        qast-op-named(v, 'callstatic', '&prefix:<->')
    and not qast-op-named(v, 'call', '&prefix:<->')
}, 'a setting prefix compiles to a static callee lookup';

qast-is 'my $x = 1; my $y = $x++', -> \v {
        qast-op-named(v, 'callstatic', '&postfix:<++>')
    and not qast-op-named(v, 'call', '&postfix:<++>')
}, 'a setting postfix compiles to a static callee lookup';

qast-is '{ my sub prefix:<neg>(\a) { return 0 }; my $x = 1; neg $x }', :full, -> \v {
    not qast-op-named(v, 'callstatic', '&prefix:<neg>')
}, 'a nested user prefix keeps the plain callee lookup';

qast-is '{ my sub infix:<mul>(\a, \b) { return 3 }; my $x = 1; my $y = 2; $x mul $y }', :full, -> \v {
    not qast-op-named(v, 'callstatic', '&infix:<mul>')
}, 'a nested user infix keeps the plain callee lookup';

qast-is 'my &prefix:<-> = sub ($a) { 99 }; my $x = 5; my $y = -$x', -> \v {
    not qast-op-named(v, 'callstatic', '&prefix:<->')
}, 'a prefix bound through an operator variable keeps the plain callee lookup';

# A routine the syntax calls without a call node of its own compiles the
# same static lookup: a subscript, an array or hash composer, a term that
# names a routine, a list operator, the subscript a capture variable
# reads the match with, and the operator beneath an assignment or reverse
# meta-op.
qast-is 'my @a = 1, 2; my $x = @a[0]', -> \v {
        qast-op-named(v, 'callstatic', '&postcircumfix:<[ ]>')
    and not qast-op-named(v, 'call', '&postcircumfix:<[ ]>')
}, 'an array subscript compiles to a static callee lookup';

qast-is 'my %h = a => 1; my $x = %h{"a"}', -> \v {
        qast-op-named(v, 'callstatic', '&postcircumfix:<{ }>')
    and not qast-op-named(v, 'call', '&postcircumfix:<{ }>')
}, 'a hash subscript compiles to a static callee lookup';

qast-is 'my %h = a => 1; my $x = %h<a>', -> \v {
        qast-op-named(v, 'callstatic', '&postcircumfix:<{ }>')
    and not qast-op-named(v, 'call', '&postcircumfix:<{ }>')
}, 'a literal hash subscript compiles to a static callee lookup';

qast-is 'my @a = 1, 2; my $x = @a.[0]', -> \v {
        qast-op-named(v, 'callstatic', '&postcircumfix:<[ ]>')
    and not qast-op-named(v, 'call', '&postcircumfix:<[ ]>')
}, 'a subscript applied with a dot compiles to a static callee lookup';

qast-is '$_ = [1, 2]; my $x = .[0]', -> \v {
        qast-op-named(v, 'callstatic', '&postcircumfix:<[ ]>')
    and not qast-op-named(v, 'call', '&postcircumfix:<[ ]>')
}, 'a subscript applied to the topic compiles to a static callee lookup';

qast-is 'my int @a = 1, 2; my int $i = 0; my $x = @a[$i]', -> \v {
        qast-op-named(v, 'callstatic', '&postcircumfix:<[ ]>')
    and not qast-op-named(v, 'call', '&postcircumfix:<[ ]>')
}, 'the general call behind a native subscript compiles to a static callee lookup';

qast-is 'sub postcircumfix:<[ ]>(\a, \i) { 2 }; my @a = 1; my $x = @a[0]', -> \v {
        qast-op-named(v, 'callstatic', '&postcircumfix:<[ ]>')
    and not qast-op-named(v, 'call', '&postcircumfix:<[ ]>')
}, 'a subscript against a routine declared in the outermost scope compiles to a static callee lookup';

qast-is '{ my sub postcircumfix:<[ ]>(\a, \i) { 2 }; my @a = 1; my $x = @a[0] }', :full, -> \v {
    not qast-op-named(v, 'callstatic', '&postcircumfix:<[ ]>')
}, 'a subscript against a nested user routine keeps the plain callee lookup';

qast-is 'my &postcircumfix:<[ ]> = sub (\a, \i) { 1 }; my @a = 1; my $x = @a[0]', -> \v {
    not qast-op-named(v, 'callstatic', '&postcircumfix:<[ ]>')
}, 'a subscript bound through a routine variable keeps the plain callee lookup';

qast-is 'my &rand = sub { 1 }; my $x = rand', -> \v {
    not qast-op-named(v, 'callstatic', '&rand')
}, 'the rand term bound through a routine variable keeps the plain callee lookup';

qast-is 'my %h = a => 1; %h{"a"} := 5', -> \v {
        qast-op-named(v, 'callstatic', '&postcircumfix:<{ }>')
    and not qast-op-named(v, 'call', '&postcircumfix:<{ }>')
}, 'a bind into a hash subscript compiles to a static callee lookup';

qast-is 'my %h = a => 1; %h<a> := 5', -> \v {
        qast-op-named(v, 'callstatic', '&postcircumfix:<{ }>')
    and not qast-op-named(v, 'call', '&postcircumfix:<{ }>')
}, 'a bind into a literal hash subscript compiles to a static callee lookup';

qast-is 'my $x = 1; my $h = :{ a => $x }', -> \v {
        qast-op-named(v, 'callstatic', '&circumfix:<:{ }>')
    and not qast-op-named(v, 'call', '&circumfix:<:{ }>')
}, 'an object hash composer compiles to a static callee lookup';

qast-is 'my @a = 1, 2; @a[0] := 5', -> \v {
        qast-op-named(v, 'callstatic', '&postcircumfix:<[ ]>')
    and not qast-op-named(v, 'call', '&postcircumfix:<[ ]>')
}, 'a bind into an array subscript compiles to a static callee lookup';

qast-is 'my $x = 1; my $a = [$x, 2]', -> \v {
        qast-op-named(v, 'callstatic', '&circumfix:<[ ]>')
    and not qast-op-named(v, 'call', '&circumfix:<[ ]>')
}, 'an array composer compiles to a static callee lookup';

qast-is 'my $x = 1; my $h = { a => $x }', -> \v {
        qast-op-named(v, 'callstatic', '&circumfix:<{ }>')
    and not qast-op-named(v, 'call', '&circumfix:<{ }>')
}, 'a hash composer compiles to a static callee lookup';

qast-is 'my $x = rand', -> \v {
        qast-op-named(v, 'callstatic', '&rand')
    and not qast-op-named(v, 'call', '&rand')
}, 'the rand term compiles to a static callee lookup';

qast-is 'my $x = time', -> \v {
        qast-op-named(v, 'callstatic', '&term:<time>')
    and not qast-op-named(v, 'call', '&term:<time>')
}, 'a named term compiles to a static callee lookup';

qast-is 'my $x = 1; my $l = ($x, 2)', -> \v {
        qast-op-named(v, 'callstatic', '&infix:<,>')
    and not qast-op-named(v, 'call', '&infix:<,>')
}, 'a list operator compiles to a static callee lookup';

qast-is 'my $s = 0; my $x = 2; $s += $x', -> \v {
        qast-op-named(v, 'callstatic', '&infix:<+>')
    and not qast-op-named(v, 'call', '&infix:<+>')
}, 'the operator beneath an assignment meta-op compiles to a static callee lookup';

qast-is 'my $x = 1; my $y = 2; my $z = $x R- $y', -> \v {
        qast-op-named(v, 'callstatic', '&infix:<->')
    and not qast-op-named(v, 'call', '&infix:<->')
}, 'the operator beneath a reverse meta-op compiles to a static callee lookup';

qast-is '"ab" ~~ /(a)/; my $x = $0', -> \v {
        qast-op-named(v, 'callstatic', '&postcircumfix:<[ ]>')
    and not qast-op-named(v, 'call', '&postcircumfix:<[ ]>')
}, 'a positional capture variable compiles to a static callee lookup';

qast-is '"ab" ~~ /$<x>=(a)/; my $x = $<x>', -> \v {
        qast-op-named(v, 'callstatic', '&postcircumfix:<{ }>')
    and not qast-op-named(v, 'call', '&postcircumfix:<{ }>')
}, 'a named capture variable compiles to a static callee lookup';

# The soft pragma promises late rebinding, so this frontend declines
# the mark under it, while the legacy optimizer still pins a setting
# callee that is not itself soft.
if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'use soft; my $x = 5; my $y = -$x', -> \v {
        not qast-op-named(v, 'callstatic', '&prefix:<->')
    }, 'a setting prefix under the soft pragma keeps the plain callee lookup';
    qast-is 'sub sfoo() { 1 }; BEGIN &sfoo does role { method soft(--> True) { } }; sfoo()', :full, -> \v {
        not qast-op-named(v, 'callstatic', '&sfoo')
    }, 'a call to a routine marked soft keeps the plain callee lookup';
    # The legacy frontend compiles the empty set term to a shape of its
    # own.
    qast-is 'my $x = ∅', -> \v {
            qast-op-named(v, 'callstatic', '&set')
        and not qast-op-named(v, 'call', '&set')
    }, 'the empty set term compiles to a static callee lookup';
    # The legacy frontend compiles a smartmatch to a shape of its own.
    qast-is 'sub f() { 2 }; my $x = 1; my $y = $x ~~ f()', -> \v {
            qast-op-named(v, 'callstatic', '&infix:<~~>')
        and not qast-op-named(v, 'call', '&infix:<~~>')
    }, 'a smartmatch compiles to a static callee lookup';
    qast-is 'use soft; my @a = 1; my $x = @a[0]', -> \v {
        not qast-op-named(v, 'callstatic', '&postcircumfix:<[ ]>')
    }, 'an array subscript under the soft pragma keeps the plain callee lookup';
    qast-is 'use soft; my $x = rand', -> \v {
        not qast-op-named(v, 'callstatic', '&rand')
    }, 'the rand term under the soft pragma keeps the plain callee lookup';
    qast-is 'use soft; my $x = 1; my $l = ($x, 2)', -> \v {
        not qast-op-named(v, 'callstatic', '&infix:<,>')
    }, 'a list operator under the soft pragma keeps the plain callee lookup';
    # A routine declared after the use takes the name at run time, so
    # the lookup the use resolved to is not the one to pin.
    qast-is 'my $x = rand; sub rand() { 1 }', -> \v {
        not qast-op-named(v, 'callstatic', '&rand')
    }, 'the rand term shadowed by a later routine keeps the plain callee lookup';
    qast-is 'my @a = 1; my $x = @a[0]; sub postcircumfix:<[ ]>(\a, \i) { 2 }', -> \v {
        not qast-op-named(v, 'callstatic', '&postcircumfix:<[ ]>')
    }, 'an array subscript shadowed by a later routine keeps the plain callee lookup';
    qast-is '"ab" ~~ /(a)/; my $x = $0; sub postcircumfix:<[ ]>(\a, \i) { 2 }', -> \v {
        not qast-op-named(v, 'callstatic', '&postcircumfix:<[ ]>')
    }, 'a positional capture variable shadowed by a later routine keeps the plain callee lookup';
    qast-is 'use soft; my $x = 1; my $a = [$x, 2]', -> \v {
        not qast-op-named(v, 'callstatic', '&circumfix:<[ ]>')
    }, 'an array composer under the soft pragma keeps the plain callee lookup';
    qast-is 'use soft; my $x = 1; my $y = 2; my $z = $x R- $y', -> \v {
        not qast-op-named(v, 'callstatic', '&infix:<->')
    }, 'the operator beneath a reverse meta-op under the soft pragma keeps the plain callee lookup';
}
else {
    skip 'the soft shape is specific to the RakuAST frontend', 12;
}

# A routine marked soft promises late rebinding, so a wrapper
# installed at runtime must take effect at a call site compiled
# before it.
{
    sub swrapped() { 1 }
    BEGIN &swrapped does role { method soft(--> True) { } };
    is swrapped(), 1, 'a routine marked soft runs normally before any wrap';
    &swrapped.wrap(sub () { 1 + callsame });
    is swrapped(), 2,
        'a wrapper installed at runtime takes effect at a call site compiled before it';
}

# These observe that statically looked up callees still behave.
{
    my $s = "HI";
    is chars($s), 2, 'a setting routine called by name returns its value';
}
{
    sub f { return 42; 99 }
    is f(), 42, 'return unwinds through a static callee lookup';
}
{
    is (gather { take 1; take 2 }).join(','), '1,2',
        'take reaches the enclosing gather through a static callee lookup';
}
{
    sub g { fail "nope" }
    my $f = g();
    ok $f ~~ Failure, 'fail produces a Failure through a static callee lookup';
    $f.so;
}
{
    my @a = 1, 2, 3;
    is elems(@a), 3, 'a multi setting routine called by name returns its value';
}
{
    my $out = do { use soft; my $s = "hi"; uc $s };
    is $out, 'HI', 'a setting routine still runs under the soft pragma';
}
# These subs live in the test file's outermost scope so the calls below
# exercise the static lookup path at runtime. Their names are distinct from
# the ones in the compiled snippets above, which share this file's context.
my $base = 10;
sub rt-fact($n) { return 1 if $n < 2; rt-fact($n - 1) * $n }
multi sub rt-mf(Int $x) { return 1 }
multi sub rt-mf(Str $x) { return 2 }
sub rt-add($x) { $base + $x }

is rt-fact(5), 120, 'a recursive outermost-scope sub computes through static lookups';
is rt-mf(1), 1, 'a multi called with an Int picks the Int candidate';
is rt-mf("x"), 2, 'a multi called with a Str picks the Str candidate';
is rt-add(5), 15, 'an outermost-scope sub closing over a mainline lexical reads it';

# Chained comparisons still follow the chaining protocol through static
# operator lookups.
my $lo = 1;
my $mid = 2;
my $hi = 3;
ok $lo < $mid < $hi, 'a true chained comparison holds through static links';
nok $lo < $hi < $mid, 'a false chained comparison fails through static links';
my $rt-mid-calls = 0;
sub rt-mid { $rt-mid-calls++; 2 }
ok 1 < rt-mid() < 3, 'a chained comparison with a call in the middle holds';
is $rt-mid-calls, 1, 'the middle operand of a chained comparison runs once';
ok ?(any(1, 2) < 3), 'a Junction autothreads through a static comparison';

# Operator calls still behave through static callee lookups.
my $neg-me = 5;
is -$neg-me, -5, 'a setting prefix called through a static lookup returns its value';
my $step-me = 5;
my $stepped = $step-me++;
is $stepped, 5, 'a postfix increment through a static lookup yields the original value';
is $step-me, 6, 'a postfix increment through a static lookup steps the variable';
sub infix:<rt-mul>($a, $b) { $a * $b }
is 2 rt-mul 3, 6, 'a user infix declared in the outermost scope computes through a static lookup';
sub prefix:<rt-neg>($a) { 0 - $a }
&prefix:<rt-neg>.wrap(-> $a { 999 });
is rt-neg 5, 999, 'a wrapped prefix in the outermost scope runs its wrapper through a static lookup';

# Routines the syntax calls without a call node still behave through
# static callee lookups, and so does the operator beneath a meta-op.
{
    my @a = 1, 2;
    my %h = a => 3, b => 4;
    is @a[1], 2, 'an array subscript through a static lookup reads the element';
    is %h{'a'}, 3, 'a hash subscript through a static lookup reads the value';
    is %h<b>, 4, 'a literal hash subscript through a static lookup reads the value';
    @a[0] := 9;
    is @a[0], 9, 'a bind into an array subscript through a static lookup binds the element';
    my $x = 5;
    is [$x, 2].elems, 2, 'an array composer through a static lookup composes the array';
    is { a => $x }<a>, 5, 'a hash composer through a static lookup composes the hash';
    ok 0 <= rand < 1, 'the rand term through a static lookup yields a number below one';
    ok time > 0, 'a named term through a static lookup yields its value';
    is ∅.elems, 0, 'the empty set term through a static lookup yields the empty set';
    is ($x, 2).elems, 2, 'a list operator through a static lookup builds the list';
    my $sum = 1;
    $sum += $x;
    is $sum, 6, 'an assignment meta-op through a static lookup steps the variable';
    is 2 R- 5, 3, 'a reverse meta-op through a static lookup applies the reversed operator';
    "ab" ~~ /(a)$<x>=(b)/;
    is $0, 'a', 'a positional capture variable through a static lookup reads the match';
    is $<x>, 'b', 'a named capture variable through a static lookup reads the match';
}
{
    sub rt-rand() { 7 }
    is rand, 7, 'the rand term reaches a routine declared in the outermost scope of its block';
    sub rand() { rt-rand() }
}
{
    sub rt-mk($n) { my sub postcircumfix:<[ ]>(\a, \i) { $n }; my @a = 1; @a[0] }
    is rt-mk(5), 5, 'a subscript reaches a nested routine on the first entry';
    is rt-mk(6), 6, 'a subscript reaches the fresh clone of a nested routine on the next entry';
}
{
    my &rand = sub { 1 };
    my $first = rand;
    &rand = sub { 2 };
    is rand, 2, 'the rand term sees a routine variable rebound after an earlier use';
}
# An assignment into a subscript goes to the subscripted object's assign
# method, which decides the assignment when the read would not yield a
# container.
{
    class RtKeyed {
        has %.s;
        method AT-KEY($k) { %!s{$k} // 'none' }
        method ASSIGN-KEY($k, $v) { %!s{$k} = "set-$v" }
    }
    my $c = RtKeyed.new;
    my $k = 'a';
    $c{$k} = 5;
    is $c.s<a>, 'set-5', 'an assignment into a hash subscript reaches the assign method';
    class RtIndexed {
        has $.got;
        method AT-POS(*@i) { 'none' }
        method ASSIGN-POS(*@i) { $!got = @i.join(',') }
    }
    my $d = RtIndexed.new;
    $d[0; 1] = 5;
    is $d.got, '0,1,5', 'an assignment into a multi-dimensional subscript reaches the assign method';
}

# The fatalize pass recognizes a static callee lookup both as a call whose
# Failure it promotes and as a boolifying consumer that disarms its argument.
sub rt-will-fail() { fail 'nope' }
{
    sub rt-nested-fail() { fail 'nope' }
    lives-ok { use fatal; my $x = defined rt-nested-fail(); 1 },
        'use fatal respects defined through a static callee lookup';
}
dies-ok { use fatal; my $x = rt-will-fail(); 1 },
    'use fatal promotes a Failure from a static callee lookup';

# vim: expandtab shiftwidth=4
