use lib <t/packages/Test-Helpers>;
use Test::Helpers;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 72;

# A meta-op over a setting operator whose name no later declaration
# shadows is formed once at compile time and emitted as a constant,
# since the operator lookup yields the same code object at run time. A
# meta-op formed at run time makes the formation call and allocates a
# closure per evaluation, which stays the path for a lexical operator,
# for a meta-op operand, and for a setting operator a later declaration
# shadows. The shapes the assertions pin down are this frontend's.

sub qast-wval-callee (Mu $qast --> Bool:D) {
    if nqp::istype($qast, QAST::Op)
        && ($qast.op eq 'call' || $qast.op eq 'chain') {
        for $qast.list {
            return True if nqp::istype($_, QAST::WVal);
            last;
        }
    }
    if qast-descendable $qast {
        for $qast.list {
            qast-wval-callee $_ and return True;
        }
    }
    False
}

sub qast-var-named (Mu $qast, Str:D $name --> Bool:D) {
    if nqp::istype($qast, QAST::Var) && $qast.name eq $name && !$qast.decl {
        return True;
    }
    if qast-descendable $qast {
        for $qast.list {
            qast-var-named $_, $name and return True;
        }
    }
    False
}

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'my @r = (1,2) Z+ (3,4)', :full, -> \v {
        not qast-contains-call(v, '&METAOP_ZIP')
        and qast-wval-callee(v)
    }, 'a zip of a setting operator becomes a constant meta-op callee';

    qast-is 'my @r = (1,2) X~ (3,4)', :full, -> \v {
        not qast-contains-call(v, '&METAOP_CROSS')
        and qast-wval-callee(v)
    }, 'a cross of a setting operator becomes a constant meta-op callee';

    qast-is 'my @a = 1,2; my @b = 3,4; my @r = @a »+« @b', :full, -> \v {
        not qast-contains-call(v, '&METAOP_HYPER')
        and qast-wval-callee(v)
    }, 'a hyper of a setting operator becomes a constant meta-op callee';

    qast-is 'my $x = 1; my $y = 2; say $x !== $y', :full, -> \v {
        not qast-contains-call(v, '&METAOP_NEGATE')
        and qast-wval-callee(v)
    }, 'a negation of a setting operator becomes a constant meta-op callee';

    qast-is 'my @r = 1 R, 2', :full, -> \v {
        not qast-contains-call(v, '&METAOP_REVERSE')
        and qast-wval-callee(v)
    }, 'a reversal of a setting operator becomes a constant meta-op callee';

    qast-is 'sub infix:<foo>($a,$b) { $a + $b }; my @r = (1,2) Zfoo (3,4)', :full, -> \v {
        qast-contains-call(v, '&METAOP_ZIP')
    }, 'a zip of a lexical operator still forms its meta-op at run time';

    # The zip declines over an assign meta-op operand, while the plain
    # operator inside that operand still emits as a constant rather
    # than a lookup by name.
    qast-is 'my @a = 1,2; @a Z+= (3,4)', :full, -> \v {
        qast-contains-call(v, '&METAOP_ZIP')
        and qast-contains-call(v, '&METAOP_ASSIGN')
        and not qast-var-named(v, '&infix:<+>')
    }, 'a zip of an assign meta-op forms at run time over a constant operator';

    qast-is 'my @r = (1,2) ZR- (10,20)', :full, -> \v {
        qast-contains-call(v, '&METAOP_ZIP')
        and not qast-contains-call(v, '&METAOP_REVERSE')
    }, 'a zip of a reversal forms at run time over a constant reversal';

    # A standalone negated comparison compiles as the setting prefix !
    # around the plain comparison, so no chain op remains, while a link
    # of a longer chain keeps the chain protocol.
    qast-is 'my $x = 1; my $y = 2; say $x !== $y', :full, -> \v {
        not qast-contains-op(v, 'chain')
    }, 'a standalone negated comparison compiles without a chain op';

    qast-is 'my ($a,$b,$c) = 1,2,3; say $a !== $b !== $c', :full, -> \v {
        qast-contains-op(v, 'chain')
        and not qast-contains-call(v, '&METAOP_NEGATE')
    }, 'a chained negated comparison keeps its chain over constant meta-ops';

    qast-is 'use soft; my $x = 1; my $y = 2; say $x !== $y', :full, -> \v {
        qast-contains-op(v, 'chain')
    }, 'a negated comparison under the soft pragma keeps its meta-op';

    # A user operator declared after the use shadows the setting's, so
    # the meta-op forms at run time and finds the user's routine.
    qast-is 'my @r = (1,2) Z+ (3,4); sub infix:<+>($a, $b) { "user" }', :full, -> \v {
        qast-contains-call(v, '&METAOP_ZIP') and not qast-wval-callee(v)
    }, 'a zip of an operator a later declaration shadows forms its meta-op at run time';
    qast-is 'my $s = [+] 1, 2, 3; sub infix:<+>($a, $b) { "user" }', :full, -> \v {
        qast-contains-call(v, '&METAOP_REDUCE_LEFT')
    }, 'a reduce of an operator a later declaration shadows forms its meta-op at run time';
    qast-is 'my $x = 1; $x += 2; sub infix:<+>($a, $b) { "user" }', :full, -> \v {
        qast-var-named(v, '&infix:<+>')
    }, 'a compound assignment with an operator a later declaration shadows looks the operator up at run time';
    qast-is 'my @r = (1,2) Z+ (3,4) Z+ (5,6); sub infix:<+>($a, $b) { "user" }', :full, -> \v {
        qast-contains-call(v, '&METAOP_ZIP') and not qast-wval-callee(v)
    }, 'a zip chain of an operator a later declaration shadows forms its meta-op at run time';

    # The soft pragma keeps the formation at run time, so the routine
    # the meta-op runs stays wrappable.
    qast-is 'use soft; my @r = (1,2) Z+ (3,4)', :full, -> \v {
        qast-contains-call(v, '&METAOP_ZIP') and not qast-wval-callee(v)
    }, 'a zip under the soft pragma forms its meta-op at run time';
    qast-is 'use soft; my $s = [+] 1, 2, 3', :full, -> \v {
        qast-contains-call(v, '&METAOP_REDUCE_LEFT')
    }, 'a reduce under the soft pragma forms its meta-op at run time';
}
else {
    skip 'the formation shapes are specific to the RakuAST frontend', 17;
}

# A user operator declared after the use is the one every meta-op runs.
{
    my $dir = make-temp-dir;
    $dir.add('MetaLateOp.rakumod').spurt: q:to/END/;
        unit module MetaLateOp;
        our sub assign() { my $x = 1; $x += 2; $x }
        our sub reduce() { [+] 1, 2, 3 }
        our sub triangle() { [\+] 1, 2 }
        our sub zip() { (1, 2) Z+ (3, 4) }
        our sub cross() { (1, 2) X~ (3, 4) }
        our sub negate() { 1 !eq 2 }
        our sub hyper() { (1, 2, 3) >>+>> (10, 20) }
        our sub zip-assign() { my @a = 1, 2; @a Z+= (3, 4); @a }
        our sub chain() { (1, 2) Z+ (3, 4) Z+ (5, 6) }
        our sub most() { [max] 1, 2, 3 }
        my role RM { method z() { (1, 2) Z+ (3, 4) } }
        class CM does RM is export { }
        sub infix:<+>($a, $b) { 'user' }
        sub infix:<~>($a, $b) { 'user' }
        sub infix:<eq>($a, $b) { True }
        sub infix:<max>(*@a) { 'user' }
        END
    is EVAL(q[use lib $dir; use MetaLateOp; &MetaLateOp::assign()]), 'user',
        'a compound assignment runs a user infix declared after the use';
    is EVAL(q[use lib $dir; use MetaLateOp; &MetaLateOp::reduce()]), 'user',
        'a reduce runs a user infix declared after the use';
    is EVAL(q[use lib $dir; use MetaLateOp; &MetaLateOp::triangle()]).join(' '), '1 user',
        'a triangle reduce runs a user infix declared after the use';
    is EVAL(q[use lib $dir; use MetaLateOp; &MetaLateOp::zip()]).join(' '), 'user user',
        'a zip runs a user infix declared after the use';
    is EVAL(q[use lib $dir; use MetaLateOp; &MetaLateOp::cross()]).join(' '), 'user user user user',
        'a cross runs a user infix declared after the use';
    is-deeply EVAL(q[use lib $dir; use MetaLateOp; &MetaLateOp::negate()]), False,
        'a negated comparison runs a user infix declared after the use';
    is EVAL(q[use lib $dir; use MetaLateOp; &MetaLateOp::hyper()]).join(' '), 'user user user',
        'a hyper runs a user infix declared after the use';
    is EVAL(q[use lib $dir; use MetaLateOp; &MetaLateOp::zip-assign()]).join(' '), 'user user',
        'a zip assignment runs a user infix declared after the use';
    is EVAL(q[use lib $dir; use MetaLateOp; &MetaLateOp::chain()]).join(' '), 'user user',
        'a zip chain runs a user infix declared after the use';
    is EVAL(q[use lib $dir; use MetaLateOp; &MetaLateOp::most()]), 'user',
        'a list reduce runs a user infix declared after the use';
    is EVAL(q[use lib $dir; use MetaLateOp; CM.new.z]).join(' '), 'user user',
        'a zip in a precompiled role method runs a user infix declared after the role';
}

# Behavior stays identical.

is ((1,2) Z+ (3,4)).join(','), '4,6',
    'a zip of a setting operator computes through its constant meta-op';

is ((1,2) X~ (3,4)).join(','), '13,14,23,24',
    'a cross of a setting operator computes through its constant meta-op';

{
    my @a = 1,2;
    my @b = 3,4;
    is (@a »+« @b).join(','), '4,6',
        'a hyper of a setting operator computes through its constant meta-op';
}

ok 1 !== 2, 'a negated setting comparison holds through its constant meta-op';

nok 1 !== 1, 'a negated setting comparison fails where the comparison holds';

ok 1 !== 2 !== 3, 'a chain of negated comparisons holds through its links';

nok 1 !== 1 !== 3, 'a chain of negated comparisons fails on a failing link';

ok 1 !== 2 == 2, 'a chain mixing negated and plain comparisons holds';

nok 5 !== any(5,6), 'a negated comparison autothreads a Junction argument';

ok so(5 !== all(6,7)), 'a negated comparison over an all Junction collapses correctly';

nok 2 !< 3, 'a negated ordering comparison fails where the ordering holds';

{
    my $calls = 0;
    sub mid() { $calls++; 2 }
    ok 1 !== mid() !== 3, 'a negated chain with a call in the middle holds';
    is $calls, 1, 'the middle operand of a negated chain runs once';
}

{
    sub infix:<same-as>($a, $b) is equiv(&infix:<==>) { $a == $b }
    ok 1 !same-as 2, 'a negated user chaining operator holds where the comparison fails';
}

nok (* !== 2)(2), 'a curried negated comparison fails on the negated value';

ok (* !== 2)(3), 'a curried negated comparison holds elsewhere';

# Parentheses make the negation a value, as they do a plain comparison
# on both frontends. The legacy frontend alone chains through them for
# the negated form.
if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    my $paren = (1 !== 2) == True;
    ok $paren, 'a parenthesized negation compares as a value, not a chain link';
}
else {
    skip 'the legacy frontend chains through a parenthesized negation', 1;
}

nok ([!==] 1, 1, 3), 'a chain reduce of a negated comparison fails on a failing link';

# A negated comparison as the left of a smartmatch is a chain link, so
# the reduced smartmatch forms decline it and the chain protocol runs.

{
    my $typematch = 1 !== 2 ~~ Bool;
    nok $typematch, 'a smartmatch of a type declines to collapse over a negated link';
}

{
    my $negated-typematch = 1 !== 2 !~~ Bool;
    ok $negated-typematch, 'a negated smartmatch of a type declines to collapse over a negated link';
}

{
    my $litmatch = 1 !== 2 ~~ 1;
    nok $litmatch, 'a smartmatch of a literal declines to collapse over a negated link';
}

{
    my $pairmatch = 1 !== 0 ~~ :so;
    nok $pairmatch, 'a smartmatch of a pair declines to collapse over a negated link';
}

# A sequenced comparison keeps every operator property, so it links
# into a chain with a negation. The legacy frontend cannot run the
# sequenced form at all.
if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    ok 1 !== 2 S== 2, 'a chain mixing negation and sequencing holds through both links';
    nok 1 !== 1 S== 1, 'a chain mixing negation and sequencing withdraws the operand mark';
}
else {
    skip 'the sequenced comparison does not run under the legacy frontend', 2;
}

is (5 R- 1), -4, 'a reversed setting operator swaps its operands';

is ((1,2) Z=> (3,4)).map({ .key ~ ':' ~ .value }).join(','), '1:3,2:4',
    'a zip building pairs computes through its constant meta-op';

is-deeply ((1,2) R, (3,4)), ((3,4),(1,2)),
    'a reversed comma builds its list through its constant meta-op';

{
    sub infix:<foo>($a, $b) { $a * $b }
    is ((1,2) Zfoo (3,4)).join(','), '3,8',
        'a zip of a lexical operator computes through its formed meta-op';
}

is ((1,2,3) »+» (10,20)).join(','), '11,22,13',
    'a right dwim hyper extends the shorter side through its constant meta-op';

is ((1,2,3) «+« (10,20)).join(','), '11,22',
    'a left dwim hyper truncates to the fixed side through its constant meta-op';

dies-ok { ((1,2,3) »+« (1,2)).eager },
    'a non dwim hyper still dies on lists of different lengths';

is ((1,2) ZR- (10,20)).join(','), '9,18',
    'a zip of a reversal computes through its constant reversal';

nok 2 R!== 2, 'a reversal of a negation compares correctly';

{
    my @a = 1,2;
    @a Z+= (10,20);
    is @a.join(','), '11,22',
        'a zip assign computes through its constant operator';
}

{
    my $a = 3;
    $a max= 7;
    is $a, 7, 'a max assign computes through its constant operator';
}

{
    use soft;
    my $h = &infix:<+>.wrap(sub ($a, $b) { 1000 });
    my @w = (1,2) Z+ (3,4);
    &infix:<+>.unwrap($h);
    my @r = (1,2) Z+ (3,4);
    is @w.join(','), '1000,1000',
        'a wrap of a setting operator shows through its constant meta-op';
    is @r.join(','), '4,6',
        'an unwrap of a setting operator shows through its constant meta-op';
}

is ((0,2) Z|| (5,6)).join(','), '5,2',
    'a zip of a thunky operator computes through its constant meta-op';

# A sequencing meta-op forms the wrapped operator's own value, so it
# composes under the other meta-ops. The legacy frontend cannot compile
# these callable forms at all.

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    is Q|&[S+]|.EVAL.(2, 3), 5,
        'a sequenced setting operator forms as the operator itself';

    isa-ok (try Q|&[RRRRRRRRZZZZZZZZZZZZZZZZZZRRRRRRRRRSSSSSSSS+]|.EVAL), Block,
        'a deep stack of meta-ops over a setting operator still forms a callable';

    ok Q|&[!==]|.EVAL.(1, 2),
        'a negated comparison in callable form computes through its meta-op';

    # The negation resolves the setting prefix !, so a lexical prefix !
    # does not intercept a negated comparison. The legacy frontend lets
    # the lexical one intercept.
    is Q|my sub prefix:<!>($x) { 'hijack' }; 1 !== 2|.EVAL, True,
        'a lexical prefix ! does not intercept a negated comparison';
}
else {
    skip 'the callable meta-op forms do not compile under the legacy frontend', 4;
}

# The formed meta-op is a closure the compiler made, so a precompiled
# module carries it through serialization.

{
    my $dir = $*TMPDIR.add("rakuast-metaop-hoist-$*PID");
    $dir.mkdir;
    $dir.add('MetaOpHoistTest.rakumod').spurt(q:to/MODULE/);
        unit module MetaOpHoistTest;
        sub combined() is export {
            ((1,2) Z+ (3,4)).join(',') ~ '|'
              ~ ((1,2) X~ (3,4)).join(',') ~ '|'
              ~ ((1,2,3) »+» (10,20)).join(',') ~ '|'
              ~ (5 R- 1) ~ '|'
              ~ (1 !== 2)
        }
        MODULE
    my $expected = '4,6|13,14,23,24|11,22,13|-4|True';
    for 'compiles', 'loads from the precompilation store' -> $stage {
        my $proc = run $*EXECUTABLE, '-I', $dir.absolute, '-e',
            'use MetaOpHoistTest; print combined()', :out, :err;
        my $out = $proc.out.slurp(:close);
        $proc.err.slurp(:close);
        is $out, $expected, "a module using constant meta-ops $stage";
    }
    sub nuke(IO::Path $p) {
        if $p.d { nuke($_) for $p.dir; $p.rmdir }
        else { $p.unlink }
    }
    nuke($dir);
}

# vim: expandtab shiftwidth=4
