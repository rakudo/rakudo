use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 30;

# A meta-op over a setting operator is formed once at compile time and
# emitted as a constant, since the operator lookup yields the same code
# object at run time. A meta-op formed at run time makes the formation
# call and allocates a closure per evaluation, which stays the path for
# a lexical operator and for a meta-op operand. The shapes the
# assertions pin down are this frontend's.

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
    if nqp::istype($qast, QAST::Var) && $qast.name eq $name {
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
}
else {
    skip 'the formation shapes are specific to the RakuAST frontend', 8;
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
}
else {
    skip 'the callable meta-op forms do not compile under the legacy frontend', 2;
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
