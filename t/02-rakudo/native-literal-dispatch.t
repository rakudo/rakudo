use Test;
use nqp;

plan 46;

my $rakuast := nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

# A literal carries no declared type, so its representation comes from its
# context: beside an operand or argument whose type is a native kind the
# literal can hold, it is passed in that kind and dispatches accordingly,
# and it stays the boxed value it literally is otherwise. The same rule
# decides the compile time analysis and the passed representation, so the
# answer cannot depend on the optimization level or on whether a candidate
# happens to be inlinable.

my int $i = 3;

{
    multi pick-r(int $a, int $b) { "int" }
    multi pick-r(int $a, Int $b) { "Int" }
    is pick-r($i, 1), "int", 'a literal beside a native int reaches the native candidate';

    multi pick-l(Int $a, int $b) { "Int" }
    multi pick-l(int $a, int $b) { "int" }
    is pick-l(1, $i), "int", 'a literal on the left adopts the native context too';

    multi pick-n(int $a, int $b) { my $s = join "", "i", "nt"; $s }
    multi pick-n(int $a, Int $b) { my $s = join "", "I", "nt"; $s }
    is pick-n($i, 1), "int", 'the native candidate answers whether or not its body can inline';
}

{
    multi lone(int $x) { "int" }
    multi lone(Int $x) { "Int" }
    is lone(0), "Int", 'a lone literal dispatches as the boxed value it is';
    is lone($i), "int", 'a native variable reaches the native candidate';
}

{
    multi pp(int $a, int $b) { "int" }
    multi pp(int $a, Int $b) { "Int" }
    sub via-param(int $p) { pp($p, 1) }
    is via-param(3), "int", 'a parameter read provides the native context through its declared type';
}

{
    multi three(int $a, int $b, int $c) { "int" }
    multi three(int $a, int $b, Int $c) { "Int" }
    is three($i, $i, 1), "Int", 'a literal among three positionals has no single native context';

    multi named(int $a, int $b, :$k) { "int" }
    multi named(int $a, Int $b, :$k) { "Int" }
    is named($i, 1, :k), "Int", 'a named argument leaves a literal boxed';

    my class M {
        multi method m(int $a, int $b) { "int" }
        multi method m(int $a, Int $b) { "Int" }
    }
    is M.m($i, 1), "Int", 'a method call argument stays the boxed value';
}

{
    my num $n = 1e0;
    multi kindm(num $a, int $b) { "int" }
    multi kindm(num $a, Int $b) { "Int" }
    is kindm($n, 1), "Int", 'an int literal does not adopt a num context';

    multi kn(num $a, num $b) { "num" }
    multi kn(num $a, Num $b) { "Num" }
    is kn($n, 1e0), "num", 'a num literal beside a native num reaches the native candidate';

    my str $s = "x";
    multi ks(str $a, str $b) { "str" }
    multi ks(str $a, Str $b) { "Str" }
    is ks($s, "y"), "str", 'a str literal beside a native str reaches the native candidate';
    is ks($s, q:to/END/), "str", 'a constant heredoc pairs like a str literal';
    hello
    END
}

# Whether a literal has a native form depends on fitting a native int, not
# on how the Int object happens to be stored.
{
    multi wide(int $a, int $b) { "int" }
    multi wide(int $a, Int $b) { "Int" }
    is wide($i, 2147483648), "int", 'a literal above the small-Int storage limit still has its native form';
    is wide($i, 9223372036854775807), "int", 'the largest native int literal has its native form';
    is wide($i, 9223372036854775808), "Int", 'a literal too wide for a native int stays boxed';
    is-deeply $i + 2147483648, 2147483651, 'native arithmetic on a wide literal computes the right value';
}

# The CORE arithmetic consequences of the rule.
{
    my int $v = 5;
    my num $y;
    $y = 2 * $v;
    is $y, 10e0, 'a literal paired with a native int computes a native result that widens to num';

    my int $d = 4;
    dies-ok { my $r = $d div 0 }, 'div by a literal zero throws like div by a native zero';

    my int $m = 9223372036854775807;
    is $m + 1, -9223372036854775808, 'native addition of a literal wraps like native variables do';
}

# A literal is a value, so a candidate that requires a native container is
# ruled out at compile time, on both readings of the literal.
{
    sub compile-refuses($code, $desc) {
        my $error = '';
        try {
            EVAL $code;
            CATCH { default { $error = .gist.Str } }
        }
        ok $error.contains('will never work'), $desc;
    }
    compile-refuses 'multi rwl(int $x is rw) { }; rwl(42)',
        'a lone literal rules out a native rw candidate at compile time';
    compile-refuses 'multi rwp(int $a, int $b is rw) { }; my int $i = 3; rwp($i, 42)',
        'a paired literal rules out a native rw candidate at compile time';
}

# The settled candidate's native return type is carried for a paired
# literal, since the analysis read the literal exactly as it is passed.
{
    multi scaled(int $a, int $b --> int) { $a * $b }
    my int $v = 3;
    my num $y;
    $y = scaled($v, 4);
    is $y, 12e0, 'a settled native-return multi with a paired literal widens to num';
}

# The representation rule is about the arguments, not the callee, so an
# indirect call behaves like a direct one.
{
    multi pick-i(int $a, int $b) { "int" }
    multi pick-i(int $a, Int $b) { "Int" }
    my $f = &pick-i;
    is $f($i, 1), "int", 'an indirect call passes a paired literal natively';
}

# A negated operator dispatches on the operands the plain application
# passes, so a literal beside a native operand reaches the native
# candidate there too.
{
    my $picked;
    multi sub infix:<%%>(int $a, int $b) is default { $picked = "int"; True }
    multi sub infix:<%%>(int $a, Int $b) { $picked = "Int"; True }
    multi sub infix:<%%>(Int $a, int $b) { $picked = "Int"; True }
    my $r = $i !%% 1;
    is $picked, "int", 'a negated operator passes a paired literal natively';
    is $r, False, 'a negated operator negates the answer of the candidate the literal reached';
    $picked = "none";
    my $l = 1 !%% $i;
    is $picked, "int", 'a negated operator passes a paired literal on the left natively';
}
{
    my int $n = 1;
    my $r = $n !< 5;
    is $r, True, 'a negated comparison reaches a candidate declared after it';
    is $n, 43, 'the rw candidate a negated comparison reaches writes its native operand';
    multi sub infix:«<»(int $a is rw, int $b) { $a = 43; False }
}
{
    my $rev;
    multi sub infix:<+>(int $a, int $b) is default { $rev = "int"; 0 }
    multi sub infix:<+>(int $a, Int $b) { $rev = "Int"; 0 }
    multi sub infix:<+>(Int $a, int $b) { $rev = "Int"; 0 }
    my $s = 1 R+ $i;
    is $rev, "int", 'a reversed operator passes a paired literal natively';
    $rev = "none";
    my $t = $i R+ 1;
    is $rev, "int", 'a reversed operator passes a paired literal on the right natively';
}
if $rakuast {
    my $seq;
    multi sub infix:<%%>(int $a, int $b) is default { $seq = "int"; True }
    multi sub infix:<%%>(int $a, Int $b) { $seq = "Int"; True }
    my $r = $i S%% 1;
    is $seq, "int", 'a sequenced operator passes a paired literal natively';
}
else {
    skip 'the sequenced operator does not run under the legacy frontend', 1;
}
if $rakuast {
    my $picked;
    multi sub infix:<eq>(int $a, int $b) { $picked = "int"; True }
    multi sub infix:<eq>(int $a, Int $b) { $picked = "Int"; True }
    my $r = $i !eq 1;
    is $picked, "int", 'a negated comparison passes a paired literal natively';
    my $left;
    multi sub infix:<ne>(int $a, int $b) { $left = "int"; True }
    multi sub infix:<ne>(Int $a, int $b) { $left = "Int"; True }
    my $t = 1 !ne $i;
    is $left, "int", 'a negated comparison passes a paired literal on the left natively';
    is $t, False, 'a negated comparison negates the answer of the candidate the literal reached';
}
else {
    skip 'the legacy frontend boxes the literal of a negated comparison', 3;
}

# A reversed or sequenced comparison standing alone dispatches on the
# pair as the plain comparison does. A link of a longer chain takes part
# in the chain protocol instead.
{
    my $picked;
    multi sub infix:«<»(int $a, int $b) is default { $picked = "int"; True }
    multi sub infix:«<»(int $a, Int $b) { $picked = "Int"; True }
    multi sub infix:«<»(Int $a, int $b) { $picked = "Int"; True }
    my $r = 5 R< $i;
    is $picked, "int", 'a reversed comparison passes a paired literal natively';
    if $rakuast {
        $picked = "none";
        my $s = $i S< 5;
        is $picked, "int", 'a sequenced comparison passes a paired literal natively';
        nok 3 S< 4 S< 2, 'a chain of sequenced comparisons fails through its second link';
    }
    else {
        skip 'the sequenced operator does not run under the legacy frontend', 2;
    }
}

# The soft pragma keeps routines wrappable and changes no dispatch.
{
    use soft;
    my $picked;
    multi sub infix:<%%>(int $a, int $b) is default { $picked = "int"; True }
    multi sub infix:<%%>(int $a, Int $b) { $picked = "Int"; True }
    my $r = $i !%% 1;
    is $picked, "int", 'a negated operator under the soft pragma passes a paired literal natively';
}

# A negated operator still carries its adverb, which keeps the meta-op
# and so the boxed form of its operands, and keeps the operator's own
# evaluation of its operands.
if $rakuast {
    my $seen;
    multi sub infix:<%%>($a, $b, :$flag) { $seen = $flag; True }
    my $r = 4 !%% 2 :flag;
    is $r, False, 'a negated operator with an adverb negates the answer';
    is $seen, True, 'a negated operator with an adverb passes the adverb on';
}
else {
    skip 'the legacy frontend drops the adverb of a negated operator', 2;
}
if $rakuast {
    my $form;
    multi sub infix:<%%>(int $a, int $b, :$flag) { $form = "int"; True }
    multi sub infix:<%%>(Int $a, Int $b, :$flag) { $form = "Int"; True }
    my $b = $i !%% 1 :flag;
    is $form, "Int", 'a negated operator with an adverb keeps the boxed form of its operands';
}
else {
    skip 'the legacy frontend drops the adverb of a negated operator', 1;
}
{
    my $ran = 0;
    my $t = 0 !&& ($ran = 1);
    is $t, True, 'a negated short-circuit operator negates the answer';
    is $ran, 0, 'a negated short-circuit operator does not evaluate the operand the operator skips';
    my $u = 1 !&& ($ran = 2);
    is $u, False, 'a negated short-circuit operator negates the operand the operator yields';
    is $ran, 2, 'a negated short-circuit operator evaluates the operand the operator reaches';
}

# vim: expandtab shiftwidth=4
