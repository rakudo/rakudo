use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 30;

# The sunk body of a loop statement whose every piece is provably
# frame-independent is emitted inline rather than called as a block each
# iteration. An enclosing lexical used only from such a body then counts
# as frame-confined and is lowered to a local. Both frontends flatten
# and lower, so the shape assertions hold for either.

sub qast-var-decl (Mu $qast, Str:D $name, Str:D $decl, &value-ok? --> Bool:D) {
    if nqp::istype($qast, QAST::Var) && $qast.name eq $name && $qast.decl eq $decl
      && (!&value-ok || value-ok($qast.value)) {
        return True;
    }
    if qast-descendable $qast {
        for $qast.list {
            qast-var-decl($_, $name, $decl, &value-ok) and return True;
        }
    }
    False
}

sub is-sentinel (Mu $value --> Bool:D) {
    so nqp::eqaddr(nqp::decont($value), Rakudo::Internals::LoweredAwayLexical)
}

# The sentinel that replaces a lowered declaration's by-name symbol.
sub lowered-away (Mu $qast, Str:D $name --> Bool:D) {
    qast-var-decl($qast, $name, 'static', &is-sentinel)
}

# A declaration whose container stays under its name, whichever decl
# carries it.
sub kept-lexical (Mu $qast, Str:D $name --> Bool:D) {
    qast-var-decl($qast, $name, 'contvar')
      or qast-var-decl($qast, $name, 'static', -> Mu $value { !is-sentinel($value) })
}

qast-is 'my $sum = 0; my int $i = 0; while $i < 3 { $sum = $sum + 1; $i = $i + 1 }; say $sum',
    :full, -> \v {
    lowered-away(v, '$sum')
}, 'an accumulator used only from a flattenable loop body is lowered';

qast-is 'my $x = 0; my int $i = 0; while $i < 3 { my $c = { $x }; $c(); $i = $i + 1 }; say $x',
    :full, -> \v {
    kept-lexical(v, '$x')
}, 'a lexical captured by a closure inside the loop body keeps its lexical';

# The smartmatch guard is this frontend's own conservatism: it reaches
# the topic by name at emit time, which the analysis cannot see through.
# The legacy frontend analyzes the emitted QAST instead and can lower
# here, so this shape is only asserted on the RakuAST frontend.
if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'my $x = 0; my int $i = 0; while $i < 3 { $x = $x ~~ Int ?? 1 !! 2; $i = $i + 1 }; say $x',
        :full, -> \v {
        kept-lexical(v, '$x')
    }, 'a smartmatch in the loop body keeps the body a real frame';
}
else {
    skip 'smartmatch conservatism is specific to the RakuAST frontend';
}

# Behavior stays identical under flattening.

{
    my $sum = 0;
    my int $i = 0;
    while $i < 3 { my $t = 2; $sum = $sum + $t; $i = $i + 1 }
    is $sum, 6, 'a flattened body accumulates into the enclosing lexical';
}

{
    my @r;
    my int $i = 0;
    while $i < 3 { my $t = $i + 10; @r.push($t.VAR); $i = $i + 1 }
    is @r.map({ .self }).join(','), '10,11,12',
        'each iteration of a flattened body sees its own value';
    nok @r[0].VAR =:= @r[1].VAR,
        'each iteration of a flattened body gets a fresh container';
}

{
    my $s = 0;
    my int $i = 0;
    while $i < 10 { $i = $i + 1; next if $i % 2; last if $i > 6; $s = $s + $i }
    is $s, 12, 'next and last control a loop with a flattened body';
}

{
    my @c;
    my int $i = 0;
    while $i < 2 { my $t = $i * 5; @c.push({ $t }); $i = $i + 1 }
    is @c.map({ .() }).join(','), '0,5',
        'closures in a loop body capture per-iteration variables';
}

{
    my $n = 0;
    loop { $n = $n + 1; last if $n > 4 }
    is $n, 5, 'a conditionless loop statement flattens and terminates';
}

{
    my $n = 10;
    repeat { $n = $n + 1 } while $n < 3;
    is $n, 11, 'a repeat loop runs its flattened body once before the test';
}

{
    my $r = '';
    my int $i = 0;
    while $i < 3 { $i = $i + 1; given $i { when 2 { $r ~= 'two' }; default { $r ~= 'n' } } }
    is $r, 'ntwon', 'given and when inside a loop body behave unchanged';
}

{
    my $caught = '';
    my int $i = 0;
    while $i < 3 {
        $i = $i + 1;
        CATCH { default { $caught ~= 'c' } }
        die 'boom' if $i == 2;
    }
    is $caught, 'c', 'a CATCH in the loop body keeps the body a frame and fires';
}

# A bare block statement flattens under the same rules. The legacy
# optimizer cannot flatten these, since their optional topic parameter
# defeats its arity check, so the shape is only asserted here.

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'my $x = 0; { $x = 5 }; say $x', :full, -> \v {
        lowered-away(v, '$x')
    }, 'a lexical used only from a flattenable bare block statement is lowered';
}
else {
    skip 'bare block flattening is specific to the RakuAST frontend';
}

{
    my $x = 0;
    { my $t = 3; $x = $t }
    is $x, 3, 'a flattened bare block statement runs its statements';
}

{
    $_ = 42;
    { $_ := 43 }
    is $_, 42, 'a bare block that rebinds the topic keeps its frame and alias';
}

# Conditional statement branch bodies flatten the same way.

qast-is 'my $sum = 0; my int $i = 0; while $i < 9 { if $i % 2 { $sum = $sum + $i } else { $sum = $sum - 1 }; $i = $i + 1 }; say $sum',
    :full, -> \v {
    lowered-away(v, '$sum')
}, 'an accumulator used from branch bodies inside a loop body is lowered';

{
    my $sum = 0;
    my int $i = 0;
    while $i < 9 { if $i % 2 { $sum = $sum + $i } else { $sum = $sum - 1 }; $i = $i + 1 }
    is $sum, 11, 'flattened branch bodies inside a flattened loop accumulate correctly';
}

{
    my $x = 5;
    my $r = do if $x > 3 { 'big' } else { 'small' };
    is $r, 'big', 'a value-producing if with flattened branches evaluates to the branch value';
}

{
    my $r = '';
    for 1..3 -> $n { if $n == 1 { $r ~= 'a' } elsif $n == 2 { $r ~= 'b' } else { $r ~= 'c' } }
    is $r, 'abc', 'an elsif chain with flattened branches selects correctly';
}

{
    my $c = 0;
    my int $i = 0;
    while $i < 4 { unless $i == 2 { $c = $c + 1 }; $i = $i + 1 }
    is $c, 3, 'a flattened unless body runs when its condition is false';
}

{
    sub with-dynamic() { my $*D = 5; read-dynamic() }
    sub read-dynamic() { my $r; if 1 { $r = $*D }; $r }
    is with-dynamic(), 5,
        'a dynamic lookup in a branch body still walks the caller chain';
}

# Statement for and given bodies flatten as argument-taking candidates:
# a pointy body with one plain parameter has the iteration value bound
# to the parameter's local, and a plain body flattens when its topic
# goes unused. The legacy optimizer flattens neither.

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'my $sum = 0; for ^9 -> $x { $sum = $sum + $x }; say $sum', :full, -> \v {
        lowered-away(v, '$sum')
            and not kept-lexical(v, '$x')
            and not qast-var-decl(v, '$x', 'var')
    }, 'a pointy for body flattens, lowering the accumulator and dissolving the parameter';

    qast-is 'my $n = 0; for ^5 { $n = $n + 1 }; say $n', :full, -> \v {
        lowered-away(v, '$n')
    }, 'a topic-free for body flattens and lowers the accumulator';

    qast-is 'my $x = 0; given 42 { $x = 1 }; say $x', :full, -> \v {
        lowered-away(v, '$x')
    }, 'a topic-free given body flattens';
}
else {
    skip 'for and given body flattening is specific to the RakuAST frontend', 3;
}

{
    my $sum = 0;
    for ^10 -> $x { $sum = $sum + $x }
    is $sum, 45, 'a flattened pointy for body accumulates correctly';
}

{
    my $s = 0;
    for ^20 -> $i { next if $i % 2; last if $i > 8; $s = $s + $i }
    is $s, 20, 'next and last control a flattened pointy for';
}

{
    my @r;
    for 1..3 { @r.push($_) }
    is @r.join(','), '1,2,3', 'a for body using its topic behaves unchanged';
}

{
    my @a = 1, 2, 3;
    for @a <-> $e { $e++ }
    is @a.join(','), '2,3,4', 'an rw pointy parameter keeps its frame and mutates';
}

{
    my @a = 0, 1, 2;
    for @a -> $v is rw { $v++ }
    is @a.join(','), '1,2,3', 'an is-rw parameter keeps its frame and mutates';
}

{
    my @c;
    for 1..2 -> $x { @c.push({ $x }) }
    is @c.map({ .() }).join(','), '1,2',
        'closures over the parameter keep the body a frame per iteration';
}

