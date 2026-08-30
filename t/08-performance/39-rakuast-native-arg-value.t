use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 91;

# A native variable passed to a routine none of whose reachable
# candidates take that position rw is passed as a value, so a raw
# parameter holds a snapshot of the variable rather than a live view.
# A candidate the argument count cannot reach has no say.
{
    sub f(\x, $y) { -> { x } }
    my int $i = 1;
    my &c = f($i, 0); $i = 7;
    is c(), 1, 'a native argument to a raw parameter is a snapshot of the variable';
}
{
    multi sub h(int $x is rw) { $x = 5 }
    multi sub h(\x, $y) { -> { x } }
    my int $i = 1;
    my &c = h($i, 0); $i = 7;
    is c(), 1, 'an rw candidate that admits fewer positionals than the call passes has no say';
}
{
    multi sub h(int $x is rw, $y, $z) { $x = 5 }
    multi sub h(\x, $y) { -> { x } }
    my int $i = 1;
    my &c = h($i, 0); $i = 7;
    is c(), 1, 'an rw candidate that requires more positionals than the call passes has no say';
}
{
    sub f(int $x is rw) { $x = 5 }
    my int $i = 1; f($i);
    is $i, 5, 'a native argument to an rw parameter still passes the reference';
}
{
    multi sub g(int $x is rw) { $x = 5 }
    multi sub g(Str) { }
    my int $i = 1; g($i);
    is $i, 5, 'a native argument passes the reference when a reachable candidate takes it rw';
}
{
    multi sub k(int $x is rw, $y?) { $x = 5 }
    multi sub k(\x, $y, $z) { x }
    my int $i = 1; k($i, 0);
    is $i, 5, 'an rw candidate the call reaches through its optional parameter keeps the reference';
}
{
    sub nm(int $x is rw, :$z) { $x = 5 }
    my int $i = 1; nm($i, :z);
    is $i, 5, 'a named argument after the positional does not count toward the positionals';
    my int $j = 1; nm(:z, $j);
    is $j, 5, 'a named argument before the positional does not count toward the positionals';
}
{
    multi sub h(int $x is rw, $y, $z) { $x = 5 }
    multi sub h(\x) { x }
    my int $i = 1; my @two = 0, 0; h($i, |@two);
    is $i, 5, 'a flattened argument keeps the reference for an rw candidate the run time count reaches';
}
{
    proto sub p(|) { {*} }
    multi sub p(\x, $y) { -> { x } }
    my int $i = 1;
    my &c = p($i, 0); $i = 7;
    is c(), 7, 'a proto with a body keeps the reference';
}
{
    sub c(|c) { -> { c[0] } }
    my int $i = 1;
    my &r = c($i); $i = 7;
    is r(), 7, 'a capture parameter keeps the reference';
}
{
    sub f(*@a) { @a }
    my int $i = 1;
    my $r = f($i); $i = 7;
    is $r[0], 1, 'a slurpy parameter holds a snapshot of a native argument';
}

# A slurpy that keeps its arguments' containers keeps a native
# argument's reference, so it holds a live view of the variable as it
# does of a container.
{
    sub f(**@a) { @a }
    my int $i = 1;
    my $r = f($i); $i = 7;
    is $r[0], 7, 'a double-star slurpy holds a live view of a native argument';
}
{
    sub f(+@a) { @a }
    my int $i = 1;
    my $r = f($i); $i = 7;
    is $r[0], 7, 'a plus slurpy holds a live view of a native argument';
}
{
    sub f(*@a is raw) { @a }
    my int $i = 1;
    my $r = f($i); $i = 7;
    is $r[0], 7, 'a raw slurpy holds a live view of a native argument';
}

# The links of a longer chain keep the chain protocol: the middle
# operand is evaluated once and a false link ends the chain.
{
    my $a = 2;
    is-deeply (1 < $a < 3 < 2, 0 < 0.5 < 1 < 2), (False, True),
        'a chain of several links answers as a chain';
    my $n = 0;
    sub f() { $n++; 2 }
    is 5 < f() < 3, False, 'a false first link ends the chain';
    is $n, 1, 'the middle operand of a chain is evaluated once';
    my $m = 0;
    sub g() { $m++; 9 }
    is 1 < 0 < g(), False, 'a later link is not evaluated after a false one';
    is $m, 0, 'a later link is not evaluated after a false one, with its operand untouched';
}
{
    my $a = 5;
    is 1 !< $a < 3, False, 'a false negated first link ends the chain';
    my int $n = 5;
    is 1 < $n < 3, False, 'a native operand of a longer chain keeps the chain';
}

# A routine declared after the use in the same scope takes the call, so
# the reference stays where such a declaration could take it rw.
{
    my int $i = 1;
    my int $j = 2;
    is $i + $j, 'RW', 'an operator declared later in the scope takes the call';
    is $i, 43, 'an operator declared later in the scope receives the reference';
    multi sub infix:<+>(int $a is rw, int $b) { $a = 43; 'RW' }
}
{
    my int $i = 1;
    is -$i, 'RW', 'a prefix operator declared later in the scope takes the call';
    is $i, 43, 'a prefix operator declared later in the scope receives the reference';
    multi sub prefix:<->(int $a is rw) { $a = 43; 'RW' }
}
# The outer g exists for the block below to shadow.
sub g(int $x) { }
{
    my int $i = 1;
    g($i);
    is $i, 9, 'a sub declared later in the scope, shadowing an outer one, receives the reference';
    sub g(int $x is rw) { $x = 9 }
}

# A later argument that writes a native variable is seen by the callee,
# as it is when the variable is a container, which is read when the
# callee binds it.
{
    sub f(*@a) { @a.join(',') }
    my Int $a = 1; my int $b = 1;
    is f($b, ($b = 7)), f($a, ($a = 7)), 'a native argument to a slurpy parameter is read at bind time, as a container is';
}
{
    sub f(**@a) { @a.join(',') }
    my Int $a = 1; my int $b = 1;
    is f(0, $b, ($b = 7)), f(0, $a, ($a = 7)), 'a native argument after the position a slurpy starts at is read at bind time, as a container is';
}
{
    my Int $a = 4; my int $b = 4;
    is $b !%% ($b = 3), $a !%% ($a = 3), 'a native operand of a negated operator is read at bind time, as a container is';
}
{
    my Int $a = 1; my int $b = 1;
    is (0 < $b < ($b = 7)), (0 < $a < ($a = 7)), 'the middle operand of a chain is read at the bind of each link, as a container is';
    is (0 !> $b < ($b = 7)), (0 !> $a < ($a = 7)), 'the middle operand after a negated link is read at the bind of each link, as a container is';
}

# The named and flattened arguments take part in the evaluation order
# the reads follow.
{
    sub f($x, :$n) { "$x,$n" }
    my Int $a = 1; my int $b = 1;
    is f($b, :n($b = 7)), f($a, :n($a = 7)), 'a named argument that writes the variable is evaluated ahead of the native read';
    my Int $c = 1; my int $d = 1;
    is f(:n($d = 7), $d), f(:n($c = 7), $c), 'a named argument ahead of the positional that writes the variable is evaluated ahead of the native read';
}
{
    sub f($x, $y, :$n) { "$x,$y,$n" }
    my int $b = 1;
    is f($b, :n($b = 7), ($b = 9)), '7,9,7', 'the positional arguments are evaluated before the named ones, and the native read after them all';
}
{
    sub f($x, $y, $z) { "$x,$y,$z" }
    my Int $a = 1; my int $b = 1; my @one = 0;
    is f($b, |@one, ($b = 7)), f($a, |@one, ($a = 7)), 'an argument after a flattened one that writes the variable is evaluated ahead of the native read';
}
{
    sub f(int $c, $d) { "$c,$d" }
    my int $b = 1;
    sub g() { $b = 7; 5 }
    is f($b, |(g(),)), '7,5', 'a flattened argument that writes the variable is evaluated ahead of the native read';
}
{
    sub f($x, $y, $z) { "$y,$z" }
    my Int $a = 1; my int $b = 1;
    sub ha() { $a = 5; 100 }
    sub hb() { $b = 5; 100 }
    is f(0, hb(), $b), f(0, ha(), $a), 'an impure argument ahead of the native read stays where it is';
}
{
    sub g(--> int) { fail "no" }
    sub f(int $x, $y) { $y.^name }
    my int $b = 1;
    is f($b, g()), 'Failure', 'a Failure returned by an argument after the native read reaches the callee';
}
{
    sub f(int $x is rw, $y) { $x = $x + $y; $y }
    my int $b = 1;
    is f($b, ($b = 7)), 7, 'an rw parameter followed by an impure argument still receives the argument';
    is $b, 14, 'an rw parameter followed by an impure argument still writes back through the reference';
}
{
    my Int $a = 2; my int $b = 2;
    is (0 < 1 < $b < ($b = 7)), (0 < 1 < $a < ($a = 7)), 'the middle operand of a chain of three links is read at the bind of its link, as a container is';
}

{
    my int $i = 1;
    sub f() { $i = 7; 5 }
    $i max= f();
    is $i, 7, 'a native compound assignment reads its target after the argument that writes it';
}
# The temporary an impure argument binds keeps the kind its code
# yields.
{
    my class MyInt is Int { }
    sub f($x, $y) { $y.^name }
    my int $i = 1;
    sub g() { $i = 7; 3 }
    is f($i, nqp::box_i(g(), MyInt)), 'MyInt', 'a boxing op after the native read keeps its boxed type';
}
{
    multi sub d(Int $x, Int $y) { "Int/Int" }
    multi sub d(Int $x, Str $y) { "Int/Str" }
    my int $b = 1;
    is d($b, nqp::iseq_s("a","a")), 'Int/Int', 'a comparison op result after a native read keeps its integer kind';
}
{
    sub f(int $x, :$n) { "$x,$n" }
    my Int $a = 1; my int $b = 1;
    is f($b, :n(my int $t = ($b = 7))), f($a, :n(my int $u = ($a = 7))), 'a native typed named argument that writes the variable is evaluated ahead of the native read';
}

# An operator's adverb and a wrapped operand join the call like any
# other argument.
{
    sub infix:<nn>($a, $b, :$n) { "$a,$b,$n" }
    my Int $a = 1; my int $b = 1;
    is ($b nn 0 :n($b = 7)), ($a nn 0 :n($a = 7)), 'an infix adverb that writes the variable is evaluated ahead of the native read';
}
{
    sub f($x, int $y is rw) { $y = 99 }
    my int $b = 1; my int $q = 1;
    f($b, ($q));
    is $q, 99, 'a parenthesized native argument to an rw parameter still passes the reference';
}
{
    sub infix:<nn>($a, $b, :$n) { "$a,$b,$n" }
    sub imp() { 3 }
    my Int $a = 1; my int $b = 1;
    is ($b nn imp() :n($b = 9)), ($a nn imp() :n($a = 9)), 'an infix adverb joins the temporaries of an impure operand ahead of the native read';
}
# The scope of the first argument of a named call, or the node's type
# name when that argument is not a variable.
sub qast-first-arg-scope(Mu $qast, str $callee --> Str) {
    if nqp::istype($qast, QAST::Op) && $qast.name eq $callee {
        for $qast.list {
            return nqp::istype($_, QAST::Var) ?? $_.scope !! $_.^name;
        }
    }
    if qast-descendable $qast {
        for $qast.list {
            my $found = qast-first-arg-scope($_, $callee);
            return $found if $found;
        }
    }
    ''
}
# The scope of the variable of the given name where a call passes it.
sub qast-call-arg-scope(Mu $qast, str $var --> Str) {
    if nqp::istype($qast, QAST::Op) && $qast.op eq 'call' | 'callstatic' {
        for $qast.list {
            return $_.scope if nqp::istype($_, QAST::Var) && $_.name eq $var;
        }
    }
    if qast-descendable $qast {
        for $qast.list {
            my $found = qast-call-arg-scope($_, $var);
            return $found if $found;
        }
    }
    ''
}
# How many ops of the given op name call the given callee name.
sub qast-count-calls(Mu $qast, str $op, str $callee --> Int) {
    my $count = nqp::istype($qast, QAST::Op) && $qast.op eq $op && $qast.name eq $callee ?? 1 !! 0;
    if qast-descendable $qast {
        $count += qast-count-calls($_, $op, $callee) for $qast.list;
    }
    $count
}
# Whether any temporary a native read or an inlined body binds appears.
sub qast-has-temporary(Mu $qast --> Bool) {
    return True if nqp::istype($qast, QAST::Var)
        && ($qast.name.starts-with('_native_read_') || $qast.name.starts-with('_inline_arg_'));
    if qast-descendable $qast {
        for $qast.list {
            return True if qast-has-temporary($_);
        }
    }
    False
}
# The scope of the last operand of the innermost chain op, or ''.
sub qast-chain-middle-scope(Mu $qast --> Str) {
    if nqp::istype($qast, QAST::Op) && $qast.op eq 'chain' | 'chainstatic' {
        my $inner = qast-chain-middle-scope($qast.list[0]);
        return $inner if $inner;
        my $last = $qast.list[*-1];
        return nqp::istype($last, QAST::Var) ?? $last.scope !! '';
    }
    if qast-descendable $qast {
        for $qast.list {
            my $found = qast-chain-middle-scope($_);
            return $found if $found;
        }
    }
    ''
}
# Whether the named call's first argument is a statement list ending in
# the given variable's value read, with a temporary as the argument after.
sub qast-reads-last(Mu $qast, str $callee, str $var --> Bool) {
    if nqp::istype($qast, QAST::Op) && $qast.name eq $callee {
        my @args = $qast.list;
        my $slot = @args[0];
        return nqp::istype($slot, QAST::Stmts)
            && nqp::istype($slot.list[*-1], QAST::Var)
            && $slot.list[*-1].name eq $var && $slot.list[*-1].scope eq 'lexical'
            && nqp::istype(@args[1], QAST::Var) && @args[1].scope eq 'local';
    }
    if qast-descendable $qast {
        for $qast.list {
            return True if qast-reads-last($_, $callee, $var);
        }
    }
    False
}

# What follows holds under this frontend only.
if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    {
        sub o(\x, $y?) { -> { x } }
        my int $i = 1;
        my &c = o($i, 0); $i = 7;
        is c(), 1, 'an optional parameter holds a snapshot of a native argument';
    }
    qast-is 'my int $i; my $y; my $z = $i + $y', -> \v {
        qast-first-arg-scope(v, '&infix:<+>') eq 'lexical'
    }, 'a native operand of an operator passes as a value';
    qast-is 'sub f(int $x is rw) { }; my int $i; f($i)', :full, -> \v { qast-first-arg-scope(v, '&f') eq 'lexicalref' },
        'a native argument to an rw parameter passes as a reference';
    qast-is 'my int $a; my int $b; my $c = $a !< $b', -> \v {
        qast-call-arg-scope(v, '$a') eq 'lexical'
    }, 'a native operand of a negated comparison passes as a value';
    # A lone comparison dispatching on native operands is this frontend's
    # shape.
    {
        multi sub infix:«<»(int $a, int $b) is default { $a <= 1 }
        sub count(int $n) { my int $i = 0; my int $c = 0; while $i < $n { $i++; $c++ }; $c }
        is count(3), 2, 'a lone native comparison dispatches on the native operands';
    }
    {
        use soft;
        multi sub infix:«<»(int $a, int $b) is default { $a <= 1 }
        sub count(int $n) { my int $i = 0; my int $c = 0; while $i < $n { $i++; $c++ }; $c }
        is count(3), 2, 'a lone native comparison dispatches on the native operands under the soft pragma';
    }
    qast-is 'my $a; my $b; my $c = $a < $b', -> \v {
        qast-count-calls(v, 'callstatic', '&infix:«<»') == 1
            and not qast-contains-op(v, 'chainstatic') and not qast-contains-op(v, 'chain')
    }, 'a lone comparison compiles to a call';
    qast-is 'my $a; my $d = 1 < $a < 3', -> \v {
        qast-count-calls(v, 'chainstatic', '&infix:«<»') == 2
            and qast-count-calls(v, 'callstatic', '&infix:«<»') == 0
            and qast-count-calls(v, 'call', '&infix:«<»') == 0
    }, 'every link of a chain of two compiles to a chain op';
    {
        my int $i = 1;
        my int $j = 2;
        is $i < $j, 'RW', 'a comparison declared later in the scope takes the call';
        is $i, 43, 'a comparison declared later in the scope receives the reference';
        multi sub infix:«<»(int $a is rw, int $b) { $a = 43; 'RW' }
    }
    # The legacy frontend reads a native operand where it stands.
    {
        sub f($x, $y, $z) { "$x,$y,$z" }
        my Int $a = 1; my int $b = 1;
        is f($b, ($b), ++$b), f($a, ($a), ++$a), 'a parenthesized native argument is read at bind time, as a container is';
    }
    {
        my Int $a = 1; my int $b = 1;
        is $b + ($b = 7), $a + ($a = 7), 'a native left operand is read after a later operand writes it, as a container is';
    }
    {
        my Str $a = 'a'; my str $b = 'a';
        is $b ~ ($b = 'b'), $a ~ ($a = 'b'), 'a native string operand is read after a later operand writes it, as a container is';
    }
    {
        my Int $a = 1; my int $b = 1;
        is infix:<+>($b, ($b = 7)), infix:<+>($a, ($a = 7)), 'a native argument to an operator called by name is read at bind time, as a container is';
    }
    {
        my Int $a = 1; my int $b = 1;
        is $b - ($b = 7), $a - ($a = 7), 'a native operand of subtraction is read at bind time, as a container is';
    }
    {
        class C { has Int $.a = 1; has int $.b = 1; method m { ($!b + ($!b = 7), $!a + ($!a = 7)) } }
        my ($native, $boxed) = C.new.m;
        is $native, $boxed, 'a native attribute operand is read at bind time, as a container is';
    }
    {
        sub f(\x, $y) { x }
        my Int $a = 1; my int $b = 1;
        is f($b, ($b = 7)), f($a, ($a = 7)), 'a native argument to a raw parameter is read at bind time, as a container is';
    }
    {
        sub f($x, $y) { $x }
        my Int $a = 1; my int $b = 1;
        is f($b, ($b = 7)), f($a, ($a = 7)), 'a native argument to a plain parameter is read at bind time, as a container is';
    }
    {
        sub h(int $x, $y) { $x }
        my Int $a = 1; my int $b = 1;
        is h($b, ($b = 7)), h($a, ($a = 7)), 'a native argument to a native parameter is read at bind time, as a container is';
    }
    {
        sub o(\x, $y?) { x }
        my Int $a = 1; my int $b = 1;
        is o($b, ($b = 7)), o($a, ($a = 7)), 'a native argument to an optional parameter is read at bind time, as a container is';
    }
    {
        my Int $a = 1; my int $b = 1;
        is ($b < ($b = 7)), ($a < ($a = 7)), 'a lone native comparison reads its operand at bind time, as a container is';
    }
    {
        my Int $a = 1; my int $b = 1;
        is ($b !< ($b = 7)), ($a !< ($a = 7)), 'a negated native comparison reads its operand at bind time, as a container is';
    }
    {
        my Int $a = 1; my int $b = 1;
        is ($b < ($b = 7) < 9), ($a < ($a = 7) < 9), 'the first operand of a chain is read at the bind of its link, as a container is';
    }
    {
        my Int $a = 1; my int $b = 1;
        is $b + $b++, $a + $a++, 'a native operand is read after a later operand increments it, as a container is';
    }
    {
        use soft;
        my Int $a = 1; my int $b = 1;
        is $b + ($b = 7), $a + ($a = 7), 'a native operand is read at bind time under the soft pragma, as a container is';
    }
    {
        my int $b = 1;
        is $b + ($b = 7), 14, 'a native left operand reads 7 after the later operand writes it';
        my int $c = 1;
        is infix:<+>($c, ($c = 7)), 14, 'a native argument to an operator called by name reads 7 after the later argument writes it';
    }
    {
        my Int $a = 1; my int $b = 1;
        is $b + ++$b, $a + ++$a, 'a native operand of an inlined operator is read after a later operand increments it, as a container is';
        my int $c = 1;
        is $c + ++$c, 4, 'a native operand of an inlined operator reads 2 after the later operand increments it';
    }
    {
        my Str $a = 'a'; my str $b = 'a';
        is $b ~ (my str $d = ($b = 'b')), $a ~ (my Str $c = ($a = 'b')), 'a native string operand of an inlined operator is read after a later operand writes it, as a container is';
    }
    # EVAL compiles a unit of its own, where the callee is settled and
    # its body splices.
    is EVAL('sub w(int $a, int $b) { $a * $a + $b }; my int $b = 1; w($b, ++$b)'), 6,
        'a native argument used twice in an inlined body is read after a later argument increments it';
    {
        my uint $b = 1;
        is $b + ($b = 7), 14, 'a native unsigned operand reads 7 after the later operand writes it';
    }
    {
        sub f($x, $y) { "$x,$y" }
        my num $b = 1e0;
        is f($b, ($b = 7e0)), '7,7', 'a native num argument reads 7 after the later argument writes it';
    }
    {
        sub f($x, $y, $z) { "$x,$y,$z" }
        my int $b = 1;
        is f($b, ($b = 7), $b), '7,7,7', 'both reads around a writing argument read after it';
    }
    qast-is 'sub f($x, $y) { }; my int $i = 1; f($i, ($i = 7));', :full, -> \v { qast-has-temporary(v) },
        'a writing argument after a native read binds a temporary in sink context';
    qast-is 'my int $i = 1; sub f() { 2 }; my $r = $i + f()', :full, -> \v {
        qast-reads-last(v, '&infix:<+>', '$i')
    }, 'an impure operand after a native read is evaluated into a temporary ahead of the read';
    qast-is 'my int $i; my $r = $i + 1', -> \v { not qast-has-temporary(v) },
        'a literal operand after a native read needs no temporary';
    qast-is 'my int $i; my int $n; my $r = $i < $n', -> \v { not qast-has-temporary(v) },
        'a native variable operand after a native read needs no temporary';
    qast-is 'my int $b = 1; my $r = $b + ++$b', :full, -> \v { qast-has-temporary(v) },
        'an impure operand of an inlined operator binds a temporary';
    qast-is 'sub infix:<nn>($a,$b,:$n){}; my int $b = 1; my $r = $b nn 0 :n(5)', :full,
        -> \v { not qast-has-temporary(v) }, 'a pure adverb after a native read needs no temporary';
    qast-is 'sub f(int $x) { my $r = 0 < $x < g() }; sub g() { 9 }', :full,
        -> \v { qast-chain-middle-scope(v) eq 'lexical' },
        'a read-only native parameter in a chain is not promoted to a reference';
    qast-is 'my int $b; my $r = 0 < $b < ($b = 7)', -> \v { qast-chain-middle-scope(v) eq 'lexicalref' },
        'the middle operand of a chain whose last operand is impure stays a reference';
}
else {
    skip 'argument passing shapes are specific to the RakuAST frontend', 42;
}

# vim: expandtab shiftwidth=4
