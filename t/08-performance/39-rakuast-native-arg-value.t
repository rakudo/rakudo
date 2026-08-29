use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 30;

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
}
else {
    skip 'argument passing shapes are specific to the RakuAST frontend', 8;
}

# vim: expandtab shiftwidth=4
