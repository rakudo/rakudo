use lib <t/packages/Test-Helpers>;
use Test::Helpers;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 9;

# A call to a routine bound once carries the callee's declared return type
# on its QAST. A native return is offered raw through a Want alternative, so
# a native consumer skips the boxing round trip, while the Want's default
# stays the bare call so an escaping Failure or Nil reaches object context
# as the boxed object it needs to be.

my $rakuast = nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast';

# Whether a Want offers a raw native alternative that contains a call to
# $name, its default being something other than a boxing op.
sub want-offers-raw-call(Mu $qast, $name --> Bool:D) {
    if nqp::istype($qast, QAST::Want) {
        my @children = $qast.list;
        loop (my int $i = 1; $i + 1 < @children.elems; $i = $i + 2) {
            return True if @children[$i] ~~ Str && @children[$i] eq 'Ii' | 'Nn' | 'Ss'
                && qast-contains-call(@children[$i + 1], $name);
        }
    }
    if qast-descendable $qast {
        for $qast.list {
            want-offers-raw-call($_, $name) and return True;
        }
    }
    False
}

# These observe the emitted QAST. The legacy optimizer does not offer this
# shape both ways, so the assertions hold for the RakuAST frontend only.
todo 'the legacy optimizer does not offer a native return through a Want', 2
    unless $rakuast;
qast-is 'sub cr-a(--> int) { return 3 }; my int $x = cr-a()', -> \v {
    want-offers-raw-call(v, '&cr-a') and not qast-contains-op(v, 'p6box_i')
}, 'a native int return is offered raw through a Want without boxing';

qast-is 'sub cr-b(--> num) { return 3e0 }; my num $x = cr-b()', -> \v {
    want-offers-raw-call(v, '&cr-b') and not qast-contains-op(v, 'p6box_n')
}, 'a native num return is offered raw through a Want without boxing';

qast-is 'sub cr-c(--> Int) { return 3 }; my $x = cr-c()', -> \v {
    not qast-contains-op(v, 'p6box_i')
}, 'a boxed return type adds no boxing op';

# These observe that returns still behave.
{
    sub f(--> int) { return 3 }
    my int $x = f();
    is $x, 3, 'a native int return reaches a native target';
    is f() + 1, 4, 'a native int return computes in object context';
}
{
    sub f(--> num) { return 1.5e0 }
    is f() * 2, 3e0, 'a native num return computes';
}
{
    sub f(--> str) { return "hi" }
    is f() ~ "!", 'hi!', 'a native str return computes';
}
{
    sub f(--> Str) { return "ok" }
    is f(), 'ok', 'a boxed declared return gives its value';
}
{
    # An our-scoped callee imports as its Scalar container, so the return
    # type of the value it happens to hold at compile time is no promise.
    my $dir = make-temp-dir;
    $dir.add('ReassignableCallee.rakumod').spurt:
        'our &rc = sub (--> int) { 1 }';
    todo 'the legacy optimizer copies a reassignable imported callee return type'
        unless $rakuast;
    is-deeply
        (try EVAL q[use lib $dir; use ReassignableCallee; &rc = sub (--> Str) { "s" }; rc()]) // 'died',
        "s",
        'an imported our-scoped callee reassigned at runtime returns the new value';
}

# vim: expandtab shiftwidth=4
