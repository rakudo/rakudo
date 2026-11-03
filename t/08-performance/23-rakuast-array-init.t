use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 27;

# Initializing a plain array from a comma list builds the list internals
# directly and reifies, skipping the STORE dispatch. A typed, shaped, or
# trait-bearing array keeps the STORE call, whose behavior its container
# may specialize, and so does a right side that is not a comma list.

sub qast-contains-store (Mu $qast --> Bool:D) {
    qast-contains-callmethod($qast, 'STORE')
}

my $rakuast = nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast';

# These observe the emitted QAST. The equivalent legacy rewrite no longer
# fires, so the lowered assertions hold for the RakuAST frontend only.
todo 'the legacy optimizer no longer lowers this shape', 3 unless $rakuast;
qast-is 'my @a = 1, 2, 3', -> \v {
    qast-contains-callmethod(v, 'reify-until-lazy') and not qast-contains-store(v)
}, 'a plain array declaration initializer lowers past STORE';

qast-is 'my @a; @a = 1, 2, 3', -> \v {
    # The assignment form carries a STORE fallback behind the runtime
    # stock-Array guard, so only the lowered build's presence is asserted.
    qast-contains-callmethod(v, 'reify-until-lazy') and qast-contains-op(v, 'what_nd')
}, 'a plain array assignment lowers past STORE behind a type guard';

qast-is 'my @a = 1, 2, 3; my @b = @a, 4', -> \v {
    qast-contains-callmethod(v, 'reify-until-lazy') and not qast-contains-store(v)
}, 'an array-valued element still lowers';

# Declines keep the STORE call under both frontends.
qast-is 'my Int @a = 1, 2, 3', -> \v {
    qast-contains-store(v)
}, 'a typed array keeps the STORE call';

qast-is 'my @a is default(0) = 1, 2, 3', -> \v {
    qast-contains-store(v)
}, 'a trait-bearing array keeps the STORE call';

qast-is 'my @a = 1..3', -> \v {
    not qast-contains-callmethod(v, 'reify-until-lazy')
}, 'a non-comma right side keeps its own initialization';

# These observe that initialization still behaves.
{
    my @a = 1, 2, 3;
    is @a.join(','), '1,2,3', 'a lowered initializer stores the elements';
    is @a.elems, 3, 'the element count is right';
    @a[0] = 42;
    is @a[0], 42, 'the array stays mutable';
    @a.push(4);
    is @a.elems, 4, 'the array still grows';
}
{
    my @a;
    @a = 4, 5;
    is @a.join(','), '4,5', 'a lowered assignment stores the elements';
    @a = 6, 7, 8;
    is @a.join(','), '6,7,8', 'a second assignment replaces the elements';
}
{
    my @a = 1, 2;
    my @b = 0, |@a, 9;
    is @b.join(','), '0,1,2,9', 'a slip element flattens';
    my @c = @a, @a;
    is @c.elems, 2, 'an array element stays itemized';
}
{
    my @a = "x" xx 3;
    is @a.join(','), 'x,x,x', 'a generator element reifies';
}
{
    my Int @t = 1, 2;
    is @t.join(','), '1,2', 'a typed array still initializes through STORE';
}
{
    my @a is default(7) = 1, 2;
    @a[5] = 9;
    is @a[3], 7, 'a default trait still applies through STORE';
}

# The lowered build installs nothing into the variable until the eager
# part of the reification has run, so an operand that reads the target
# sees its old content, as it does through STORE.
{
    my @a = 1, 2;
    @a = |@a, 3;
    is @a.join(','), '1,2,3', 'a slip of the target appends to its old content';
}
{
    my @a = 1, 2;
    @a = @a[1], @a[0];
    is @a.join(','), '2,1', 'indexing the target reads its old content';
}
{
    my @a = 1, 2;
    my sub f() { @a[0] + 10 }
    @a = f(), 3;
    is @a.join(','), '11,3', 'a call reading the target sees its old content';
}
{
    my @a = 1, 2;
    @a = (gather take @a[0]).Slip, 5;
    is @a.join(','), '1,5', 'an eager gather reading the target sees its old content';
}

# The lowering replicates the stock Array STORE, but a bind can put any
# value behind an untyped @-sigil, so the assignment form guards on the
# variable holding the stock Array at runtime and falls back to STORE.
{
    my @a := array[int].new;
    @a = 39, 3;
    is-deeply @a, array[int].new(39, 3),
        'assignment to a declaration-bound native array stores natively';
}
{
    my @a;
    @a := array[int].new;
    @a = 4, 5;
    is-deeply @a, array[int].new(4, 5),
        'assignment to a rebound native array stores natively';
}
{
    my @a is List = 1, 2;
    throws-like { @a = 4, 5 }, X::Assignment::RO,
        'assignment to an is List container still throws through STORE';
}
{
    my class TattleArray is Array {
        has $.stored is rw;
        method STORE(|c) { $!stored = True; callsame }
    }
    my @a is TattleArray = 1, 2;
    ok @a.stored, 'assignment to an Array subclass dispatches its own STORE';
}

# An operand whose QAST declares something, like the locals a with
# modifier or an inlined metaop introduce, cannot compile in both guard
# branches and keeps the STORE call.
{
    my @a;
    @a = (1 with 2), 3;
    is-deeply @a, [1, 3], 'a with modifier operand initializes through STORE';
}
{
    my @a;
    my $x = 1;
    @a = ($x += 2), 5;
    is-deeply @a, [3, 5], 'an inlined metaop operand initializes through STORE';
}

# vim: expandtab shiftwidth=4
