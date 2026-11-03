use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 20;

# A routine declaration in statement position evaluates to a closure
# clone of the routine, formed by a method dispatch on the serialized
# code object plus a capture of the enclosing frame. When the statement
# is sunk that clone is discarded, and the routine itself is installed
# by the declaration walk, so the statement emits nothing. A routine
# declared directly in the comp unit likewise keeps the serialized
# routine as its lexical's value, with no clone formed beside it. The
# elided chains would otherwise run on every load of the compilation
# unit, once per declaration.

# The file-scope routines the behavioral tests call. Declared at the
# top level of this file so they take the comp unit branch, which a
# routine under any enclosing block does not.
sub wrap-target() { 'orig' }
my $file-var = 10;
sub reads-file-var() { $file-var + 1 }
sub never-defined() { ... }
sub stub-then-real() { ... }
sub stub-then-real() { 'real' }

my sub qast-count-callmethod (Mu $qast, $name --> Int:D) {
    my int $count = 0;
    $count = 1 if nqp::istype($qast, QAST::Op)
        && $qast.op eq 'callmethod' && $qast.name ~~ $name;
    if qast-descendable $qast {
        $count += qast-count-callmethod($_, $name) for $qast.list;
    }
    $count
}

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    # A package body compiles as an immediate call with no clone of its
    # own. The one clone each snippet keeps belongs to the body's final
    # declaration statement: as the body block's return expression it is
    # not sunk, so it still forms its closure.
    qast-is 'my class SunkPlain { method m() { } }; 1', :full, -> \v {
        qast-count-callmethod(v, 'clone') == 1
    }, 'a sunk method declaration forms no closure clone of its own';

    qast-is 'my class SunkMulti { multi method m() { }; multi method m(Int) { } }; 1', :full, -> \v {
        qast-count-callmethod(v, 'clone') == 1
    }, 'sunk multi method declarations form no closure clones of their own';

    qast-is 'sub f() { }; 1', :full, -> \v {
        not qast-contains-callmethod(v, 'clone')
    }, 'a comp unit level sub keeps the serialized routine as its lexical value';

    qast-is 'my grammar SunkG { token t { \d } }; 1', :full, -> \v {
        qast-count-callmethod(v, 'clone') == 1
    }, 'a sunk token declaration forms no closure clone of its own';

    qast-is 'sub f() { }; 1', :full, -> \v {
        not qast-contains-op(v, 'p6sink')
    }, 'a sunk routine declaration statement is not sink called';

    qast-is 'my $c = sub () { 42 }', :full, -> \v {
        qast-contains-callmethod(v, 'clone')
    }, 'a routine used as an expression still forms its closure clone';

    qast-is 'my class BodySub { my sub h() { 1 }; method m() { h } }; 1', :full, -> \v {
        qast-contains-callmethod(v, 'clone')
    }, 'a lexical sub under a class body still rebinds its closure clone';
}
else {
    skip 'shapes specific to the RakuAST frontend', 7;
}

# The declaration walk and the frame prologue's capture carry all the
# behavior a per-statement clone would.

{
    my @subs;
    for 1..3 -> $i {
        my sub s() { $i }
        @subs.push(&s);
    }
    is @subs.map({ .() }).join(','), '1,2,3',
        'a lexical sub under a repeating block closes over each run';
}

{
    my @closures;
    for 1..2 -> $i {
        @closures.push: do { my sub v() { $i } };
    }
    is @closures.map({ .() }).join(','), '1,2',
        'a declaration as a block final value still yields a fresh closure';
}

{
    my class InProcess {
        my sub h() { 'helped' }
        method m() { h() }
    }
    is InProcess.m, 'helped', 'a method reaches a lexical sub of its class body';
}

&wrap-target.wrap(sub () { 'wrapped-' ~ callsame });
is wrap-target(), 'wrapped-orig', 'wrap works on a comp unit level sub';

is reads-file-var(), 11, 'a comp unit level sub closes over comp unit lexicals';

throws-like { never-defined() }, X::StubCode,
    'calling a comp unit level stub reports the stub';

is stub-then-real(), 'real',
    'a comp unit level stub replaced by a definition calls the definition';

{
    my $caller-var = 5;
    is EVAL('sub e() { $caller-var }; e()'), 5,
        'a sub at the top of an EVAL unit closes over caller lexicals';
}

# The same holds when the unit is precompiled and its serialized
# routines load in a fresh compilation. The fixture keeps its routines
# at file scope, with no unit module wrapper, so they take the comp
# unit branch too.
{
    my $dir = $*TMPDIR.add("rakuast-decl-closures-{$*PID}");
    sub nuke(IO::Path $d) {
        for $d.dir { $_.d ?? nuke($_) !! $_.unlink }
        $d.rmdir;
    }
    LEAVE nuke($dir) if $dir.e;
    $dir.mkdir;
    $dir.add('DeclClosures.rakumod').spurt(q:to/END/);
        my $unit-var = 5;
        our sub uses-unit() { $unit-var * 2 }
        our proto sub pm(|) {*}
        multi sub pm(Int) { 'int' }
        multi sub pm(Str) { 'str' }
        our grammar PG { token TOP { \d+ } }
        our class DC {
            my sub helper() { 'H' }
            method m() { helper() ~ $unit-var }
            multi method mm() { 'zero' }
            multi method mm(Int) { 'int' }
        }
        END
    my $repo = CompUnit::Repository::FileSystem.new(:prefix($dir.Str));
    CompUnit::RepositoryRegistry.use-repository($repo);
    require ::('DeclClosures');
    is ::('&uses-unit')(), 10,
        'a precompiled comp unit level sub closes over its unit lexical';
    is ::('&pm')(1) ~ ::('&pm')('x'), 'intstr',
        'a precompiled comp unit level proto dispatches its multis';
    is ::('PG').parse('123').Str, '123',
        'a precompiled comp unit level grammar parses';
    is ::('DC').m, 'H5',
        'a precompiled method reaches its class body lexical sub';
    is ::('DC').mm ~ ::('DC').mm(1), 'zeroint',
        'precompiled multi methods dispatch';
}
