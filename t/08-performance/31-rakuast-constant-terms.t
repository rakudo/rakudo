use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 19;

# A term that resolves to a constant whose lexical is bound once
# compiles to its value. The name lookup it replaces costs on every
# read, and its op keeps the surrounding frame from ever inlining.
# A container keeps the lookup. The shapes are this frontend's.

sub qast-count-lex(Mu $qast, str $name --> Int:D) {
    my int $count = 0;
    $count++ if nqp::istype($qast, QAST::Var)
        && $qast.scope eq 'lexical' && $qast.name eq $name;
    if qast-descendable($qast) {
        for $qast.list {
            $count += qast-count-lex($_, $name);
        }
    }
    $count
}
sub qast-uses-lex(Mu $qast, str $name) { so qast-count-lex($qast, $name) }

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'my $y = IterationEnd; $y', :full, -> \v {
        not qast-uses-lex(v, 'IterationEnd')
    }, 'a setting constant term compiles to its value';

    qast-is 'my $y = True; $y', :full, -> \v {
        not qast-uses-lex(v, 'True')
    }, 'an enum value term compiles to its value';

    # The constant's declaration owns two mentions of its lexical: the
    # static declaration and the declaration statement's own value. A
    # folded use adds none.
    qast-is 'my constant X = 42; my $y = X; $y', :full, -> \v {
        qast-count-lex(v, 'X') == 2
    }, 'a unit constant term compiles to its value';

    # The enum's declaration owns one mention of each value's lexical.
    # A folded use adds none.
    qast-is 'my enum Color <red green>; my $y = green; $y', :full, -> \v {
        qast-count-lex(v, 'green') == 1
    }, 'a unit enum value term compiles to its value';

    qast-is 'use soft; my $y = IterationEnd; $y', :full, -> \v {
        qast-uses-lex(v, 'IterationEnd')
    }, 'the soft pragma keeps the lookup';

    # The declaration owns two mentions and the package search form
    # keeps its lookup, adding the third.
    qast-is 'my constant X = 42; my $a = ::X; $a', :full, -> \v {
        qast-count-lex(v, 'X') == 3
    }, 'a package search of a constant name keeps the lookup';

    # The outer declaration owns two mentions and its folded use adds
    # none. The shadowing declaration owns two more, and its use keeps
    # the lookup, adding the fifth.
    qast-is 'my constant X = 1; my $a = X; { my constant X = 2; my $b = X; }', :full, -> \v {
        qast-count-lex(v, 'X') == 5
    }, 'a use of a shadowing constant keeps the lookup while the outer use folds';
}
else {
    skip 'the constant term shapes are specific to the RakuAST frontend', 7;
}

# Behavior stays identical.

{
    my $e := IterationEnd;
    ok $e =:= IterationEnd, 'the compiled value keeps the identity of the setting constant';
    ok True, 'a folded True is still true';
    nok False, 'a folded False is still false';
    my enum Color <red green blue>;
    my $c = green;
    is $c, green, 'a folded enum value compares to itself';
    is $c.value, 1, 'a folded enum value keeps its value';
    my constant X = 4242;
    is X + 0, 4242, 'a folded unit constant computes';
}

{
    my constant X = 1;
    my $outer = X;
    my $inner;
    { my constant X = 2; $inner = X; }
    is $outer, 1, 'the outer constant reads its own value';
    is $inner, 2, 'the shadowing constant reads its own value';
    is X, 1, 'the outer constant reads unchanged after the block';
}

{
    my constant T = Int;
    my $y := T;
    ok $y =:= Int, 'a constant holding a type object reads the very type object';
}

# The folded value serializes as a reference into its own context, so
# a precompiled module must produce the same objects when its store
# compiles and when it loads. The imported container proves the
# container gate: a fold would have compiled the decontainerized
# value, and the read would no longer see a Scalar.

{
    my $dir = $*TMPDIR.add("rakuast-constant-terms-$*PID");
    $dir.mkdir;
    $dir.add('ConstantTermTest.rakumod').spurt(q:to/MODULE/);
        unit module ConstantTermTest;
        my constant ANSWER = 424242;
        my enum Shade <dark light>;
        sub iter-end() is export { IterationEnd }
        sub answer() is export { ANSWER }
        sub shade() is export { light ~ '=' ~ light.value }
        MODULE
    $dir.add('ContainerExport.rakumod').spurt(q:to/MODULE/);
        sub EXPORT() { my $v = 42; Map.new(('CV' => $v)) }
        MODULE
    my $probe = 'use ConstantTermTest; use ContainerExport; print +(iter-end() =:= IterationEnd) ~ "|" ~ answer() ~ "|" ~ shade() ~ "|" ~ CV.VAR.^name';
    for 'compiles', 'loads from the precompilation store' -> $stage {
        my $proc = run $*EXECUTABLE, '-I', $dir.absolute, '-e', $probe, :out, :err;
        my $out = $proc.out.slurp(:close);
        $proc.err.slurp(:close);
        is $out, '1|424242|light=1|Scalar', "the folded constants $stage";
    }
    sub nuke(IO::Path $p) { if $p.d { nuke($_) for $p.dir; $p.rmdir } else { $p.unlink } }
    nuke($dir);
}

# vim: expandtab shiftwidth=4
