use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 45;

# Placeholder parameters lower to frame locals like the parameters of
# a pointy block, unless a closure or a by-name lookup reaches them.
# The QAST shapes checked are those of the RakuAST frontend.

sub qast-has-local(Mu $qast, str $prefix --> Bool:D) {
    if nqp::istype($qast, QAST::Var) {
        return True if $qast.scope eq 'local' && $qast.name.starts-with($prefix);
    }
    if qast-descendable($qast) {
        for $qast.list {
            qast-has-local($_, $prefix) and return True;
        }
    }
    False
}

sub qast-count-locals(Mu $qast, str $prefix --> Int:D) {
    my %names;
    sub walk(Mu $node) {
        if nqp::istype($node, QAST::Var) {
            %names{$node.name} = 1 if $node.scope eq 'local' && $node.name.starts-with($prefix);
        }
        if qast-descendable($node) {
            walk($_) for $node.list;
        }
    }
    walk($qast);
    %names.elems
}

sub qast-has-lexical(Mu $qast, str $name --> Bool:D) {
    if nqp::istype($qast, QAST::Var) {
        return True if $qast.scope eq 'lexical' && $qast.name eq $name
            && $qast.decl ne 'static';
    }
    if qast-descendable($qast) {
        for $qast.list {
            qast-has-lexical($_, $name) and return True;
        }
    }
    False
}

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'my $f = { $^a + $^b }', :full, -> \v {
        qast-has-local(v, '$__lowered_a') and qast-has-local(v, '$__lowered_b')
        and not qast-has-lexical(v, '$a') and not qast-has-lexical(v, '$b')
    }, 'positional placeholders of a block lower to locals';
    qast-is 'my $f = { $^a + $^a }', :full, -> \v {
        qast-has-local(v, '$__lowered_a') and not qast-has-lexical(v, '$a')
    }, 'a placeholder used twice lowers each use';
    qast-is 'my $f = { $^a + $a }', :full, -> \v {
        qast-has-local(v, '$__lowered_a') and not qast-has-lexical(v, '$a')
    }, 'a plain lookup of a placeholder name lowers with it';
    qast-is 'my $f = { $:x + 1 }', :full, -> \v {
        qast-has-local(v, '$__lowered_x') and not qast-has-lexical(v, '$x')
    }, 'a named placeholder of a block lowers to a local';
    qast-is 'my $f = { my $s = 0; $s += $^a; $s }', :full, -> \v {
        qast-has-local(v, '$__lowered_a') and not qast-has-lexical(v, '$a')
    }, 'a placeholder read by a compound assignment lowers';
    qast-is 'my $f = { $^a > 0 ?? -> { $a } !! Nil }', :full, -> \v {
        qast-has-lexical(v, '$a') and not qast-has-local(v, '$__lowered_a')
    }, 'a placeholder captured by a closure keeps its lexical';
    qast-is 'my $f = { $^a.map({ $a }) }', :full, -> \v {
        qast-has-lexical(v, '$a') and not qast-has-local(v, '$__lowered_a')
    }, 'a placeholder name looked up from a closure keeps its lexical';
    qast-is 'my $f = { $^a.defined ?? "<{$a}>" !! "" }', :full, -> \v {
        qast-has-lexical(v, '$a') and not qast-has-local(v, '$__lowered_a')
    }, 'a placeholder read from an interpolated block keeps its lexical';
    qast-is 'my $f = { @_ + $^a }', :full, -> \v {
        qast-has-local(v, '$__lowered_a') and qast-has-lexical(v, '@_')
    }, 'a slurpy placeholder keeps its lexical beside a lowered positional';
    qast-is 'sub f { $^a + $^b }', :full, -> \v {
        qast-has-local(v, '$__lowered_a') and qast-has-local(v, '$__lowered_b')
        and not qast-has-lexical(v, '$a') and not qast-has-lexical(v, '$b')
    }, 'positional placeholders of a sub lower to locals';
    qast-is 'my $f = { $^a; EVAL q[$a] }', :full, -> \v {
        qast-has-lexical(v, '$a') and not qast-has-local(v, '$__lowered_a')
    }, 'a placeholder beside an EVAL keeps its lexical';
    qast-is 'my $f = { $^a; MY::<$a> }', :full, -> \v {
        qast-has-lexical(v, '$a') and not qast-has-local(v, '$__lowered_a')
    }, 'a placeholder beside a pseudo-package lookup keeps its lexical';
    qast-is 'my $f = { $^a; "a" ~~ /a { $a }/ }', :full, -> \v {
        qast-has-lexical(v, '$a') and not qast-has-local(v, '$__lowered_a')
    }, 'a placeholder read from a regex code block keeps its lexical';
    qast-is 'my $f = { if $^a { $a * 2 } }', :full, -> \v {
        qast-has-local(v, '$__lowered_a') and not qast-has-lexical(v, '$a')
    }, 'a placeholder read from a flattened if body lowers';
    qast-is 'my $f = { for 1..3 { $^a } }', :full, -> \v {
        qast-has-local(v, '$__lowered_a') and not qast-has-lexical(v, '$a')
    }, 'a placeholder of a loop body block lowers';
    qast-is 'my $f = { $^a + { $^a * 2 }(3) }', :full, -> \v {
        qast-count-locals(v, '$__lowered_a') == 2 and not qast-has-lexical(v, '$a')
    }, 'nested blocks each lower their own placeholder of one name';
    qast-is 'sub f { %_<k> ~ $^a }', :full, -> \v {
        qast-has-lexical(v, '%_') and qast-has-local(v, '$__lowered_a')
    }, 'a slurpy hash placeholder of a sub keeps its lexical beside a lowered positional';
    qast-is 'class PH1 { method m { %_<k> } }', :full, -> \v {
        qast-has-lexical(v, '%_') and not qast-has-local(v, '%__lowered')
    }, 'the slurpy hash placeholder of a method keeps its lexical';
    qast-is 'my $f = { $^a xx 2 }', :full, -> \v {
        qast-has-lexical(v, '$a') and not qast-has-local(v, '$__lowered_a')
    }, 'a placeholder read from a thunked operand keeps its lexical';
    qast-is 'my $a = 100; my $f = { $^a + 1 }', :full, -> \v {
        qast-count-locals(v, '$__lowered_a') == 2 and not qast-has-lexical(v, '$a')
    }, 'a placeholder shadowing an outer lexical lowers apart from it';
}
else {
    skip 'placeholder lowering is decided by the RakuAST frontend', 20;
}

{
    my $f = { $^a + $^b };
    is $f(1, 2), 3, 'positional placeholders bind in order';
    is $f.arity, 2, 'the block has the arity of its placeholders';
    is $f.signature.params.map(*.name).join(','), '$a,$b', 'the signature names the placeholders';
}
{
    my $f = { $^b - $^a };
    is $f(1, 5), 4, 'placeholders bind by name order, not by use order';
}
{
    my $f = { $^a + $a };
    is $f(4), 8, 'a plain lookup of a placeholder name reads the parameter';
}
{
    my $f = { $:x * $:y };
    is $f(:x(3), :y(5)), 15, 'named placeholders bind by name';
}
{
    my $f = { my $s = 0; $s += $^a; $s * 2 };
    is $f(21), 42, 'a placeholder feeds a compound assignment';
}
{
    my $f = { $^a > 0 ?? -> { $a * 2 } !! Nil };
    is $f(21)(), 42, 'a closure sees the placeholder it captures';
    my @g = { $^a; (1..3).map({ $_ * $a }) }(10);
    is-deeply @g, [10, 20, 30], 'a closure reads the placeholder name from its outer block';
}
{
    my $f = { $^a.defined ?? "<{$a}>" !! "" };
    is $f('x'), '<x>', 'an interpolated block reads the placeholder of its block';
}
{
    my $f = { @_.elems + $^a };
    is $f(1, 2, 3), 3, 'a slurpy placeholder takes the arguments after the positional';
}
{
    sub f { $^a ~ $^b }
    is f('x', 'y'), 'xy', 'placeholders of a sub bind in order';
}
{
    is { $^a; EVAL '$a' }(42), 42, 'an EVAL reads the placeholder by name';
    is { $^a; MY::<$a> }(42), 42, 'a pseudo-package lookup reads the placeholder by name';
    is { $^a; -> { OUTER::<$a> }() }(42), 42, 'an OUTER lookup from a closure reads the placeholder by name';
    is { $^a; "a" ~~ /a { $a }/; $/.Str }('x'), 'a', 'a regex code block sees the placeholder';
}
{
    is { if $^a { $a * 2 } }(21), 42, 'a placeholder reads inside a flattened if body';
    is-deeply { my @r = do for 1..3 { $^a }; @r }(), [1, 2, 3], 'a loop body block binds its own placeholder';
    is { $^a + { $^a * 2 }(3) }(1), 7, 'nested blocks bind their own placeholder of one name';
    my $a = 100;
    my $f = { $^a + 1 };
    is $f(1), 2, 'a placeholder shadows an outer lexical of its name';
    is $a, 100, 'the outer lexical is untouched by the placeholder';
    is { $^n <= 1 ?? 1 !! $n * &?BLOCK($n - 1) }(5), 120, 'a block recurses through its placeholder';
}
{
    dies-ok { { $^a = 5 }(1) }, 'a positional placeholder cannot be assigned';
    dies-ok { { $:x = 5 }(:x(1)) }, 'a named placeholder cannot be assigned';
    is { $^a + $:x }.count, 1, 'the block counts only its positional placeholder';
}
