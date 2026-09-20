use lib <t/packages/Test-Helpers>;
use Test::Helpers;
use Test;
use experimental :rakuast;

plan 7;

# The optimize phase transforms the tree between check and QAST generation.
# These tests cover where the phase runs and how it is turned off, with
# constant folding as the observable rewrite.

# --target=optimize shows the rewrite, --optimize=off prevents it, and
# --target=ast (before the optimize stage) still holds the operator. These
# dumps are RakuAST frontend output, so the block is pinned to that frontend.
{
    temp %*ENV<RAKUDO_RAKUAST> = '1';
    is-run 'my $x = 2 + 3', 'the optimize stage tree holds no operator application',
        :compiler-args['--target=optimize'], :out({ not .contains('ApplyInfix') });
    is-run 'my $x = 2 + 3', '--optimize=off leaves the operator in the tree',
        :compiler-args['--optimize=off', '--target=optimize'], :out(*.contains('ApplyInfix'));
    is-run 'my $x = 2 + 3', 'the ast stage tree holds the operator before optimize',
        :compiler-args['--target=ast'], :out(*.contains('ApplyInfix'));
}

# A synthetic AST compiled with EVAL goes through the phase too. The phase
# rewrites the tree in place, the same way begin and check annotate it in
# place, so after EVAL the tree holds the literal.
{
    my $ast = RakuAST::StatementList.new(
        RakuAST::Statement::Expression.new(expression => RakuAST::ApplyInfix.new(
            left  => RakuAST::IntLiteral.new(2),
            infix => RakuAST::Infix.new('+'),
            right => RakuAST::IntLiteral.new(3))));
    is $ast.EVAL, 5, 'a synthetic AST EVALs to the right value';
    ok $ast.DEPARSE.contains('5'), 'EVAL runs the optimize phase over a synthetic tree';
}

# Running the pass again over an already-optimized tree is a no-op, so it is
# safe to re-run (EVAL of a reflected AST optimizes it again).
{
    my $cu = Q[my $x = (2 + 3) ?? 4 * 5 !! 0].AST(:compunit);
    $cu.optimize($cu.resolver);
    my $once = $cu.DEPARSE;
    $cu.optimize($cu.resolver);
    is $cu.DEPARSE, $once, 'the optimize pass is idempotent';
}

# The same program runs identically with and without optimization.
{
    my $prog = 'say 7 ** -1; say 1 ?? "then" !! "else"; say (False || 42); say (2 ** 10).Str';
    my $on  = (run $*EXECUTABLE, '-e', $prog, :out).out.slurp(:close);
    my $off = (run $*EXECUTABLE, '--optimize=off', '-e', $prog, :out).out.slurp(:close);
    is $on, $off, 'optimized and unoptimized runs produce identical output';
}

# vim: expandtab shiftwidth=4
