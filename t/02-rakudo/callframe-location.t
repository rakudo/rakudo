use Test;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;

plan 6;

is-deeply EVAL("\n\n" ~ q[constant C = (callframe(0).line, 1)[0]; C == $?LINE]), True,
    'callframe in a list in a constant reports the line of its code';
is-deeply EVAL("\n\n" ~ q[constant C = (callframe(0).file, 1)[0]; C eq callframe(0).file]), True,
    'callframe in a list in a constant reports the file of its code';
is-deeply EVAL("\n\n" ~ q[constant C = ((callframe(0).line == $?LINE)] ~ "\n" ~ q[?? True !! False, 1)[0]; C]), True,
    'callframe in a list in a constant over two lines reports the line it starts on';
is-deeply EVAL("\n\n" ~ q[my class A { has $.l is default((callframe(0).line, 1)[0]) }; A.new.l == $?LINE]), True,
    'callframe in a list in a trait argument on one line reports that line';
is-deeply EVAL("\n\n" ~ q[constant C = (callframe(0).line, { 1 })[0]; C == $?LINE]), True,
    'callframe beside a block in a list in a constant reports the line of its code';
lives-ok {
    my $ast := Q[constant C = (callframe(0).line, 1)[0]; sub f() { C }; f()].AST;
    my sub strip($node) {
        $node.set-origin(RakuAST::Origin.new(:from(0), :to(1))) if $node.can('set-origin');
        $node.visit-children(&strip);
    }
    strip($ast);
    $ast.EVAL;
}, 'code whose origins have no source compiles';
