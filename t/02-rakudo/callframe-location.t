use Test;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;

plan 19;

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

is-deeply EVAL("\n\n" ~ q[constant C = callframe(0).line; C == $?LINE]), True,
    'callframe in a constant reports the line of its code';
is-deeply EVAL("\n\n" ~ q[constant C = callframe(0).file; C eq callframe(0).file]), True,
    'callframe in a constant reports the file of its code';
is-deeply EVAL("\n\n" ~ q[constant C = callframe.line; C == $?LINE]), True,
    'callframe without arguments in a constant reports the line of its code';
is-deeply EVAL("\n\n" ~ q[constant C = 0 || callframe(0).line; C == $?LINE]), True,
    'callframe right of || in a constant reports the line of its code';
is-deeply EVAL("\n\n" ~ q[constant C = &callframe(0).line; C == $?LINE]), True,
    'a call of &callframe in a constant reports the line of its code';
is-deeply EVAL("\n\n" ~ q[constant C = CallFrame.new.line; C == $?LINE]), True,
    'CallFrame.new in a constant reports the line of its code';
is-deeply EVAL("\n\n" ~ q[my class A { has $.l is default(callframe(0).line) }; A.new.l == $?LINE]), True,
    'callframe in a trait argument reports the line of the trait';
is-deeply EVAL("\n\n" ~ q[my class A { has $.l is default(True ?? callframe(0).line !! 0) }; A.new.l == $?LINE]), True,
    'callframe in a ternary trait argument on one line reports that line';
is-deeply EVAL("\n\n" ~ q[constant C = callframe(0).line == $?LINE] ~ "\n" ~ q[?? True !! False; C]), True,
    'callframe in a constant over two lines reports the line it starts on';
is-deeply EVAL("\n\n" ~ q[my constant &cf = &callframe; constant C = cf(0).line; C == $?LINE]), True,
    'a call of an alias of callframe in a constant reports the line of its code';
is-deeply EVAL("\n\n" ~ q[my constant CF = CallFrame; constant C = CF.new.line; C == $?LINE]), True,
    'new of an alias of CallFrame in a constant reports the line of its code';
nok EVAL(RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Constant.new(
          name => 'C',
          initializer => RakuAST::Initializer::Assign.new(
            RakuAST::ApplyPostfix.new(
              operand => RakuAST::Call::Name.new(
                name => RakuAST::Name.from-identifier('callframe'),
                args => RakuAST::ArgList.new(RakuAST::IntLiteral.new(0))),
              postfix => RakuAST::Call::Method.new(
                name => RakuAST::Name.from-identifier('file')))))),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Term::Name.new(RakuAST::Name.from-identifier('C'))))
    ).contains('src/Raku/ast'),
    'callframe in a constant of an AST given to EVAL does not report a file of the compiler';
is-deeply EVAL("\n\n" ~ q[constant C = callframe(0).line + { 0 }(); C == $?LINE]), True,
    'callframe beside a block in a constant reports the line of its code';
