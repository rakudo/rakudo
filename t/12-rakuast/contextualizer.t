use MONKEY-SEE-NO-EVAL;
use Test;

plan 4;

my $ast;
sub ast(RakuAST::Node:D $node --> Nil) {
    $ast := $node;
}

subtest 'Hash contextualizer from empty sequence' => {
    # %()
    ast RakuAST::Contextualizer::Hash.new(RakuAST::StatementSequence.new());
    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $result {
        is-deeply $result, Hash.new(),
            "$type: Contextualizer gives expected result";
    }
}

subtest 'List contextualizer from empty sequence' => {
    # @()
    ast RakuAST::Contextualizer::List.new(RakuAST::StatementSequence.new());
    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $result {
        is-deeply $result, List.new(),
            "$type: Contextualizer gives expected result";
    }
}

subtest 'Hash contextualizer from pairs' => {
    # %(a => 1, b => 2)
    ast RakuAST::Contextualizer::Hash.new(
        RakuAST::StatementSequence.new(
            RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyListInfix.new(
                    infix => RakuAST::Infix.new(','),
                    operands => [
                        RakuAST::FatArrow.new(key => 'a', value => RakuAST::IntLiteral.new(1)),
                        RakuAST::FatArrow.new(key => 'b', value => RakuAST::IntLiteral.new(2))
                    ]
                )
            )
        )
    );
    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $result {
        is-deeply $result, Hash.new((a => 1, b => 2)),
            "$type: Contextualizer gives expected result";
    }
}

subtest 'List contextualizer from pairs' => {
    # @(a => 1, b => 2)
    ast RakuAST::Contextualizer::List.new(
        RakuAST::StatementSequence.new(
            RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyListInfix.new(
                    infix => RakuAST::Infix.new(','),
                    operands => [
                        RakuAST::FatArrow.new(key => 'a', value => RakuAST::IntLiteral.new(1)),
                        RakuAST::FatArrow.new(key => 'b', value => RakuAST::IntLiteral.new(2))
                    ]
                )
            )
        )
    );
    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $result {
        is-deeply $result, (a => 1, b => 2),
            "$type: Contextualizer gives expected result";
    }
}
