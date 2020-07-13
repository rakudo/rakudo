use MONKEY-SEE-NO-EVAL;
use Test;

plan 2;

is-deeply
        EVAL(RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
                RakuAST::StatementPrefix::Do.new(
                    RakuAST::Statement::Expression.new(
                        RakuAST::IntLiteral.new(137)
                    )
                )
            )
        )),
        137,
        'The do statement prefix works with a statement';

is-deeply
        EVAL(RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
                RakuAST::StatementPrefix::Do.new(
                    RakuAST::Block.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
                        RakuAST::Statement::Expression.new(
                            RakuAST::IntLiteral.new(199)
                        )
                    )))
                )
            )
        )),
        199,
        'The do statement prefix works with a block';
