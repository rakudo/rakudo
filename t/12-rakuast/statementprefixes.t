use MONKEY-SEE-NO-EVAL;
use Test;

plan 6;

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

{
    my $warned = False;
    CONTROL {
        default {
            $warned = True;
            .resume;
        }
    }
    sub do-warning() {
        warn "oops";
        "survived"
    }

    is-deeply
            EVAL(RakuAST::StatementList.new(
                RakuAST::Statement::Expression.new(
                    RakuAST::StatementPrefix::Quietly.new(
                        RakuAST::Statement::Expression.new(
                            RakuAST::Call::Name.new(
                                name => RakuAST::Name.from-identifier('do-warning')
                            )
                        )
                    )
                )
            )),
            "survived",
            'The quietly statement prefix works with a statement';
    nok $warned, 'The warning was suppressed';

    is-deeply
            EVAL(RakuAST::StatementList.new(
                RakuAST::Statement::Expression.new(
                    RakuAST::StatementPrefix::Quietly.new(
                        RakuAST::Block.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
                            RakuAST::Statement::Expression.new(
                                RakuAST::Call::Name.new(
                                    name => RakuAST::Name.from-identifier('do-warning')
                                )
                            )
                        )))
                    )
                )
            )),
            "survived",
            'The quietly statement prefix works with a block';
    nok $warned, 'The warning was suppressed';
}
