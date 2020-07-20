use MONKEY-SEE-NO-EVAL;
use Test;

plan 36;

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

{
    my $done = False;
    sub do-takes() {
        $done = True;
        take 111;
        take 222;
    }
    my \result = EVAL(RakuAST::StatementList.new(
        RakuAST::Statement::Expression.new(
            RakuAST::StatementPrefix::Gather.new(
                RakuAST::Statement::Expression.new(
                    RakuAST::Call::Name.new(
                        name => RakuAST::Name.from-identifier('do-takes')
                    )
                )
            )
        )
    ));
    isa-ok result, Seq, 'Got a Seq back from gather (expression form)';
    nok $done, 'The gather is lazy';
    my @elems = result;
    is-deeply @elems, [111, 222], 'Got correct result from the gather expression';
}

{
    my $done = False;
    sub do-takes() {
        $done = True;
        take 333;
        take 444;
    }
    my \result = EVAL(RakuAST::StatementList.new(
        RakuAST::Statement::Expression.new(
            RakuAST::StatementPrefix::Gather.new(
                RakuAST::Block.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                        RakuAST::Call::Name.new(
                            name => RakuAST::Name.from-identifier('do-takes')
                        )
                    )
                )))
            )
        )
    ));
    isa-ok result, Seq, 'Got a Seq back from gather (block form)';
    nok $done, 'The gather is lazy';
    my @elems = result;
    is-deeply @elems, [333, 444], 'Got correct result from the gather block';
}

{
    my class ContextMe {
        has @.called;
        method race() { @!called.push('race'); 'result' }
        method hyper() { @!called.push('hyper'); 'result' }
        method lazy() { @!called.push('lazy'); 'result' }
        method eager() { @!called.push('eager'); 'result' }
    }

    for <race hyper lazy eager> -> $context {
        my $c = ContextMe.new;
        is-deeply
                EVAL(RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                        RakuAST::StatementPrefix::{tclc $context}.new(
                            RakuAST::Statement::Expression.new(
                                RakuAST::Var::Lexical.new('$c')
                            )
                        )
                    )
                )),
                'result',
                "The $context statement prefix works with a statement";
        is-deeply $c.called, [$context], 'Correct context method was called';
    }

    for <race hyper lazy eager> -> $context {
        my $c = ContextMe.new;
        is-deeply
                EVAL(RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                        RakuAST::StatementPrefix::{tclc $context}.new(
                            RakuAST::Block.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
                                RakuAST::Statement::Expression.new(
                                    RakuAST::Var::Lexical.new('$c')
                                )
                            )))
                        )
                    )
                )),
                'result',
                "The $context statement prefix works with a block";
        is-deeply $c.called, [$context], 'Correct context method was called';
    }
}

is-deeply
    EVAL(RakuAST::StatementPrefix::Try.new(
        RakuAST::Statement::Expression.new(
            RakuAST::IntLiteral.new(99)
        )
    )),
    99,
    'try statement prefix with expression producing value results in the value';
is-deeply $!, Nil, 'The $! variable is Nil when not exception';

is-deeply
    EVAL(RakuAST::StatementPrefix::Try.new(
        RakuAST::Statement::Expression.new(
            RakuAST::Call::Name.new(
                name => RakuAST::Name.from-identifier('die'),
                args => RakuAST::ArgList.new(RakuAST::StrLiteral.new('hard'))
            )
        )
    )),
    Nil,
    'try statement prefix with throwing expression handles the exception';
is $!, 'hard', '$! is populated with the exception';

is-deeply
    EVAL(RakuAST::StatementPrefix::Try.new(
        RakuAST::Block.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
                RakuAST::IntLiteral.new(999)
            )
        )))
    )),
    999,
    'try statement prefix with block producing value results in the value';
is-deeply $!, Nil, 'The $! variable is Nil when not exception';

is-deeply
    EVAL(RakuAST::StatementPrefix::Try.new(
        RakuAST::Block.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
                RakuAST::Call::Name.new(
                    name => RakuAST::Name.from-identifier('die'),
                    args => RakuAST::ArgList.new(RakuAST::StrLiteral.new('another day'))
                )
            )
        )))
    )),
    Nil,
    'try statement prefix with throwing block handles the exception';
is $!, 'another day', '$! is populated with the exception';
