use MONKEY-SEE-NO-EVAL;
use Test;

plan 43;

is-deeply  # do 137
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

is-deeply  # do { 199 }
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

    is-deeply  # quietly do-warning()
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

    is-deeply  # quietly { do-warning() }
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
    # gather do-takes()
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
    # gather { do-takes() }
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
        is-deeply  # race|hyper|lazy|eager $c
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
        is-deeply  # race|hyper|lazy|eager { $c }
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

is-deeply  # try 99
    EVAL(RakuAST::StatementPrefix::Try.new(
        RakuAST::Statement::Expression.new(
            RakuAST::IntLiteral.new(99)
        )
    )),
    99,
    'try statement prefix with expression producing value results in the value';
is-deeply $!, Nil, 'The $! variable is Nil when not exception';

is-deeply  # try die "hard"
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

is-deeply  # try { 999 }
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

is-deeply  # try { die "another day" }
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

{  # start 111
    my $promise = EVAL(RakuAST::StatementPrefix::Start.new(
        RakuAST::Statement::Expression.new(
            RakuAST::IntLiteral.new(111)
        )
    ));
    isa-ok $promise, Promise, 'start statement prefix with expression evalutes to Promise';
    is-deeply await($promise), 111, 'Correct result from Promise';
}

{  # start { 137 }
    my $promise = EVAL(RakuAST::StatementPrefix::Start.new(
        RakuAST::Block.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
                RakuAST::IntLiteral.new(137)
            )
        )))
    ));
    isa-ok $promise, Promise, 'start statement prefix with block evalutes to Promise';
    is-deeply await($promise), 137, 'Correct result from Promise';
}

{
    my $/ = 42;
    # start $/
    my $promise = EVAL(RakuAST::StatementPrefix::Start.new(
        RakuAST::Statement::Expression.new(
            RakuAST::Var::Lexical.new('$/')
        )
    ));
    todo 'fresh specials nyi';
    nok await($promise) ~~ 42, 'A start has a fresh $/';
}

{
    my $! = 42;
    # start $!
    my $promise = EVAL(RakuAST::StatementPrefix::Start.new(
        RakuAST::Statement::Expression.new(
            RakuAST::Var::Lexical.new('$!')
        )
    ));
    todo 'fresh specials nyi';
    nok await($promise) ~~ 42, 'A start has a fresh $!';
}

is-deeply  # BEGIN 12
    EVAL(RakuAST::StatementPrefix::Phaser::Begin.new(
        RakuAST::Statement::Expression.new(
            RakuAST::IntLiteral.new(12)
        )
    )),
    12,
    'BEGIN phaser producing a literal expression works';
