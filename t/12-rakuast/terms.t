use MONKEY-SEE-NO-EVAL;
use Test;

plan 5;

{
    my $class = EVAL RakuAST::Package.new:
        scope => 'my',
        package-declarator => 'class',
        how => Metamodel::ClassHOW,
        name => RakuAST::Name.from-identifier('TestClass'),
        body => RakuAST::Block.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
                RakuAST::Method.new(
                    name => RakuAST::Name.from-identifier('meth-a'),
                    body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
                        RakuAST::Statement::Expression.new(
                            RakuAST::IntLiteral.new(99)
                        )
                    ))
                )
            ),
            RakuAST::Statement::Expression.new(
                RakuAST::Method.new(
                    name => RakuAST::Name.from-identifier('meth-b'),
                    body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
                        RakuAST::Statement::Expression.new(RakuAST::ApplyPostfix.new(
                            operand => RakuAST::Term::Self.new,
                            postfix => RakuAST::Call::Method.new(
                                name => RakuAST::Name.from-identifier('meth-a')
                            )
                        ))
                    ))
                )
            )
        )));
    is $class.meth-b(), 99, 'Method call via self works';
}

is-deeply
    EVAL(RakuAST::Statement::Given.new(
        source => RakuAST::StrLiteral.new('argh'),
        body => RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
                RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                        RakuAST::Term::TopicCall.new(RakuAST::Call::Method.new(
                            name => RakuAST::Name.from-identifier('uc')
                        ))
                    )
                )))
    )),
    'ARGH',
    'Topic call applies the call to $_';

isa-ok EVAL(RakuAST::Term::Named.new('now')),
    Instant,
    'now named term can be called';

isa-ok EVAL(RakuAST::Term::Rand.new),
    Num,
    'rand term works';

is-deeply EVAL(RakuAST::Term::EmptySet.new),
    ∅,
    'Empty set term works';
