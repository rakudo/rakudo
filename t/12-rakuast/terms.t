use MONKEY-SEE-NO-EVAL;
use Test;

plan 9;

{  # my class TestClass { method meth-a() { 99 }; method meth-b() { self.meth-a } }
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

is-deeply  # given argh { .uc }
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

# now
isa-ok EVAL(RakuAST::Term::Named.new('now')),
    Instant,
    'now named term can be called';

# rand
isa-ok EVAL(RakuAST::Term::Rand.new),
    Num,
    'rand term works';

# ∅
is-deeply EVAL(RakuAST::Term::EmptySet.new),
    ∅,
    'Empty set term works';

# True
is-deeply EVAL(RakuAST::Term::Name.new(RakuAST::Name.from-identifier('True'))),
    True,
    'Name term works with single-part name';

# Bool::True
is-deeply EVAL(RakuAST::Term::Name.new(RakuAST::Name.from-identifier-parts('Bool', 'True'))),
    True,
    'Name term works with multi-part name';

# Whatever
isa-ok EVAL(RakuAST::Term::Whatever.new),
    Whatever,
    'Whatever term works';

# HyperWhatever
isa-ok EVAL(RakuAST::Term::HyperWhatever.new),
    HyperWhatever,
    'HyperWhatever term works';
