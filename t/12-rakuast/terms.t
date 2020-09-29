use MONKEY-SEE-NO-EVAL;
use Test;

plan 11;

my $ast;

subtest 'Method call via self' => {
    # my class TestClass {
    #     method meth-a() { 99 }
    #     method meth-b() { self.meth-a }
    # }
    $ast := RakuAST::Package.new(
      scope => 'my',
      package-declarator => 'class',
      name  => RakuAST::Name.from-identifier('TestClass'),
      how   => Metamodel::ClassHOW,
      body  => RakuAST::Block.new(
        body => RakuAST::Blockoid.new(
          RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
              expression => RakuAST::Method.new(
                name => RakuAST::Name.from-identifier('meth-a'),
                body => RakuAST::Blockoid.new(
                  RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                      expression => RakuAST::IntLiteral.new(99)
                    )
                  ),
                )
              )
            ),
            RakuAST::Statement::Expression.new(
              expression => RakuAST::Method.new(
                name => RakuAST::Name.from-identifier('meth-b'),
                body => RakuAST::Blockoid.new(
                  RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                      expression => RakuAST::ApplyPostfix.new(
                        operand => RakuAST::Term::Self.new,
                          postfix => RakuAST::Call::Method.new(
                          name => RakuAST::Name.from-identifier('meth-a')
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, Mu $class {
        is-deeply $class.meth-b(), 99,
          "$type: Method call via self works";
    }
}

subtest 'Topic call applies the call to $_' => {
    # given argh { .uc }
    $ast := RakuAST::Statement::Given.new(
      source => RakuAST::StrLiteral.new('argh'),
      body => RakuAST::Block.new(
        body => RakuAST::Blockoid.new(
          RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
              expression => RakuAST::Term::TopicCall.new(
                RakuAST::Call::Method.new(
                  name => RakuAST::Name.from-identifier('uc')
                )
              )
            )
          )
        )
      )
    );

    is-deeply $_, 'ARGH'
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'now named term can be called' => {
    # now
    $ast := RakuAST::Term::Named.new('now');

    isa-ok $_, Instant
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'rand term works' => {
    # rand
    $ast := RakuAST::Term::Rand.new;

    isa-ok $_, Num
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Empty set term works' => {
    # ∅
    $ast := RakuAST::Term::EmptySet.new;

    is-deeply $_, ∅,
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Name term works with single-part name' => {
    # True
    $ast := RakuAST::Term::Name.new(
      RakuAST::Name.from-identifier('True')
    );

    is-deeply $_, True,
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Name term works with multi-part name' => {
    # Bool::True
    $ast := RakuAST::Term::Name.new(
      RakuAST::Name.from-identifier-parts('Bool', 'True')
    );

    is-deeply $_, True,
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Whatever term works' => {
    # *
    $ast := RakuAST::Term::Whatever.new;

    isa-ok $_, Whatever,
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Hyperwhatever term works' => {
    # **
    $ast := RakuAST::Term::HyperWhatever.new;

    isa-ok $_, HyperWhatever,
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Capture term can be constructed with a term' => {
    my $var = 4;

    # \$var
    $ast := RakuAST::Term::Capture.new(
      RakuAST::Var::Lexical.new('$var')
    );

    is-deeply $_, \(4)
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Capture term can be constructed with an arg list' => {
    # \(6, :x)
    $ast := RakuAST::Term::Capture.new(
      RakuAST::ArgList.new(
        RakuAST::IntLiteral.new(6),
        RakuAST::ColonPair::True.new(key => 'x')
      )
    );

    is-deeply $_, \(6, :x)
      for EVAL($ast), EVAL($ast.DEPARSE);
}

# vim: expandtab shiftwidth=4
