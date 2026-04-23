# Regression fixture for https://github.com/rakudo/rakudo/issues/6137
# Precompiling this module used to fail with "missing static code ref for
# closure" because the Sub returned by BEGIN-time EVAL kept references to
# unregistered static code refs from the inner (nested) compunit's bytecode.
our &foo = BEGIN {
    RakuAST::Sub.new(
      name => RakuAST::Name.from-identifier("foo"),
      body => RakuAST::Blockoid.new(
        RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Call::Name::WithoutParentheses.new(
              name => RakuAST::Name.from-identifier("say"),
              args => RakuAST::ArgList.new(
                RakuAST::StrLiteral.new("from-precomped-6137")
              )
            )
          )
        )
      )
    ).EVAL
}
