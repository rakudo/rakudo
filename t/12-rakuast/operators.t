use MONKEY-SEE-NO-EVAL;
use Test;

plan 23;

my $ast;

subtest 'Application of an infix operator on two literals' => {

    # 44 + 22
    $ast := RakuAST::ApplyInfix.new(
      left => RakuAST::IntLiteral.new(44),
      infix => RakuAST::Infix.new('+'),
      right => RakuAST::IntLiteral.new(22)
    );
    is-deeply $_, 66
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'The special form || operator works' => {

    # 22 || 44
    $ast := RakuAST::ApplyInfix.new(
      left => RakuAST::IntLiteral.new(22),
      infix => RakuAST::Infix.new('||'),
      right => RakuAST::IntLiteral.new(44)
    );
    is-deeply $_, 22
      for EVAL($ast), EVAL($ast.DEPARSE);

    # 0 || 44
    $ast := RakuAST::ApplyInfix.new(
      left => RakuAST::IntLiteral.new(0),
      infix => RakuAST::Infix.new('||'),
      right => RakuAST::IntLiteral.new(44)
    );
    is-deeply $_, 44
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'The special form or operator works' => {

    # 22 or 44
    $ast := RakuAST::ApplyInfix.new(
      left => RakuAST::IntLiteral.new(22),
      infix => RakuAST::Infix.new('or'),
      right => RakuAST::IntLiteral.new(44)
    );
    is-deeply $_, 22
      for EVAL($ast), EVAL($ast.DEPARSE);

    # 0 or 44
    $ast := RakuAST::ApplyInfix.new(
      left => RakuAST::IntLiteral.new(0),
      infix => RakuAST::Infix.new('or'),
      right => RakuAST::IntLiteral.new(44)
    );
    is-deeply $_, 44
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'The special form && operator works' => {

    # 22 && 44
    $ast := RakuAST::ApplyInfix.new(
      left => RakuAST::IntLiteral.new(22),
      infix => RakuAST::Infix.new('&&'),
      right => RakuAST::IntLiteral.new(44)
    );
    is-deeply $_, 44
      for EVAL($ast), EVAL($ast.DEPARSE);

    # 0 && 44
    $ast := RakuAST::ApplyInfix.new(
      left => RakuAST::IntLiteral.new(0),
      infix => RakuAST::Infix.new('&&'),
      right => RakuAST::IntLiteral.new(44)
    );
    is-deeply $_, 0
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'The special form and operator works' => {

    # 22 and 44
    $ast := RakuAST::ApplyInfix.new(
      left => RakuAST::IntLiteral.new(22),
      infix => RakuAST::Infix.new('and'),
      right => RakuAST::IntLiteral.new(44)
    );
    is-deeply $_, 44
      for EVAL($ast), EVAL($ast.DEPARSE);

    # 0 and 44
    $ast := RakuAST::ApplyInfix.new(
      left => RakuAST::IntLiteral.new(0),
      infix => RakuAST::Infix.new('and'),
      right => RakuAST::IntLiteral.new(44)
    );
    is-deeply $_, 0
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Application of a prefix operator to a literal' => {

    # ?2
    $ast := RakuAST::ApplyPrefix.new(
      prefix => RakuAST::Prefix.new('?'),
      operand => RakuAST::IntLiteral.new(2)
    );
    is-deeply $_, True
      for EVAL($ast), EVAL($ast.DEPARSE);

    # ?0
    $ast := RakuAST::ApplyPrefix.new(
      prefix => RakuAST::Prefix.new('?'),
      operand => RakuAST::IntLiteral.new(0)
    );
    is-deeply $_, False
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Application of a (user-defined) postfix operator to a literal' => {
    sub postfix:<!>($n) {
        [*] 1..$n
    }

    # 4!
    $ast := RakuAST::ApplyPostfix.new(
      operand => RakuAST::IntLiteral.new(4),
      postfix => RakuAST::Postfix.new('!'),
    );
    is-deeply $_, 24
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Basic assignment to a Scalar container' => {
    my $a = 1;

    # $a = 4
    $ast := RakuAST::ApplyInfix.new(
      left => RakuAST::Var::Lexical.new('$a'),
      infix => RakuAST::Infix.new('='),
      right => RakuAST::IntLiteral.new(4)
    );

    is-deeply EVAL($ast), 4;
    $a = 666;
    is-deeply EVAL($ast.DEPARSE), 4;
}

subtest 'Basic single-dimension array index' => {
    my @a = 10..20;

    # @a[5]
    $ast := RakuAST::ApplyPostfix.new(
      operand => RakuAST::Var::Lexical.new('@a'),
      postfix => RakuAST::Postcircumfix::ArrayIndex.new(
        RakuAST::SemiList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::IntLiteral.new(5)
          )
        )
      )
    );
    is-deeply $_, 15
      for EVAL($ast); #, EVAL($ast.DEPARSE);
}

subtest 'Zen array slice' => {
    my @a = 10..20;

    # @a[]
    $ast := RakuAST::ApplyPostfix.new(
      operand => RakuAST::Var::Lexical.new('@a'),
      postfix => RakuAST::Postcircumfix::ArrayIndex.new(
        RakuAST::SemiList.new
      )
    );
    is-deeply $_, @a
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Multi-dimensional array indexing' => {
    my @a[3;3] = <a b c>, <d e f>, <g h i>;

    # @a[2;1]
    $ast := RakuAST::ApplyPostfix.new(
      operand => RakuAST::Var::Lexical.new('@a'),
      postfix => RakuAST::Postcircumfix::ArrayIndex.new(
        RakuAST::SemiList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::IntLiteral.new(2)
          ),
          RakuAST::Statement::Expression.new(
            expression => RakuAST::IntLiteral.new(1)
          )
        )
      )
    );

    is-deeply $_, 'h'
      for EVAL($ast), EVAL($ast.DEPARSE);
}

{
    my %h = a => 'add', s => 'subtract';

    subtest 'Basic single-dimension hash index' => {
        # %h{('s',)}
        $ast := RakuAST::ApplyPostfix.new(
          operand => RakuAST::Var::Lexical.new('%h'),
          postfix => RakuAST::Postcircumfix::HashIndex.new(
            RakuAST::SemiList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::StrLiteral.new('s')
              )
            )
          )
        );
        is-deeply $_, 'subtract'
          for EVAL($ast), EVAL($ast.DEPARSE);
    }

    subtest 'Zen hash slice' => {
        # %h{}
        $ast := RakuAST::ApplyPostfix.new(
          operand => RakuAST::Var::Lexical.new('%h'),
          postfix => RakuAST::Postcircumfix::HashIndex.new(
            RakuAST::SemiList.new()
          )
        );
        is-deeply $_, %h
          for EVAL($ast), EVAL($ast.DEPARSE);
    }

    subtest 'Basic literal hash index' => {
        # %h<s>
        $ast := RakuAST::ApplyPostfix.new(
          operand => RakuAST::Var::Lexical.new('%h'),
          postfix => RakuAST::Postcircumfix::LiteralHashIndex.new(
            RakuAST::QuotedString.new(
              segments => [RakuAST::StrLiteral.new('s')],
              processors => ['words','val']
            )
          )
        );
        is-deeply $_, 'subtract'
          for EVAL($ast), EVAL($ast.DEPARSE);
    }

    subtest 'Literal hash index with multiple keys' => {
        # %h<s a>
        $ast := RakuAST::ApplyPostfix.new(
          operand => RakuAST::Var::Lexical.new('%h'),
          postfix => RakuAST::Postcircumfix::LiteralHashIndex.new(
            RakuAST::QuotedString.new(
              segments => [RakuAST::StrLiteral.new('s a')],
              processors => ['words','val']
            )
          )
        );
        is-deeply $_, ('subtract', 'add'),
          for EVAL($ast), EVAL($ast.DEPARSE);
    }

    subtest 'Empty literal hash index works as zen slice' => {
        # %h<>
        $ast := RakuAST::ApplyPostfix.new(
          operand => RakuAST::Var::Lexical.new('%h'),
          postfix => RakuAST::Postcircumfix::LiteralHashIndex.new(
            RakuAST::QuotedString.new(
              segments => [RakuAST::StrLiteral.new('')],
              processors => ['words','val']
            )
          )
        );
        is-deeply $_, %h
          for EVAL($ast), EVAL($ast.DEPARSE);
    }
}

subtest 'Multi-dimensional hash indexing' => {
    my %h = x => { :1a, :2b }, y => { :3a, :4b };

    # %h{'y';'a'}
    $ast := RakuAST::ApplyPostfix.new(
      operand => RakuAST::Var::Lexical.new('%h'),
      postfix => RakuAST::Postcircumfix::HashIndex.new(
        RakuAST::SemiList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::StrLiteral.new('y')
          ),
          RakuAST::Statement::Expression.new(
            expression => RakuAST::StrLiteral.new('a')
          )
        )
      )
    );
    is-deeply $_, (3,), # Is this actually a CORE.setting bug?
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Application of a list infix operator on three operands' => {
    # (10, 11, 12)
    $ast := RakuAST::ApplyListInfix.new(
      infix => RakuAST::Infix.new(','),
      operands => (
        RakuAST::IntLiteral.new(10),
        RakuAST::IntLiteral.new(11),
        RakuAST::IntLiteral.new(12),
      )
    );
    is-deeply $_, (10, 11, 12),
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Application of a list infix operator on no operands' => {
    # ()
    $ast := RakuAST::ApplyListInfix.new(
      infix => RakuAST::Infix.new(','),
      operands => ()
    );
    is-deeply $_, (),
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Chaining operator has correct outcome' => {
    my $x = 4;

    # 5 > $x++ > 3
    $ast := RakuAST::ApplyInfix.new(
      left => RakuAST::ApplyInfix.new(
        left => RakuAST::IntLiteral.new(5),
        infix => RakuAST::Infix::Chaining.new('>'),
        right => RakuAST::ApplyPostfix.new(
          postfix => RakuAST::Postfix.new('++'),
          operand => RakuAST::Var::Lexical.new('$x')
        )
      ),
      infix => RakuAST::Infix::Chaining.new('>'),
      right => RakuAST::IntLiteral.new(3)
    );

    is-deeply EVAL($ast), True;
    is-deeply $x, 5;

    $x = 4;
    is-deeply EVAL($ast.DEPARSE), True;
    is-deeply $x, 5;
}

subtest 'Correct outcome of ternary operator' => {
    # $a ?? 22 !! 33
    $ast := RakuAST::Ternary.new(
      condition => RakuAST::Var::Lexical.new('$a'),
      then => RakuAST::IntLiteral.new(22),
      else => RakuAST::IntLiteral.new(33)
    );

    my $a = 1;
    is-deeply $_, 22
      for EVAL($ast), EVAL($ast.DEPARSE);

    $a = 0;
    is-deeply $_, 33
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Application of dotty infix `.`' => {
    # "foo".uc
    $ast := RakuAST::ApplyDottyInfix.new(
      left => RakuAST::StrLiteral.new('foo'),
      infix => RakuAST::DottyInfix::Call.new(),
      right => RakuAST::Call::Method.new(
        name => RakuAST::Name.from-identifier('uc')
      )
    );

    is-deeply $_, 'FOO'
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Application of dotty infix `.=` evaluates to expected value' => {
    my $var = 'foo';

    # $var .= tc
    $ast := RakuAST::ApplyDottyInfix.new(
      left => RakuAST::Var::Lexical.new('$var'),
      infix => RakuAST::DottyInfix::CallAssign.new(),
      right => RakuAST::Call::Method.new(
        name => RakuAST::Name.from-identifier('tc')
      )
    );

    is-deeply EVAL($ast), 'Foo';
    is-deeply $var, 'Foo';

    $var = 'foo';
    is-deeply EVAL($ast.DEPARSE), 'Foo';
    is-deeply $var, 'Foo';
}

# vim: expandtab shiftwidth=4
