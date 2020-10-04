use MONKEY-SEE-NO-EVAL;
use Test;

plan 13;

my $ast;
sub ast(RakuAST::Node:D $node --> Nil) {
    $ast := $node;
    diag $ast.DEPARSE.chomp;
}

subtest 'One-part quoted string with literal piece' => {
    # "hello"
    ast RakuAST::QuotedString.new(
      :segments[RakuAST::StrLiteral.new('hello')]
    );

    is-deeply $_, 'hello'
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Quoted string with interpolated string variable works' => {
    my $str = 'hello,';

    # "$str world"
    ast RakuAST::QuotedString.new(
      :segments[
        RakuAST::Var::Lexical.new('$str'),
        RakuAST::StrLiteral.new(' world')
      ]
    );
    is-deeply $_, 'hello, world'
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Quoted string with interpolated integer variable' => {
    my $int = 42;

    # "$int is the answer"
    ast RakuAST::QuotedString.new(
      :segments[
        RakuAST::Var::Lexical.new('$int'),
        RakuAST::StrLiteral.new(' is the answer')
      ]
    );
    is-deeply $_, '42 is the answer'
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Quoted string consisting of only interpolated integer variable' => {
    my $int = 42;

    # "$int"
    ast RakuAST::QuotedString.new(
      :segments[
        RakuAST::Var::Lexical.new('$int'),
      ]
    );
    is-deeply $_, '42'
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Quoted string with 3 parts works' => {
    my $int = 42;

    # "The answer is $int of course"
    ast RakuAST::QuotedString.new(
      :segments[
        RakuAST::StrLiteral.new('The answer is '),
        RakuAST::Var::Lexical.new('$int'),
        RakuAST::StrLiteral.new(' of course!')
      ]
    );
    is-deeply $_, 'The answer is 42 of course!'
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Quoted string involving an interpolated block' => {
    my $bv = 'interpolated';

    # "An { $bv } block"
    ast RakuAST::QuotedString.new(
      :segments[
        RakuAST::StrLiteral.new('An '),
        RakuAST::Block.new(
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::Var::Lexical.new('$bv')
              )
            )
          )
        ),
        RakuAST::StrLiteral.new(' block')
      ]
    );
    is-deeply $_, 'An interpolated block'
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'words processor splits a single literal string into words' => {
    # qqw/ foo bar 42 /
    ast RakuAST::QuotedString.new(
      :segments[RakuAST::StrLiteral.new('foo bar 42')],
      :processors['words']
    );
    is-deeply $_, ("foo", "bar", "42")
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Words processor applied to a quoted string with interpolation' => {
    my $stuff = 'r baz';

    # qqw/ ba$stuff 66 /
    ast RakuAST::QuotedString.new(
      :segments[
        RakuAST::StrLiteral.new('ba'),
        RakuAST::Var::Lexical.new('$stuff'),
        RakuAST::StrLiteral.new(' 66')
      ],
      :processors['words']
    );
    is-deeply $_, ("bar", "baz", "66")
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Using the val processor alone' => {
    # qq:v/42/
    ast RakuAST::QuotedString.new(
      :segments[RakuAST::StrLiteral.new('42')],
      :processors['val']
    );
    is-deeply $_, IntStr.new(42,'42')
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Using the val processor alone with interpolation' => {
    my $end = '6';

    # qq:v/4$end/
    ast RakuAST::QuotedString.new(
      :segments[
        RakuAST::StrLiteral.new('4'),
        RakuAST::Var::Lexical.new('$end')
      ],
      :processors['val']
    );
    is-deeply $_, <46>
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Using the words and val processor' => {
    # <foo bar 42>
    ast RakuAST::QuotedString.new(
      :segments[RakuAST::StrLiteral.new('foo bar 42')],
      :processors['words', 'val']
    );
    is-deeply $_, ("foo", "bar", val("42"))
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Words processor applied to a quoted string with interpolation' => {
    my $stuff = 'r baz';

    # no source representation possible atm
    $ast := RakuAST::QuotedString.new(
      :segments[
        RakuAST::StrLiteral.new('ba'),
        RakuAST::Var::Lexical.new('$stuff'),
        RakuAST::StrLiteral.new(' 66')
      ],
      :processors['words', 'val']
    );
    is-deeply EVAL($ast), ('bar', 'baz', val("66"));
}

subtest 'Using the exec processor alone gives expected result' => {
    # qx/ echo 123 /
    ast RakuAST::QuotedString.new(
      :segments[RakuAST::StrLiteral.new('echo 123')],
      :processors['exec']
    );
    is-deeply $_, "123\n"
      for EVAL($ast), EVAL($ast.DEPARSE);
}

# vim: expandtab shiftwidth=4
