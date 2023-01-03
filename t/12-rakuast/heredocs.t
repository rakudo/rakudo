# NOTE: if you're adding / adapting tests here, you probably want to do
#       the same in t/12-rakuast/strings.t

use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

plan 13;

my $ast;
sub ast(RakuAST::Node:D $node --> Nil) {
    $ast := $node;
    diag $ast.DEPARSE.chomp;
}

subtest 'One-part heredoc with literal piece' => {
    # "hello\n\ngoodbye\n"
    ast RakuAST::Heredoc.new(
      :segments[RakuAST::StrLiteral.new("hello\n\ngoodbye\n")],
      :stop("    CODE\n")
    );

    my $deparsed := $ast.DEPARSE;
    is-deeply $deparsed, 'qq:to/CODE/
    hello

    goodbye
    CODE
',
      'is DEPARSE ok';

    is-deeply $_, "hello\n\ngoodbye\n"
      for EVAL($ast), EVAL($deparsed);
}

subtest 'Heredoc with interpolated string variable works' => {
    my $str = 'hello,';

    # "$str world\n"
    ast RakuAST::Heredoc.new(
      :segments[
        RakuAST::Var::Lexical.new('$str'),
        RakuAST::StrLiteral.new(" world\n")
      ],
      :stop("MESSAGE\n")
    );

    my $deparsed := $ast.DEPARSE;
    is-deeply $deparsed, 'qq:to/MESSAGE/
$str world
MESSAGE
',
      'is DEPARSE ok';

    is-deeply $_, "hello, world\n"
      for EVAL($ast), EVAL($deparsed);
}

subtest 'Heredoc with interpolated integer variable' => {
    my $int = 42;

    # "$int is the answer\n"
    ast RakuAST::Heredoc.new(
      :segments[
        RakuAST::Var::Lexical.new('$int'),
        RakuAST::StrLiteral.new(" is the answer\n")
      ],
      :stop("  INTY\n")
    );

    my $deparsed := $ast.DEPARSE;
    is-deeply $deparsed, 'qq:to/INTY/
  $int is the answer
  INTY
',
      'is DEPARSE ok';

    is-deeply $_, "42 is the answer\n"
      for EVAL($ast), EVAL($deparsed);
}

subtest 'Heredoc consisting of only interpolated integer variable' => {
    # NOTE: No Raku representation is possible without at least a string
    #       with a newline in it, so maybe this test is a bit over the top.
    my $int = 42;

    # "$int\n"
    ast RakuAST::Heredoc.new(
      :segments[
        RakuAST::Var::Lexical.new('$int'),
        RakuAST::StrLiteral.new("\n")
      ],
      :stop(" NONE\n")
    );

    my $deparsed := $ast.DEPARSE;
    is-deeply $deparsed, 'qq:to/NONE/
 $int
 NONE
',
      'is DEPARSE ok';

    is-deeply $_, "42\n"
      for EVAL($ast), EVAL($deparsed);
}

subtest 'Heredoc with 3 parts works' => {
    my $int = 42;

    # "The answer is $int of course"
    ast RakuAST::Heredoc.new(
      :segments[
        RakuAST::StrLiteral.new('The answer is '),
        RakuAST::Var::Lexical.new('$int'),
        RakuAST::StrLiteral.new(" of course!\n")
      ],
      :stop("    MORE\n")
    );

    my $deparsed := $ast.DEPARSE;
    is-deeply $deparsed, 'qq:to/MORE/
    The answer is $int of course!
    MORE
',
      'is DEPARSE ok';

    is-deeply $_, "The answer is 42 of course!\n"
      for EVAL($ast), EVAL($deparsed);
}

subtest 'Heredoc involving an interpolated block' => {
    my $bv = 'interpolated';

    # "An { $bv } block\n"
    ast RakuAST::Heredoc.new(
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
        RakuAST::StrLiteral.new(" block\n")
      ],
      :stop("\n")
    );

    my $deparsed := $ast.DEPARSE;
    is-deeply $deparsed, 'qq:to//
An {
    $bv
} block

',
      'is DEPARSE ok';

    is-deeply $_, "An interpolated block\n"
      for EVAL($ast), EVAL($deparsed);
}

subtest 'words processor splits a heredoc into words' => {
    # qqw/ foo bar 42 /
    ast RakuAST::Heredoc.new(
      :segments[RakuAST::StrLiteral.new("foo bar 42\n")],
      :processors['words'],
      :stop("WORDS\n")
    );

    my $deparsed := $ast.DEPARSE;
    is-deeply $deparsed, 'qq:w:to/WORDS/
foo bar 42
WORDS
',
      'is DEPARSE ok';

    is-deeply $_, ("foo", "bar", "42")
      for EVAL($ast), EVAL($deparsed);
}

subtest 'Words processor applied to a heredoc with interpolation' => {
    my $stuff = 'r baz';

    # qqw/ ba$stuff 66 /
    ast RakuAST::Heredoc.new(
      :segments[
        RakuAST::StrLiteral.new('ba'),
        RakuAST::Var::Lexical.new('$stuff'),
        RakuAST::StrLiteral.new(" 66\n")
      ],
      :processors['words'],
      :stop("WORDS\n")
    );

    my $deparsed := $ast.DEPARSE;
    is-deeply $deparsed, 'qq:w:to/WORDS/
ba$stuff 66
WORDS
',
      'is DEPARSE ok';

    is-deeply $_, ("bar", "baz", "66")
      for EVAL($ast), EVAL($deparsed);
}

subtest 'Using the val processor alone on a heredoc' => {
    # qq:v/42/
    ast RakuAST::Heredoc.new(
      :segments[RakuAST::StrLiteral.new("42\n")],
      :processors['val'],
      :stop("VAL\n")
    );

    my $deparsed := $ast.DEPARSE;
    is-deeply $deparsed, 'qq:v:to/VAL/
42
VAL
',
      'is DEPARSE ok';

#    is-deeply $_, IntStr.new(42,"42\n")
#      for EVAL($ast), EVAL($ast.DEPARSE);
    is-deeply EVAL($ast),      IntStr.new(42,"42\n");
    todo("problem in parsing static heredocs and :v");
    is-deeply EVAL($deparsed), IntStr.new(42,"42\n");
}

subtest 'Using the val processor in heredoc with interpolation' => {
    my $end = '6';

    # qq:v/4$end/
    ast RakuAST::Heredoc.new(
      :segments[
        RakuAST::StrLiteral.new('4'),
        RakuAST::Var::Lexical.new('$end'),
        RakuAST::StrLiteral.new("\n")
      ],
      :processors['val']
      :stop("VAL\n")
    );

    my $deparsed := $ast.DEPARSE;
    is-deeply $deparsed, 'qq:v:to/VAL/
4$end
VAL
',
      'is DEPARSE ok';

    is-deeply $_, IntStr.new(46,"46\n") 
      for EVAL($ast), EVAL($deparsed);
}

subtest 'Using the words and val processor in a heredoc' => {
    # <foo bar 42>
    ast RakuAST::Heredoc.new(
      :segments[RakuAST::StrLiteral.new("foo bar 42\n")],
      :processors['words', 'val'],
      :stop("VALS\n")
    );

    my $deparsed := $ast.DEPARSE;
    is-deeply $deparsed, 'qq:w:v:to/VALS/
foo bar 42
VALS
',
      'is DEPARSE ok';

    is-deeply $_, ("foo", "bar", IntStr.new(42,"42"))
      for EVAL($ast), EVAL($deparsed);
}

subtest 'Words processor applied to a heredoc with interpolation' => {
    my $stuff = 'r baz';

    # qq:w:v/ba$stuff 66/
    $ast := RakuAST::Heredoc.new(
      :segments[
        RakuAST::StrLiteral.new('ba'),
        RakuAST::Var::Lexical.new('$stuff'),
        RakuAST::StrLiteral.new(" 66\n")
      ],
      :processors['words', 'val'],
      :stop("HUH\n")
    );

    my $deparsed := $ast.DEPARSE;
    is-deeply $deparsed, 'qq:w:v:to/HUH/
ba$stuff 66
HUH
',
      'is DEPARSE ok';

    is-deeply $_, ('bar', 'baz', IntStr.new(66,"66"))
      for EVAL($ast), EVAL($deparsed);
}

subtest 'Using the exec processor alone gives expected result' => {
    # qx/ echo 123 /
    ast RakuAST::Heredoc.new(
      :segments[RakuAST::StrLiteral.new('echo 123')],
      :processors['exec'],
      :stop("GO\n")
    );

    my $deparsed := $ast.DEPARSE;
    is-deeply $deparsed, 'qq:x:to/GO/
echo 123
GO
',
      'is DEPARSE ok';

    is-deeply $_, "123\n"
      for EVAL($ast), EVAL($deparsed);
}

# vim: expandtab shiftwidth=4
