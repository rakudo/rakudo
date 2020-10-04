use MONKEY-SEE-NO-EVAL;
use Test;

plan 15;

my $ast;
sub ast(RakuAST::Node:D $node --> Nil) {
    $ast := $node;
    diag $ast.DEPARSE.chomp;
}

subtest 'The do statement prefix works with a statement' => {
    # do 137
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::StatementPrefix::Do.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::IntLiteral.new(137)
          )
        )
      )
    );

    is-deeply $_, 137
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'The do statement prefix works with a block' => {
    # do { 199 }
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::StatementPrefix::Do.new(
          RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
              RakuAST::StatementList.new(
                RakuAST::Statement::Expression.new(
                  expression => RakuAST::IntLiteral.new(199)
                )
              )
            )
          )
        )
      )
    );

    is-deeply $_, 199
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'The quietly statement prefix works' => {
    my $warned;
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

    # quietly do-warning()
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::StatementPrefix::Quietly.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Call::Name.new(
              name => RakuAST::Name.from-identifier('do-warning')
            )
          )
        )
      )
    );

    $warned = False;
    is-deeply EVAL($ast), "survived", 'with a statement';
    nok $warned, 'The warning was suppressed';

    $warned = False;
    is-deeply EVAL($ast.DEPARSE), "survived", 'DEPARSE with a statement';
    nok $warned, 'The warning was suppressed';

    # quietly { do-warning() }
    $ast := RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::StatementPrefix::Quietly.new(
          RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
              RakuAST::StatementList.new(
                RakuAST::Statement::Expression.new(
                  expression => RakuAST::Call::Name.new(
                    name => RakuAST::Name.from-identifier('do-warning')
                  )
                )
              )
            )
          )
        )
      )
    );

    $warned = False;
    is-deeply EVAL($ast), "survived", 'with a block';
    nok $warned, 'The warning was suppressed';

    $warned = False;
    is-deeply EVAL($ast.DEPARSE), "survived", 'DEPARSE with a block';
    nok $warned, 'The warning was suppressed';
}

subtest 'The gather statement prefix works on a statement' => {
    my $done;
    sub do-takes() {
        $done = True;
        take 111;
        take 222;
    }

    # gather do-takes()
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::StatementPrefix::Gather.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Call::Name.new(
              name => RakuAST::Name.from-identifier('do-takes')
            )
          )
        )
      )
    );

    $done = False;
    for EVAL($ast), EVAL($ast.DEPARSE) -> \result {
        isa-ok result, Seq, 'Got a Seq back from gather (expression form)';
        is-deeply $done, False, 'The gather is lazy';
        my @elems = result;
        is-deeply $done, True, 'Gathered the takes';
        is-deeply @elems, [111, 222], 'Got correct result from the gather expression';
        $done = False;
    }
}

subtest 'The gather statement prefix works on a block' => {
    my $done;
    sub do-takes() {
        $done = True;
        take 333;
        take 444;
    }
    # gather { do-takes() }
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::StatementPrefix::Gather.new(
          RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
              RakuAST::StatementList.new(
                RakuAST::Statement::Expression.new(
                  expression => RakuAST::Call::Name.new(
                    name => RakuAST::Name.from-identifier('do-takes')
                  )
                )
              )
            )
          )
        )
      )
    );

    $done = False;
    for EVAL($ast), EVAL($ast.DEPARSE) -> \result {
        isa-ok result, Seq, 'Got a Seq back from gather (block form)';
        is-deeply $done, False, 'The gather is lazy';
        my @elems = result;
        is-deeply $done, True, 'Gathered the takes';
        is-deeply @elems, [333, 444], 'Got correct result from the gather expression';
        $done = False;
    }
}

subtest "The race / hyper / lazy / eager statement prefixes work" => {
    my class ContextMe {
        has @.called;
        method race()  { @!called.push('race');  'result' }
        method hyper() { @!called.push('hyper'); 'result' }
        method lazy()  { @!called.push('lazy');  'result' }
        method eager() { @!called.push('eager'); 'result' }
    }

    for <race hyper lazy eager> -> $context {
        my $c;
        my $result;

        # race|hyper|lazy|eager $c
        ast RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::StatementPrefix::{tclc $context}.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::Var::Lexical.new('$c')
              )
            )
          )
        );

        $c = ContextMe.new;
        $result := EVAL($ast);
        is-deeply $result, 'result', "$context works with a statement";
        is-deeply $c.called, [$context], 'Correct context method was called';

        $c = ContextMe.new;
        $result := EVAL($ast.DEPARSE);
        is-deeply $result, 'result', "DEPARSE $context works with a statement";
        is-deeply $c.called, [$context], 'Correct context method was called';
    }

    for <race hyper lazy eager> -> $context {
        my $c;
        my $result;

        # race|hyper|lazy|eager { $c }
        ast RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::StatementPrefix::{tclc $context}.new(
              RakuAST::Block.new(
                body => RakuAST::Blockoid.new(
                  RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                      expression => RakuAST::Var::Lexical.new('$c')
                    )
                  )
                )
              )
            )
          )
        );

        $c = ContextMe.new;
        $result := EVAL($ast);
        is-deeply $result, 'result', "$context works with a block";
        is-deeply $c.called, [$context], 'Correct context method was called';

        $c = ContextMe.new;
        $result := EVAL($ast.DEPARSE);
        is-deeply $result, 'result', "DEPARSE $context works with a block";
        is-deeply $c.called, [$context], 'Correct context method was called';
    }
}

subtest 'try statement prefix with expression producing value results' => {
    # try 99
    ast RakuAST::StatementPrefix::Try.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::IntLiteral.new(99)
      )
    );

    is-deeply EVAL($ast), 99, 'AST: correct result';
    is-deeply $!, Nil, 'AST: $! is Nil when not exception';

    is-deeply EVAL($ast.DEPARSE), 99, 'DEPARSE: correct result';
    todo 'string eval does not set $!, also in master';
    is-deeply $!, Nil, 'DEPARSE: $! is Nil when not exception';
}

subtest 'try statement prefix with throwing expression handles the exception' => {
    # try die "hard"
    ast RakuAST::StatementPrefix::Try.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Call::Name.new(
          name => RakuAST::Name.from-identifier('die'),
          args => RakuAST::ArgList.new(RakuAST::StrLiteral.new('hard'))
        )
      )
    );

    $! = 42;
    is-deeply EVAL($ast), Nil, 'AST';
    is-deeply $!.Str, 'hard', '$! is populated with the exception';

    $! = 42;
    is-deeply EVAL($ast.DEPARSE), Nil, 'DEPARSE';
    is-deeply $!.Str, 'hard', '$! is populated with the exception';
}

subtest 'try statement prefix with block producing value results' => {
    # try { 999 }
    ast RakuAST::StatementPrefix::Try.new(
      RakuAST::Block.new(
        body => RakuAST::Blockoid.new(
          RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
              expression => RakuAST::IntLiteral.new(999)
            )
          )
        )
      )
    );

    is-deeply EVAL($ast), 999, 'AST: correct result';
    is-deeply $!, Nil, 'AST: $! is Nil when not exception';

    is-deeply EVAL($ast.DEPARSE), 999, 'DEPARSE: correct result';
    todo 'string eval does not set $!, also in master';
    is-deeply $!, Nil, 'DEPARSE: $! is Nil when not exception';
}

subtest 'try statement prefix with throwing block handles the exception' => {
    # try { die "another day" }
    ast RakuAST::StatementPrefix::Try.new(
      RakuAST::Block.new(
        body => RakuAST::Blockoid.new(
          RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
              expression => RakuAST::Call::Name.new(
                name => RakuAST::Name.from-identifier('die'),
                args => RakuAST::ArgList.new(RakuAST::StrLiteral.new('another day'))
              )
            )
          )
        )
      )
    );

    $! = 42;
    is-deeply EVAL($ast), Nil, 'AST';
    is-deeply $!.Str, 'another day', '$! is populated with the exception';

    $! = 42;
    is-deeply EVAL($ast.DEPARSE), Nil, 'DEPARSE';
    is-deeply $!.Str, 'another day', '$! is populated with the exception';
}

subtest 'start statement prefix with expression evalutes to Promise' => {
    # start 111
    ast RakuAST::StatementPrefix::Start.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::IntLiteral.new(111)
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $promise {
        isa-ok $promise, Promise, $type;
        is-deeply await($promise), 111, 'Correct result from Promise';
    }
}

subtest 'start statement prefix with block evalutes to Promise' => {
    # start { 137 }
    ast RakuAST::StatementPrefix::Start.new(
      RakuAST::Block.new(
        body => RakuAST::Blockoid.new(
          RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
              expression => RakuAST::IntLiteral.new(137)
            )
          )
        )
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $promise {
        isa-ok $promise, Promise, $type;
        is-deeply await($promise), 137, 'Correct result from Promise';
    }
}

subtest 'A start has a fresh $/' => {
    # start $/
    ast RakuAST::StatementPrefix::Start.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Var::Lexical.new('$/')
      )
    );

    {
        my $/ = 42;
        todo 'fresh specials nyi';
        nok await(EVAL($ast)) ~~ 42, 'AST: A start has a fresh $/';
    }

    {
        my $/ = 666;
        nok await(EVAL($ast.DEPARSE)) ~~ 666, 'DEPARSE: A start has a fresh $/';
    }
}

subtest 'A start has a fresh $!' => {
    # start $!
    ast RakuAST::StatementPrefix::Start.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Var::Lexical.new('$!')
      )
    );

    {
        my $! = 42;
        todo 'fresh specials nyi';
        nok await(EVAL($ast)) ~~ 42, 'AST: A start has a fresh $!';
    }

    {
        my $! = 666;
        nok await(EVAL($ast.DEPARSE)) ~~ 666, 'DEPARSE: A start has a fresh $!';
    }
}

subtest 'BEGIN phaser producing a literal expression works' => {
    # BEGIN 12
    ast RakuAST::StatementPrefix::Phaser::Begin.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::IntLiteral.new(12)
      )
    );
    is-deeply $_, 12
      for EVAL($ast), EVAL($ast.DEPARSE);
}

# vim: expandtab shiftwidth=4
