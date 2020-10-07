use MONKEY-SEE-NO-EVAL;
use Test;

plan 6;

my $ast;
sub ast(RakuAST::Node:D $node --> Nil) {
    $ast := $node;
    diag $ast.DEPARSE.chomp;
}

subtest 'can make a simple ... stub' => {
    # ...
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Stub::Fail.new
      )
    );

#    for 'AST','DEPARSE' -> $type {
    for 'DEPARSE' -> $type {   # for now
        throws-like { EVAL($type eq 'AST' ?? $ast !! $ast.DEPARSE) },
          X::StubCode,
          message => 'Stub code executed'
        ;
    }
}

subtest 'can make a ... stub with info' => {
    # ...
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Stub::Fail.new(
          args => RakuAST::ArgList.new(
            RakuAST::StrLiteral.new("foobar")
          )
        )
      )
    );

#    for 'AST','DEPARSE' -> $type {
    for 'DEPARSE' -> $type {   # for now
        throws-like { EVAL($type eq 'AST' ?? $ast !! $ast.DEPARSE) },
          X::StubCode,
          message => "foobar"
        ;
    }
}

subtest 'can make a simple !!! stub' => {
    # ...
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Stub::Die.new
      )
    );

#    for 'AST','DEPARSE' -> $type {
    for 'DEPARSE' -> $type {   # for now
        throws-like { EVAL($type eq 'AST' ?? $ast !! $ast.DEPARSE) },
          X::StubCode,
          message => 'Stub code executed'
        ;
    }
}

subtest 'can make a !!! stub with info' => {
    # ...
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Stub::Die.new(
          args => RakuAST::ArgList.new(
            RakuAST::StrLiteral.new("foobar")
          )
        )
      )
    );

#    for 'AST','DEPARSE' -> $type {
    for 'DEPARSE' -> $type {   # for now
        throws-like { EVAL($type eq 'AST' ?? $ast !! $ast.DEPARSE) },
          X::StubCode,
          message => "foobar"
        ;
    }
}

subtest 'can make a simple ??? stub' => {
    # ...
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Stub::Warn.new
      )
    );

#    for 'AST','DEPARSE' -> $type {
    for 'DEPARSE' -> $type {   # for now
        EVAL($type eq 'AST' ?? $ast !! $ast.DEPARSE);
        CONTROL {
            isa-ok $_, CX::Warn, "$type: did we get a warning?";
            is .message, "Stub code executed", "$type: the right warning";
            .resume;
        }
    }
}

subtest 'can make a ??? stub with info' => {
    # ...
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Stub::Warn.new(
          args => RakuAST::ArgList.new(
            RakuAST::StrLiteral.new("foobar")
          )
        )
      )
    );

#    for 'AST','DEPARSE' -> $type {
    for 'DEPARSE' -> $type {   # for now
        EVAL($type eq 'AST' ?? $ast !! $ast.DEPARSE);
        CONTROL {
            isa-ok $_, CX::Warn, "$type: did we get a warning?";
            is .message, "foobar", "$type: the right warning";
            .resume;
        }
    }
}

# vim: expandtab shiftwidth=4
