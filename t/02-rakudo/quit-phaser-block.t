use Test;
use experimental :rakuast;

plan 4;

throws-like q:to/CODE/, X::Syntax::Missing, what => 'block',
    react { QUIT say 1; whenever Supply.from-list(1) { } }
    CODE
    'QUIT followed by a statement asks for a block';

throws-like q:to/CODE/, X::Syntax::Missing, what => 'block',
    react { QUIT -> $e { }; whenever Supply.from-list(1) { } }
    CODE
    'QUIT followed by a pointy block asks for a block';

is (try EVAL q:to/CODE/),
    my $message;
    react {
        whenever Supply.from-list(1).map({ die "boom" }) {
            QUIT { default { $message = .message } }
        }
    }
    $message
    CODE
    'boom',
    'a QUIT block receives the exception as its topic';

throws-like {
    RakuAST::StatementPrefix::Phaser::Quit.new(
      RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new(1)))
}, X::TypeCheck::Binding::Parameter, 'a QUIT node takes only a block';
