use MONKEY-SEE-NO-EVAL;
use Test;

plan 5;

is-deeply
    EVAL(RakuAST::QuotedString.new(
        RakuAST::StrLiteral.new('hello')
    )),
    'hello',
    'One-part quoted string with literal piece comes out as the right thing';

my $str = 'hello, ';
is-deeply
    EVAL(RakuAST::QuotedString.new(
        RakuAST::Var::Lexical.new('$str'),
        RakuAST::StrLiteral.new('world')
    )),
    'hello, world',
    'Quoted string with interpolated string variable works';

my $int = 42;
is-deeply
    EVAL(RakuAST::QuotedString.new(
        RakuAST::Var::Lexical.new('$int'),
        RakuAST::StrLiteral.new(' is the answer')
    )),
    '42 is the answer',
    'Quoted string with interpolated integer variable works';

is-deeply
    EVAL(RakuAST::QuotedString.new(
        RakuAST::Var::Lexical.new('$int'),
    )),
    '42',
    'Quoted string consisting of only interpolated integer variable gives a string';

is-deeply
    EVAL(RakuAST::QuotedString.new(
        RakuAST::StrLiteral.new('The answer is '),
        RakuAST::Var::Lexical.new('$int'),
        RakuAST::StrLiteral.new(' of course!')
    )),
    'The answer is 42 of course!',
    'Quoted string with 3 parts works';
