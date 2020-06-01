use MONKEY-SEE-NO-EVAL;
use Test;

plan 4;

sub no-args() {
    444
}
is-deeply
        EVAL(RakuAST::Call::Name.new(
            name => RakuAST::Name.from-identifier('no-args')
        )),
        444,
        'Can make a named call with no arguments';

sub one-arg($x) {
    9 * $x
}
is-deeply
        EVAL(RakuAST::Call::Name.new(
            name => RakuAST::Name.from-identifier('one-arg'),
            args => RakuAST::ArgList.new(RakuAST::IntLiteral.new(5))
        )),
        45,
        'Can make a named call with one positional argument';

sub two-args($x, $y) {
    $x - $y
}
is-deeply
        EVAL(RakuAST::Call::Name.new(
            name => RakuAST::Name.from-identifier('two-args'),
            args => RakuAST::ArgList.new(
                RakuAST::IntLiteral.new(5),
                RakuAST::IntLiteral.new(3),
            )
        )),
        2,
        'Can make a named call with two positional arguments';

my $target = -> $a, $b { $a - $b }
is-deeply
        EVAL(RakuAST::ApplyPostfix.new(
            operand => RakuAST::Var::Lexical.new('$target'),
            postfix => RakuAST::Call::Term.new(
                args => RakuAST::ArgList.new(
                    RakuAST::IntLiteral.new(9),
                    RakuAST::IntLiteral.new(4),
                )
            )
        )),
        5,
        'Can make a call on a term with two positional arguments';
