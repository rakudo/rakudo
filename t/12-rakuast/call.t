use MONKEY-SEE-NO-EVAL;
use Test;

plan 17;

sub no-args() {
    444
}
is-deeply  # no-args()
        EVAL(RakuAST::Call::Name.new(
            name => RakuAST::Name.from-identifier('no-args')
        )),
        444,
        'Can make a named call with no arguments';

sub one-arg($x) {
    9 * $x
}
is-deeply  # one-arg(5)
        EVAL(RakuAST::Call::Name.new(
            name => RakuAST::Name.from-identifier('one-arg'),
            args => RakuAST::ArgList.new(RakuAST::IntLiteral.new(5))
        )),
        45,
        'Can make a named call with one positional argument';

sub two-args($x, $y) {
    $x - $y
}
is-deeply  # two-args(5, 3)
        EVAL(RakuAST::Call::Name.new(
            name => RakuAST::Name.from-identifier('two-args'),
            args => RakuAST::ArgList.new(
                RakuAST::IntLiteral.new(5),
                RakuAST::IntLiteral.new(3),
            )
        )),
        2,
        'Can make a named call with two positional arguments';

sub two-named(:$n1, :$n2) {
    $n1 / $n2
}
is-deeply  # two-named(n1 => 200, n2 => 4)
        EVAL(RakuAST::Call::Name.new(
            name => RakuAST::Name.from-identifier('two-named'),
            args => RakuAST::ArgList.new(
                RakuAST::FatArrow.new(
                    key => 'n1',
                    value => RakuAST::IntLiteral.new(200)
                ),
                RakuAST::FatArrow.new(
                    key => 'n2',
                    value => RakuAST::IntLiteral.new(4)
                )
            )
        )),
        50.0,
        'Can make a named call with two named arguments';

is-deeply  # two-named(n1 => 200, n2 => 4, n1 => 400)
        EVAL(RakuAST::Call::Name.new(
            name => RakuAST::Name.from-identifier('two-named'),
            args => RakuAST::ArgList.new(
                RakuAST::FatArrow.new(
                    key => 'n1',
                    value => RakuAST::IntLiteral.new(200)
                ),
                RakuAST::FatArrow.new(
                    key => 'n2',
                    value => RakuAST::IntLiteral.new(4)
                ),
                RakuAST::FatArrow.new(
                    key => 'n1',
                    value => RakuAST::IntLiteral.new(400)
                ),
            )
        )),
        100.0,
        'Duplicated named arguments are correctly handled';

my $target = -> $a, $b { $a - $b }
is-deeply  # $target(9, 4)
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

class TestTarget {
    my $.route = 66;
    method subtract($x, $y) { $x - $y }
}
is-deeply  # TestTarget.route
        EVAL(RakuAST::ApplyPostfix.new(
            operand => RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('TestTarget')),
            postfix => RakuAST::Call::Method.new(
                name => RakuAST::Name.from-identifier('route')
            )
        )),
        66,
        'Can make a call on a method without arguments';
is-deeply  # TestTarget.subtract(14, 6)
        EVAL(RakuAST::ApplyPostfix.new(
            operand => RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('TestTarget')),
            postfix => RakuAST::Call::Method.new(
                name => RakuAST::Name.from-identifier('subtract'),
                args => RakuAST::ArgList.new(
                    RakuAST::IntLiteral.new(14),
                    RakuAST::IntLiteral.new(6),
                )
            )
        )),
        8,
        'Can make a call on a method with positional arguments';

is-deeply  # 42.WHAT
        EVAL(RakuAST::ApplyPostfix.new(
            operand => RakuAST::IntLiteral.new(42),
            postfix => RakuAST::Call::Method.new(
                name => RakuAST::Name.from-identifier('WHAT')
            )
        )),
        Int,
        'Method call WHAT compiles into MOP primitive';
is-deeply  # 42.HOW
        EVAL(RakuAST::ApplyPostfix.new(
            operand => RakuAST::IntLiteral.new(42),
            postfix => RakuAST::Call::Method.new(
                name => RakuAST::Name.from-identifier('HOW')
            )
        )),
        Int.HOW,
        'Method call HOW compiles into MOP primitive';
isa-ok     # 42.WHO
        EVAL(RakuAST::ApplyPostfix.new(
            operand => RakuAST::IntLiteral.new(42),
            postfix => RakuAST::Call::Method.new(
                name => RakuAST::Name.from-identifier('WHO')
            )
        )),
        Stash,
        'Method call WHO compiles into MOP primitive';
is-deeply  # 42.DEFINITE
        EVAL(RakuAST::ApplyPostfix.new(
            operand => RakuAST::IntLiteral.new(42),
            postfix => RakuAST::Call::Method.new(
                name => RakuAST::Name.from-identifier('DEFINITE')
            )
        )),
        True,
        'Method call DEFINITE compiles into MOP primitive';
is-deeply  # 42.REPR
        EVAL(RakuAST::ApplyPostfix.new(
            operand => RakuAST::IntLiteral.new(42),
            postfix => RakuAST::Call::Method.new(
                name => RakuAST::Name.from-identifier('REPR')
            )
        )),
        'P6opaque',
        'Method call REPR compiles into MOP primitive';

{
    my @args;
    is-deeply  # no-args(|@args)
            EVAL(RakuAST::Call::Name.new(
                name => RakuAST::Name.from-identifier('no-args'),
                args => RakuAST::ArgList.new(
                    RakuAST::ApplyPrefix.new(
                        prefix => RakuAST::Prefix.new('|'),
                        operand => RakuAST::Var::Lexical.new('@args')
                    )
                )
            )),
            444,
            'Can make a call that flattens arguments (empty flattening list)';

    @args = 95, 40;
    is-deeply  # two-args(|@args)
            EVAL(RakuAST::Call::Name.new(
                name => RakuAST::Name.from-identifier('two-args'),
                args => RakuAST::ArgList.new(
                    RakuAST::ApplyPrefix.new(
                        prefix => RakuAST::Prefix.new('|'),
                        operand => RakuAST::Var::Lexical.new('@args')
                    )
                )
            )),
            55,
            'Can make a call that flattens two positional arguments';

    my %args;
    is-deeply  # no-args(|%args)
            EVAL(RakuAST::Call::Name.new(
                name => RakuAST::Name.from-identifier('no-args'),
                args => RakuAST::ArgList.new(
                    RakuAST::ApplyPrefix.new(
                        prefix => RakuAST::Prefix.new('|'),
                        operand => RakuAST::Var::Lexical.new('%args')
                    )
                )
            )),
            444,
            'Can make a call that flattens arguments (empty flattening hash)';

    %args<n1 n2> = 60, 12;
    is-deeply  # two-named(|%args)
            EVAL(RakuAST::Call::Name.new(
                name => RakuAST::Name.from-identifier('two-named'),
                args => RakuAST::ArgList.new(
                    RakuAST::ApplyPrefix.new(
                        prefix => RakuAST::Prefix.new('|'),
                        operand => RakuAST::Var::Lexical.new('%args')
                    )
                )
            )),
            5.0,
            'Can make a call that flattens two named arguments';
}
