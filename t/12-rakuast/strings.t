use MONKEY-SEE-NO-EVAL;
use Test;

plan 13;

is-deeply  # "hello"
    EVAL(RakuAST::QuotedString.new(
        :segments[RakuAST::StrLiteral.new('hello')]
    )),
    'hello',
    'One-part quoted string with literal piece comes out as the right thing';

my $str = 'hello,';
is-deeply  # "$str world"
    EVAL(RakuAST::QuotedString.new(:segments[
        RakuAST::Var::Lexical.new('$str'),
        RakuAST::StrLiteral.new(' world')
    ])),
    'hello, world',
    'Quoted string with interpolated string variable works';

my $int = 42;
is-deeply  # "$int is the answer"
    EVAL(RakuAST::QuotedString.new(:segments[
        RakuAST::Var::Lexical.new('$int'),
        RakuAST::StrLiteral.new(' is the answer')
    ])),
    '42 is the answer',
    'Quoted string with interpolated integer variable works';

is-deeply  # "$int"
    EVAL(RakuAST::QuotedString.new(:segments[
        RakuAST::Var::Lexical.new('$int'),
    ])),
    '42',
    'Quoted string consisting of only interpolated integer variable gives a string';

is-deeply  # "The answer is $int of course"
    EVAL(RakuAST::QuotedString.new(:segments[
        RakuAST::StrLiteral.new('The answer is '),
        RakuAST::Var::Lexical.new('$int'),
        RakuAST::StrLiteral.new(' of course!')
    ])),
    'The answer is 42 of course!',
    'Quoted string with 3 parts works';

my $bv = 'interpolated';
is-deeply  # "An { $bv } block"
    EVAL(RakuAST::QuotedString.new(:segments[
        RakuAST::StrLiteral.new('An '),
        RakuAST::Block.new(
            body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
                RakuAST::Statement::Expression.new(
                    expression => RakuAST::Var::Lexical.new('$bv')
                )
            ))
        ),
        RakuAST::StrLiteral.new(' block')
    ])),
    'An interpolated block',
    'Quoted string involving an interpolated block';

is-deeply  # qqw/ foo bar 42 /
    EVAL(RakuAST::QuotedString.new(
        :segments[RakuAST::StrLiteral.new('foo bar 42')],
        :processors['words']
    )),
    ("foo", "bar", "42"),
    'Using the words processor splits a single literal string into words';

{
    my $stuff = 'r baz';
    is-deeply  # qqw/ ba$stuff 66 /
        EVAL(RakuAST::QuotedString.new(
            :segments[
                RakuAST::StrLiteral.new('ba'),
                RakuAST::Var::Lexical.new('$stuff'),
                RakuAST::StrLiteral.new(' 66')
            ],
            :processors['words']
        )),
        ("bar", "baz", "66"),
        'Words processor applied to a quoted string with interpolation works';
}

is-deeply  # <42>
    EVAL(RakuAST::QuotedString.new(
        :segments[RakuAST::StrLiteral.new('42')],
        :processors['val']
    )),
    val("42"),
    'Using the val processor alone gives expected result';

{
    my $end = '6';
    is-deeply  #           XXX  not sure how to express this
        EVAL(RakuAST::QuotedString.new(
            :segments[
                RakuAST::StrLiteral.new('4'),
                RakuAST::Var::Lexical.new('$end')
            ],
            :processors['val']
        )),
        val("46"),
        'Using the val processor alone with interpolation gives expected result';
}

is-deeply  # < foo bar 42 >
    EVAL(RakuAST::QuotedString.new(
        :segments[RakuAST::StrLiteral.new('foo bar 42')],
        :processors['words', 'val']
    )),
    ("foo", "bar", val("42")),
    'Using the words and val processor one after the other gives expected result';

{
    my $stuff = 'r baz';
    is-deeply  #           XXX  not sure how to express this
        EVAL(RakuAST::QuotedString.new(
            :segments[
                RakuAST::StrLiteral.new('ba'),
                RakuAST::Var::Lexical.new('$stuff'),
                RakuAST::StrLiteral.new(' 66')
            ],
            :processors['words', 'val']
        )),
        ("bar", "baz", val("66")),
        'Words processor applied to a quoted string with interpolation works';
}

is-deeply  # qx/ echo 123 /
    EVAL(RakuAST::QuotedString.new(
        :segments[RakuAST::StrLiteral.new('echo 123')],
        :processors['exec']
    )),
    "123\n",
    'Using the exec processor alone gives expected result';

# vim: expandtab shiftwidth=4
