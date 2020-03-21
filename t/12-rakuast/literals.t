use MONKEY-SEE-NO-EVAL;
use Test;

plan 8;

is-deeply EVAL(RakuAST::IntLiteral.new(42)), 42,
        'RakuAST::IntLiteral with constant';

{
    my $a = 100;
    my $b = 6;
    is-deeply EVAL(RakuAST::IntLiteral.new($a + $b)), 106,
            'RakuAST::IntLiteral with calculated value';
}

is-deeply EVAL(RakuAST::NumLiteral.new(4e2)), 4e2,
        'RakuAST::NumLiteral with constant';

{
    my $a = 2e4;
    my $b = 5e1;
    is-deeply EVAL(RakuAST::NumLiteral.new($a + $b)), 2e4 + 5e1,
            'RakuAST::NumLiteral with calculated value';
}

is-deeply EVAL(RakuAST::RatLiteral.new(1.5)), 1.5,
        'RakuAST::RatLiteral with constant';

{
    my $a = 4.2;
    my $b = 0.3;
    is-deeply EVAL(RakuAST::RatLiteral.new($a + $b)), 4.5,
            'RakuAST::RatLiteral with calculated value';
}

is-deeply EVAL(RakuAST::VersionLiteral.new(v4.2)), v4.2,
        'RakuAST::VersionLiteral with constant';
is-deeply EVAL(RakuAST::VersionLiteral.new(Version.new('6.66'))), v6.66,
        'RakuAST::VersionLiteral with constructed version';
