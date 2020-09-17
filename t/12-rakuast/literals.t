use MONKEY-SEE-NO-EVAL;
use Test;

plan 8;

subtest 'RakuAST::IntLiteral with constant' => {
    # 42
    my $ast := RakuAST::IntLiteral.new(42);
    is-deeply EVAL($ast), 42;
    is-deeply $ast.DEPARSE, "42";
}

subtest 'RakuAST::IntLiteral with calculated value' => {
    my $a = 100;
    my $b = 6;

    # 106
    my $ast := RakuAST::IntLiteral.new($a + $b);
    is-deeply EVAL($ast), 106;
    is-deeply $ast.DEPARSE, "106";
}

subtest 'RakuAST::NumLiteral with constant' => {
    # 4e2
    my $ast := RakuAST::NumLiteral.new(4e2);
    is-deeply EVAL($ast), 4e2;
    is-deeply $ast.DEPARSE, "400e0";
}

{
    my $a = 2e4;
    my $b = 5e1;
    is-deeply  # 20050e0
        EVAL(RakuAST::NumLiteral.new($a + $b)), 2e4 + 5e1,
            'RakuAST::NumLiteral with calculated value';
}

is-deeply  # 1.5
    EVAL(RakuAST::RatLiteral.new(1.5)), 1.5,
        'RakuAST::RatLiteral with constant';

{
    my $a = 4.2;
    my $b = 0.3;
    is-deeply  # 4.5
        EVAL(RakuAST::RatLiteral.new($a + $b)), 4.5,
            'RakuAST::RatLiteral with calculated value';
}

is-deeply  # v4.2
    EVAL(RakuAST::VersionLiteral.new(v4.2)), v4.2,
        'RakuAST::VersionLiteral with constant';
is-deeply  # v6.66
    EVAL(RakuAST::VersionLiteral.new(Version.new('6.66'))), v6.66,
        'RakuAST::VersionLiteral with constructed version';
