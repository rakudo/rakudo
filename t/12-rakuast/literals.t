use MONKEY-SEE-NO-EVAL;
use Test;

plan 8;

# NOTE: this only works when there are no variable references
sub ast-ok(
  RakuAST::Node:D $ast,
  Mu $value,
  $comment = "testing for: $value.raku()"
) {
    subtest $comment => {
        plan 2;
        is-deeply EVAL($ast), $value;
        is-deeply EVAL(try $ast.DEPARSE), $value;
    }
}

# 42
ast-ok RakuAST::IntLiteral.new(42), 42,
  'RakuAST::IntLiteral with constant';

{
    my $a = 100;
    my $b = 6;

    # 106
    ast-ok RakuAST::IntLiteral.new($a + $b), 100 + 6,
      'RakuAST::IntLiteral with calculated value';
}

# 4e2
ast-ok RakuAST::NumLiteral.new(4e2), 4e2,
  'RakuAST::NumLiteral with constant';

{
    my $a = 2e4;
    my $b = 5e1;

    # 20050e0
    ast-ok RakuAST::NumLiteral.new($a + $b), 2e4 + 5e1,
      'RakuAST::NumLiteral with calculated value';
}

# 1.5
ast-ok RakuAST::RatLiteral.new(1.5), 1.5,
  'RakuAST::RatLiteral with constant';

{
    my $a = 4.2;
    my $b = 0.3;

    # 4.5
    ast-ok RakuAST::RatLiteral.new($a + $b), 4.2 + 0.3,
      'RakuAST::RatLiteral with calculated value';
}

# v4.2
ast-ok RakuAST::VersionLiteral.new(v4.2), v4.2,
  'RakuAST::VersionLiteral with constant';
# v6.66
ast-ok RakuAST::VersionLiteral.new(Version.new('6.66')), v6.66,
  'RakuAST::VersionLiteral with constructed version';

# vim: expandtab shiftwidth=4
