use MONKEY-SEE-NO-EVAL;
use Test;

plan 1;

is-deeply
        EVAL(RakuAST::FatArrow.new(
            key => 'answer',
            value => RakuAST::IntLiteral.new(42)
        )),
        (answer => 42),
        'Fat arrow syntax forms a Pair';
