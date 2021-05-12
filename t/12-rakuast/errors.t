use Test;

plan 1;

throws-like { EVAL(RakuAST::Term::Self.new) },
    X::Syntax::Self::WithoutObject;
