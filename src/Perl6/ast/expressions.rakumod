# Everything that can appear as an expression does RakuAST::Expression.
class RakuAST::Expression is RakuAST::Node {
}

# Everything that is a kind of term does RakuAST::Term.
class RakuAST::Term is RakuAST::Expression {
}
