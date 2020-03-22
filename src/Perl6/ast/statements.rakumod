# Everything that can appear at statement level does RakuAST::Statement.
class RakuAST::Statement is RakuAST::Node {
}

# A list of statements, often appearing as the body of a block.
class RakuAST::StatementList is RakuAST::Node {
    has List $!statements;

    method new(*@statements) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::StatementList, '$!statements', @statements);
        $obj
    }

    method statements() {
        my $list := nqp::create(List);
        nqp::bindattr($list, List, '$!reified', $!statements);
        $list
    }

    method push(RakuAST::Statement $statement) {
        nqp::push($!statements, $statement);
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $stmts := QAST::Stmts.new;
        my @statements := $!statements;
        for @statements {
            $stmts.push($_.IMPL-TO-QAST($context));
        }
        $stmts
    }

    method visit-children(Code $visitor) {
        my @statements := $!statements;
        for @statements {
            $visitor($_);
        }
    }
}

# An expression statement is a statement consisting of the evaluation of an
# expression. It may have modifiers also, and the expression may consist of a
# single term.
class RakuAST::Statement::Expression is RakuAST::Statement {
    has RakuAST::Expression $.expression;

    method new($expression) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Statement::Expression, '$!expression', $expression);
        $obj
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        $!expression.IMPL-TO-QAST($context)
    }

    method visit-children(Code $visitor) {
        $visitor($!expression);
    }
}
