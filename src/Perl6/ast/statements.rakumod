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
        self.IMPL-WRAP-LIST($!statements)
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

    method is-empty() {
        nqp::elems($!statements) == 0 ?? True !! False
    }
}

# A semilist is a semicolon-separted list of statements, but used for the
# purpose of multi-dimensional array and hash indexing.
class RakuAST::SemiList is RakuAST::StatementList is RakuAST::ImplicitLookups {
    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Var::Lexical.new('&infix:<,>'),
        ])
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my @statements := nqp::getattr(self, RakuAST::StatementList, '$!statements');
        my int $n := nqp::elems(@statements);
        if $n == 1 {
            nqp::atpos(@statements, 0).IMPL-TO-QAST($context)
        }
        else {
            my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
            my $list := QAST::Op.new(
                :op('call'),
                :name(@lookups[0].resolution.lexical-name)
            );
            for @statements {
                $list.push($_.IMPL-TO-QAST($context));
            }
            $list
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

# An unless statement control.
class RakuAST::Statement::Unless is RakuAST::Statement is RakuAST::ImplicitLookups {
    has RakuAST::Expression $.condition;
    has RakuAST::Block $.body;

    method new(RakuAST::Expression :$condition, RakuAST::Block :$body) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Statement::Unless, '$!condition', $condition);
        nqp::bindattr($obj, RakuAST::Statement::Unless, '$!body', $body);
        $obj
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Simple.new('Empty'),
        ])
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
        QAST::Op.new(
            :op('unless'),
            $!condition.IMPL-TO-QAST($context),
            $!body.IMPL-TO-QAST($context, :immediate),
            @lookups[0].IMPL-TO-QAST($context)
        )
    }

    method visit-children(Code $visitor) {
        $visitor($!condition);
        $visitor($!body);
    }
}
