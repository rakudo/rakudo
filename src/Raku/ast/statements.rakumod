# Everything that can appear at statement level does RakuAST::Statement.
class RakuAST::Statement is RakuAST::Node {
}

# A list of statements, often appearing as the body of a block.
class RakuAST::StatementList is RakuAST::SinkPropagator {
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

    method propagate-sink(Bool $is-sunk, Bool :$has-block-parent) {
        # Sink all statements, with the possible exception of the last one (only if
        # we are not sunk).
        my @statements := $!statements;
        my int $i := 0;
        my int $n := nqp::elems(@statements);
        my int $wanted-statement := $is-sunk ?? -1 !! $n - 1;
        while $i < $n {
            my $cur-statement := @statements[$i];
            $cur-statement.apply-sink($i == $wanted-statement ?? False !! True);
            if $has-block-parent && nqp::istype($cur-statement, RakuAST::BlockStatementSensitive) {
                $cur-statement.mark-block-statement();
            }
            $i++;
        }
        Nil
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
class RakuAST::Statement::Expression is RakuAST::Statement is RakuAST::SinkPropagator {
    has RakuAST::Expression $.expression;

    method new($expression) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Statement::Expression, '$!expression', $expression);
        $obj
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        $!expression.IMPL-TO-QAST($context)
    }

    method propagate-sink(Bool $is-sunk) {
        $!expression.apply-sink($is-sunk) if $is-sunk;
    }

    method visit-children(Code $visitor) {
        $visitor($!expression);
    }
}

# An unless statement control.
class RakuAST::Statement::Unless is RakuAST::Statement is RakuAST::ImplicitLookups
                                 is RakuAST::SinkPropagator {
    has RakuAST::Expression $.condition;
    has RakuAST::Block $.body;

    method new(RakuAST::Expression :$condition!, RakuAST::Block :$body!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Statement::Unless, '$!condition', $condition);
        nqp::bindattr($obj, RakuAST::Statement::Unless, '$!body', $body);
        $obj
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Empty')),
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

    method propagate-sink(Bool $is-sunk) {
        $!condition.apply-sink(False);
        $!body.body.statement-list.apply-sink($is-sunk);
    }

    method visit-children(Code $visitor) {
        $visitor($!condition);
        $visitor($!body);
    }
}

# The base for various kinds of loop. Used directly for the loop construct,
# and subclassed with assorted defaults for while/until/repeat.
class RakuAST::Statement::Loop is RakuAST::Statement is RakuAST::ImplicitLookups
                               is RakuAST::Sinkable is RakuAST::SinkPropagator
                               is RakuAST::BlockStatementSensitive {
    # The setup expression for the loop.
    has RakuAST::Expression $.setup;

    # The condition of the loop.
    has RakuAST::Expression $.condition;

    # The body of the loop.
    has RakuAST::Block $.body;

    # The increment expression for the loop.
    has RakuAST::Expression $.increment;

    method new(RakuAST::Block :$body!, RakuAST::Expression :$condition,
               RakuAST::Expression :$setup, RakuAST::Expression :$increment) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Statement::Loop, '$!body', $body);
        nqp::bindattr($obj, RakuAST::Statement::Loop, '$!condition', $condition);
        nqp::bindattr($obj, RakuAST::Statement::Loop, '$!setup', $setup);
        nqp::bindattr($obj, RakuAST::Statement::Loop, '$!increment', $increment);
        $obj
    }

    # Is the condition negated?
    method negate() { False }

    # Is the loop always executed once?
    method repeat() { False }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Nil')),
        ])
    }

    method IMPL-DISCARD-RESULT() {
        self.is-block-statement || self.sunk
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        if self.IMPL-DISCARD-RESULT {
            # Select correct node type for the loop and produce it.
            my str $op := self.repeat
                ?? (self.negate ?? 'repeat_until' !! 'repeat_while')
                !! (self.negate ?? 'until' !! 'while');
            my $loop-qast := QAST::Op.new(
                :$op,
                $!condition ?? $!condition.IMPL-TO-QAST($context) !! QAST::IVal.new( :value(1) ),
                $!body.IMPL-TO-QAST($context, :immediate),
            );
            if $!increment {
                $loop-qast.push($!increment.IMPL-TO-QAST($context));
            }

            # Prepend setup code and evaluate to Nil if not sunk.
            my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
            my $wrapper := QAST::Stmt.new();
            if $!setup {
                $wrapper.push($!setup.IMPL-TO-QAST($context));
            }
            $wrapper.push($loop-qast);
            unless self.sunk {
                $wrapper.push(@lookups[0].IMPL-TO-QAST($context));
            }

            $wrapper
        }
        else {
            nqp::die('Compilation of lazy loops NYI')
        }
    }

    method propagate-sink(Bool $is-sunk) {
        $!condition.apply-sink(False);
        $!body.body.statement-list.apply-sink(self.IMPL-DISCARD-RESULT ?? True !! False);
        $!setup.apply-sink(True) if $!setup;
        $!increment.apply-sink(True) if $!increment;
    }

    method visit-children(Code $visitor) {
        $visitor($!condition);
        $visitor($!body);
        $visitor($!setup) if $!setup;
        $visitor($!increment) if $!increment;
    }
}

# A while loop.
class RakuAST::Statement::Loop::While is RakuAST::Statement::Loop {
}

# An until loop.
class RakuAST::Statement::Loop::Until is RakuAST::Statement::Loop {
    method negate() { True }
}

# A repeat while loop.
class RakuAST::Statement::Loop::RepeatWhile is RakuAST::Statement::Loop {
    method repeat() { True }
}

# A repeat until loop.
class RakuAST::Statement::Loop::RepeatUntil is RakuAST::Statement::Loop {
    method negate() { True }
    method repeat() { True }
}
