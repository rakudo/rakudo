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
            if $has-block-parent {
                if nqp::istype($cur-statement, RakuAST::BlockStatementSensitive) {
                    $cur-statement.mark-block-statement();
                }
                elsif nqp::istype($cur-statement, RakuAST::Statement::Expression) {
                    my $expr := $cur-statement.expression;
                    if nqp::istype($expr, RakuAST::BlockStatementSensitive) {
                        $expr.mark-block-statement();
                    }
                }
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

# Mark out things that immediately consume their body, rather than needing it as
# a closure.
class RakuAST::IMPL::ImmediateBlockUser is RakuAST::Node {
}

# An if conditional, with optional elsif/orwith/else parts.
class RakuAST::Statement::If is RakuAST::Statement is RakuAST::ImplicitLookups
                             is RakuAST::SinkPropagator is RakuAST::IMPL::ImmediateBlockUser {
    has RakuAST::Expression $.condition;
    has RakuAST::Expression $.then;
    has Mu $!elsifs;
    has RakuAST::Block $.else;

    method new(RakuAST::Expression :$condition!, RakuAST::Expression :$then!,
               List :$elsifs, RakuAST::Block :$else) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Statement::If, '$!condition', $condition);
        nqp::bindattr($obj, RakuAST::Statement::If, '$!then', $then);
        nqp::bindattr($obj, RakuAST::Statement::If, '$!elsifs',
            self.IMPL-UNWRAP-LIST($elsifs // []));
        nqp::bindattr($obj, RakuAST::Statement::If, '$!else', $else // RakuAST::block);
        $obj
    }
    
    method elsifs() {
        self.IMPL-WRAP-LIST($!elsifs)
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST: $!else
            ?? []
            !! [RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Empty'))]
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        # Start with the else (or Empty).
        my $cur-end;
        if $!else {
            $cur-end := $!else.IMPL-TO-QAST($context, :immediate);
        }
        else {
            my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
            $cur-end := @lookups[0].IMPL-TO-QAST($context);
        }

        # Add the branches.
        my int $i := nqp::elems($!elsifs);
        while $i-- > 0 {
            my $branch := $!elsifs[$i];
            $cur-end := QAST::Op.new(
                :op($branch.IMPL-QAST-TYPE),
                $branch.condition.IMPL-TO-QAST($context),
                $branch.then.IMPL-TO-QAST($context, :immediate),
                $cur-end
            );
        }

        # Finally, the initial condition.
        QAST::Op.new(
            :op(self.IMPL-QAST-TYPE),
            $!condition.IMPL-TO-QAST($context),
            $!then.IMPL-TO-QAST($context, :immediate),
            $cur-end
        )
    }

    method IMPL-QAST-TYPE() { 'if' }

    method propagate-sink(Bool $is-sunk) {
        $!condition.apply-sink(False);
        $!then.body.apply-sink($is-sunk);
        for $!elsifs {
            $_.condition.apply-sink(False);
            $_.then.body.apply-sink($is-sunk);
        }
        if $!else {
            $!else.body.apply-sink($is-sunk);
        }
    }

    method visit-children(Code $visitor) {
        $visitor($!condition);
        $visitor($!then);
        for $!elsifs {
            $visitor($_.condition);
            $visitor($_.then);
        }
        if $!else {
            $visitor($!else);
        }
    }
}

# A with conditional, with optional elsif/orwith/else parts.
class RakuAST::Statement::With is RakuAST::Statement::If {
    method IMPL-QAST-TYPE() { 'with' }
}

# An elsif part. Not a standalone RakuAST node; can only be used inside of an If
# or With node.
class RakuAST::Statement::Elsif {
    has RakuAST::Expression $.condition;
    has RakuAST::Expression $.then;

    method new(RakuAST::Expression :$condition!, RakuAST::Expression :$then!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Statement::Elsif, '$!condition', $condition);
        nqp::bindattr($obj, RakuAST::Statement::Elsif, '$!then', $then);
        $obj
    }

    method IMPL-QAST-TYPE() { 'if' }
}

# An orwith part. Not a standalone RakuAST node; can only be used inside of an If
# or With node.
class RakuAST::Statement::Orwith is RakuAST::Statement::Elsif {
    method IMPL-QAST-TYPE() { 'with' }
}

# An unless statement control.
class RakuAST::Statement::Unless is RakuAST::Statement is RakuAST::ImplicitLookups
                                 is RakuAST::SinkPropagator is RakuAST::IMPL::ImmediateBlockUser {
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
        $!body.body.apply-sink($is-sunk);
    }

    method visit-children(Code $visitor) {
        $visitor($!condition);
        $visitor($!body);
    }
}

# A without statement control.
class RakuAST::Statement::Without is RakuAST::Statement is RakuAST::ImplicitLookups
                                  is RakuAST::SinkPropagator is RakuAST::IMPL::ImmediateBlockUser {
    has RakuAST::Expression $.condition;
    has RakuAST::Block $.body;

    method new(RakuAST::Expression :$condition!, RakuAST::Block :$body!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Statement::Without, '$!condition', $condition);
        nqp::bindattr($obj, RakuAST::Statement::Without, '$!body', $body);
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
            :op('without'),
            $!condition.IMPL-TO-QAST($context),
            $!body.IMPL-TO-QAST($context, :immediate),
            @lookups[0].IMPL-TO-QAST($context)
        )
    }

    method propagate-sink(Bool $is-sunk) {
        $!condition.apply-sink(False);
        $!body.body.apply-sink($is-sunk);
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
                               is RakuAST::BlockStatementSensitive
                               is RakuAST::IMPL::ImmediateBlockUser {
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
        $!body.body.apply-sink(self.IMPL-DISCARD-RESULT ?? True !! False);
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

# A for loop.
class RakuAST::Statement::For is RakuAST::Statement
                              is RakuAST::Sinkable is RakuAST::SinkPropagator
                              is RakuAST::BlockStatementSensitive {
    # The thing to iterate over.
    has RakuAST::Expression $.source;

    # The body of the loop.
    has RakuAST::Block $.body;

    # The mode of evaluation, (defaults to serial, may be race or hyper also).
    has str $.mode;

    method new(RakuAST::Expression :$source!, RakuAST::Block :$body!, str :$mode) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Statement::For, '$!source', $source);
        nqp::bindattr($obj, RakuAST::Statement::For, '$!body', $body);
        nqp::bindattr_s($obj, RakuAST::Statement::For, '$!mode', $mode || 'serial');
        $obj
    }

    method IMPL-DISCARD-RESULT() {
        self.is-block-statement || self.sunk
    }

    method propagate-sink(Bool $is-sunk) {
        $!source.apply-sink(False);
        $!body.body.apply-sink(self.IMPL-DISCARD-RESULT ?? True !! False);
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        # TODO various optimized forms are possible here
        self.IMPL-TO-QAST-GENERAL($context)
    }

    # The most general, least optimized, form for a `for` loop, which
    # delegates to `map`.
    method IMPL-TO-QAST-GENERAL(RakuAST::IMPL::QASTContext $context) {
        # Figure out the execution mode modifiers to apply.
        my str $mode := $!mode;
        my str $after-mode := '';
        if $mode eq 'lazy' {
            $mode := 'serial';
            $after-mode := 'lazy';
        }
        else {
            $after-mode := self.IMPL-DISCARD-RESULT ?? 'sink' !! 'eager';
        }

        # Bind the source into a temporary.
        my $for-list-name := QAST::Node.unique('for-list');
        my $bind-source := QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :name($for-list-name), :scope('local'), :decl('var') ),
            $!source.IMPL-TO-QAST($context)
        );

        # Produce the map call.
        my $body-qast := $!body.IMPL-TO-QAST($context);
        my $map-call := QAST::Op.new(
            :op('if'),
            QAST::Op.new( :op('iscont'), QAST::Var.new( :name($for-list-name), :scope('local') ) ),
            QAST::Op.new(
                :op<callmethod>, :name<map>,
                QAST::Var.new( :name($for-list-name), :scope('local') ),
                $body-qast,
                QAST::IVal.new( :value(1), :named('item') )
            ),
            QAST::Op.new(
                :op<callmethod>, :name<map>,
                QAST::Op.new(
                    :op<callmethod>, :name($mode),
                    QAST::Var.new( :name($for-list-name), :scope('local') )
                ),
                $body-qast
            )
        );

        # Apply after mode to the map result, and we're done.
        QAST::Stmts.new(
            $bind-source,
            QAST::Op.new( :op<callmethod>, :name($after-mode), $map-call )
        )
    }

    method visit-children(Code $visitor) {
        $visitor($!source);
        $visitor($!body);
    }
}
