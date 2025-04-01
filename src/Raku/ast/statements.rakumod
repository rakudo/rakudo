# Block or statement, used in statement prefixes which can take either a
# block or a statement
class RakuAST::Blorst
  is RakuAST::Node
{
    method as-block() {
        nqp::die("RakuAST::Blorst classes must define 'as-block'. " ~ self.HOW.name(self) ~ " does not.")
    }
}


# Something that can be the target of a contextualizer.
class RakuAST::Contextualizable
  is RakuAST::Node {}

# A label, which can be placed on a statement.
class RakuAST::Label
  is RakuAST::Declaration
  is RakuAST::ImplicitLookups
  is RakuAST::Meta
{
    has str $.name;

    method new(str $name) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Label, '$!name', $name);
        $obj
    }

    method default-scope() { 'my' }

    method allowed-scopes() { ['my'] }

    method lexical-name() { $!name }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Label')),
        ])
    }

    method PRODUCE-META-OBJECT() {
        my $label-type :=
          self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].resolution.compile-time-value;
        # TODO line, prematch, postmatch
        $label-type.new(:name($!name), :line(0), :prematch(''), :postmatch(''))
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        QAST::Var.new(
            :scope('lexical'), :decl('static'), :name($!name),
            :value(self.meta-object)
        )
    }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context, Mu :$rvalue) {
        my $label := self.meta-object;
        $context.ensure-sc($label);
        QAST::WVal.new( :value($label) )
    }
}

# Everything that can appear at statement level does RakuAST::Statement.
class RakuAST::Statement
  is RakuAST::Blorst
{
    has Mu  $.labels;
    has int $.trace;
    has int $.statement-id;

    # this attribute is only for collecting doc blocks in the Raku grammar
    # so that they can be inserted into the appropriate statement list at
    # the appropriate time.  It has no meaning anywhere else.
    has Mu $!doc-blocks;

    method set-labels(List $labels) {
        nqp::bindattr(self, RakuAST::Statement, '$!labels',
          $labels ?? self.IMPL-UNWRAP-LIST($labels) !! []);
        Nil
    }
    method add-label(RakuAST::Label $label) {
        nqp::push($!labels, $label);
        Nil
    }
    method labels() { self.IMPL-WRAP-LIST($!labels) }

    # Attach any collected RakuDoc blocks to this statement
    method attach-doc-blocks() {
        my @collected := $*DOC-BLOCKS-COLLECTED;
        if nqp::elems(@collected) {
            nqp::bindattr(self, RakuAST::Statement, '$!doc-blocks', @collected);
            $*DOC-BLOCKS-COLLECTED := [];
        }
    }

    # Add any RakuDoc blocks attached to this statement to the given
    # statementlisty object, and add the statement itself to it as well.
    method add-to-statements($statements) {
        if $!doc-blocks {
            for $!doc-blocks {
                $statements.add-doc-block($_);
            }
        }
        $statements.add-statement(self);
        Nil
    }

    method set-statement-id(int $statement-id) {
        nqp::bindattr_i(self, RakuAST::Statement, '$!statement-id', $statement-id);
    }

    method set-trace(Bool $trace) {
        nqp::bindattr_i(self, RakuAST::Statement, '$!trace', $trace ?? 1 !! 0);
    }

    method visit-labels(Code $visitor) {
        for $!labels {
            $visitor($_);
        }
        Nil
    }

    method IMPL-DISCARD-RESULT() {
        False
    }

    method IMPL-WRAP-WHEN-OR-DEFAULT(RakuAST::IMPL::QASTContext $context, Mu $qast) {
        my $succeed-qast := QAST::Op.new( :op('call'), :name('&succeed'), $qast );
        QAST::Op.new(
            :op('handle'),
            $succeed-qast,
            'PROCEED',
            QAST::Op.new(
                :op('getpayload'),
                QAST::Op.new( :op('exception') )
            )
        )
    }

    method as-block() {
        RakuAST::Block.new:
            :body(RakuAST::Blockoid.new:
                RakuAST::StatementList.new: self)
    }
}

# Some nodes cause their child nodes to gain an implicit, or even required,
# topic. They can supply a callback to do that during resolution.
class RakuAST::ImplicitBlockSemanticsProvider
  is RakuAST::Node
{
    method apply-implicit-block-semantics() {
        nqp::die('apply-implicit-block-semantics not implemented by ' ~ self.HOW.name(self));
    }
}

class RakuAST::ForLoopImplementation
  is RakuAST::Node
{
    method IMPL-FOR-QAST(RakuAST::IMPL::QASTContext $context, str $mode,
            str $after-mode, Mu $source-qast, Mu $body-qast, RakuAST::Label $label?) {
        # TODO various optimized forms are possible here
        self.IMPL-TO-QAST-GENERAL($context, $mode, $after-mode, $source-qast,
            $body-qast, $label // RakuAST::Label)
    }

    # The most general, least optimized, form for a `for` loop, which
    # delegates to `map`.
    method IMPL-TO-QAST-GENERAL(RakuAST::IMPL::QASTContext $context, str $mode,
            str $after-mode, Mu $source-qast, Mu $body-qast, RakuAST::Label $label) {
        # Bind the source into a temporary.
        my $for-list-name := QAST::Node.unique('for-list');
        my $bind-source := QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :name($for-list-name), :scope('local'), :decl('var') ),
            $source-qast
        );

        # Produce the map call.
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
        if $label {
            my $label-qast := $label.IMPL-LOOKUP-QAST($context);
            $label-qast.named('label');
            $map-call[1].push($label-qast);
            $map-call[2].push($label-qast);
        }

        # Apply after mode to the map result, and we're done.
        self.IMPL-SET-NODE(
            QAST::Stmts.new(
                $bind-source,
                QAST::Op.new( :op<callmethod>, :name($after-mode), $map-call )), :key)
    }
}

# A list of statements, often appearing as the body of a block.
class RakuAST::StatementList
  is RakuAST::SinkPropagator
  is RakuAST::ImplicitLookups
  is RakuAST::CheckTime
{
    has List $!statements;
    has int $!is-sunk;
    has Mu   $.code-statements;  # internal only: actual code statements

    method new(*@statements, Bool :$trace) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::StatementList, '$!statements',@statements);
        nqp::bindattr_i($obj, RakuAST::StatementList, '$!is-sunk', 0);

        # make sure any code statements are known in the internal code-only list
        my @code;
        nqp::bindattr($obj, RakuAST::StatementList, '$!code-statements', @code);
        for @statements {
            unless nqp::istype($_,RakuAST::Doc::Block) {
                nqp::push(@code,$_);
            }
        }

        $obj
    }

    method add-doc-block(RakuAST::Statement $doc-block) {
        nqp::push($!statements, $doc-block);
    }
    method insert-doc-block(int $i, RakuAST::Node $block) {
        nqp::splice($!statements,nqp::list($block),$i,0);
    }
    method add-statement(RakuAST::Statement $statement) {
        nqp::push(     $!statements, $statement);
        nqp::push($!code-statements, $statement);
    }
    method unshift-statement(RakuAST::Statement $statement) {
        nqp::unshift(     $!statements, $statement);
        nqp::unshift($!code-statements, $statement);
    }
    method statements() {
        self.IMPL-WRAP-LIST($!statements)
    }

    method IMPL-NON-EMPTY-CODE-STATEMENTS() {
        my @stmts;
        for self.code-statements {
            nqp::push(@stmts, $_) unless nqp::istype($_, RakuAST::Statement::Empty);
        }
        @stmts
    }

    # Return whether there are any whenevers
    method any-whenevers() {
        for $!statements {
            return True
              if nqp::istype($_,RakuAST::Statement::Whenever)
              || (nqp::istype($_,RakuAST::Block) && $_.any-whenevers)
        }
        False
    }

    # Return whether there is a single whenever and it's the last one
    method single-last-whenever() {
        if nqp::isconcrete(self) {
            my int $i := +$!statements;
            if $i {
                my $last := $!statements[--$i];
                if nqp::istype($last,RakuAST::Statement::Whenever) {
                    return False                      # embedded whenevers
                      if $last.body.any-whenevers;

                    while $i-- {
                        return False if nqp::istype(  # found another here!
                          $!statements[$i],
                          RakuAST::Statement::Whenever
                        );
                    }
                    return True;                      # no other found
                }
            }
        }
        False
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $catch-seen := 0;
        my $control-seen := 0;
        for $!code-statements {
            if nqp::istype($_, RakuAST::Statement::Catch) {
                if $catch-seen {
                    self.add-sorry:
                      $resolver.build-exception: 'X::Phaser::Multiple', block => 'CATCH';
                }
                $catch-seen++;
            }
            if nqp::istype($_, RakuAST::Statement::Control) {
                if $control-seen {
                    self.add-sorry:
                      $resolver.build-exception: 'X::Phaser::Multiple', block => 'CONTROL';
                }
                $control-seen++;
            }
        }
        True
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Blob')),
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Nil')),
        ])
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context, :$immediate) {
        my $stmts := self.IMPL-SET-NODE(QAST::Stmts.new, :key);
        my @statements := self.IMPL-NON-EMPTY-CODE-STATEMENTS;
        my $Nil := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[1];
        for @statements {
            my $qast := $_.IMPL-TO-QAST($context);
            my $stmt-orig := $_.origin;
            if nqp::isconcrete($stmt-orig) && $stmt-orig.is-key {
                $qast := QAST::Stmt.new(
                    :node($stmt-orig.as-match),
                    $qast
                );
            }
            if $_.trace && nqp::isconcrete($stmt-orig) && !$*CU.precompilation-mode {
                my $Blob :=
                  self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].compile-time-value;
                $context.ensure-sc($Blob);
                my $line := $stmt-orig.source.original-line($stmt-orig.from);
                my $file := $stmt-orig.source.original-file;
                my $code := nqp::substr($stmt-orig.source.orig, $stmt-orig.from, $stmt-orig.to - $stmt-orig.from);
                if $code ne 'use trace' {
                    my $id := $_.statement-id;
                    $stmts.push(QAST::Op.new(
                        :op<writefh>,
                        QAST::Op.new(:op<getstderr>),
                        QAST::Op.new(
                            :op('encode'),
                            QAST::SVal.new(:value("$id ($file line $line)\n$code\n")),
                            QAST::SVal.new(:value('utf8')),
                            QAST::Op.new(
                                :op('callmethod'), :name('new'),
                                QAST::WVal.new( :value($Blob) )
                            )
                        )
                    ));
                }
            }
            $stmts.push($qast);
        }
        if !nqp::elems(@statements) {
            $stmts.push($Nil.IMPL-TO-QAST($context));
        }
        else {
            my $last-stmt := @statements[nqp::elems(@statements) - 1];
            if $!is-sunk || $last-stmt.IMPL-DISCARD-RESULT {
                $stmts.push($Nil.IMPL-TO-QAST($context));
            }
            else {
                my $last := $stmts[nqp::elems($stmts) - 1];
                $last.final(1);
                $stmts.returns($last.returns);
            }
        }
        $stmts
    }

    method propagate-sink(Bool $is-sunk, Bool :$has-block-parent) {
        # Sink all statements, with the possible exception of the last one (only if
        # we are not sunk).
        my @statements := self.IMPL-NON-EMPTY-CODE-STATEMENTS;
        my int $i;
        my int $n := nqp::elems(@statements);
        my int $wanted-statement := $is-sunk ?? -1 !! $n - 1;
        while $i < $n {
            my $cur-statement := @statements[$i];
            $cur-statement.apply-sink($i == $wanted-statement ?? False !! True);
            if $has-block-parent && nqp::istype($cur-statement, RakuAST::BlockStatementSensitive) {
                $cur-statement.mark-block-statement();
            }
            ++$i;
        }
        nqp::bindattr_i(self, RakuAST::StatementList, '$!is-sunk', $is-sunk ?? 1 !! 0);
        Nil
    }

    method visit-children(Code $visitor) {
        for $!statements {
            $visitor($_);
        }
    }

    method is-empty() {
        nqp::elems(self.code-statements) == 0 ?? True !! False
    }

    method has-compile-time-value() {
        self.IMPL-IS-SINGLE-EXPRESSION && $!statements[0].has-compile-time-value
    }

    method maybe-compile-time-value() {
        $!statements[0].maybe-compile-time-value
    }

    method IMPL-IS-SINGLE-EXPRESSION {
        nqp::elems($!statements) == 1
        && nqp::istype($!statements[0], RakuAST::Statement::Expression)
    }

    method IMPL-CAN-INTERPRET() {
        for self.code-statements {
            return False unless $_.IMPL-CAN-INTERPRET;
        }
        True
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        my $result;
        my @statements := self.code-statements;
        for @statements {
            $result := $_.IMPL-INTERPRET($ctx);
        }
        $!is-sunk
          || !@statements
          || @statements[nqp::elems(@statements) - 1].IMPL-DISCARD-RESULT
            ?? self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[1].compile-time-value
            !! $result
    }
}

# A semilist is a semicolon-separted list of statements, but used for the
# purpose of multi-dimensional array and hash indexing.
class RakuAST::SemiList
  is RakuAST::StatementList
  is RakuAST::ImplicitLookups
{
    method propagate-sink(Bool $is-sunk, Bool :$has-block-parent) {
        # Sink all statements only if the whole list is sunk
        # If not, we're gonna need all values as they are used for creating an indexing list.
        my @statements := self.code-statements;
        my int $i;
        my int $n := nqp::elems(@statements);
        while $i < $n {
            my $cur-statement := @statements[$i];
            $cur-statement.apply-sink($is-sunk);
            ++$i;
        }
        nqp::bindattr_i(self, RakuAST::StatementList, '$!is-sunk', $is-sunk ?? 1 !! 0);
        Nil
    }

    # Tries to get a literal value for a quoted string. If that is not
    # possible, returns Nil.
    method literal-value() {
        if self.IMPL-IS-SINGLE-EXPRESSION && self.IMPL-CAN-INTERPRET {
            return self.IMPL-INTERPRET(RakuAST::IMPL::InterpContext.new);
        }
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Var::Lexical.new('&infix:<,>'),
        ])
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my @statements := self.code-statements;
        my int $n := nqp::elems(@statements);
        if $n == 0 && $*COMPILING_CORE_SETTING == 1 {
            my $list := nqp::create(List);
            $context.ensure-sc($list);
            QAST::WVal.new(:value($list));
        }
        elsif $n == 1 {
            nqp::atpos(@statements, 0).IMPL-TO-QAST($context)
        }
        else {
            my $comma := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0];
            my $name := $comma.is-resolved ?? $comma.resolution.lexical-name !! '&infix:<,>';
            my $list := QAST::Op.new(:op('call'), :$name);
            for @statements {
                $list.push($_.IMPL-TO-QAST($context));
            }
            $list
        }
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        my @statements := self.code-statements;
        my int $n := nqp::elems(@statements);
        if $n == 1 {
            nqp::atpos(@statements, 0).IMPL-INTERPRET($ctx)
        }
        else {
            my @list;
            for @statements {
                nqp::push(@list, $_.IMPL-INTERPRET($ctx));
            }
            self.IMPL-WRAP-LIST(@list)
        }
    }
}

# A statement sequence is a semicolon-separated list of statements, used as
# the target for a contextualizer. Like a normal statement list, it puts all
# but its last statement in sink context and evaluates to the value of the
# final statement. However, if empty it evaluates instead to an empty list.
class RakuAST::StatementSequence
  is RakuAST::StatementList
  is RakuAST::ImplicitLookups
  is RakuAST::Contextualizable
{
    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Var::Lexical.new('&infix:<,>'),
        ])
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my @statements;
        for self.code-statements {
            @statements.push($_)
              unless nqp::istype($_,RakuAST::Statement::Empty);
        }

        my int $n := nqp::elems(@statements);
        if $n == 1 {
            nqp::atpos(@statements, 0).IMPL-TO-QAST($context)
        }
        elsif $n == 0 {
            my $name :=
              self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].resolution.lexical-name;
            QAST::Op.new(:op('call'), :$name);
        }
        else {
            my $stmts := self.IMPL-SET-NODE(QAST::Stmts.new, :key);
            for @statements {
                $stmts.push($_.IMPL-TO-QAST($context));
            }
            $stmts
        }
    }
}

# Done by all classes that always produce Nil
class RakuAST::ProducesNil
  is RakuAST::ImplicitLookups
{
    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Nil')),
        ])
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].IMPL-TO-QAST($context)
    }
}

# Any empty statement. Retained because it can have a semantic effect (for
# example, in block vs. hash distinction with a leading `;`).
class RakuAST::Statement::Empty
  is RakuAST::Statement
  is RakuAST::ProducesNil
{
    method new(List :$labels) {
        my $obj := nqp::create(self);
        $obj.set-labels($labels);
        $obj
    }

    method visit-children(Code $visitor) {
        self.visit-labels($visitor);
    }
}

# An expression statement is a statement consisting of the evaluation of an
# expression. It may have modifiers also, and the expression may consist of a
# single term.
class RakuAST::Statement::Expression
  is RakuAST::Statement
  is RakuAST::SinkPropagator
  is RakuAST::Sinkable
  is RakuAST::BlockStatementSensitive
  is RakuAST::BeginTime
  is RakuAST::CheckTime
{
    has RakuAST::Expression $.expression;
    has RakuAST::StatementModifier::Condition $.condition-modifier;
    has RakuAST::StatementModifier::Loop $.loop-modifier;
    has RakuAST::ExpressionThunk $.loop-thunk;
    has RakuAST::ExpressionThunk $.condition-thunk;
    has RakuAST::ExpressionThunk $.expression-thunk;

    method new(RakuAST::Expression :$expression!, List :$labels,
               RakuAST::StatementModifier::Condition :$condition-modifier,
               RakuAST::StatementModifier::Loop :$loop-modifier) {
        my $obj := nqp::create(self);
        $obj.set-expression($expression);
        $obj.set-labels($labels);
        nqp::bindattr($obj, RakuAST::Statement::Expression, '$!condition-modifier',
            $condition-modifier // RakuAST::StatementModifier::Condition);
        nqp::bindattr($obj, RakuAST::Statement::Expression, '$!loop-modifier',
            $loop-modifier // RakuAST::StatementModifier::Loop);
        $obj
    }

    method set-expression(RakuAST::Expression $expression) {
        nqp::bindattr(self, RakuAST::Statement::Expression, '$!expression',
          $expression // RakuAST::Expression);
        Nil
    }

    method replace-condition-modifier(RakuAST::StatementModifier::Condition $condition-modifier) {
        nqp::bindattr(self, RakuAST::Statement::Expression, '$!condition-modifier',
            $condition-modifier);
        Nil
    }

    method replace-loop-modifier(RakuAST::StatementModifier::Loop $loop-modifier) {
        nqp::bindattr(self, RakuAST::Statement::Expression, '$!loop-modifier',
            $loop-modifier);
        Nil
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        # If there is a condition modifier, and the expression is a block
        # with either an explicit signature or a placeholder signature,
        # we should compile it immediate in order that the arity is set
        # right and things like `{ say $^x } if $y` work. Blocks with no
        # signature aren't handled this way, since the $_ should not be
        # counted.
        my $qast;
        if $!condition-modifier && nqp::istype($!expression, RakuAST::Block) &&
                ($!expression.signature || $!expression.placeholder-signature) {
            $qast := $!condition-modifier.IMPL-WRAP-QAST($context,
                $!expression.IMPL-TO-QAST($context, :immediate));
        }
        else {
            $qast := $!expression.IMPL-TO-QAST($context);
            if self.sunk && $!expression.needs-sink-call {
                $qast := QAST::Op.new( :op('p6sink'), $qast );
            }
            $qast := $!condition-modifier.IMPL-WRAP-QAST($context, $qast)
                if $!condition-modifier && (!$!loop-modifier || !$!loop-modifier.handles-condition);
        }
        if $!loop-modifier {
            my $sink := self.IMPL-DISCARD-RESULT;
            $qast := $!loop-modifier.IMPL-WRAP-QAST($context, $qast, :$sink,
                :block(nqp::istype(self.expression, RakuAST::Block)));
        }
        $qast
    }

    # StatementModifier::WhileUntil needs us to thunk loop-condition, condition-modifier and expression
    # but crucially it only needs this if we're not a block statement and we're not sunk. The problem is
    # that at BEGIN time we don't know that yet. But BEGIN time is the latest that we can thunk those
    # expressions. We may not even get a CHECK time if this statement is executed at BEGIN time. Thus
    # we thunk just in case and if it turns out we're sunk, we have to undo that thunking again.
    method IMPL-UNTHUNK() {
        if $!loop-modifier && nqp::istype($!loop-modifier, RakuAST::StatementModifier::WhileUntil) && nqp::defined($!loop-thunk) {
            $!loop-modifier.expression.IMPL-REMOVE-THUNK($!loop-thunk)
                unless $!loop-modifier.IMPL-UNNEGATE-IF-NEEDED;
            nqp::bindattr(self, RakuAST::Statement::Expression, '$!loop-thunk', RakuAST::ExpressionThunk);
            if $!condition-modifier {
                nqp::die('cond thunk not defined?') unless nqp::defined($!condition-thunk);
                $!expression.IMPL-REMOVE-THUNK($!condition-thunk);
                nqp::bindattr(self, RakuAST::Statement::Expression, '$!condition-thunk', RakuAST::ExpressionThunk);
            }

            if !nqp::istype($!expression, RakuAST::Block) {
                nqp::die('expr thunk not defined') unless nqp::defined($!expression-thunk);
                $!expression.IMPL-REMOVE-THUNK($!expression-thunk);
                nqp::bindattr(self, RakuAST::Statement::Expression, '$!expression-thunk', RakuAST::ExpressionThunk);
            }
        }
    }

    method IMPL-DISCARD-RESULT() {
        (
            self.is-block-statement
                && $!loop-modifier
                && !nqp::istype($!loop-modifier, RakuAST::StatementModifier::Given)
            || self.sunk
        ) ?? 1 !! 0
    }

    method propagate-sink(Bool $is-sunk) {
        self.IMPL-UNTHUNK() if $is-sunk;
        $!expression.apply-sink($is-sunk, :okifnil($!loop-modifier ?? True !! False));
        $!condition-modifier.apply-sink(False) if $!condition-modifier;
        $!loop-modifier.apply-sink(False) if $!loop-modifier;
    }

    method mark-block-statement() {
        self.IMPL-UNTHUNK();
        nqp::findmethod(RakuAST::BlockStatementSensitive, 'mark-block-statement')(self);
        if nqp::istype($!expression, RakuAST::BlockStatementSensitive) {
            $!expression.mark-block-statement();
        }
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        if $!loop-modifier {
            my $thunk := $!loop-modifier.expression-thunk;
            if $thunk && !nqp::istype($!expression, RakuAST::Block) {
                # only need to thunk the condition if we also have a loop thunk
                if $!condition-modifier {
                    my $thunk := $!condition-modifier.expression-thunk;
                    $!expression.wrap-with-thunk($thunk);
                    $thunk.ensure-begin-performed($resolver, $context);
                }
                $!expression.wrap-with-thunk($thunk);
                $thunk.ensure-begin-performed($resolver, $context);
            }

            # See IMPL-UNTHUNK for important information
            if (nqp::istype($!loop-modifier, RakuAST::StatementModifier::WhileUntil)) {
                $!loop-modifier.IMPL-NEGATE-IF-NEEDED($resolver, $context);
                my $loop-thunk := RakuAST::ExpressionThunk.new;
                $!loop-modifier.expression.wrap-with-thunk($loop-thunk);
                $loop-thunk.ensure-begin-performed($resolver, $context);
                nqp::bindattr(self, RakuAST::Statement::Expression, '$!loop-thunk', $loop-thunk);

                if !nqp::istype($!expression, RakuAST::Block) {
                    if $!condition-modifier {
                        my $thunk := $!condition-modifier.expression-thunk;
                        $!expression.wrap-with-thunk($thunk);
                        $thunk.ensure-begin-performed($resolver, $context);
                        nqp::bindattr(self, RakuAST::Statement::Expression, '$!condition-thunk', $thunk);
                    }
                    my $thunk := RakuAST::ExpressionThunk.new;
                    $!expression.wrap-with-thunk($thunk);
                    $thunk.ensure-begin-performed($resolver, $context);
                    nqp::bindattr(self, RakuAST::Statement::Expression, '$!expression-thunk', $thunk);
                }
            }
        }
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
    }

    method visit-children(Code $visitor) {
        $visitor($!expression);
        $visitor($!condition-modifier) if $!condition-modifier;
        $visitor($!loop-modifier) if $!loop-modifier;
        self.visit-labels($visitor);
    }

    method has-compile-time-value() {
        !$!condition-modifier && !$!loop-modifier && $!expression.has-compile-time-value
    }

    method maybe-compile-time-value() {
        $!expression.maybe-compile-time-value;
    }

    method IMPL-CAN-INTERPRET() {
        !$!condition-modifier && !$!loop-modifier && $!expression.IMPL-CAN-INTERPRET
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        $!expression.IMPL-INTERPRET($ctx)
    }
}

# Mark out things that immediately consume their body, rather than needing it as
# a closure.
class RakuAST::IMPL::ImmediateBlockUser
  is RakuAST::Node
{
    method IMPL-IMMEDIATELY-USES(RakuAST::Node $node) { True }
}

# Base class for if / with conditional, with optional elsif/orwith/else parts
class RakuAST::Statement::IfWith
  is RakuAST::Statement
  is RakuAST::ImplicitLookups
  is RakuAST::SinkPropagator
  is RakuAST::IMPL::ImmediateBlockUser
  is RakuAST::ImplicitBlockSemanticsProvider
{
    has RakuAST::Expression $.condition;
    has RakuAST::Expression $.then;
    has List                $!elsifs;
    has RakuAST::Block      $.else;

    method new(
      RakuAST::Expression :$condition!,
      RakuAST::Expression :$then!,
                     List :$elsifs,
           RakuAST::Block :$else,
                     List :$labels
    ) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj,RakuAST::Statement::IfWith,'$!condition',$condition);
        nqp::bindattr($obj,RakuAST::Statement::IfWith,'$!then',$then);
        $obj.set-elsifs($elsifs);
        $obj.set-else($else);
        $obj.set-labels($labels);
        $obj
    }

    method set-elsifs($elsifs) {
        nqp::bindattr(self,RakuAST::Statement::IfWith,'$!elsifs',
          $elsifs ?? self.IMPL-UNWRAP-LIST($elsifs) !! []
        );
        Nil
    }
    method elsifs() { self.IMPL-WRAP-LIST($!elsifs) }

    method set-else($else) {
        nqp::bindattr(self,RakuAST::Statement::IfWith,'$!else',
          $else // RakuAST::Block);
    }

    method apply-implicit-block-semantics() {
        my int $last-was-with;
        if self.IMPL-QAST-TYPE eq 'with' {
            $last-was-with := 1;
            $!then.set-implicit-topic(True, :required);
        }
        else {
            $!then.set-implicit-topic(False, :local);
        }
        for $!elsifs {
            $_.apply-implicit-block-semantics();
            $last-was-with := $_.IMPL-QAST-TYPE eq 'with';
        }
        if $!else {
            if $last-was-with {
                $!else.set-implicit-topic(True, :required);
            }
            else {
                $!else.set-implicit-topic(False, :local);
            }
        }
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
            $cur-end :=
              self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].IMPL-TO-QAST($context);
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
        self.visit-labels($visitor);
    }

    method IMPL-IMMEDIATELY-USES(RakuAST::Node $node) {
        return False if $node =:= $!condition;
        for $!elsifs {
            return False if $node =:= $_.condition;
        }
        True
    }
}

# An if conditional, with optional elsif/orwith/else parts.
class RakuAST::Statement::If
  is RakuAST::Statement::IfWith
{
    method IMPL-QAST-TYPE() { 'if' }
}

# A with conditional, with optional elsif/orwith/else parts.
class RakuAST::Statement::With
  is RakuAST::Statement::IfWith
{
    method IMPL-QAST-TYPE() { 'with' }
}

# An elsif part. Not a standalone RakuAST node; can only be used inside
# of an Statement::If or Statement::With node.
class RakuAST::Statement::Elsif {
    has RakuAST::Expression $.condition;
    has RakuAST::Expression $.then;

    method new(RakuAST::Expression :$condition!, RakuAST::Expression :$then!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Statement::Elsif, '$!condition', $condition);
        nqp::bindattr($obj, RakuAST::Statement::Elsif, '$!then', $then);
        $obj
    }

    method apply-implicit-block-semantics() {
        $!then.set-implicit-topic(False, :local);
    }

    method IMPL-QAST-TYPE() { 'if' }
}

# An orwith part. Not a standalone RakuAST node; can only be used inside of an If
# or With node.
class RakuAST::Statement::Orwith
  is RakuAST::Statement::Elsif
{
    method IMPL-QAST-TYPE() { 'with' }

    method apply-implicit-block-semantics() {
        self.then.set-implicit-topic(True, :required);
    }
}

# An unless statement control.
class RakuAST::Statement::Unless
  is RakuAST::Statement
  is RakuAST::ImplicitLookups
  is RakuAST::SinkPropagator
  is RakuAST::IMPL::ImmediateBlockUser
  is RakuAST::ImplicitBlockSemanticsProvider
{
    has RakuAST::Expression $.condition;
    has RakuAST::Block $.body;

    method new(RakuAST::Expression :$condition!, RakuAST::Block :$body!, List :$labels) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Statement::Unless, '$!condition', $condition);
        nqp::bindattr($obj, RakuAST::Statement::Unless, '$!body', $body);
        $obj.set-labels($labels);
        $obj
    }

    method apply-implicit-block-semantics() {
        $!body.set-implicit-topic(False, :local);
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Empty')),
        ])
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new(
            :op('unless'),
            $!condition.IMPL-TO-QAST($context),
            $!body.IMPL-TO-QAST($context, :immediate),
            self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].IMPL-TO-QAST($context)
        )
    }

    method propagate-sink(Bool $is-sunk) {
        $!condition.apply-sink(False);
        $!body.apply-sink($is-sunk);
    }

    method visit-children(Code $visitor) {
        $visitor($!condition);
        $visitor($!body);
        self.visit-labels($visitor);
    }

    method IMPL-IMMEDIATELY-USES(RakuAST::Node $node) {
        $node =:= $!body
    }
}

# A without statement control.
class RakuAST::Statement::Without
  is RakuAST::Statement
  is RakuAST::ImplicitLookups
  is RakuAST::SinkPropagator
  is RakuAST::IMPL::ImmediateBlockUser
  is RakuAST::ImplicitBlockSemanticsProvider
{
    has RakuAST::Expression $.condition;
    has RakuAST::Block $.body;

    method new(RakuAST::Expression :$condition!, RakuAST::Block :$body!, List :$labels) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Statement::Without, '$!condition', $condition);
        nqp::bindattr($obj, RakuAST::Statement::Without, '$!body', $body);
        $obj.set-labels($labels);
        $obj
    }

    method apply-implicit-block-semantics() {
        $!body.set-implicit-topic(True, :required);
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Empty')),
        ])
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new(
            :op('without'),
            $!condition.IMPL-TO-QAST($context),
            $!body.IMPL-TO-QAST($context, :immediate),
            self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].IMPL-TO-QAST($context)
        )
    }

    method propagate-sink(Bool $is-sunk) {
        $!condition.apply-sink(False);
        $!body.apply-sink($is-sunk);
    }

    method visit-children(Code $visitor) {
        $visitor($!condition);
        $visitor($!body);
        self.visit-labels($visitor);
    }

    method IMPL-IMMEDIATELY-USES(RakuAST::Node $node) {
        $node =:= $!body
    }
}

# The base for various kinds of loop. Used directly for the loop construct,
# and subclassed with assorted defaults for while/until/repeat.
class RakuAST::Statement::Loop
  is RakuAST::Statement
  is RakuAST::BeginTime
  is RakuAST::ImplicitLookups
  is RakuAST::Sinkable
  is RakuAST::SinkPropagator
  is RakuAST::BlockStatementSensitive
  is RakuAST::IMPL::ImmediateBlockUser
  is RakuAST::ImplicitBlockSemanticsProvider
{
    # The setup expression for the loop.
    has RakuAST::Expression $.setup;

    # The condition of the loop.
    has RakuAST::Expression $.condition;

    # The body of the loop.
    has RakuAST::Block $.body;

    # The increment expression for the loop.
    has RakuAST::Expression $.increment;

    has RakuAST::ExpressionThunk $.condition-thunk;
    has RakuAST::ExpressionThunk $.increment-thunk;

    method new(RakuAST::Block :$body!, RakuAST::Expression :$condition,
               RakuAST::Expression :$setup, RakuAST::Expression :$increment,
               List :$labels) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Statement::Loop, '$!body', $body);
        nqp::bindattr($obj, RakuAST::Statement::Loop, '$!condition', $condition);
        nqp::bindattr($obj, RakuAST::Statement::Loop, '$!setup', $setup);
        nqp::bindattr($obj, RakuAST::Statement::Loop, '$!increment', $increment);
        $obj.set-labels($labels);
        $obj
    }

    # Is the condition negated?
    method negate() { False }

    # Is the loop always executed once?
    method repeat() { False }

    method apply-implicit-block-semantics() {
        $!body.set-implicit-topic(False, :local);
    }

    method mark-block-statement() {
        self.IMPL-UNTHUNK() unless self.IMPL-HAS-UNDO-PHASERS;
        nqp::findmethod(RakuAST::BlockStatementSensitive, 'mark-block-statement')(self);
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Nil')),
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Seq'))
        ])
    }

    method IMPL-DISCARD-RESULT() {
        self.is-block-statement || self.sunk
    }

    method IMPL-HAS-UNDO-PHASERS() {
        nqp::elems($!body.IMPL-UNWRAP-LIST($!body.meta-object.phasers('UNDO'))) ?? 1 !! 0;
    }

    # We need to thunk condition and increment but crucially, only if we're not
    # a block statement and we're not sunk. The problem is that at BEGIN time
    # we don't know that yet. But BEGIN time is the latest that we can thunk
    # those expressions. We may not even get a CHECK time if this statement is
    # executed at BEGIN time. Thus we thunk just in case and if it turns out
    # we're sunk, we have to undo that thunking again.
    method IMPL-UNTHUNK() {
        if nqp::defined($!condition-thunk) {
            if self.negate {
                # No need to unthunk as we're throwing away the thunked ApplyPostfix
                nqp::bindattr(self, RakuAST::Statement::Loop, '$!condition', $!condition.operand);
            }
            else {
                $!condition.IMPL-REMOVE-THUNK($!condition-thunk)
            }
            nqp::bindattr(self, RakuAST::Statement::Loop, '$!condition-thunk', RakuAST::ExpressionThunk);
        }
        if nqp::defined($!increment-thunk) {
            $!increment.IMPL-REMOVE-THUNK($!increment-thunk);
            nqp::bindattr(self, RakuAST::Statement::Loop, '$!increment-thunk', RakuAST::ExpressionThunk);
        }
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        # See IMPL-UNTHUNK for important information
        my $while := !self.negate;
        unless (!$!increment && $!condition && $!condition.has-compile-time-value && $!condition.maybe-compile-time-value == $while) {
            if ($!condition) {
                if self.negate {
                    nqp::bindattr(self, RakuAST::Statement::Loop, '$!condition', RakuAST::ApplyPostfix.new(
                        :postfix(
                            RakuAST::Call::Method.new(:name(RakuAST::Name.from-identifier('not')))
                        ),
                        :operand($!condition),
                    ));
                    $!condition.ensure-begin-performed($resolver, $context);
                }
                my $thunk := RakuAST::ExpressionThunk.new;
                $!condition.wrap-with-thunk($thunk);
                $thunk.ensure-begin-performed($resolver, $context);
                nqp::bindattr(self, RakuAST::Statement::Loop, '$!condition-thunk', $thunk);
            }

            if ($!increment) {
                my $thunk := RakuAST::ExpressionThunk.new;
                $!increment.wrap-with-thunk($thunk);
                $thunk.ensure-begin-performed($resolver, $context);
                nqp::bindattr(self, RakuAST::Statement::Loop, '$!increment-thunk', $thunk);
            }
        }
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $phasers := nqp::getattr($!body.meta-object, Block, '$!phasers');
        my @next-phasers := nqp::ishash($phasers) && nqp::existskey($phasers, 'NEXT') ?? $phasers<NEXT> !! [];
        my @last-phasers := nqp::ishash($phasers) && nqp::existskey($phasers, 'LAST') ?? $phasers<LAST> !! [];
        my @undo-phasers := nqp::ishash($phasers) && nqp::existskey($phasers, 'UNDO') ?? $phasers<UNDO> !! [];
        my @labels := self.IMPL-UNWRAP-LIST(self.labels);
        if self.IMPL-DISCARD-RESULT && !nqp::elems(@undo-phasers) {
            # Select correct node type for the loop and produce it.
            my str $op := self.repeat
                ?? (self.negate ?? 'repeat_until' !! 'repeat_while')
                !! (self.negate ?? 'until' !! 'while');
            my $loop-qast := QAST::Op.new(
                :$op,
                $!condition ?? $!condition.IMPL-TO-QAST($context) !! QAST::IVal.new( :value(1) ),
                $!body.IMPL-TO-QAST($context, :immediate),
            );
            my @post;
            if @next-phasers {
                for @next-phasers {
                    $context.ensure-sc($_);
                    @post.push(QAST::Op.new(:op('call'), QAST::WVal.new(:value($_))));
                }
            }
            if $!increment {
                @post.push($!increment.IMPL-TO-QAST($context));
            }
            if @post {
                $loop-qast.push(nqp::elems(@post) == 1 ?? @post[0] !! QAST::Stmts.new(|@post));
            }

            # Add a label if there is one.
            if @labels {
                my $label-qast := @labels[0].IMPL-LOOKUP-QAST($context);
                $label-qast.named('label');
                $loop-qast.push($label-qast);
            }

            # Prepend setup code and evaluate to Nil if not sunk.
            my $wrapper := self.IMPL-SET-NODE(QAST::Stmt.new(), :key);
            if $!setup {
                $wrapper.push($!setup.IMPL-TO-QAST($context));
            }
            $wrapper.push($loop-qast);
            for @last-phasers {
                $context.ensure-sc($_);
                $wrapper.push(QAST::Op.new(:op('call'), QAST::WVal.new(:value($_))));
            }
            unless self.sunk {
                $wrapper.push(
                  self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].IMPL-TO-QAST($context)
                );
            }

            $wrapper
        }
        elsif ($!condition || $!increment) {
            my $Seq := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[1].IMPL-TO-QAST($context);
            my $while := !self.negate;
            if (!$!increment && $!condition.has-compile-time-value && $!condition.maybe-compile-time-value == $while) {
                my $qast := QAST::Stmts.new;
                my $loop-qast := QAST::Op.new(:op('callmethod'), :name('from-loop'),
                    $Seq,
                    $!body.IMPL-TO-QAST($context),
                );
                if @labels {
                    my $label-qast := @labels[0].IMPL-LOOKUP-QAST($context);
                    $label-qast.named('label');
                    $loop-qast.push($label-qast);
                }
                $qast.push: $loop-qast;
                $qast
            }
            else {
                my $Seq := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[1].IMPL-TO-QAST($context);
                my $qast := QAST::Op.new(:op<callmethod>, :name('from-loop'),
                    $Seq,
                    $!body.IMPL-TO-QAST($context),
                    $!condition.IMPL-TO-QAST($context),
                );
                $qast.push: $!increment.IMPL-TO-QAST($context) if $!increment;
                if @labels {
                    my $label-qast := @labels[0].IMPL-LOOKUP-QAST($context);
                    $label-qast.named('label');
                    $qast.push($label-qast);
                }
                #TODO next-phasers
                if @last-phasers {
                    $qast := QAST::Stmts.new(:resultchild(0), $qast);
                    for @last-phasers {
                        $context.ensure-sc($_);
                        $qast.push(QAST::Op.new(:op('call'), QAST::WVal.new(:value($_))));
                    }
                }
                if self.IMPL-DISCARD-RESULT { # In case we're here because of UNDO phasers
                    $qast := QAST::Op.new(:op('p6sink'), $qast);
                }
                if $!setup {
                    QAST::Stmt.new(
                        $!setup.IMPL-TO-QAST($context),
                        $qast
                    );
                }
                else {
                    $qast
                }
            }
        }
        else {
            my $Seq := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[1].IMPL-TO-QAST($context);
            # In theory we could use the from-loop candidate without condition
            # for plain loop but that would create a lazy loop and for unknown
            # reason the old implementation didn't go that route.
            my $cond := -> { 1 };
            $context.ensure-sc($cond);
            my $qast := QAST::Op.new(:op<callmethod>, :name('from-loop'),
                $Seq,
                $!body.IMPL-TO-QAST($context),
                QAST::WVal.new(:value($cond)),
            );
            if @labels {
                my $label-qast := @labels[0].IMPL-LOOKUP-QAST($context);
                $label-qast.named('label');
                $qast.push($label-qast);
            }
            if @next-phasers {
                my $run-phasers := -> { $_() for @next-phasers };
                $context.ensure-sc($run-phasers);
                $qast.push(QAST::WVal.new(:value($run-phasers)));
            }
            if @last-phasers {
                $qast := QAST::Stmts.new(:resultchild(0), $qast);
                for @last-phasers {
                    $context.ensure-sc($_);
                    $qast.push(QAST::Op.new(:op('call'), QAST::WVal.new(:value($_))));
                }
            }
            $qast
        }
    }

    method propagate-sink(Bool $is-sunk) {
        self.IMPL-UNTHUNK() if $is-sunk && ! self.IMPL-HAS-UNDO-PHASERS;
        $!condition.apply-sink(False) if $!condition;
        $!body.apply-sink(self.IMPL-DISCARD-RESULT ?? True !! False);
        $!setup.apply-sink(True) if $!setup;
        $!increment.apply-sink(True) if $!increment;
    }

    method visit-children(Code $visitor) {
        $visitor($!setup) if $!setup;
        $visitor($!condition) if $!condition;
        $visitor($!increment) if $!increment;
        $visitor($!body);
        self.visit-labels($visitor);
    }

    method IMPL-IMMEDIATELY-USES(RakuAST::Node $node) {
        my @undo-phasers := $!body.IMPL-UNWRAP-LIST($!body.meta-object.phasers('UNDO'));
        self.sunk && !nqp::elems(@undo-phasers) && $node =:= $!body
    }
}

# A while loop.
class RakuAST::Statement::Loop::While
  is RakuAST::Statement::Loop
{ }

# An until loop.
class RakuAST::Statement::Loop::Until
  is RakuAST::Statement::Loop
{
    method negate() { True }
}

# A repeat while loop.
class RakuAST::Statement::Loop::RepeatWhile
  is RakuAST::Statement::Loop
{
    method repeat() { True }
}

# A repeat until loop.
class RakuAST::Statement::Loop::RepeatUntil
  is RakuAST::Statement::Loop
{
    method negate() { True }
    method repeat() { True }
}

# A for loop.
class RakuAST::Statement::For
  is RakuAST::Statement
  is RakuAST::ForLoopImplementation
  is RakuAST::Sinkable
  is RakuAST::SinkPropagator
  is RakuAST::BlockStatementSensitive
  is RakuAST::ImplicitBlockSemanticsProvider
{
    # The thing to iterate over.
    has RakuAST::Expression $.source;

    # The body of the loop.
    has RakuAST::Block $.body;

    # The block to run if nothing to iterate over
    has RakuAST::Block $.otherwise;

    # The mode of evaluation, (defaults to serial, may be race or hyper also).
    has str $.mode;

    method new(
      RakuAST::Expression :$source!,
           RakuAST::Block :$body!,
           RakuAST::Block :$otherwise,
                      str :$mode,
                     List :$labels
    ) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Statement::For, '$!source', $source);
        nqp::bindattr($obj, RakuAST::Statement::For, '$!body', $body);
        nqp::bindattr($obj, RakuAST::Statement::For, '$!otherwise', $otherwise);
        nqp::bindattr_s($obj, RakuAST::Statement::For, '$!mode', $mode || 'serial');
        $obj.set-labels($labels);
        $obj
    }

    method replace-mode(str $mode) {
        nqp::bindattr_s(self, RakuAST::Statement::For, '$!mode', $mode);
        Nil
    }

    method replace-otherwise(RakuAST::Block $otherwise) {
        nqp::bindattr(self, RakuAST::Statement::For, '$!otherwise', $otherwise);
        Nil
    }

    method IMPL-DISCARD-RESULT() {
        self.is-block-statement || self.sunk
    }

    method propagate-sink(Bool $is-sunk) {
        $!source.apply-sink(False);
        $!body.apply-sink(self.IMPL-DISCARD-RESULT ?? True !! False);
    }

    method apply-implicit-block-semantics() {
        $!body.set-implicit-topic(True, :required);
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        # Figure out the execution mode modifiers to apply.
        my str $mode := $!mode;
        my str $after-mode := '';
        if $!otherwise {
            $mode := 'serial';
            $after-mode := 'eager';
        }
        elsif $mode eq 'lazy' {
            $mode := 'serial';
            $after-mode := 'lazy';
        }
        else {
            $after-mode := self.IMPL-DISCARD-RESULT ?? 'sink' !! 'eager';
        }

        # Delegate to the for loop compilation helper (which we pass various
        # attributes to in order to make it callable for the statement modifier
        # form also).
        my @labels := self.IMPL-UNWRAP-LIST(self.labels);
        my $qast := self.IMPL-FOR-QAST(
          $context,
          $mode,
          $after-mode,
          $!source.IMPL-TO-QAST($context),
          $!body.IMPL-TO-QAST($context),
          @labels ?? @labels[0] !! RakuAST::Label
        );

        $!otherwise
          ?? QAST::Op.new(:op<unless>,
               $qast,
               QAST::Op.new(:op<call>,
                 $!otherwise.IMPL-TO-QAST($context)
               )
             )
          !! $qast
    }


    method visit-children(Code $visitor) {
        $visitor($!source);
        $visitor($!body);
        $visitor($!otherwise) if $!otherwise;
        self.visit-labels($visitor);
    }
}

# A given statement.
class RakuAST::Statement::Given
  is RakuAST::Statement
  is RakuAST::SinkPropagator
  is RakuAST::ImplicitBlockSemanticsProvider
{
    # The thing to topicalize.
    has RakuAST::Expression $.source;

    # The body of the given statement.
    has RakuAST::Block $.body;

    method new(RakuAST::Expression :$source!, RakuAST::Block :$body!, List :$labels) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Statement::Given, '$!source', $source);
        nqp::bindattr($obj, RakuAST::Statement::Given, '$!body', $body);
        $obj.set-labels($labels);
        $obj
    }

    method propagate-sink(Bool $is-sunk) {
        $!source.apply-sink(False);
        $!body.apply-sink($is-sunk);
    }

    method apply-implicit-block-semantics() {
        $!body.set-implicit-topic(True, :required);
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new(
            :op('call'),
            $!body.IMPL-TO-QAST($context),
            $!source.IMPL-TO-QAST($context)
        )
    }

    method visit-children(Code $visitor) {
        $visitor($!source);
        $visitor($!body);
        self.visit-labels($visitor);
    }
}

# A when statement, smart-matching the topic against the specified expression,
# with `succeed`/`proceed` handling.
class RakuAST::Statement::When
  is RakuAST::Statement
  is RakuAST::SinkPropagator
  is RakuAST::ImplicitBlockSemanticsProvider
  is RakuAST::ImplicitLookups
  is RakuAST::BeginTime
{
    has RakuAST::Expression $.condition;
    has RakuAST::Block $.body;

    method new(RakuAST::Expression :$condition!, RakuAST::Block :$body!, List :$labels) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Statement::When, '$!condition', $condition);
        nqp::bindattr($obj, RakuAST::Statement::When, '$!body', $body);
        $obj.set-labels($labels);
        $obj
    }

    method apply-implicit-block-semantics() {
        $!body.set-implicit-topic(False);
    }

    method propagate-sink(Bool $is-sunk) {
        $!condition.apply-sink(False);
        $!body.apply-sink(False); # Used as enclosing block outcome
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $block := $resolver.find-attach-target('block') //
            $resolver.find-attach-target('compunit');
        if $block {
            $block.require-succeed-handler();
        }
        else {
            nqp::die('when found no enclosing block to attach to');
        }
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Var::Lexical.new('$_'),
        ])
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $sm-qast := QAST::Op.new(
            :op('callmethod'), :name('ACCEPTS'),
            $!condition.IMPL-TO-QAST($context),
            self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].IMPL-TO-QAST($context)
        );
        QAST::Op.new(
            :op('if'),
            $sm-qast,
            self.IMPL-WRAP-WHEN-OR-DEFAULT($context,
                QAST::Op.new( :op('call'), $!body.IMPL-TO-QAST($context) )
            ))
    }

    method visit-children(Code $visitor) {
        $visitor($!condition);
        $visitor($!body);
        self.visit-labels($visitor);
    }
}

# A whenever statement.
class RakuAST::Statement::Whenever
  is RakuAST::Statement
  is RakuAST::SinkPropagator
  is RakuAST::ImplicitBlockSemanticsProvider
  is RakuAST::ParseTime
{
    has RakuAST::Expression $.trigger;
    has RakuAST::Block      $.body;

    method new(
      RakuAST::Expression :$trigger!,
           RakuAST::Block :$body!,
                     List :$labels
    ) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj,RakuAST::Statement::Whenever,'$!trigger',$trigger);
        nqp::bindattr($obj,RakuAST::Statement::Whenever,'$!body',$body);
        $obj.set-labels($labels);
        $obj
    }

    method propagate-sink(Bool $is-sunk) {
        $!trigger.apply-sink(False);
        $!body.apply-sink($is-sunk);
    }

    method apply-implicit-block-semantics() {
        $!body.set-implicit-topic(True, :required);
    }

    method PERFORM-PARSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $wheneverable := $resolver.find-attach-target('wheneverable');
        if $wheneverable {
            $wheneverable.IMPL-ADD-WHENEVER(self);
        }
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new(:op<call>, :name<&WHENEVER>,
          QAST::Op.new(:op<hllize>,
            $!trigger.IMPL-TO-QAST($context)
          ),
          $!body.IMPL-TO-QAST($context)
        )
    }

    method visit-children(Code $visitor) {
        $visitor($!trigger);
        $visitor($!body);
        self.visit-labels($visitor);
    }
}

# A default statement.
class RakuAST::Statement::Default
  is RakuAST::Statement
  is RakuAST::SinkPropagator
  is RakuAST::ImplicitBlockSemanticsProvider
  is RakuAST::BeginTime
{
    has RakuAST::Block $.body;

    method new(RakuAST::Block :$body!, List :$labels) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Statement::Default, '$!body', $body);
        $obj.set-labels($labels);
        $obj
    }

    method apply-implicit-block-semantics() {
        $!body.set-implicit-topic(1);
    }

    method propagate-sink(Bool $is-sunk) {
        $!body.apply-sink(False); # Used as enclosing block outcome
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $block := $resolver.find-attach-target('block') //
            $resolver.find-attach-target('compunit');
        if $block {
            $block.require-succeed-handler();
        }
        else {
            nqp::die('default found no enclosing block to attach to');
        }
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        self.IMPL-WRAP-WHEN-OR-DEFAULT($context,
            QAST::Op.new( :op('call'), $!body.IMPL-TO-QAST($context) )
        )
    }

    method visit-children(Code $visitor) {
        $visitor($!body);
        self.visit-labels($visitor);
    }
}

# The commonalities of exception handlers (CATCH and CONTROL).
class RakuAST::Statement::ExceptionHandler
  is RakuAST::Statement
  is RakuAST::SinkPropagator
  is RakuAST::ImplicitBlockSemanticsProvider
  is RakuAST::ProducesNil
{
    has RakuAST::Block $.body;

    method new(RakuAST::Block :$body!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Statement::ExceptionHandler, '$!body', $body);
        $obj.set-labels(Mu);
        $obj
    }

    method apply-implicit-block-semantics() {
        $!body.set-implicit-topic(True, :exception);
        $!body.set-fresh-variables(:match, :exception);
    }

    method propagate-sink(Bool $is-sunk) {
        $!body.apply-sink(True);
    }

    method visit-children(Code $visitor) {
        $visitor($!body);
        self.visit-labels($visitor);
    }
}

# A CATCH statement.
class RakuAST::Statement::Catch
  is RakuAST::Statement::ExceptionHandler
  is RakuAST::BeginTime
{
    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $block := $resolver.find-attach-target('block') //
            $resolver.find-attach-target('compunit');
        if $block {
            $block.attach-catch-handler(self);
        }
        else {
            nqp::die('CATCH found no enclosing block to attach to');
        }
    }
}

# A CONTROL statement.
class RakuAST::Statement::Control
  is RakuAST::Statement::ExceptionHandler
  is RakuAST::BeginTime
{
    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $block := $resolver.find-attach-target('block') //
            $resolver.find-attach-target('compunit');
        if $block {
            $block.attach-control-handler(self);
        }
        else {
            nqp::die('CONTROL found no enclosing block to attach to');
        }
    }
}

class RakuAST::Categorical {
    has Str $.category;
    has Str $.opname;
    has Str $.subname;
    has RakuAST::Declaration $.declarand;

    method new(Str :$category!, Str :$opname!, Str :$subname!, RakuAST::Declaration :$declarand!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Categorical, '$!category', $category);
        nqp::bindattr($obj, RakuAST::Categorical, '$!opname', $opname);
        nqp::bindattr($obj, RakuAST::Categorical, '$!subname', $subname);
        nqp::bindattr($obj, RakuAST::Categorical, '$!declarand', $declarand);
        $obj
    }

    method canname {
        $!category ~ ':sym' ~ RakuAST::ColonPairish.IMPL-QUOTE-VALUE($!opname);
    }
}

class RakuAST::ModuleLoading {
    has List $!categoricals;
    has Hash $!superseded-declarators;
    has Hash $!declarators;
    has Hash $!unchecked-declarators;

    method categoricals() {
        self.IMPL-WRAP-LIST($!categoricals)
    }

    method superseded-declarators() {
        $!superseded-declarators // nqp::hash
    }

    method added-declarators() {
        $!declarators // nqp::hash
    }

    method unchecked-declarators() {
        $!unchecked-declarators // nqp::hash
    }

    method IMPL-LOAD-MODULE(RakuAST::Resolver $resolver, RakuAST::Name $module-name) {
        # Build dependency specification for the module.
        my $dependency-specification := $resolver.type-from-setting(
          'CompUnit', 'DependencySpecification'
        );
        my $opts := nqp::hash();
        for $module-name.IMPL-UNWRAP-LIST($module-name.colonpairs) {
            $opts{$_.key} := $_.simple-compile-time-quote-value;
        }
        my $spec := $dependency-specification.new(
            :short-name($module-name.canonicalize(:colonpairs(False))),
            :from($opts<from> // 'Perl6'),
            :auth-matcher($opts<auth> // True),
            :api-matcher($opts<api> // True),
            :version-matcher($opts<ver> // True),
        );

        # Load it using the registry.
        my $registry := $resolver.type-from-setting(
          'CompUnit', 'RepositoryRegistry'
        );
        my $comp-unit := $registry.head.need($spec);
        my $globalish := $comp-unit.handle.globalish-package;
        self.IMPL-IMPORT-ONE($resolver, self.IMPL-STASH-HASH($globalish));
        $comp-unit
    }

    method IMPL-IMPORT(RakuAST::Resolver $resolver, Mu $handle, Mu $arglist, Str :$module) {
        my $EXPORT := $handle.export-package;
        if nqp::isconcrete($EXPORT) {
            $EXPORT := $EXPORT.FLATTENABLE_HASH();
            # TODO deal with non-default imports due to tags
            my @to-import := ['MANDATORY'];
            my @positional-imports;
            if nqp::isconcrete($arglist) {
                my $Pair := $resolver.setting-constant('Pair');
                for $arglist -> $tag {
                    if nqp::istype($tag, $Pair) {
                        my str $tag-name := nqp::unbox_s($tag.key);
                        unless nqp::existskey($EXPORT, $tag-name) {
                            $resolver.build-exception('X::Import::NoSuchTag', source-package => $module, :tag($tag-name)).throw;
                        }
                        nqp::push(@to-import, $tag-name);
                    }
                    else {
                        nqp::push(@positional-imports, $tag);
                    }
                }
            }
            else {
                nqp::push(@to-import, 'DEFAULT');
            }
            for @to-import -> str $tag {
                if nqp::existskey($EXPORT, $tag) {
                    self.IMPL-IMPORT-ONE($resolver, self.IMPL-STASH-HASH($EXPORT{$tag}.WHO));
                }
            }

            my &EXPORT := $handle.export-sub;
            if nqp::isconcrete(&EXPORT) {
                my $result := &EXPORT(|@positional-imports);
                if nqp::istype($result, Map) {
                    my $storage := $result.hash.FLATTENABLE_HASH();
                    self.IMPL-IMPORT-ONE(
                        $resolver,
                        $storage,
                        :need-decont(!(nqp::what($result) =:= Map)),
                    );
                }
                else {
                    nqp::die("&EXPORT sub did not return a Map");
                }
            }
        }
    }

    method IMPL-IMPORT-ONE(RakuAST::Resolver $resolver, Mu $stash, Bool :$need-decont) {
        my $target-scope := $resolver.current-scope;
        for self.IMPL-SORTED-KEYS($stash) -> $key {
            next if $key eq 'EXPORT';
            my $value := $stash{$key};
            if $need-decont && nqp::islt_i(nqp::index('$&', nqp::substr($key,0,1)),0) {
                $value := nqp::decont($value);
            }
            my $declarand := RakuAST::Declaration::Import.new:
                    :lexical-name($key), :compile-time-value($value);

            my $existing := nqp::isconcrete($declarand.compile-time-value)
                ?? Nil
                !! $resolver.resolve-lexical-constant($key);
            if $existing {
                $existing.merge($declarand, :$resolver) unless $existing.compile-time-value =:= $declarand.compile-time-value;
            }
            else {
                $target-scope.merge-generated-lexical-declaration: $declarand, :$resolver;
            }

            my $categorical := $key ~~ /^ '&' (\w+) [ ':<' (.+) '>' | ':«' (.+) '»' ] $/;
            if $categorical {
                nqp::push($!categoricals, RakuAST::Categorical.new(
                    :category($categorical[0]),
                    :opname($categorical[1]),
                    :subname(nqp::substr($key, 1)),
                    :$declarand
                ));
            }
        }
    }

    method IMPL-IMPORT-EXPORTHOW(RakuAST::Resolver $resolver, Mu $handle) {
        my $EXPORTHOW := $handle.export-how-package;
        if nqp::defined($EXPORTHOW) {
            $EXPORTHOW.pairs.map(-> $pair {
                my str $key := $pair.key;
                if $key eq 'SUPERSEDE' {
                    my %SUPERSEDE := self.IMPL-STASH-HASH($pair.value.WHO);
                    nqp::bindattr(self, RakuAST::ModuleLoading, '$!superseded-declarators', nqp::hash)
                        unless nqp::isconcrete($!superseded-declarators);
                    for %SUPERSEDE {
                        my str $pdecl := $_.key;
                        my $meta  := nqp::decont($_.value);
                        $!superseded-declarators{$pdecl} := $meta;
                    }
                }
                elsif $key eq 'DECLARE' {
                    nqp::bindattr(self, RakuAST::ModuleLoading, '$!declarators', nqp::hash)
                        unless nqp::isconcrete($!declarators);
                    my %DECLARE := self.IMPL-STASH-HASH($pair.value.WHO);
                    for %DECLARE {
                        my str $pdecl := $_.key;
                        my $meta  := nqp::decont($_.value);
                        $!declarators{$pdecl} := $meta;
                    }
                }
                else {
                    if $key eq nqp::lc($key) {
                        # The legacy API behaves like an unchecked supersede.
                        # It was supposed to go away long ago according to this
                        # original comment:
                        # XXX Can give deprecation warning in the future, remove
                        # before 6.0.0.
                        # However, we also land here when one declares an our
                        # scoped EXPORTHOW package instead of a my scoped. This
                        # will pick up all declarators declared in the setting.
                        # So unfortunately we cannot error out here. Maybe the
                        # setting's EXPORTHOW can be adapted?
                        # Also there even seem to be spectests still using this
                        # old API.
                        nqp::bindattr(self, RakuAST::ModuleLoading, '$!unchecked-declarators', nqp::hash)
                            unless nqp::isconcrete($!unchecked-declarators);
                        my str $pdecl := $pair.key;
                        my $meta  := nqp::decont($pair.value);
                        $!unchecked-declarators{$pdecl} := $meta;
                    }
                    else {
                        $resolver.build-exception('X::EXPORTHOW::InvalidDirective', directive => $key).throw;
                    }
                }
            }).eager;
        }
    }

    method IMPL-STASH-HASH(Mu $pkg) {
        my $hash := $pkg;
        unless nqp::ishash($hash) {
            $hash := $hash.FLATTENABLE_HASH();
        }
        $hash
    }
}

# A use statement.
class RakuAST::Statement::Use
  is RakuAST::Statement
  is RakuAST::BeginTime
  is RakuAST::ProducesNil
  is RakuAST::ModuleLoading
{
    has RakuAST::Name $.module-name;
    has RakuAST::Expression $.argument;

    method new(RakuAST::Name :$module-name!, RakuAST::Expression :$argument, List :$labels) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Statement::Use, '$!module-name', $module-name);
        nqp::bindattr($obj, RakuAST::ModuleLoading, '$!categoricals', []);
        nqp::bindattr($obj, RakuAST::Statement::Use, '$!argument',
            $argument // RakuAST::Expression);
        $obj.set-labels($labels);
        $obj
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        # Evaluate the argument to the module load, if any.
        my $arglist := $!argument
            ?? self.IMPL-BEGIN-TIME-EVALUATE($!argument, $resolver, $context).List.FLATTENABLE_LIST
            !! Nil;

        my $comp-unit := self.IMPL-LOAD-MODULE($resolver, $!module-name);
        self.IMPL-IMPORT($resolver, $comp-unit.handle, $arglist, :module($!module-name.canonicalize));
        self.IMPL-IMPORT-EXPORTHOW($resolver, $comp-unit.handle);
    }

    method visit-children(Code $visitor) {
        $visitor($!module-name);
        $visitor($!argument) if $!argument;
        self.visit-labels($visitor);
    }
}

# A need statement.
class RakuAST::Statement::Need
  is RakuAST::Statement
  is RakuAST::BeginTime
  is RakuAST::ProducesNil
  is RakuAST::ModuleLoading
{
    has List $!module-names;

    method new(List :$module-names!, List :$labels) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Statement::Need, '$!module-names',
          self.IMPL-UNWRAP-LIST($module-names));
        nqp::bindattr($obj, RakuAST::ModuleLoading, '$!categoricals', []);
        $obj.set-labels($labels);
        $obj
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        for $!module-names {
            self.IMPL-LOAD-MODULE($resolver, $_);
        }
    }

    method module-names() { self.IMPL-WRAP-LIST($!module-names) }

    method visit-children(Code $visitor) {
        for $!module-names {
            $visitor($_);
        }
        self.visit-labels($visitor);
    }
}

# An import statement.
class RakuAST::Statement::Import
  is RakuAST::Statement
  is RakuAST::ParseTime
  is RakuAST::BeginTime
  is RakuAST::ProducesNil
  is RakuAST::ModuleLoading
  is RakuAST::Lookup
  is RakuAST::ImplicitLookups
{
    has RakuAST::Name $.module-name;
    has RakuAST::Expression $.argument;

    method new(RakuAST::Name :$module-name!, RakuAST::Expression :$argument, List :$labels) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Statement::Import, '$!module-name', $module-name);
        nqp::bindattr($obj, RakuAST::ModuleLoading, '$!categoricals', []);
        nqp::bindattr($obj, RakuAST::Statement::Import, '$!argument',
            $argument // RakuAST::Expression);
        $obj.set-labels($labels);
        $obj
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier-parts('CompUnit', 'Handle')),
        ])
    }

    method PERFORM-PARSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $resolved := $resolver.resolve-name-constant($!module-name);
        if $resolved {
            self.set-resolution($resolved);
        }
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        # Evaluate the argument to the import, if any.
        my $arglist := $!argument
            ?? self.IMPL-BEGIN-TIME-EVALUATE($!argument, $resolver, $context).List.FLATTENABLE_LIST
            !! Nil;

        my $module := self.resolution.compile-time-value;
        my $CompUnitHandle := self.get-implicit-lookups().AT-POS(0).compile-time-value;
        my $handle := $CompUnitHandle.from-unit($module.WHO);
        self.IMPL-IMPORT($resolver, $handle, $arglist, :module($!module-name.canonicalize));
        self.IMPL-IMPORT-EXPORTHOW($resolver, $handle);
    }

    method visit-children(Code $visitor) {
        $visitor($!module-name);
        $visitor($!argument) if $!argument;
        self.visit-labels($visitor);
    }
}

# A require statement.
class RakuAST::Statement::Require
  is RakuAST::Statement
  is RakuAST::Sinkable
  is RakuAST::BeginTime
  is RakuAST::ImplicitLookups
{
    has RakuAST::Name $.module-name;
    has RakuAST::Expression $.file;
    has RakuAST::Expression $.argument;
    has List $!arglist;
    has RakuAST::Package $!module;
    has RakuAST::Node $!existing-lookup;

    method new(RakuAST::Name :$module-name!, RakuAST::Expression :$file, RakuAST::Expression :$argument) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Statement::Require, '$!module-name', $module-name // RakuAST::Name);
        nqp::bindattr($obj, RakuAST::Statement::Require, '$!file', $file // RakuAST::Expression);
        nqp::bindattr($obj, RakuAST::Statement::Require, '$!argument',
            $argument // RakuAST::Expression);
        $obj
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        my @lookups;
        nqp::push(@lookups, RakuAST::Type::Setting.new(RakuAST::Name.from-identifier-parts('CompUnit', 'DependencySpecification')));
        nqp::push(@lookups, RakuAST::Type::Setting.new(RakuAST::Name.from-identifier-parts('CompUnit', 'RepositoryRegistry')));
        self.IMPL-WRAP-LIST(@lookups)
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        # Evaluate the argument to the import, if any.
        my @arglist := $!argument
            ?? self.IMPL-BEGIN-TIME-EVALUATE($!argument, $resolver, $context).List.FLATTENABLE_LIST
            !! Nil;
        nqp::bindattr(self, RakuAST::Statement::Require, '$!arglist', @arglist);

        my $target-scope := $resolver.current-scope;
        my $stash := Stash.new;
        $context.ensure-sc($stash);
        $target-scope.merge-generated-lexical-declaration:
            :$resolver,
            RakuAST::VarDeclaration::Implicit::Constant.new:
                :name<%?REQUIRE-SYMBOLS>,
                :value($stash);

        if @arglist {
            for @arglist {
                my $declarand := RakuAST::Type::Capture.new: RakuAST::Name.from-identifier($_.Str);
                $target-scope.merge-generated-lexical-declaration: $declarand, :$resolver;
            }
        }

        if $!module-name && !$!module-name.is-indirect-lookup() {
            my $top := $!module-name.parts.AT-POS(0);
            my $resolved := $resolver.resolve-name(RakuAST::Name.new($top));
            nqp::bindattr(self, RakuAST::Statement::Require, '$!existing-lookup', $resolved);

            nqp::bindattr(
                self,
                RakuAST::Statement::Require,
                '$!module',
                RakuAST::Package.new(:scope<my>, :name($!module-name.without-colonpair('file')), :is-require-stub),
            );
            $!module.to-begin-time($resolver, $context);
            $!module.set-is-stub(True);
        }

        Nil
    }

    method IMPL-SHORT-NAME(RakuAST::IMPL::QASTContext $context) {
        if $!module-name.is-indirect-lookup {
            if $!module-name.is-multi-part {
                my $qast := QAST::Op.new(:op<call>, :name('&infix:<,>'));
                for $!module-name.IMPL-UNWRAP-LIST($!module-name.parts) {
                    $qast.push: $_.IMPL-QAST-INDIRECT-LOOKUP-PART($context, Mu, 0)
                }
                QAST::Op.new(:op<callmethod>, :name<join>,
                    $qast,
                    QAST::SVal.new(:value<::>)
                )
            }
            else {
                $!module-name.indirect-lookup-part.expr.IMPL-TO-QAST($context)
            }
        }
        else {
            QAST::SVal.new(:value($!module-name.canonicalize))
        }
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $lookups := self.get-implicit-lookups;
        my $depspec  := $lookups.AT-POS(0).compile-time-value;
        my $registry := $lookups.AT-POS(1).compile-time-value;

        my $compunit-qast;
        my $file;
        if $!module-name && (my $file-cp := $!module-name.first-colonpair('file')) {
            $file := $file-cp.IMPL-VALUE-QAST($context);
        }
        if $!module-name && !nqp::defined($file) {
            my $short-name := self.IMPL-SHORT-NAME($context);
            $short-name.named('short-name');
            my $spec := QAST::Op.new(
                :op('callmethod'), :name('new'),
                QAST::WVal.new(:value($depspec)),
                $short-name,
            );
            $compunit-qast := QAST::Op.new(
                :op('callmethod'), :name('need'),
                QAST::Op.new(
                    :op('callmethod'), :name('head'),
                    QAST::WVal.new(:value($registry)),
                ),
                $spec,
            );
        }
        else {
            my $file-qast := ($file // $!file.IMPL-TO-QAST($context));
            $compunit-qast := QAST::Op.new(
                :op('callmethod'), :name('load'),
                QAST::Op.new(
                    :op('callmethod'), :name('head'),
                    QAST::WVal.new(:value($registry)),
                ),
                QAST::Op.new(
                    :op('callmethod'), :name('IO'),
                    $file-qast,
                ),
            );
        }

        my $require-qast := QAST::Op.new(:op<call>, :name<&REQUIRE_IMPORT>, $compunit-qast);
        # A list of the components of the pre-existing outer symbols name (if any)
        my $existing-path := QAST::Var.new( :name('Any'), :scope('lexical') );
        # The top level package object of the  pre-existing outer package (if any)
        my $top-existing := QAST::Var.new( :name('Any'), :scope('lexical') );
        # The name of the lexical stub we insert (if any)
        my $lexical-stub;
        if $!module-name && !$!module-name.is-indirect-lookup() {
            my $current := nqp::null;
            my @components := nqp::clone(self.IMPL-UNWRAP-LIST($!module-name.parts));
            my $top := @components.shift.name;
            $existing-path := QAST::Op.new(:op<call>, :name('&infix:<,>'));
            my $existing-lookup := $!existing-lookup;
            if $existing-lookup && nqp::istype($existing-lookup, RakuAST::CompileTimeValue) {
                my $existing := $existing-lookup.compile-time-value;
                $current := nqp::who($existing);
                $top-existing := QAST::WVal.new(:value($existing));
                $existing-path.push: QAST::SVal.new(:value($top));
            }
            else {
                $lexical-stub := QAST::SVal.new(:value($top));
            }

            if !nqp::isnull($current) {
                for @components -> $component-part {
                    my $component := $component-part.name;
                    if nqp::existskey($current,$component) {
                        $existing-path.push: QAST::SVal.new(:value($component));
                        $current := nqp::who($current{$component});
                    }
                    else {
                        last;
                    }
                }
            }
        }
        $require-qast.push($existing-path);
        $require-qast.push($top-existing);
        $require-qast.push($lexical-stub // QAST::Var.new( :name('Any'), :scope('lexical') ));
        if $!module-name && $!module {
            my $scalar := $!module.compile-time-value;
            $context.ensure-sc($scalar);
            $require-qast.push(QAST::WVal.new(:value($scalar)));
            my $name-parts := QAST::Op.new(:op<call>, :name('&infix:<,>'));
            for self.IMPL-UNWRAP-LIST($!module-name.parts) {
                $name-parts.push: QAST::SVal.new(:value($_.name));
            }
            $context.ensure-sc($name-parts);
            $require-qast.push($name-parts);
        }
        else {
            $require-qast.push(QAST::WVal.new(:value(Any)));
            $require-qast.push(QAST::WVal.new(:value(Any)));
        }
        if $!argument {
            for $!arglist {
                $require-qast.push(QAST::SVal.new(:value($_.Str)));
            }
        }

        my $qast := QAST::Stmts.new;
        $qast.push($require-qast);
        unless self.sunk {
            $qast.push: $!module-name
                ?? $!module-name.IMPL-QAST-INDIRECT-LOOKUP($context)
                !! $!file.IMPL-TO-QAST($context);
        }
        $qast
    }

    method visit-children(Code $visitor) {
        $visitor($!module-name) if $!module-name;
        $visitor($!file) if $!file;
        $visitor($!argument) if $!argument;
        $visitor($!module) if $!module;
    }
}
