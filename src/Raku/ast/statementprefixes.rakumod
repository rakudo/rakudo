# The base of all statement prefixes.
class RakuAST::StatementPrefix is RakuAST::Term {
    has RakuAST::Blorst $.blorst;

    method new(RakuAST::Blorst $blorst) {
        my $obj := nqp::create(self);
        unless self.allowed-on-for-statement {
            if nqp::istype($blorst, RakuAST::Statement::For) {
                nqp::die('Do not use this statement prefix on a RakuAST::Statement::For; ' ~
                    'instead, set the mode on that node');
            }
        }
        nqp::bindattr($obj, RakuAST::StatementPrefix, '$!blorst', $blorst);
        $obj
    }

    method allowed-on-for-statement() { True }

    method IMPL-CALLISH-QAST(RakuAST::IMPL::QASTContext $context) {
        if nqp::istype($!blorst, RakuAST::Block) {
            QAST::Op.new( :op('call'), $!blorst.IMPL-TO-QAST($context) )
        }
        else {
            $!blorst.IMPL-TO-QAST($context)
        }
    }

    method visit-children(Code $visitor) {
        $visitor($!blorst);
    }
}

# The `do` statement prefix.
class RakuAST::StatementPrefix::Do is RakuAST::StatementPrefix is RakuAST::SinkPropagator {
    method propagate-sink(Bool $is-sunk) {
        self.blorst.apply-sink($is-sunk);
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        self.IMPL-CALLISH-QAST($context)
    }
}

# The `quietly` statement prefix.
class RakuAST::StatementPrefix::Quietly is RakuAST::StatementPrefix is RakuAST::SinkPropagator {
    method propagate-sink(Bool $is-sunk) {
        self.blorst.apply-sink($is-sunk);
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new(
            :op('handle'),
            self.IMPL-CALLISH-QAST($context),
            'WARN',
            QAST::Op.new( :op('resume'), QAST::Op.new( :op('exception') ) )
        )
    }
}

# The `race` statement prefix.
class RakuAST::StatementPrefix::Race is RakuAST::StatementPrefix {
    method allowed-on-for-statement() { False }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new(
            :op('callmethod'), :name('race'),
            self.IMPL-CALLISH-QAST($context),
        )
    }
}

# The `hyper` statement prefix.
class RakuAST::StatementPrefix::Hyper is RakuAST::StatementPrefix {
    method allowed-on-for-statement() { False }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new(
            :op('callmethod'), :name('hyper'),
            self.IMPL-CALLISH-QAST($context),
        )
    }
}

# The `lazy` statement prefix.
class RakuAST::StatementPrefix::Lazy is RakuAST::StatementPrefix {
    method allowed-on-for-statement() { False }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new(
            :op('callmethod'), :name('lazy'),
            self.IMPL-CALLISH-QAST($context),
        )
    }
}

# The `eager` statement prefix.
class RakuAST::StatementPrefix::Eager is RakuAST::StatementPrefix {

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new(
            :op('callmethod'), :name('eager'),
            self.IMPL-CALLISH-QAST($context),
        )
    }
}

# The `try` statement prefix.
class RakuAST::StatementPrefix::Try is RakuAST::StatementPrefix is RakuAST::SinkPropagator
                                    is RakuAST::ImplicitLookups {
    method propagate-sink(Bool $is-sunk) {
        self.blorst.apply-sink($is-sunk);
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Nil')),
            RakuAST::Var::Lexical.new('$!')
        ])
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        # If it's a block that already has a CATCH handler, just run it.
        my $blorst := self.blorst;
        if nqp::istype($blorst, RakuAST::Block) && $blorst.IMPL-HAS-CATCH-HANDLER {
            self.IMPL-CALLISH-QAST($context)
        }

        # Otherwise, need to wrap it in exception handler logic.
        else {
            my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
            my $nil := @lookups[0].IMPL-TO-QAST($context);
            my $bang := @lookups[1].IMPL-TO-QAST($context);
            QAST::Op.new(
                :op('handle'),

                # Success path puts Nil into $! and evaluates to the block.
                QAST::Stmt.new(
                    :resultchild(0),
                    self.IMPL-CALLISH-QAST($context),
                    QAST::Op.new( :op('p6assign'), $bang, $nil )
                ),

                # On failure, capture the exception object into $!.
                'CATCH', QAST::Stmts.new(
                    QAST::Op.new(
                        :op('p6assign'),
                        $bang,
                        QAST::Op.new(
                            :name<&EXCEPTION>, :op<call>,
                            QAST::Op.new( :op('exception') )
                        ),
                    ),
                    $nil
                )
            )
        }
    }
}

# Done by statement prefixes that insist on thunking expressions into a code
# object.
class RakuAST::StatementPrefix::Thunky is RakuAST::StatementPrefix
                                       is RakuAST::Meta is RakuAST::Code {
    method PRODUCE-META-OBJECT() {
        if nqp::istype(self.blorst, RakuAST::Block) {
            # Block, already has a meta-object.
            self.blorst.meta-object
        }
        else {
            # Create code object with default empty signature.
            my $signature := nqp::create(Signature);
            nqp::bindattr($signature, Signature, '@!params', nqp::list());
            my $code := nqp::create(Code);
            nqp::bindattr($code, Code, '$!signature', $signature);
            $code
        }
    }

    method IMPL-QAST-DECL-CODE(RakuAST::IMPL::QASTContext $context) {
        if nqp::istype(self.blorst, RakuAST::Block) {
            # Block already, so we need add nothing.
            QAST::Op.new( :op('null') )
        }
        else {
            my $block := QAST::Block.new(
                :blocktype('declaration_static'),
                self.blorst.IMPL-TO-QAST($context)
            );
            self.IMPL-LINK-META-OBJECT($context, $block);
            $block
        }
    }
}

# The `gather` statement prefix.
class RakuAST::StatementPrefix::Gather is RakuAST::StatementPrefix::Thunky is RakuAST::SinkPropagator {
    method propagate-sink(Bool $is-sunk) {
        self.blorst.apply-sink(True);
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new( :op('call'), :name('&GATHER'), self.IMPL-CLOSURE-QAST($context) )
    }
}

# The `start` statement prefix.
class RakuAST::StatementPrefix::Start is RakuAST::StatementPrefix::Thunky
                                      is RakuAST::SinkPropagator
                                      is RakuAST::ImplicitLookups {
    method propagate-sink(Bool $is-sunk) {
        self.blorst.apply-sink(False);
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Promise')),
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('True')),
        ])
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
        my $promise := @lookups[0].IMPL-TO-QAST($context);
        my $qast := QAST::Op.new(
            :op('callmethod'), :name('start'),
            $promise,
            self.IMPL-CLOSURE-QAST($context)
        );
        unless $context.lang-version eq 'c' {
            my $true := @lookups[1].IMPL-TO-QAST($context);
            $true.named('report-broken-if-sunk');
            $qast.push($true);
        }
        $qast
    }
}

# Done by all phasers. Serves as little more than a marker for phasers, for
# easing locating them all.
class RakuAST::StatementPrefix::Phaser is RakuAST::StatementPrefix {
}

# Done by all phasers that don't produce a result.
class RakuAST::StatementPrefix::Phaser::Sinky is RakuAST::StatementPrefix::Phaser
                                              is RakuAST::ImplicitLookups
                                              is RakuAST::SinkPropagator {
    method propagate-sink(Bool $is-sunk) {
        self.blorst.apply-sink(True);
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Nil')),
        ])
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
        @lookups[0].IMPL-TO-QAST($context)
    }
}

# The BEGIN phaser.
class RakuAST::StatementPrefix::Phaser::Begin is RakuAST::StatementPrefix::Phaser
                                              is RakuAST::BeginTime {
    has Mu $!produced-value;

    # Perform BEGIN-time evaluation.
    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        nqp::bindattr(self, RakuAST::StatementPrefix::Phaser::Begin,
            '$!produced-value', self.IMPL-BEGIN-TIME-EVALUATE(self.blorst, $resolver, $context));
        Nil
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := $!produced-value;
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
    }
}

# The END phaser.
class RakuAST::StatementPrefix::Phaser::End is RakuAST::StatementPrefix::Phaser::Sinky
                                            is RakuAST::StatementPrefix::Thunky
                                            is RakuAST::Attaching {

    method attach(RakuAST::Resolver $resolver) {
        $resolver.find-attach-target('compunit').add-end-phaser(self);
    }
}
