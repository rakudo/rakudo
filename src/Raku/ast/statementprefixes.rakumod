# The base of all statement prefixes.
class RakuAST::StatementPrefix is RakuAST::Term {
    has RakuAST::Blorst $.blorst;

    method new(RakuAST::Blorst $blorst) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::StatementPrefix, '$!blorst', $blorst);
        $obj
    }

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

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        self.IMPL-CALLISH-QAST($context)
    }
}

# The `quietly` statement prefix.
class RakuAST::StatementPrefix::Quietly is RakuAST::StatementPrefix is RakuAST::SinkPropagator {
    method propagate-sink(Bool $is-sunk) {
        self.blorst.apply-sink($is-sunk);
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new(
            :op('handle'),
            self.IMPL-CALLISH-QAST($context),
            'WARN',
            QAST::Op.new( :op('resume'), QAST::Op.new( :op('exception') ) )
        )
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

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
        @lookups[0].IMPL-TO-QAST($context)
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
