# Marker for all kinds of circumfix.
class RakuAST::Circumfix is RakuAST::Term is RakuAST::Contextualizable {
}

# Grouping parentheses circumfix.
class RakuAST::Circumfix::Parentheses is RakuAST::Circumfix {
    has RakuAST::SemiList $.semilist;

    method new(RakuAST::SemiList $semilist) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Circumfix::Parentheses, '$!semilist', $semilist);
        $obj
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        $!semilist.IMPL-TO-QAST($context)
    }

    method visit-children(Code $visitor) {
        $visitor($!semilist);
    }
}

# Array composer circumfix.
class RakuAST::Circumfix::ArrayComposer is RakuAST::Circumfix is RakuAST::Lookup
                                        is RakuAST::ColonPairish {
    has RakuAST::SemiList $.semilist;

    method new(RakuAST::SemiList $semilist) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Circumfix::ArrayComposer, '$!semilist', $semilist);
        $obj
    }

    method canonicalize() {
        my @parts;
        for self.IMPL-UNWRAP-LIST(self.semilist.statements) {
            nqp::die('canonicalize NYI for non-simple colonpairs: ' ~ $_.HOW.name($_))
                unless nqp::istype($_, RakuAST::Statement::Expression);
            nqp::push(@parts, "'" ~ $_.expression.literal-value ~ "'");
        }
        '[' ~ nqp::join('; ', @parts) ~ ']'
    }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-lexical('&circumfix:<[ ]>');
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $name := self.resolution.lexical-name;
        QAST::Op.new(
            :op('call'), :$name,
            $!semilist.IMPL-TO-QAST($context)
        )
    }

    method visit-children(Code $visitor) {
        $visitor($!semilist);
    }
}

# Hash composer circumfix. In Raku syntax, blocks and hash composers are
# distinguished based upon a number of criteria, applied after parsing the
# thing as a block. At the AST level there are two distinct node types:
# this, and Block (for the case it's a block). The Block node has a method
# on it for performing this disambiguation.
class RakuAST::Circumfix::HashComposer is RakuAST::Circumfix is RakuAST::Lookup {
    has RakuAST::Expression $.expression;

    method new(RakuAST::Expression $expression?) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Circumfix::HashComposer, '$!expression',
            $expression // RakuAST::Expression);
        $obj
    }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-lexical('&circumfix:<{ }>');
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $name := self.resolution.lexical-name;
        my $op := QAST::Op.new( :op('call'), :$name );
        if $!expression {
            $op.push($!expression.IMPL-TO-QAST($context));
        }
        $op
    }

    method visit-children(Code $visitor) {
        $visitor($!expression) if $!expression;
    }
}
