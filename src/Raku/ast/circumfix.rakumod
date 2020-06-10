# Marker for all kinds of circumfix.
class RakuAST::Circumfix is RakuAST::Term {
}

# Grouping parentheses circumfix.
class RakuAST::Circumfix::Parentheses is RakuAST::Circumfix {
    has RakuAST::SemiList $.semilist;

    method new(RakuAST::SemiList $semilist) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Circumfix::Parentheses, '$!semilist', $semilist);
        $obj
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        $!semilist.IMPL-TO-QAST($context)
    }

    method visit-children(Code $visitor) {
        $visitor($!semilist);
    }
}

# Array composer circumfix.
class RakuAST::Circumfix::ArrayComposer is RakuAST::Circumfix is RakuAST::Lookup {
    has RakuAST::SemiList $.semilist;

    method new(RakuAST::SemiList $semilist) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Circumfix::ArrayComposer, '$!semilist', $semilist);
        $obj
    }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-lexical('&circumfix:<[ ]>');
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
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
