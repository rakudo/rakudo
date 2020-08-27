class RakuAST::Term::Self is RakuAST::Term is RakuAST::Lookup {
    method new() {
        nqp::create(self)
    }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-lexical('self');
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        self.resolution.IMPL-LOOKUP-QAST($context)
    }
}

class RakuAST::Term::TopicCall is RakuAST::Term is RakuAST::ImplicitLookups {
    has RakuAST::Postfixish $.call;

    method new(RakuAST::Postfixish $call) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Term::TopicCall, '$!call', $call);
        $obj
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Var::Lexical.new('$_'),
        ])
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $topic := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0];
        $!call.IMPL-POSTFIX-QAST($context, $topic.resolution.IMPL-LOOKUP-QAST($context))
    }
}
