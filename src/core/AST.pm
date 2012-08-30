# XXX: Would like to have this class as Perl6::AST, but ran up against
#      problems with the serialization context calling it that.
my class AST {
    has $!past;
    has $!quasi_context;

    submethod BUILD(:$past) {
        $!past := $past;
    }

    method incarnate($quasi_context, @unquote_asts) {
        my $incarnation = self.clone();
        nqp::bindattr(nqp::p6decont($incarnation), AST, '$!past', $incarnation.evaluate_unquotes(@unquote_asts));
        nqp::bindattr(nqp::p6decont($incarnation), AST, '$!quasi_context', $quasi_context);
        return $incarnation;
    }

    method evaluate_unquotes(@unquote_asts) {
        my $pasts := nqp::list();
        for @unquote_asts {
            # TODO: find and report macro name
            X::TypeCheck::Splice.new(
                got      => $_,
                expected => AST,
                action   => 'unquote evaluation',
            ).throw unless $_ ~~ AST;
            nqp::push($pasts, nqp::getattr(nqp::p6decont($_), AST, '$!past'))
        }
        $!past.evaluate_unquotes($pasts);
    }

    method is_quasi_ast {
        so $!quasi_context;
    }
}
