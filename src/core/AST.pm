# XXX: Would like to have this class as Perl6::AST, but ran up against
#      problems with the serialization context calling it that.
my class AST {
    has $!past;
    has $!quasi_context;

    submethod BUILD(:$past) {
        $!past := $past;
    }

    method incarnate($quasi_context) {
        my $incarnation = self.clone();
        nqp::bindattr(nqp::p6decont($incarnation), AST, '$!quasi_context', $quasi_context);
        return $incarnation;
    }

    method is_quasi_ast {
        so $!quasi_context;
    }
}
