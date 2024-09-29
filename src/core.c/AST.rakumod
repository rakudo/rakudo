# XXX: Would like to have this class as Perl6::AST, but ran up against
#      problems with the serialization context calling it that.
my class AST {
    has $!past;
    has $!quasi_context;
    has $!Str;

    submethod BUILD(:$past --> Nil) { $!past := $past }

    method incarnate($quasi_context, @unquote_asts) {
        my $incarnation = self.clone();
        nqp::bindattr(nqp::decont($incarnation), AST, '$!past', $incarnation.evaluate_unquotes(@unquote_asts));
        nqp::bindattr(nqp::decont($incarnation), AST, '$!quasi_context', $quasi_context);
        $incarnation;
    }

    method evaluate_unquotes(@unquote_asts) {
        my $pasts := nqp::list();
        for @unquote_asts {
            # TODO: find and report macro name
            X::TypeCheck::Splice.new(
                got      => $_,
                expected => AST,
                action   => 'unquote evaluation',
            ).throw unless nqp::istype($_,AST);
            nqp::push($pasts, nqp::getattr(nqp::decont($_), AST, '$!past'))
        }
        $!past.evaluate_unquotes($pasts);
    }

    method is_quasi_ast {
        so $!quasi_context;
    }

    method Str {
        $!Str;
    }
}

# vim: expandtab shiftwidth=4
