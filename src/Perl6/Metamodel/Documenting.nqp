role Perl6::Metamodel::Documenting {
    has $!leading_docs;
    has $!trailing_docs;

    method set_leading_docs($new) {
        $!leading_docs := $new
    }
    method leading_docs() {
        $!leading_docs
    }
    method set_trailing_docs($new) {
        $!trailing_docs := $new
    }
    method trailing_docs() {
        $!trailing_docs
    }

    method docs() {
        # XXX WIP
        my $docs := $!leading_docs;
        if nqp::defined($!trailing_docs) {
            if nqp::defined($docs) {
                $docs := nqp::concat($docs, nqp::concat("\n", $!trailing_docs));
            } else {
                $docs := $!trailing_docs;
            }
        }

        $docs
    }
}
