role Perl6::Metamodel::Documenting {
    has $!leading_docs;
    method set_leading_docs($new) {
        $!leading_docs := $new
    }
    method leading_docs() {
        $!leading_docs
    }

    method docs() {
        $!leading_docs
    }
}
