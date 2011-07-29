role Perl6::Metamodel::Documenting {
    has $!docs;
    method set_docs($new) {
        $!docs := $new
    }
    method docs() {
        $!docs
    }
}
