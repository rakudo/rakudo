# Rakudo-specific class used as part of the RakuAST to QAST
# translation.
class RakuAST::IMPL::QASTContext {
    has Mu $.sc;
    has Mu $.post-deserialize;
    has Mu $.code-ref-blocks;
    has int $!num-code-refs;

    method new(Mu :$sc!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::IMPL::QASTContext, '$!sc', $sc);
        nqp::bindattr($obj, RakuAST::IMPL::QASTContext, '$!post-deserialize', []);
        nqp::bindattr($obj, RakuAST::IMPL::QASTContext, '$!code-ref-blocks', []);
        nqp::bindattr_i($obj, RakuAST::IMPL::QASTContext, '$!num-code-refs', 0);
        $obj
    }

    # Get the handle of the serialization context.
    method sc-handle() {
        nqp::scgethandle($!sc)
    }

    # The langauge version we're compiling.
    # TODO implement this properly
    method lang-version() { 'd' }

    method is-moar() {
        nqp::getcomp('Raku').backend.name eq 'moar'
    }

    # Ensure that the passed object is in a serialization context.
    method ensure-sc(Mu $obj is raw) {
        if nqp::isnull(nqp::getobjsc($obj)) {
            my $sc := $!sc;
            nqp::setobjsc($obj, $sc);
            my int $idx := nqp::scobjcount($sc);
            nqp::scsetobj($sc, $idx, $obj);
        }
        $obj
    }

    method add-code-ref(Mu $code-ref, Mu $block) {
        my int $code-ref-idx := $!num-code-refs;
        nqp::bindattr_i(self, RakuAST::IMPL::QASTContext, '$!num-code-refs', $code-ref-idx + 1);
        nqp::push($!code-ref-blocks, $block);
        nqp::scsetcode($!sc, $code-ref-idx, $code-ref);
    }

    # Run the passed fixup producer and add the QAST it returns to fixup tasks
    # only if we're not in pre-comp.
    method add-fixup-task(Mu $fixup-producer) {
        # TODO conditional on if we're doing precomp
        $!post-deserialize.push($fixup-producer());
    }

    # Run the passed QAST whether we are in a fixup or pre-comp'd deserialize
    # context.
    method add-fixup-and-deserialize-task(Mu $qast) {
        $!post-deserialize.push($qast);
    }
}

# Rakudo-specific class used for holding state used during interpretation of
# simple code at BEGIN time.
class RakuAST::IMPL::InterpContext {
}
