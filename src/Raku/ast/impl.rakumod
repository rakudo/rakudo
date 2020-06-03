# Rakudo-specific class used as part of the RakuAST to QAST
# translation.
class RakuAST::IMPL::QASTContext {
    has Mu $.sc;
    has Mu $.post-deserialize;

    method new(Mu :$sc!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::IMPL::QASTContext, '$!sc', $sc);
        nqp::bindattr($obj, RakuAST::IMPL::QASTContext, '$!post-deserialize', []);
        $obj
    }

    method ensure-sc(Mu $obj is raw) {
        if nqp::isnull(nqp::getobjsc($obj)) {
            my $sc := $!sc;
            nqp::setobjsc($obj, $sc);
            my int $idx := nqp::scobjcount($sc);
            nqp::scsetobj($sc, $idx, $obj);
        }
        $obj
    }

    method add-fixup-task(Mu $fixup-producer) {
        # TODO conditional on if we're doing precomp
        $!post-deserialize.push($fixup-producer());
    }
}
