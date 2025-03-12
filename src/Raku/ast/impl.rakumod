# Rakudo-specific class used as part of the RakuAST to QAST
# translation.
class RakuAST::IMPL::QASTContext {
    has Mu $.sc;
    has Mu $.post-deserialize;
    has Mu $.code-ref-blocks;
    has int $!precompilation-mode;
    has Mu $.setting;

    # Mapping of sub IDs to their code objects; used for fixing up in
    # dynamic compilation.
    has Hash $!sub-id-to-code-object;

    # Mapping of sub IDs to any code objects that were cloned during
    # compilation before we had chance to compile the code. These are
    # not true closures (in those cases the surrounding scope that it
    # would close over is also compiled), but rather are clones for
    # things like proto method derivation.
    has Hash $!sub-id-to-cloned-code-objects;

    # Mapping of sub IDs to SC indexes of code stubs.
    has Hash $!sub-id-to-sc-idx;

    # Clean-up tasks, to do after CHECK time.
    has List $.cleanup-tasks;

    has int $.is-nested;
    has Mu $.language-revision; # Same type as in CORE-SETTING-REV

    method new(Mu :$sc!, int :$precompilation-mode, :$setting, :$language-revision) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::IMPL::QASTContext, '$!sc', $sc);
        nqp::bindattr_i($obj, RakuAST::IMPL::QASTContext, '$!precompilation-mode', $precompilation-mode);
        nqp::bindattr($obj, RakuAST::IMPL::QASTContext, '$!post-deserialize', []);
        nqp::bindattr($obj, RakuAST::IMPL::QASTContext, '$!code-ref-blocks', []);
        nqp::bindattr($obj, RakuAST::IMPL::QASTContext, '$!sub-id-to-code-object', {});
        nqp::bindattr($obj, RakuAST::IMPL::QASTContext, '$!sub-id-to-cloned-code-objects', {});
        nqp::bindattr($obj, RakuAST::IMPL::QASTContext, '$!sub-id-to-sc-idx', {});
        nqp::bindattr($obj, RakuAST::IMPL::QASTContext, '$!cleanup-tasks', []);
        nqp::bindattr_i($obj, RakuAST::IMPL::QASTContext, '$!is-nested', 0);
        nqp::bindattr($obj, RakuAST::IMPL::QASTContext, '$!setting', $setting);
        nqp::bindattr($obj, RakuAST::IMPL::QASTContext, '$!language-revision', $language-revision);
        $obj
    }

    method create-nested() {
        my $context := nqp::clone(self);
        nqp::bindattr($context, RakuAST::IMPL::QASTContext, '$!cleanup-tasks', []);
        nqp::bindattr_i($context, RakuAST::IMPL::QASTContext, '$!is-nested', 1);
        $context
    }

    # Get the handle of the serialization context.
    method sc-handle() {
        nqp::scgethandle($!sc)
    }

    # The langauge version we're compiling.
    # TODO use revision internally
    method lang-version() {
        nqp::chr(98 + $!language-revision.Int)
    }

    method is-moar() {
#?if moar
        True
#?endif
#?if !moar
        False
#?endif
    }

    method is-precompilation-mode() {
        $!precompilation-mode
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
        my int $code-ref-idx := nqp::elems($!code-ref-blocks);
        nqp::push($!code-ref-blocks, $block);
        nqp::scsetcode($!sc, $code-ref-idx, $code-ref);
        $!sub-id-to-sc-idx{$block.cuid} := $code-ref-idx;
    }

    # Run the passed fixup producer and add the QAST it returns to fixup tasks
    # only if we're not in pre-comp.
    method add-fixup-task(Mu $fixup-producer) {
        unless self.is-precompilation-mode {
            $!post-deserialize.push($fixup-producer());
        }
    }

    method add-deserialize-task(Mu $deserialize-producer) {
        if self.is-precompilation-mode {
            $!post-deserialize.push($deserialize-producer());
        }
    }

    # Run the passed QAST whether we are in a fixup or pre-comp'd deserialize
    # context.
    method add-fixup-and-deserialize-task(Mu $qast) {
        $!post-deserialize.push($qast);
    }

    method sub-id-to-code-object() {
        $!sub-id-to-code-object
    }

    method sub-id-to-sc-idx() {
        $!sub-id-to-sc-idx
    }

    method add-clone-for-cuid($clone, $cuid) {
        unless $!sub-id-to-cloned-code-objects{$cuid} {
            $!sub-id-to-cloned-code-objects{$cuid} := [];
        }
        $!sub-id-to-cloned-code-objects{$cuid}.push($clone);
    }

    method sub-id-to-cloned-code-objects() {
        $!sub-id-to-cloned-code-objects
    }

    method add-cleanup-task($task) {
        nqp::push($!cleanup-tasks, $task)
    }
}

# Rakudo-specific class used for holding state used during interpretation of
# simple code at BEGIN time.
class RakuAST::IMPL::InterpContext {
    method new() {
        nqp::create(self)
    }
}
