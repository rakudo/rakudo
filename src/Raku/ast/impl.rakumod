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
        # Give the nested context its own post-deserialize bucket so
        # add-fixup-task pushes only land on the inner compunit, not the
        # shared outer one. Without this, compunit.rakumod has to force the
        # nested compunit's :post_deserialize to [] to avoid polluting the
        # outer's serialized fixups, which silently throws away the runtime
        # $!do bind that IMPL-LINK-META-OBJECT emits for the non-precomp
        # case.
        nqp::bindattr($context, RakuAST::IMPL::QASTContext, '$!post-deserialize', []);
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

    # Reconnect freshly-compiled code refs to the Code objects and SC slots
    # stashed during stubbing. :drain-compstuff-fixups is only needed when
    # the caller populated @!compstuff[3] (i.e. the non-nested non-precomp
    # branch of IMPL-LINK-META-OBJECT); nested/precomp callers leave it off
    # and null @!compstuff via cleanup-tasks instead. When $block-cuid is
    # passed, returns the matching code ref; otherwise returns Mu.
    method IMPL-FIXUP-COMPILED-CODEREFS(Mu $coderefs, $block-cuid?, :$drain-compstuff-fixups) {
        my int $n := nqp::elems($coderefs);
        my int $i := 0;
        my $result;
        while $i < $n {
            my $coderef := nqp::atpos($coderefs, $i);
            my $subid := nqp::getcodecuid($coderef);

            if nqp::existskey($!sub-id-to-code-object, $subid) {
                my $code-obj := $!sub-id-to-code-object{$subid};
                nqp::setcodeobj($coderef, $code-obj);
                nqp::bindattr($code-obj, Code, '$!do', $coderef);
                if $drain-compstuff-fixups {
                    my $fixups := nqp::getattr($code-obj, Code, '@!compstuff')[3];
                    if $fixups {
                        $fixups.pop() while $fixups.list;
                    }
                    nqp::bindattr($code-obj, Code, '@!compstuff', nqp::null());
                }
            }

            if nqp::existskey($!sub-id-to-cloned-code-objects, $subid) {
                for $!sub-id-to-cloned-code-objects{$subid} -> $code-obj {
                    my $clone := nqp::clone($coderef);
                    nqp::setcodeobj($clone, $code-obj);
                    nqp::bindattr($code-obj, Code, '$!do', $clone);
                    if $drain-compstuff-fixups {
                        my $fixups := nqp::getattr($code-obj, Code, '@!compstuff')[3];
                        if $fixups {
                            $fixups.pop() while $fixups.list;
                        }
                        nqp::bindattr($code-obj, Code, '@!compstuff', nqp::null());
                    }
                }
            }

            if nqp::existskey($!sub-id-to-sc-idx, $subid) {
                nqp::markcodestatic($coderef);
                nqp::scsetcode($!sc, $!sub-id-to-sc-idx{$subid}, $coderef);
            }

            if $block-cuid && $subid eq $block-cuid {
                $result := $coderef;
            }
            $i := $i + 1;
        }
        $result
    }
}

# Rakudo-specific class used for holding state used during interpretation of
# simple code at BEGIN time.
class RakuAST::IMPL::InterpContext {
    method new() {
        nqp::create(self)
    }
}

# Shared metamodel-archetype helpers, called from RakuAST nodes anywhere
# that needs to inspect a type object's archetypes. archetypes() must be
# called with the type as argument: DefiniteHOW and CoercionHOW stash the
# archetype in a type parameter and return a non-generic prototype when
# called bare (Metamodel/DefiniteHOW.nqp, Metamodel/CoercionHOW.nqp), so
# `$v.HOW.archetypes.generic` silently misreports for those HOWs. Routing
# through these helpers keeps callers from having to remember the
# argument form.
class RakuAST::IMPL::Archetypes {
    method is-generic(Mu $v) {
        nqp::can($v.HOW, 'archetypes')
            && $v.HOW.archetypes($v).generic
    }
}

# Builds a Scalar container descriptor, picking the Untyped variant for Mu
# nominals so STORE accepts NQP-typed values. Emulates create_container_descriptor
# in src/Perl6/World.nqp.
class RakuAST::IMPL::Containers {
    method create-descriptor(Mu :$of!, Mu :$default, int :$dynamic, :$name) {
        my $d := nqp::eqaddr($default, Mu) ?? $of !! $default;
        my $cd-type := nqp::eqaddr($of, Mu) ?? ContainerDescriptor::Untyped !! ContainerDescriptor;
        $cd-type.new(:$of, :default($d), :$dynamic, :$name)
    }
}
