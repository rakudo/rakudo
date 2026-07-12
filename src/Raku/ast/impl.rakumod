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

    # Optional nested Perl6::World; when set, add-code-ref delegates
    # to it so the shared $!num_code_refs counter advances.
    has Mu $.world-bridge;

    # Per-cuid parse-time resolver snapshot, used by dynamic-EVAL fallback
    # and role-body lexical fixup. Held here rather than on the AST so
    # nodes don't retain Resolver instances (and drag MVMContext into the
    # SC). Drained at CompUnit cleanup time in precompilation mode so the
    # serialized bytecode does not pin Resolver state; post-precomp load
    # of that bytecode then degrades to the call-site scope. Outside
    # precomp the map lives for the QASTContext's lifetime.
    has Hash $!cuid-to-parse-time-resolver;
    has Bool $!parse-time-resolver-cleanup-scheduled;

    # Code objects whose IMPL-STUB-CODE bound a freshcoderef to
    # Code.$!do but whose IMPL-LINK-META-OBJECT has not yet registered
    # that coderef with the SC. If the owning AST is discarded before
    # QAST emission (e.g. block-or-hash repurposing a Block as a Hash
    # composer), the code object is left with a non-SC'd coderef that
    # breaks serialization if anything else pulls it into the SC.
    # cleanup-orphan-stubs nulls Code.$!do for whatever still sits here
    # right before the QAST::CompUnit is handed to the backend.
    # Keyed by code object identity so AST nodes that share a meta-object
    # (a phaser StatementPrefix and its blorst Block) produce one entry
    # that either node's finalize call clears.
    has Hash $!stubbed-code-objects;

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
        nqp::bindattr($obj, RakuAST::IMPL::QASTContext, '$!world-bridge', Mu);
        nqp::bindattr($obj, RakuAST::IMPL::QASTContext, '$!cuid-to-parse-time-resolver', {});
        nqp::bindattr($obj, RakuAST::IMPL::QASTContext, '$!stubbed-code-objects', {});
        $obj
    }

    method set-world-bridge(Mu $world) {
        nqp::bindattr(self, RakuAST::IMPL::QASTContext, '$!world-bridge', $world);
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
        nqp::bindattr($context, RakuAST::IMPL::QASTContext, '$!cuid-to-parse-time-resolver', {});
        nqp::bindattr($context, RakuAST::IMPL::QASTContext, '$!stubbed-code-objects', {});
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

    method register-stubbed-code-object(Mu $code-obj) {
        my str $key := ~nqp::objectid($code-obj);
        $!stubbed-code-objects{$key} := $code-obj;
        Nil
    }

    method has-stubbed-code-object(Mu $code-obj) {
        nqp::existskey($!stubbed-code-objects, ~nqp::objectid($code-obj))
    }

    method mark-code-object-finalized(Mu $code-obj) {
        my str $key := ~nqp::objectid($code-obj);
        nqp::deletekey($!stubbed-code-objects, $key);
        Nil
    }

    # Null Code.$!do on every still-unfinalized code object so the
    # serializer cannot follow it to the orphan stub freshcoderef.
    method cleanup-orphan-stubs() {
        for $!stubbed-code-objects {
            nqp::bindattr(nqp::iterval($_), Code, '$!do', nqp::null());
        }
        Nil
    }

    method add-code-ref(Mu $code-ref, Mu $block) {
        my int $code-ref-idx;
        if nqp::isconcrete($!world-bridge) {
            $code-ref-idx := $!world-bridge.add_root_code_ref($code-ref, $block);
        }
        else {
            $code-ref-idx := nqp::elems($!code-ref-blocks);
            nqp::push($!code-ref-blocks, $block);
            nqp::scsetcode($!sc, $code-ref-idx, $code-ref);
        }
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

    method record-parse-time-resolver(str $cuid, $resolver) {
        $!cuid-to-parse-time-resolver{$cuid} := $resolver;
        if !$!parse-time-resolver-cleanup-scheduled && self.is-precompilation-mode {
            nqp::bindattr(self, RakuAST::IMPL::QASTContext,
              '$!parse-time-resolver-cleanup-scheduled', True);
            my $ctx := self;
            self.add-cleanup-task({
                nqp::bindattr($ctx, RakuAST::IMPL::QASTContext,
                  '$!cuid-to-parse-time-resolver', {})
            });
        }
        Nil
    }

    method parse-time-resolver(str $cuid) {
        nqp::existskey($!cuid-to-parse-time-resolver, $cuid)
          ?? $!cuid-to-parse-time-resolver{$cuid}
          !! Mu
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
    # Optional compile pipeline state. IMPL-BEGIN-TIME-CALL and
    # IMPL-BEGIN-TIME-EVALUATE set these when they have them. A node's
    # IMPL-INTERPRET can read them to forward to meta-object, so
    # subclasses like RakuAST::Type::Parameterized can evaluate
    # arguments without compile-time values via IMPL-BEGIN-TIME-EVALUATE.
    has Mu $.resolver;
    has Mu $.context;

    method new(:$resolver, :$context) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::IMPL::InterpContext, '$!resolver', $resolver);
        nqp::bindattr($obj, RakuAST::IMPL::InterpContext, '$!context', $context);
        $obj
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
