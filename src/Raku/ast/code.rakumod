# A blockoid represents the block part of some kind of code declaration.
class RakuAST::Blockoid
  is RakuAST::SinkPropagator
  is RakuAST::BeginTime
{
    has RakuAST::StatementList $.statement-list;

    method new(RakuAST::StatementList $statement-list?) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Blockoid, '$!statement-list',
            $statement-list // RakuAST::StatementList.new);
        $obj
    }

    method propagate-sink(Bool $is-sunk) {
        $!statement-list.propagate-sink($is-sunk, :has-block-parent(True))
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        $!statement-list.to-begin-time($resolver, $context); # In case it's the default we created in the ctor.
        Nil
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context, :$immediate) {
        my $stmts := $!statement-list.IMPL-TO-QAST($context);
        if nqp::elems($stmts.list) == 0 {
            $stmts.push(QAST::WVal.new( :value(Nil) ));
        }
        $stmts
    }

    method visit-children(Code $visitor) {
        $visitor($!statement-list);
    }

    method IMPL-CAN-INTERPRET() {
        $!statement-list.IMPL-CAN-INTERPRET
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        $!statement-list.IMPL-INTERPRET($ctx)
    }
}

class RakuAST::OnlyStar
  is RakuAST::Blockoid
  is RakuAST::Term
{
    method new() {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Blockoid, '$!statement-list',
          RakuAST::StatementList.new);
        $obj
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context, :$immediate) {
        nqp::findmethod(RakuAST::Expression, 'IMPL-TO-QAST')(self, $context)
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        True  # `{*}` dispatches to candidates, so it is never useless when sunk
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        # The dispatch op is the only code in an onlystar body, and a
        # frame's location, as reported by Code.file and Code.line, comes
        # from its first annotation. Carry the origin on a statement node
        # so the frame is annotated at all. Without one the location
        # degrades to the bytecode file, which breaks consumers that
        # recognize setting routines by the SETTING:: file prefix, the
        # way Routine.IS-SETTING-ONLY-D does for the smartmatch
        # dispatcher's junction handling.
        self.IMPL-SET-NODE(
            QAST::Stmts.new(
                QAST::Op.new(
                    :op('dispatch'),
                    QAST::SVal.new( :value('boot-resume') ),
                    QAST::IVal.new( :value(nqp::const::DISP_ONLYSTAR) ))),
            :key);
    }

    method IMPL-REGEX-TOP-LEVEL-QAST(
      RakuAST::IMPL::QASTContext  $context,
                              Mu  $code-object,
                                  %mods,
                             int :$no-scan,
                              Mu :$body-qast,
                             str :$name
    ) {
        QAST::Op.new(
            :op('callmethod'), :name('!protoregex'),
            QAST::Var.new( :name('self'), :scope('local') ),
            QAST::SVal.new( :value($name) ))
    }
}

# Marker for all code-y things.
class RakuAST::Code
  is RakuAST::ParseTime
{
    has Bool $.custom-args;
    has Mu $!qast-block;
    has str $!cuid;

    # A BEGIN-time use forces compilation ahead of the unit's optimize
    # walk, and the QAST block that compilation forms is cached. These
    # record that the cache was formed early, and with what arguments,
    # so the block can be re-formed once the marks are known.
    has int $!begin-time-cached;
    # Set when a dynamic compilation forms this block, so the sites that
    # hand out a closure of it or declare it bind its do to the running
    # compilation.
    has int $!dynamically-compiled;
    has str $!begin-cache-blocktype;
    has Mu $!begin-cache-expression;

    # A control-flow statement (if/unless/with/without/while/until/loop) runs
    # its branch inline, so `&?BLOCK` inside it means the enclosing real block,
    # not the branch. Such branches are marked here to be skipped when locating
    # the block `&?BLOCK` refers to.
    has int $!immediate-block-user-body;

    # Set once this block has grown the implicit `&?BLOCK` lexical, so a second
    # reference does not add a duplicate declaration.
    has int $!has-block-variable;

    method PERFORM-PARSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        Nil
    }

    method set-immediate-block-user-body() {
        nqp::bindattr_i(self, RakuAST::Code, '$!immediate-block-user-body', 1);
    }
    method is-immediate-block-user-body() { $!immediate-block-user-body }

    # Ensure this block declares the implicit `&?BLOCK` lexical, bound to its
    # own code object. A reference to `&?BLOCK` requests this on the innermost
    # enclosing real block, so inner control-flow branches resolve `&?BLOCK`
    # lexically to it regardless of whether they end up as their own frame.
    method IMPL-ENSURE-BLOCK-VARIABLE() {
        unless $!has-block-variable {
            nqp::bindattr_i(self, RakuAST::Code, '$!has-block-variable', 1);
            self.add-generated-lexical-declaration(
                RakuAST::VarDeclaration::Implicit::CurrentBlock.new);
        }
    }

    method IMPL-EXTRA-BEGIN-TIME-DECLS(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        []
    }

    method set-custom-args() {
        nqp::bindattr(self, RakuAST::Code, '$!custom-args', True);
    }

    method IMPL-CLOSURE-QAST(RakuAST::IMPL::QASTContext $context, Bool :$regex) {
        my $code-obj := self.meta-object;
        $context.ensure-sc($code-obj);
        self.IMPL-QAST-BLOCK($context, :blocktype<declaration_static>);
        my $clone := QAST::Op.new(
            :op('callmethod'), :name('clone'),
            QAST::WVal.new( :value($code-obj) ).annotate_self('past_block', $!qast-block).annotate_self('code_object', $code-obj)
        );
        self.IMPL-TWEAK-REGEX-CLONE($context, $clone) if $regex;
        my $closure := QAST::Op.new( :op('p6capturelex'), $clone );
        $!dynamically-compiled && !$context.is-precompilation-mode
            ?? QAST::Stmts.new(self.IMPL-DYNAMIC-DO-REBIND-QAST($context), $closure)
            !! $closure
    }

    # A dynamic compilation binds the do it produced, whose outer is a
    # snapshot of the compile-time scope. A unit run as a script emits the
    # same block again, and a closure captured at BEGIN time keeps running
    # the dynamic frame, so both compilations stay live at runtime. Each
    # site that clones the code object first binds its do to the block of
    # the compilation that is running, so the clone captures that frame.
    # A clone registered before the dynamic compilation never captured a
    # frame, so it gets a fresh do from that block as well. A precompiled
    # unit needs none of this: loading it binds every do to its own
    # frames.
    method IMPL-DYNAMIC-DO-REBIND-QAST(RakuAST::IMPL::QASTContext $context) {
        my $code-obj := self.meta-object;
        my $block := $!qast-block;
        nqp::die('IMPL-DYNAMIC-DO-REBIND-QAST needs the QAST block formed first')
            unless $block;
        my $stmts := QAST::Stmts.new(
            QAST::Op.new(
                :op('bindattr'),
                QAST::WVal.new( :value($code-obj) ),
                QAST::WVal.new( :value(Code) ),
                QAST::SVal.new( :value('$!do') ),
                QAST::BVal.new( :value($block) )
            )
        );
        my %clones := $context.sub-id-to-cloned-code-objects();
        if nqp::existskey(%clones, $!cuid) {
            for %clones{$!cuid} -> $clone {
                $context.ensure-sc($clone);
                my $tmp := $stmts.unique('dynamic_do');
                $stmts.push(QAST::Stmt.new(
                    QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :name($tmp), :scope('local'), :decl('var') ),
                        QAST::Op.new( :op('clone'), QAST::BVal.new( :value($block) ) )
                    ),
                    # The fresh do takes its code object before the clone
                    # binds it, so a call racing this rebind never runs a
                    # do that answers for the original.
                    QAST::Op.new(
                        :op('setcodeobj'),
                        QAST::Var.new( :name($tmp), :scope('local') ),
                        QAST::WVal.new( :value($clone) )
                    ),
                    QAST::Op.new(
                        :op('bindattr'),
                        QAST::WVal.new( :value($clone) ),
                        QAST::WVal.new( :value(Code) ),
                        QAST::SVal.new( :value('$!do') ),
                        QAST::Var.new( :name($tmp), :scope('local') )
                    )
                ));
            }
        }
        $stmts
    }

    method IMPL-QAST-BLOCK(RakuAST::IMPL::QASTContext $context, str :$blocktype,
            RakuAST::Expression :$expression) {
        unless ($!qast-block) {
            self.IMPL-FINISH-CODE-OBJECT($context, :$blocktype, :$expression);
        }
        self.IMPL-MAYBE-REBUILD-BEGIN-TIME-CACHED-BLOCK($context);
        $!qast-block
    }

    # Which code nodes take the re-formation.
    method IMPL-REBUILD-ELIGIBLE() { 0 }

    method IMPL-BEGIN-TIME-CACHED() { $!begin-time-cached }

    # The re-formation runs only between a unit's optimize walk and that
    # unit's emission, never inside a dynamic compilation.
    method IMPL-MAYBE-REBUILD-BEGIN-TIME-CACHED-BLOCK(RakuAST::IMPL::QASTContext $context) {
        if $!begin-time-cached && $context.optimize-performed
            && self.IMPL-REBUILD-ELIGIBLE
            && !nqp::ifnull(nqp::getlexdyn('$*IMPL-COMPILE-DYNAMICALLY'), 0) {
            self.IMPL-REBUILD-BEGIN-TIME-CACHED-BLOCK($context);
        }
        Nil
    }

    method IMPL-FINISH-CODE-OBJECT(RakuAST::IMPL::QASTContext $context, str :$blocktype,
            RakuAST::Expression :$expression) {
        my $block := self.IMPL-QAST-FORM-BLOCK($context, :$blocktype, :$expression);
        self.IMPL-LINK-META-OBJECT($context, $block);
        nqp::bindattr(self, RakuAST::Code, '$!qast-block', $block);
        if nqp::ifnull(nqp::getlexdyn('$*IMPL-COMPILE-DYNAMICALLY'), 0) {
            nqp::bindattr_i(self, RakuAST::Code, '$!begin-time-cached', 1);
            nqp::bindattr_i(self, RakuAST::Code, '$!dynamically-compiled', 1);
            if self.IMPL-REBUILD-ELIGIBLE {
                nqp::bindattr_s(self, RakuAST::Code, '$!begin-cache-blocktype', $blocktype);
                nqp::bindattr(self, RakuAST::Code, '$!begin-cache-expression', $expression);
            }
        }
    }

    # Re-form the cached QAST block from the AST once the optimize walk
    # has set its marks, grafting the result into the same QAST::Block
    # object. The graft keeps the object every registration holds: the
    # code ref block list, the compstuff fixups, and the cuid all keep
    # resolving to the block the unit emits.
    # A context serialized by the early compilation rebinds its lexicals
    # by name into this frame at load, so the unused implicits keep
    # their names as bare slots during this emission.
    method IMPL-REBUILD-BEGIN-TIME-CACHED-BLOCK(RakuAST::IMPL::QASTContext $context) {
        nqp::bindattr_i(self, RakuAST::Code, '$!begin-time-cached', 0);
        my $block := $!qast-block;
        if nqp::can(self, 'signature') && nqp::isconcrete(self.signature) {
            self.signature.IMPL-RESET-SIGNATURE-PARAMS();
        }
        if nqp::can(self, 'placeholder-signature') && nqp::isconcrete(self.placeholder-signature) {
            self.placeholder-signature.IMPL-RESET-SIGNATURE-PARAMS();
        }
        my $*EMIT-BEGIN-SHAPE := 1;
        my $formed := self.IMPL-QAST-FORM-BLOCK($context,
            :blocktype($!begin-cache-blocktype),
            :expression($!begin-cache-expression));
        $block.set_children($formed.list);
        $block.arity($formed.arity);
        $block.custom_args($formed.custom_args);
        $block.has_exit_handler($formed.has_exit_handler);
        nqp::bindattr($block, QAST::Block, '%!symbol',
            nqp::getattr($formed, QAST::Block, '%!symbol'));
        nqp::bindattr($block, QAST::Block, '%!local_debug_map',
            nqp::getattr($formed, QAST::Block, '%!local_debug_map'));
        my %ann := nqp::getattr($formed, QAST::Node, '%!annotations');
        if nqp::ishash(%ann) {
            for %ann {
                $block.annotate(nqp::iterkey_s($_), nqp::iterval($_));
            }
        }
        Nil
    }

    method IMPL-STUB-CODE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $code-obj := self.meta-object;
        nqp::bindattr_s(self, RakuAST::Code, '$!cuid', QAST::Block.next-cuid());

        # Stash it under the QAST block unique ID.
        my str $cuid := $!cuid;
        $context.sub-id-to-code-object(){$cuid} := $code-obj;

        $context.record-parse-time-resolver($cuid, $resolver.clone);

        my $precomp;
        my $compiler-thunk := {
            my $*IMPL-COMPILE-DYNAMICALLY := 1;
            # This emission caches the QAST block before the unit's
            # optimize phase has rewritten the tree and decided which
            # lexicals become locals, and the unit's own emission reuses
            # the cache. Optimize and decide for this code object here so
            # both compilations agree. A block cached by an earlier
            # formation is emitted as it is.
            unless $!qast-block {
                self.IMPL-OPTIMIZE-AHEAD-OF-UNIT($resolver, $context);
                RakuAST::IMPL::VarLowering.analyze-routine(self, $resolver);
            }
            my $block := self.IMPL-QAST-BLOCK($context, :blocktype<declaration_static>);
            $precomp := self.IMPL-COMPILE-DYNAMICALLY($resolver, $context, $block);
        };
        my $stub := nqp::freshcoderef(sub (*@pos, *%named) {
            my $code-obj := nqp::getcodeobj(nqp::curcode());
            unless $precomp {
                $compiler-thunk();
            }
            unless nqp::isnull($code-obj) {
                return $code-obj(|@pos, |%named);
            }
        });

        nqp::bindattr($code-obj, Code, '$!do', $stub);
        nqp::markcodestatic($stub);
        nqp::markcodestub($stub);
        nqp::setcodeobj($stub, $code-obj);

        # Create the compiler stuff array and stick it in the code object.
        # Also add clearup task to remove it again later.
        my @compstuff;
        nqp::bindattr($code-obj, Code, '@!compstuff', @compstuff);
        $context.add-cleanup-task(sub () {
            nqp::bindattr($code-obj, Code, '@!compstuff', nqp::null());
        });

        @compstuff[1] := $compiler-thunk; # Used by multi-dispatcher to force compilation
        @compstuff[2] := sub ($orig, $clone) {
            my $do := nqp::getattr($clone, Code, '$!do');
            nqp::markcodestub($do);
            $context.add-cleanup-task(sub () {
                nqp::bindattr($clone, Code, '@!compstuff', nqp::null());
            });
            $context.add-clone-for-cuid($clone, $cuid);
        }

        $context.register-stubbed-code-object($code-obj);

        $stub
    }

    method IMPL-LINK-META-OBJECT(RakuAST::IMPL::QASTContext $context, Mu $block) {
        # Obtain the meta-object and connect it to the code block.
        my $code-obj := self.meta-object;
        $context.ensure-sc($code-obj);

        # Associate QAST block with code object, which will ensure it is
        # fixed up as needed.
        $block.code_object($code-obj);

        my @compstuff := nqp::getattr($code-obj, Code, '@!compstuff');
        # @!compstuff is null on a re-compile of a shared AST: the
        # previous compile's cleanup nulled it and $!begin-performed
        # keeps IMPL-STUB-CODE from re-running.
        if nqp::isnull(@compstuff) {
            @compstuff := nqp::list();
            nqp::bindattr($code-obj, Code, '@!compstuff', @compstuff);
        }
        my $cuid := $!cuid;
        $block.set-cuid($!cuid);

        # A code object stubbed during another compilation (a tree from
        # Str.AST handed to EVAL, for example) is unknown to this context,
        # so IMPL-FIXUP-COMPILED-CODEREFS would never bind its freshly
        # compiled code ref. Register it here so the fixup finds it.
        my %sub-id-to-code-object := $context.sub-id-to-code-object();
        unless nqp::existskey(%sub-id-to-code-object, $cuid) {
            %sub-id-to-code-object{$cuid} := $code-obj;
        }

        # The stubbing context schedules the cleanup that nulls @!compstuff.
        # When another compilation links the code object, that task lives on
        # a context that never finalizes (Str.AST abandons its parse context)
        # or already ran, so this compilation must null the compiler state
        # itself or the compiler thunk leaks into the serialized graph.
        unless $context.has-stubbed-code-object($code-obj) {
            $context.add-cleanup-task(sub () {
                nqp::bindattr($code-obj, Code, '@!compstuff', nqp::null());
            });
        }

        my $fixups := QAST::Stmts.new();
        unless $context.is-precompilation-mode {
            # We need to do a fixup of the code block for the non-precompiled case.
            $fixups.push(
                QAST::Op.new(
                    :op('bindattr'),
                    QAST::WVal.new( :value($code-obj) ),
                    QAST::WVal.new( :value(Code) ),
                    QAST::SVal.new( :value('$!do') ),
                    QAST::BVal.new( :value($block) )
                )
            );
            my $push-clone-fixup := sub ($clone) {
                my $tmp := $fixups.unique('tmp_block_fixup');
                $fixups.push(QAST::Stmt.new(
                    QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :name($tmp), :scope('local'), :decl('var') ),
                        QAST::Op.new( :op('clone'), QAST::BVal.new( :value($block) ) )
                    ),
                    QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new(
                            :name('$!do'), :scope('attribute'),
                            QAST::WVal.new( :value($clone) ),
                            QAST::WVal.new( :value(Code) )
                        ),
                        QAST::Var.new( :name($tmp), :scope('local') ),
                    ),
                    QAST::Op.new(
                        :op('setcodeobj'),
                        QAST::Var.new( :name($tmp), :scope('local') ),
                        QAST::WVal.new( :value($clone) )
                    )));
            };
            @compstuff[2] := sub ($orig, $clone) {
                $context.ensure-sc($clone);
                $context.add-cleanup-task(sub () {
                    nqp::bindattr($clone, Code, '@!compstuff', nqp::null());
                });
                $context.add-clone-for-cuid($clone, $cuid);
                $push-clone-fixup($clone);
            }
            # A clone made before this point, by BEGIN-time code such as
            # `.wrap` cloning the routine it wraps, was registered by the
            # stub-time callback, which has no code block to build a fixup
            # from. Give each one the same load-time replacement of its
            # compile-time do, so it closes over the runtime frames.
            my %clones := $context.sub-id-to-cloned-code-objects();
            if nqp::existskey(%clones, $cuid) {
                for %clones{$cuid} -> $clone {
                    $context.ensure-sc($clone);
                    $push-clone-fixup($clone);
                }
            }
            @compstuff[3] := $fixups;
        }

        @compstuff[0] := $block;

        $context.add-code-ref(nqp::getattr($code-obj, Code, '$!do'), $block);

        $context.add-fixup-task(-> {
            $fixups
        });

        $context.mark-code-object-finalized($code-obj);
    }

    method IMPL-FIXUP-DYNAMICALLY-COMPILED-BLOCK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context, Mu $block) {
        # Parse-time resolver if still cached; else call-site resolver.
        my $parse-time-resolver := $context.parse-time-resolver($!cuid) || $resolver;
        my $visit-block;
        my $visit-children;

        my @blocks;
        my $current-block;
        $visit-block := sub ($block) {
            nqp::push(@blocks, $current-block := nqp::hash);
            $visit-children($block);
            nqp::pop(@blocks);
            $current-block := nqp::elems(@blocks) ?? @blocks[nqp::elems(@blocks) - 1] !! NQPMu;
        }

        my %seen;
        my $declared-in-cu := sub ($name) {
            for @blocks {
                if nqp::existskey($_, $name) {
                    %seen{$name} := 1;
                    return 1;
                }
            }
            return 0;
        }

        my $visit-var := sub ($var) {
            my str $scope := $var.scope;
            my str $decl := $var.decl;
            my str $name := $var.name;
            if $scope eq 'attribute' || $scope eq 'attributeref' || $scope eq 'positional' || $scope eq 'associative' {
                $visit-children($var);
            }
            if $decl {
                $current-block{$name} := $var;
                if $decl eq 'param' {
                    $visit-children($var);
                    my $default := $var.default;
                    if $default {
                        $visit-children(QAST::Stmts.new($default));
                    }
                }
            }
            else {
                if ($scope eq 'lexical' || $scope eq 'lexicalref')
                  && ! $declared-in-cu($name) && !%seen{$name} {
                    my $value := $var.ann('compile-time-value');
                    if !($value =:= NQPMu) {
                        %seen{$name} := 1;
                        $block[0].push(
                            QAST::Var.new(:scope<lexical>, :decl<static>, :$name, :$value)
                        );
                    }
                    # The specials $_, $/, $! and $¢ live in a frame of the
                    # calling code, but the declaration they resolve to
                    # produces a fresh container as its compile-time value.
                    # Skipping them keeps their references late-bound, so
                    # the runtime lookup reaches the caller's container.
                    elsif $name ne '$_' && $name ne '$/' && $name ne '$!' && $name ne '$¢' {
                        my $lexical := $parse-time-resolver.resolve-lexical($name);
                        my $of := $lexical && nqp::istype($lexical, RakuAST::VarDeclaration::Simple)
                            ?? $lexical.IMPL-OF-TYPE
                            !! Mu;
                        # A native scalar has no container to share, and a
                        # native slot cannot hold a static value for runtime
                        # frames to copy. Declare a fresh slot of the same
                        # native type. A write to it stays local to this
                        # compiled code.
                        if $lexical && (my int $prim-spec := nqp::objprimspec($of)) && $lexical.sigil eq '$' {
                            $context.ensure-sc($of);
                            %seen{$name} := 1;
                            my $slot := QAST::Var.new(
                                :scope<lexical>, :decl<var>, :$name, :returns($of)
                            );
                            # An int or num slot starts out as 0, but a str
                            # slot starts out as a VM-level null string, so
                            # bind its empty-string default explicitly.
                            $block[0].push($prim-spec == 3
                                ?? QAST::Op.new(:op('bind'), $slot, QAST::SVal.new(:value('')))
                                !! $slot
                            );
                        }
                        # Any other by-reference use stays late-bound for
                        # the runtime lookup to satisfy.
                        elsif $scope eq 'lexicalref' {
                        }
                        elsif $lexical
                          && !nqp::istype($lexical, RakuAST::Declaration::External)
                          && !nqp::istype($lexical, RakuAST::CompileTimeValue)
                          && !nqp::eqat($name, '!__REGEX_CAPTURE_', 0)
                          # A dynamic variable (a `*` twigil) resolves by a
                          # runtime stack-walk, not from a lexical container, so
                          # leave it to the handling below.
                          && nqp::substr($name, 1, 1) ne '*' {
                            # A declared but non-constant lexical in an active
                            # scope (for example a sigilless `my \x` or a `my
                            # $x`), unbound at this BEGIN point. Reproduce the
                            # value it holds there, as the legacy frontend does
                            # by referring to it in place.
                            if nqp::index('$@%&', nqp::substr($name, 0, 1)) >= 0 {
                                # A sigil'd variable has a container. When the
                                # declaration can produce it, bind that very
                                # container here: the declaration's frame need
                                # not exist at BEGIN time (a routine's, for a
                                # trait argument like `is memoized(my %h)`),
                                # and runtime frames start from a copy of it,
                                # so a BEGIN-time write is visible at runtime.
                                # An anonymous declaration has no name for
                                # other code to share the container through,
                                # so it, and anything else without a producible
                                # container, stays late-bound for the runtime
                                # lookup to satisfy.
                                if nqp::istype($lexical, RakuAST::VarDeclaration::Simple)
                                    && !nqp::istype($lexical, RakuAST::VarDeclaration::Anonymous)
                                    && !nqp::objprimspec($lexical.meta-object) {
                                    my $value := $lexical.meta-object;
                                    $context.ensure-sc($value);
                                    %seen{$name} := 1;
                                    $block[0].push(
                                        QAST::Var.new(:scope<lexical>, :decl<static>, :$name, :$value)
                                    );
                                }
                            }
                            else {
                                # A sigilless binding has no container, so a
                                # late-bound lookup would yield a raw VMNull. Its
                                # unbound value is Mu, so inline that.
                                my $mu := $parse-time-resolver.resolve-lexical-constant('Mu');
                                my $value := $mu ?? $mu.compile-time-value !! Mu;
                                $context.ensure-sc($value);
                                %seen{$name} := 1;
                                $block[0].push(
                                    QAST::Var.new(:scope<lexical>, :decl<static>, :$name, :$value)
                                );
                            }
                        }
                        else {
                            my $decl := $parse-time-resolver.resolve-lexical-constant($name);
                            if $decl {
                                $decl.to-begin-time($resolver, $context); # Ensure any required lookups are resolved
                                my $value := $decl.compile-time-value;
                                $context.ensure-sc($value);
                                %seen{$name} := 1;
                                $block[0].push(
                                    QAST::Var.new(:scope<lexical>, :decl<static>, :$name, :$value)
                                );
                            }
                            elsif nqp::eqat($name, '!__REGEX_CAPTURE_', 0) {
                                # A regex capture lexical is bound and used within
                                # this compiled unit, but its declaration lives in
                                # an enclosing scope outside the unit. Declare it
                                # here, as that scope otherwise would.
                                %seen{$name} := 1;
                                $block[0].push(
                                    QAST::Var.new(:scope<lexical>, :decl<var>, :$name)
                                );
                            }
                            else {
                                nqp::die("Could not find a compile-time-value for lexical $name");
                            }
                        }
                    }
                }
            }
            $var
        }

        # A flattened body block lands in the frame as a statement list
        # rather than a block of its own, and a run may skip it.
        my int $flattened := 0;

        $visit-children := sub ($node) {
            my int $i := 0;
            my int $n := nqp::elems($node);
            while $i < $n {
                my $visit := $node[$i];
                $visit := $visit.shallow_clone if nqp::istype($visit, QAST::Node);
                $node[$i] := $visit;
                if nqp::istype($visit, QAST::Op) {
                    my $op := $visit.op;
                    if ($op eq 'call' || $op eq 'callstatic' || $op eq 'chain' || $op eq 'chainstatic') && $visit.name {
                        if ! $declared-in-cu($visit.name) {
                            my $routine := $parse-time-resolver.resolve-lexical-constant($visit.name);
                            if $routine {
                                my $value := $routine.compile-time-value;
                                $context.ensure-sc($value);
                                $visit.name(nqp::null);
                                $visit.unshift(QAST::WVal.new(:$value));
                            }
                            elsif nqp::elems(@blocks) == 2 && !$flattened {
                                # we're in top level block (excluding wrapper) and not in a
                                # flattened body, so routines would definitely get called.
                                # Can't do so if we couldn't find it.
                                $resolver.build-exception(
                                    'X::Undeclared::Symbols',
                                    :unk_routines(nqp::hllizefor(nqp::hash($visit.name, [self.origin ?? self.origin.as-match.line !! -1]), 'Raku'))
                                ).throw
                            } # else leave in the runtime lookup for post-declared subs
                        }
                    }
                    $visit-children($visit)
                }
                elsif nqp::istype($visit, QAST::Block) {
                    $visit-block($visit);
                }
                elsif nqp::istype($visit, QAST::Stmt) || nqp::istype($visit, QAST::Stmts) || nqp::istype($visit, QAST::ParamTypeCheck) {
                    my int $body := $visit.ann('flattened-body') ?? 1 !! 0;
                    $flattened := $flattened + 1 if $body;
                    $visit-children($visit);
                    $flattened := $flattened - 1 if $body;
                }
                elsif nqp::istype($visit, QAST::Var) {
                    $node[$i] := $visit-var($visit);
                }
                else {
                }
                $i := $i + 1;
            }
        }

        $visit-block($block);
    }

    # The optimize walk over one code object compiled ahead of the
    # unit's optimize phase, with the parse-time resolver, when one was
    # recorded, so names resolve in the scope the code object was
    # declared in.
    method IMPL-OPTIMIZE-AHEAD-OF-UNIT(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $walk-resolver := $context.parse-time-resolver($!cuid) || $resolver;
        # The check phase settles sink states only after this compilation,
        # and the walk and the lowering read them.
        self.IMPL-CALCULATE-SINK();
        my $*NO-CT-DISPATCH := nqp::existskey(nqp::getenvhash(), 'RAKUDO_NO_CT_DISPATCH');
        my int $enclosing-ahead      := $walk-resolver.IMPL-AHEAD-OF-UNIT-WALK;
        my int $enclosing-structural := $walk-resolver.IMPL-STRUCTURAL-WALK;
        my int $scope-depth          := $walk-resolver.IMPL-SCOPE-DEPTH;
        my int $package-depth        := $walk-resolver.IMPL-PACKAGE-DEPTH;
        $walk-resolver.IMPL-SET-AHEAD-OF-UNIT-WALK(1,
            nqp::istrue(nqp::ifnull(nqp::getlexdyn('$*COMPILING_CORE_SETTING'), 0)) ?? 1 !! 0);
        # The resolver outlives this walk, so its stacks and flags go back
        # to what they were, whether the walk returns or throws.
        {
            CATCH {
                $walk-resolver.IMPL-UNWIND-SCOPES($scope-depth);
                $walk-resolver.IMPL-UNWIND-PACKAGES($package-depth);
                $walk-resolver.IMPL-SET-AHEAD-OF-UNIT-WALK($enclosing-ahead, $enclosing-structural);
                nqp::rethrow($_);
            }
            self.IMPL-OPTIMIZE($walk-resolver);
        }
        $walk-resolver.IMPL-SET-AHEAD-OF-UNIT-WALK($enclosing-ahead, $enclosing-structural);
        Nil
    }

    method IMPL-COMPILE-DYNAMICALLY(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context, Mu $block) {
        nqp::bindattr_i(self, RakuAST::Code, '$!dynamically-compiled', 1);
        my $wrapper := QAST::Block.new(QAST::Stmts.new(), nqp::clone($block));
        $wrapper.annotate('DYN_COMP_WRAPPER', 1);

        # Parse-time resolver if still cached; else call-site resolver.
        my $parse-time-resolver := $context.parse-time-resolver($!cuid) || $resolver;
        my $package := $parse-time-resolver.current-package;
        $context.ensure-sc($package);

        my $comp-unit := $resolver.find-attach-target("compunit");
        # When $comp-unit.is-eval, all required declarations will be included in QAST when
        # ForeignCode::EVAL calls RakuAST::CompUnit::IMPL-TO-QAST-COMP-UNIT.
        # Other forms of dynamic compilation (CHECK, most notably) need to manually add the
        # $comp-unit's implicit declarations to the QAST pre-amble. This is in order for CHECK-time
        # lexical lookups into UNIT::<*> to resolve even as the $comp-unit is still in the
        # process of compiling.
        if ! $comp-unit.is-eval && nqp::elems(my @decls := $comp-unit.PRODUCE-IMPLICIT-DECLARATIONS // []) {
            for @decls {
                if nqp::istype($_, RakuAST::VarDeclaration::Implicit) {
                    # CompUnit's generated $?PACKAGE points to the generated GLOBAL, so we update it here.
                    # (Otherwise all sorts of subtle side effects occur with eg `use experimental :pack`)
                    $_.set-value($package) if $_.lexical-name eq '$?PACKAGE';
                    $wrapper[0].push($_.IMPL-QAST-DECL($context))
                }
            }
        } else {
            $wrapper[0].push(QAST::Var.new(
                :name('$_'), :scope('lexical'),
                :decl('contvar'), :value(Mu)
            ));
            $wrapper[0].push(QAST::Var.new(
                :name('$/'), :scope('lexical'),
                :decl('contvar'), :value(Nil)
            ));
            $wrapper[0].push(QAST::Var.new(
                :name('$?PACKAGE'), :scope('lexical'),
                :decl('static'), :value($package)
            ));
        }

        # Mark the frame as code of the unit being compiled, so a begin-time
        # indirect lookup can consult the compiler's resolver from it. The
        # marker is this compilation's identity token: foreign code run
        # during a begin-time effect has no frame carrying it, and a frame
        # from another compilation carries a different one.
        my $marker := $context.begin-time-marker;
        $context.ensure-sc($marker);
        $wrapper[0].push(QAST::Var.new(
            :name('!BEGIN_TIME_MARKER'), :scope('lexical'),
            :decl('static'), :value($marker)
        ));

        for self.IMPL-EXTRA-BEGIN-TIME-DECLS($resolver, $context) {
            # A code node the compiled block takes a closure of, declared
            # here as the scope enclosing that block would in a unit.
            if nqp::istype($_, RakuAST::Code) && !nqp::istype($_, RakuAST::Declaration) {
                $wrapper[0].push($_.IMPL-QAST-DECL-CODE($context));
            }
            elsif nqp::istype($_, RakuAST::CompileTimeValue) {
                my $value := $_.compile-time-value;
                $context.ensure-sc($value);
                $wrapper[0].push(QAST::Var.new(
                    :name($_.lexical-name), :scope('lexical'),
                    :decl('static'), :$value)
                );
            }
        }

        self.IMPL-FIXUP-DYNAMICALLY-COMPILED-BLOCK($resolver, $context, $wrapper);

        my $qast-compunit := QAST::CompUnit.new(
            :hll('Raku'),
            :sc($context.sc()),
            :compilation_mode(0),
            $wrapper
        );
        my $comp := $*HLL-COMPILER // nqp::getcomp("Raku");
        my $from := $comp.qast-stage;
        my $precomp := $comp.compile($qast-compunit, :$from, :compunit_ok(1));
        my $mainline := $comp.backend.compunit_mainline($precomp);
        # Wire the wrapper's outer to the resolver's setting so :name lookups
        # for setting symbols resolve instead of returning VMNull.  Anchored
        # on the setting (always reaches setting symbols) rather than the
        # dynamic caller (which may not, under nested AST EVAL).  Same trick
        # ForeignCode::EVAL uses for AST-form EVAL.
        my $outer-ctx := $resolver.setting;
        nqp::forceouterctx($mainline, $outer-ctx)
          unless nqp::isnull($outer-ctx);
        $mainline();

        # Fix up Code object associations (including nested blocks).
        # We un-stub any code objects for already-compiled inner blocks
        # to avoid wasting re-compiling them, and also to help make
        # parametric role outer chain work out. Also set up their static
        # lexpads, if they have any.
        my @coderefs := $comp.backend.compunit_coderefs($precomp);
        $context.IMPL-FIXUP-COMPILED-CODEREFS(@coderefs, $block.cuid, :drain-compstuff-fixups)
    }


    # Some things get cloned many times with an outer lexical scope that
    # we never enter. This makes sure we capture them as needed.

    # When code runs at BEGIN time, such as role bodies and BEGIN
    # blocks, we need to ensure we get lexical outers fixed up
    # properly when deserializing after pre-comp. To do this we
    # make a list of closures, which each point to the outer
    # context. These survive serialization and thus point at what
    # has to be fixed up.
    method IMPL-BEGIN-TIME-LEXICAL-FIXUP(RakuAST::IMPL::QASTContext $context, Mu $block, RakuAST::LexicalFixup $lexical-fixup) {
        my $has_nested_blocks := 0;
        my $todo := nqp::list($block);
        while $todo {
            my $stmts := nqp::shift($todo);
            for @($stmts) {
                if nqp::istype($_, QAST::Block) {
                    $has_nested_blocks := 1;
                    last;
                }
                if nqp::istype($_, QAST::Stmts) {
                    nqp::push($todo, $_);
                }
            }
        }
        return [] unless $has_nested_blocks;

        # Parse-time resolver from the QASTContext side table; this path
        # runs at compile time and the caller doesn't have its own
        # resolver to fall back to.
        my $resolver := $context.parse-time-resolver($!cuid);
        my $throwaway_block_ast := RakuAST::Block.new(:!implicit-topic);
        $throwaway_block_ast.set-implicit-topic(0);
        $throwaway_block_ast.set-no-implicit-match();
        $throwaway_block_ast.to-begin-time($resolver, $context);
        my $throwaway_block_past := $throwaway_block_ast.IMPL-QAST-BLOCK($context, :blocktype<declaration>);
        $throwaway_block_past.name('!LEXICAL_FIXUP');
        $throwaway_block_past.annotate('outer', $block);
        $block[1].push($throwaway_block_past);
        my $throwaway_block := $throwaway_block_ast.meta-object;
        $context.ensure-sc($throwaway_block);

        # Create a list and put it in the SC.
        my $fixup_list := FixupList.new($context.sc-handle());
        $context.ensure-sc($fixup_list);

        # Set up capturing code.
        my $c_block_ast := RakuAST::Block.new(:!implicit-topic);
        $c_block_ast.set-no-implicit-match();
        $c_block_ast.to-begin-time($resolver, $context);
        my $c_block := $c_block_ast.IMPL-QAST-BLOCK($context, :blocktype<declaration_static>);
        $c_block.name('!LEXICAL_FIXUP_CSCOPE');
        $context.ensure-sc($c_block);

        # Return a QAST node that we can push the dummy closure.
        my $fixup := QAST::Op.new(
            :op('callmethod'), :name('add_unresolved'),
            QAST::WVal.new( :value($fixup_list) )
        );

        $fixup.push(QAST::Op.new(
                :op('p6capturelex'),
                QAST::Op.new(
                    :op('callmethod'), :name('clone'),
                    QAST::WVal.new( :value($throwaway_block) ).annotate_self('past_block', $throwaway_block_past).annotate_self('code_object', $throwaway_block)
                )));
        $block[1].push($fixup);

        $lexical-fixup.set-block($c_block_ast, $fixup_list);
        [$throwaway_block_past, $fixup]
    }

    method IMPL-APPEND-SIGNATURE-RETURN(RakuAST::IMPL::QASTContext $context, Mu $qast-stmts) {
        my $signature := self.signature;
        if $signature && $signature.provides-return-value {
            $qast-stmts.push($signature.returns.IMPL-TO-QAST($context));
        }
        $qast-stmts
    }

    method needs-sink-call() { False }

    method signature() { Nil }
}

class RakuAST::LexicalFixup
  is RakuAST::Declaration
{
    has RakuAST::Block $!block;
    has FixupList $!fixup-list;

    method new() {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::LexicalFixup, '$!block', RakuAST::Block);
        nqp::bindattr($obj, RakuAST::LexicalFixup, '$!fixup-list', LexicalFixup);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', 'my');
        $obj
    }

    method set-block(RakuAST::Block $block, FixupList $fixup-list) {
        nqp::bindattr(self, RakuAST::LexicalFixup, '$!block', $block);
        nqp::bindattr(self, RakuAST::LexicalFixup, '$!fixup-list', $fixup-list);
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        if $!block {
            QAST::Stmts.new(
                $!block.IMPL-QAST-DECL-CODE($context),
                QAST::Op.new(
                    :op('callmethod'), :name('resolve'),
                    QAST::WVal.new( :value($!fixup-list) ),
                    QAST::Op.new( :op('takeclosure'), $!block.IMPL-QAST-BLOCK($context, :blocktype<declaration_static>)),
                )
            )
        }
        else {
            QAST::Stmt.new;
        }
    }

    method lexical-name() { '' }
}

# The base of all expression thunks, which produce a code object of some kind
# that wraps the thunk.
class RakuAST::ExpressionThunk
  is RakuAST::Code
  is RakuAST::Meta
  is RakuAST::BeginTime
{
    has RakuAST::ExpressionThunk $.next;
    has RakuAST::Signature $!signature;

    # A callback producing QAST (or Mu) to run at the start of the thunk body,
    # before the wrapped expression. A callback, not stored QAST, because its
    # content (a loop's NEXT phasers) is only known once the body compiles.
    has Mu $!prelude-producer;

    # The expression the block was formed around, for a compilation of
    # the thunk on its own.
    has RakuAST::Expression $!formed-expression;

    method new() {
        nqp::create(self)
    }

    method set-next(RakuAST::ExpressionThunk $next) {
        nqp::bindattr(self, RakuAST::ExpressionThunk, '$!next', $next);
        Nil
    }

    method IMPL-SET-PRELUDE-PRODUCER(Mu $producer) {
        nqp::bindattr(self, RakuAST::ExpressionThunk, '$!prelude-producer', $producer);
        Nil
    }

    method thunk-kind() {
        self.HOW.name(self)
    }

    method thunk-details() {
        ''
    }

    method declare-topic() {
        False
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.IMPL-STUB-CODE($resolver, $context);

        Nil
    }

    # Called to produce the QAST::Block for the thunk, which should be pushed
    # into the passed `$target`. If there is a next thunk in `$!next` then it
    # should be compiled recursively and the expression passed along; otherwise,
    # the expression itself should be compiled and used as the body.
    # An expression that is itself a code node, as `try` of a bare
    # statement is, has the thunk body take a closure of it, and the
    # scope enclosing the thunk is what declares its block. A thunk
    # compiled on its own, as a constant's value is, has that
    # compilation's wrapper declare the block instead.
    method IMPL-EXTRA-BEGIN-TIME-DECLS(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        nqp::istype($!formed-expression, RakuAST::Code) ?? [$!formed-expression] !! []
    }

    method IMPL-THUNK-CODE-QAST(RakuAST::IMPL::QASTContext $context, Mu $target,
            RakuAST::Expression $expression) {

        my $block := self.IMPL-QAST-BLOCK($context, :$expression);
        # Link and push the produced code block.
        $target.push($block);
    }

    method IMPL-QAST-FORM-BLOCK(RakuAST::IMPL::QASTContext $context,
            str :$blocktype, RakuAST::Expression :$expression!) {
        nqp::bindattr(self, RakuAST::ExpressionThunk, '$!formed-expression', $expression);
        # From the block, compiling the signature.
        my $signature := self.IMPL-GET-OR-PRODUCE-SIGNATURE;
        my $stmts := QAST::Stmts.new();
        for self.IMPL-UNWRAP-LIST($signature.parameters) {
            $stmts.push($_.target.IMPL-QAST-DECL($context)) if $_.target.lexical-name ne '$_' || self.declare-topic;
        }
        $stmts.push($signature.IMPL-QAST-BINDINGS($context));
        my $block :=
            self.IMPL-SET-NODE(
                QAST::Block.new(
                    :blocktype('declaration_static'),
                    $stmts),
                :key);
        $stmts := QAST::Stmts.new();
        if nqp::istype(self, RakuAST::ImplicitDeclarations) {
            for self.IMPL-UNWRAP-LIST(self.get-implicit-declarations()) -> $decl {
                if $decl.is-simple-lexical-declaration {
                    nqp::push($stmts, $decl.IMPL-QAST-DECL($context));
                }
            }
        }
        if nqp::istype($expression, RakuAST::ImplicitDeclarations) {
            for self.IMPL-UNWRAP-LIST($expression.get-implicit-declarations()) -> $decl {
                if nqp::istype($decl, RakuAST::VarDeclaration::Implicit::State) && $decl.is-simple-lexical-declaration {
                    nqp::push($stmts, $decl.IMPL-QAST-DECL($context));
                }
            }
        }
        # A `my`-scoped anonymous variable declared in the expression can never
        # be named from outside, so it belongs to this thunk. Emit its
        # declaration here rather than leaving it in the enclosing scope, where a
        # thunk compiled on its own (as a constant's value is) would not find its
        # storage. State variables keep their own persistence machinery.
        my $anon-decl := -> $node {
            nqp::istype($node, RakuAST::VarDeclaration::Anonymous) && $node.scope eq 'my'
        };
        if $anon-decl($expression) {
            nqp::push($stmts, $expression.IMPL-QAST-DECL($context));
        }
        my @code-todo := [$expression];
        while @code-todo {
            my $visit := @code-todo.shift;
            $visit.visit-children: -> $node {
                if nqp::istype($node, RakuAST::ImplicitDeclarations) {
                    for self.IMPL-UNWRAP-LIST($node.get-implicit-declarations()) -> $decl {
                        if nqp::istype($decl, RakuAST::VarDeclaration::Implicit::State) && $decl.is-simple-lexical-declaration {
                            nqp::push($stmts, $decl.IMPL-QAST-DECL($context));
                        }
                    }
                }
                if $anon-decl($node) {
                    nqp::push($stmts, $node.IMPL-QAST-DECL($context));
                }
                # A signature literal's block owns the compiled code its
                # meta-object links as $!code; an enclosing statement scope
                # emits that declaration itself but stops at this thunk's
                # block boundary, so emit it here or the signature reaches
                # runtime with no code to bind under.
                if nqp::istype($node, RakuAST::FakeSignature) {
                    nqp::push($stmts, $node.block.IMPL-QAST-DECL-CODE($context));
                }
                unless nqp::istype($node, RakuAST::LexicalScope) {
                    @code-todo.push($node);
                }
            }
        }

        my $nested-blocks := $expression.IMPL-QAST-NESTED-BLOCK-DECLS($context);
        $stmts.push($nested-blocks) if nqp::elems($nested-blocks.list);

        $block.push($stmts) if $stmts.list;
        $block.arity($signature.arity);

        if nqp::isconcrete($!prelude-producer) {
            my $prelude := $!prelude-producer($context);
            $block.push($prelude) if nqp::isconcrete($prelude);
        }

        # If there's an inner thunk the body evaluates to that.
        if $!next {
            $!next.IMPL-THUNK-CODE-QAST($context, $block[nqp::elems($block) - 1], $expression);
            my $value := $!next.IMPL-THUNK-VALUE-QAST($context);
            $block.push($value) if $value;
        }

        # Otherwise, we evaluate to the expression.
        else {
            my $qast := self.IMPL-THUNK-TWEAK-EXPRESSION($context,
                $expression.IMPL-EXPR-QAST($context));
            $qast := QAST::Op.new( :op('p6sink'), $qast ) if $expression.needs-sink-call && $expression.sunk;
            $block.push($qast);
        }

        $block
    }

    # Produces a Code object that corresponds to the thunk.
    method IMPL-THUNK-VALUE-QAST(RakuAST::IMPL::QASTContext $context) {
        my $qast := self.IMPL-CLOSURE-QAST($context);
        $qast.annotate('thunked', 1);
        $qast;
    }

    # The type of code object produced. Defaults to Code; override to produce
    # something else.
    method IMPL-THUNK-OBJECT-TYPE() { Code }

    # The signature for the code object produced. Defaults to the empty
    # signature; override to produce something else
    method IMPL-THUNK-SIGNATURE() {
        RakuAST::Signature.new
    }

    # A method to tweak the expression QAST that is produced. Override it
    # to do such a tweak.
    method IMPL-THUNK-TWEAK-EXPRESSION(RakuAST::IMPL::QASTContext $context, Mu $qast) {
        $qast
    }

    # A callback for when the thunk meta-object is produced, potentially to
    # update some other meta-object that wants to reference it.
    method IMPL-THUNK-META-OBJECT-PRODUCED(Mu $meta) {
    }

    method IMPL-GET-OR-PRODUCE-SIGNATURE() {
        $!signature // nqp::bindattr(self, RakuAST::ExpressionThunk, '$!signature',
            self.IMPL-THUNK-SIGNATURE)
    }

    method PRODUCE-META-OBJECT(:$resolver, :$context) {
        my $code := nqp::create(self.IMPL-THUNK-OBJECT-TYPE);
        my $signature := self.IMPL-GET-OR-PRODUCE-SIGNATURE;
        nqp::bindattr($code, Code, '$!signature', $signature.meta-object);
        nqp::bindattr($signature.meta-object, Signature, '$!code', $code);
        self.IMPL-THUNK-META-OBJECT-PRODUCED($code);
        $code
    }

    method IMPL-UPDATE-SIGNATURE() {
        return Nil unless self.has-meta-object;

        my $code := self.meta-object;
        nqp::bindattr(self, RakuAST::ExpressionThunk, '$!signature', self.IMPL-THUNK-SIGNATURE);
        my $signature := $!signature;
        nqp::bindattr($code, Code, '$!signature', $signature.meta-object);
        nqp::bindattr($signature.meta-object, Signature, '$!code', $code);
        self.IMPL-THUNK-META-OBJECT-PRODUCED($code);
    }
}

# A code object that can have placeholder parameters.
class RakuAST::PlaceholderParameterOwner
  is RakuAST::Node
{
    # Any placeholder parameters that have been attached
    has Mu $!attached-placeholder-parameters;

    # A map grouping placeholder parameters by name, for error checking and
    # compilation.
    has Mu $!placeholder-map;

    # Cached generated placeholder signature.
    has RakuAST::Signature $!placeholder-signature;

    method IMPL-IS-IN-METHOD() {
        False
    }

    method add-placeholder-parameter(RakuAST::VarDeclaration::Placeholder $placeholder) {
        unless nqp::islist($!attached-placeholder-parameters) {
            nqp::bindattr(self, RakuAST::PlaceholderParameterOwner,
                '$!attached-placeholder-parameters', []);
        }
        my $name := $placeholder.lexical-name;
        if self.IMPL-HAS-PARAMETER($name) || (self.IMPL-IS-IN-METHOD || nqp::istype(self, RakuAST::Methodish)) && $name eq '%_' {
            # matches an explicitly declared parameter
            $placeholder.IMPL-ALREADY-DECLARED(True);
        }
        else {
            for $!attached-placeholder-parameters {
                if $_.lexical-name eq $name {
                    # same placeholder is used multiple times
                    $placeholder.IMPL-ALREADY-DECLARED(True);
                    return Nil
                }
            }
            nqp::push($!attached-placeholder-parameters, $placeholder);
        }
        Nil
    }

    method has-placeholder-parameters() {
        my $params := $!attached-placeholder-parameters;
        nqp::islist($params) && nqp::elems($params) ?? True !! False
    }

    method IMPL-HAS-PARAMETER(Str $name) {
        False
    }

    method IMPL-PLACEHOLDER-MAP() {
        unless nqp::ishash($!placeholder-map) {
            my %map;
            if self.has-placeholder-parameters {
                for $!attached-placeholder-parameters -> $param {
                    my str $key := $param.lexical-name;
                    (%map{$key} || (%map{$key} := [])).push($param);
                }
            }
            nqp::bindattr(self, RakuAST::PlaceholderParameterOwner,
                '$!placeholder-map', %map);
        }
        $!placeholder-map
    }

    # Gets the placeholder signature. Only reliable after resolution has taken
    # place.
    method placeholder-signature() {
        # Return Nil if there isn't one to generate, or the cached one if we have
        # it.
        return Nil unless self.has-placeholder-parameters();
        return $!placeholder-signature if $!placeholder-signature;

        # Group and sort parameters.
        my @positionals;
        my @nameds;
        my @slurpies;
        for self.IMPL-PLACEHOLDER-MAP() {
            my $placeholder := $_.value[0];
            if nqp::istype($placeholder, RakuAST::VarDeclaration::Placeholder::Positional) {
                my int $insert-at;
                my str $desigil-insert := nqp::substr($placeholder.lexical-name, 1);
                while $insert-at < nqp::elems(@positionals) {
                    my str $desigil-cur := nqp::substr(@positionals[$insert-at].lexical-name, 1);
                    last if $desigil-insert lt $desigil-cur;
                    ++$insert-at;
                }
                nqp::splice(@positionals, [$placeholder], $insert-at, 0);
            }
            elsif nqp::istype($placeholder, RakuAST::VarDeclaration::Placeholder::Named) {
                my int $insert-at;
                my str $desigil-insert := nqp::substr($placeholder.lexical-name, 1);
                while $insert-at < nqp::elems(@nameds) {
                    my str $desigil-cur := nqp::substr(@nameds[$insert-at].lexical-name, 1);
                    last if $desigil-insert lt $desigil-cur;
                    ++$insert-at;
                }
                nqp::splice(@nameds, [$placeholder], $insert-at, 0);
            }
            else {
                if $placeholder.lexical-name eq '@_' { # @_ before %_
                    @slurpies.unshift($placeholder);
                }
                else {
                    @slurpies.push($placeholder);
                }
            }
        }

        # Add to signature.
        my @parameters;
        for @positionals, @nameds, @slurpies -> @placeholders {
            for @placeholders {
                @parameters.push($_.generate-parameter());
            }
        }
        my $signature := RakuAST::Signature.new(:@parameters);
        nqp::bindattr(self, RakuAST::PlaceholderParameterOwner,
            '$!placeholder-signature', $signature);
        $signature
    }
}

class RakuAST::ScopePhaser {
    has Bool $!has-exit-handler;
    has Bool $!is-loop-body;
    has List $!ENTER;
    has List $!LEAVE;
    has List $!KEEP;
    has List $!LEAVE-ORDER;
    has List $!UNDO;
    has List $!FIRST;
    has List $!NEXT;
    has List $!LAST;
    has List $!PRE;
    has List $!POST;
    has List $!QUIT;
    has List $!TEMP; # Really not yet implemented.
    has List $!CLOSE;
    has RakuAST::Block $!let;
    has RakuAST::Block $!temp;
    has int $!next-enter-phaser-result;
    has int $!needs-result;

    # The FIRST trigger container is minted once per phaser owner, so a
    # formation that runs again reuses it instead of leaving another
    # serialized container behind.
    has Mu $!first-trigger-container;

    method add-phaser(
      Str $name,
      RakuAST::StatementPrefix::Phaser $phaser,
      :$has-exit-handler
    ) {
        my $attr := '$!' ~ $name;
        my $list := nqp::getattr(self, RakuAST::ScopePhaser, $attr);
        $list := nqp::bindattr(self, RakuAST::ScopePhaser, $attr, [])
          unless $list;

        for $list {
            if nqp::eqaddr($_, $phaser) {
                return;
            }
        }
        nqp::push($list, $phaser);
        nqp::bindattr(self, RakuAST::ScopePhaser, '$!has-exit-handler', True)
          if $has-exit-handler;
    }

    method IMPL-ADD-PHASER-TO-LEAVE-ORDER(Str $type, RakuAST::StatementPrefix::Phaser $phaser) {
        my $list := nqp::getattr(self, RakuAST::ScopePhaser, '$!LEAVE-ORDER');
        $list := nqp::bindattr(self, RakuAST::ScopePhaser, '$!LEAVE-ORDER', [])
          unless $list;

        for $list {
            if nqp::eqaddr($_, $phaser) {
                return;
            }
        }
        nqp::push($list, [$type, $phaser]);
    }

    method add-leave-phaser(RakuAST::StatementPrefix::Phaser $phaser) {
        self.add-phaser('LEAVE', $phaser, :has-exit-handler(True));
        self.IMPL-ADD-PHASER-TO-LEAVE-ORDER('LEAVE', $phaser);
    }

    method add-keep-phaser(RakuAST::StatementPrefix::Phaser $phaser) {
        self.add-phaser('KEEP', $phaser, :has-exit-handler(True));
        self.IMPL-ADD-PHASER-TO-LEAVE-ORDER('KEEP', $phaser);
    }

    method add-undo-phaser(RakuAST::StatementPrefix::Phaser $phaser) {
        self.add-phaser('UNDO', $phaser, :has-exit-handler(True));
        self.IMPL-ADD-PHASER-TO-LEAVE-ORDER('UNDO', $phaser);
    }

    # The scope names the phaser's result lexical here. A phaser node
    # added outside a parse never reaches begin time, so it has to supply
    # its own meta-object.
    method add-enter-phaser(RakuAST::StatementPrefix::Phaser::Enter $phaser) {
        self.add-phaser('ENTER', $phaser);
        $phaser.set-result-name('__enter_phaser_result_' ~ $!next-enter-phaser-result);
        nqp::bindattr_i(self, RakuAST::ScopePhaser, '$!next-enter-phaser-result', $!next-enter-phaser-result + 1);
        Nil
    }

    method set-needs-result(Bool $needs-result) {
        nqp::bindattr_i(self, RakuAST::ScopePhaser, '$!needs-result', $needs-result ?? 1 !! 0);
    }

    method needs-result() {
        return 1 if $!needs-result;
        if nqp::istype(self, RakuAST::Meta) {
            my $phasers := nqp::getattr(self.meta-object, Block, '$!phasers');
            nqp::ishash($phasers) && (
                nqp::existskey($phasers, 'UNDO')
                || nqp::existskey($phasers, 'KEEP')
                || nqp::existskey($phasers, 'POST')
            ) ?? 2 !! 0
        }
        else {
            0
        }
    }

    method set-has-let() {
        nqp::bindattr(self, RakuAST::ScopePhaser, '$!has-exit-handler', True);
        nqp::bindattr(self, RakuAST::ScopePhaser, '$!let', RakuAST::Block.new(:implicit-topic(False)));
    }

    method set-has-temp() {
        nqp::bindattr(self, RakuAST::ScopePhaser, '$!has-exit-handler', True);
        nqp::bindattr(self, RakuAST::ScopePhaser, '$!temp', RakuAST::Block.new(:implicit-topic(False)));
    }

    # Primarily used to detect whether a phaser has been applied appropriately (some only work on loops)
    method set-is-loop-body() {
        nqp::bindattr(self, RakuAST::ScopePhaser, '$!is-loop-body', True);
    }

    method is-loop-body() {
        nqp::getattr(self, RakuAST::ScopePhaser, '$!is-loop-body') // False
    }

    method has-loop-phasers() {
        return True if $!FIRST || $!NEXT || $!LAST;
        if nqp::istype(self, RakuAST::Meta) {
            my $phasers := nqp::getattr(self.meta-object, Block, '$!phasers');
            nqp::ishash($phasers) && (
                nqp::existskey($phasers, 'FIRST')
                || nqp::existskey($phasers, 'NEXT')
                || nqp::existskey($phasers, 'LAST')
            ) ?? True !! False
        }
        else {
            False
        }
    }

    method has-any-phasers() {
        return True
          if $!ENTER || $!LEAVE || $!KEEP  || $!UNDO || $!FIRST || $!NEXT
          || $!LAST  || $!PRE   || $!POST  || $!QUIT || $!TEMP  || $!CLOSE
          || $!let   || $!temp;
        if nqp::istype(self, RakuAST::Meta) {
            nqp::isconcrete(nqp::getattr(self.meta-object, Block, '$!phasers'))
              ?? True !! False
        }
        else {
            False
        }
    }

    method add-list-to-code-object(Str $attr, $code-object) {
        my $list := nqp::getattr(self, RakuAST::ScopePhaser, $attr);
        if $list {
            my $name := nqp::substr($attr,2);  # $!FOO -> FOO
            for $list {
                $code-object.add_phaser($name, $_.meta-object);
            }
        }
    }

    method add-phasers-to-code-object($code-object) {
        self.add-list-to-code-object('$!ENTER', $code-object);
        self.add-list-to-code-object('$!FIRST', $code-object);
        self.add-list-to-code-object( '$!NEXT', $code-object);
        self.add-list-to-code-object( '$!LAST', $code-object);
        self.add-list-to-code-object( '$!QUIT', $code-object);
        self.add-list-to-code-object(  '$!PRE', $code-object);
        self.add-list-to-code-object( '$!POST', $code-object);
        self.add-list-to-code-object('$!CLOSE', $code-object);

        if $!LEAVE-ORDER {
            for $!LEAVE-ORDER {
                $code-object.add_phaser($_[0], $_[1].meta-object);
            }
        }

        if $!let {
            $code-object.add_phaser('UNDO', $!let.meta-object);
        }
        if $!temp {
            $code-object.add_phaser('LEAVE', $!temp.meta-object);
        }
    }

    # A phaser fires as the code object the routine holds, with no
    # closure site of its own, so a routine compiled dynamically binds
    # each phaser's do to the running compilation on entry. A phaser
    # node that is not code itself, such as QUIT, hands its blorst out
    # as the code object.
    method IMPL-PHASER-DO-REBINDS(RakuAST::IMPL::QASTContext $context) {
        my $stmts := QAST::Stmts.new;
        my @nodes;
        for '$!ENTER', '$!FIRST', '$!NEXT', '$!LAST', '$!QUIT', '$!PRE', '$!POST', '$!CLOSE' {
            my $list := nqp::getattr(self, RakuAST::ScopePhaser, $_);
            if $list {
                nqp::push(@nodes, $_) for $list;
            }
        }
        if $!LEAVE-ORDER {
            nqp::push(@nodes, $_[1]) for $!LEAVE-ORDER;
        }
        nqp::push(@nodes, $!let) if $!let;
        nqp::push(@nodes, $!temp) if $!temp;
        for @nodes {
            # A thunked phaser with a block body shares that block's code
            # object, so the block is the node the dynamic compilation
            # marked. A phaser that is not code itself, such as QUIT,
            # hands its blorst out as the code object too.
            my $code := nqp::can($_, 'blorst') && nqp::istype($_.blorst, RakuAST::Code)
                ?? $_.blorst
                !! nqp::istype($_, RakuAST::Code) ?? $_ !! Mu;
            next unless nqp::isconcrete($code);
            $code.IMPL-QAST-BLOCK($context, :blocktype<declaration_static>);
            if nqp::getattr_i($code, RakuAST::Code, '$!dynamically-compiled') {
                $stmts.push($code.IMPL-DYNAMIC-DO-REBIND-QAST($context));
            }
        }
        $stmts
    }

    method add-phasers-handling-code(RakuAST::IMPL::Context $context, Mu $qast) {
        my $block := nqp::istype(self, RakuAST::Code) ?? self.meta-object !! NQPMu;
        my $phasers := nqp::isconcrete($block) ?? nqp::getattr($block, Block, '$!phasers') !! NQPMu;

        unless $context.is-precompilation-mode {
            my $rebinds := self.IMPL-PHASER-DO-REBINDS($context);
            $qast[0].push($rebinds) if nqp::elems($rebinds.list);
        }

        if $!has-exit-handler || self.needs-result > 1 || $phasers && (nqp::istype($phasers, Code) || nqp::existskey($phasers, 'LEAVE') || nqp::existskey($phasers, 'POST')) {
            $qast.has_exit_handler(1);
        }

        if $!PRE || $phasers && nqp::ishash($phasers) && nqp::existskey($phasers, 'PRE') {
            my $pre-setup := QAST::Stmts.new;
            my %seen;
            if $!PRE {
                for $!PRE {
                    $pre-setup.push($_.IMPL-CALLISH-QAST($context));
                    %seen{nqp::objectid($_.meta-object)} := 1;
                }
            }
            if $block {
                my $pre-phasers := $block.phasers('PRE');
                if nqp::isconcrete($pre-phasers) {
                    for $pre-phasers.FLATTENABLE_LIST {
                        unless %seen{nqp::objectid($_)} {
                            $context.ensure-sc($_);
                            $pre-setup.push(QAST::Op.new(:op<call>, QAST::WVal.new(:value($_))));
                        }
                    }
                }
            }

            $qast[0].push(QAST::Op.new( :op('p6setpre') ));
            $qast[0].push($pre-setup);
            $qast[0].push(QAST::Op.new( :op('p6clearpre') ));
        }

        if $!FIRST || $phasers && nqp::ishash($phasers) && nqp::existskey($phasers, 'FIRST') {
            my $first-setup := QAST::Stmts.new;
            my $calls := QAST::Stmts.new(
                QAST::Op.new(:op<call>, :name<&infix:<=>>,
                    QAST::Var.new(:scope<lexical>, :name<!phaser_first_triggered>),
                    QAST::WVal.new(:value(True))
                )
            );
            my $container := $!first-trigger-container;
            unless nqp::isconcrete($container) {
                my $descriptor := ContainerDescriptor.new(:of(Bool), :name('!phaser_first_triggered'), :default(False), :dynamic(0));
                $container := nqp::create(Scalar);
                nqp::bindattr($container, Scalar, '$!descriptor', $descriptor);
                nqp::bindattr($container, Scalar, '$!value', False);
                nqp::bindattr(self, RakuAST::ScopePhaser, '$!first-trigger-container', $container);
            }
            $context.ensure-sc($container);
            $first-setup.push(
                QAST::Var.new(:scope<lexical>, :name<!phaser_first_triggered>, :decl<statevar>, :value($container))
            );
            $first-setup.push(
                QAST::Op.new(:op<unless>,
                    QAST::Var.new(:scope<lexical>, :name<!phaser_first_triggered>),
                    $calls
                )
            );
            my %seen;
            if $!FIRST {
                for $!FIRST {
                    $calls.push($_.IMPL-CALLISH-QAST($context));
                    %seen{nqp::objectid($_.meta-object)} := 1;
                }
            }
            if $block {
            my $first-phasers := $block.phasers('FIRST');
                if nqp::isconcrete($first-phasers) {
                    for $first-phasers.FLATTENABLE_LIST {
                        unless %seen{nqp::objectid($_)} {
                            $context.ensure-sc($_);
                            $calls.push(QAST::Op.new(:op<call>, QAST::WVal.new(:value($_))));
                        }
                    }
                }
            }
            $qast[0].push: $first-setup;
        }

        if $!ENTER || $phasers && nqp::ishash($phasers) && nqp::existskey($phasers, 'ENTER') {
            my $enter-setup := QAST::Stmts.new;
            my %seen;
            if $!ENTER {
                for $!ENTER {
                    my $result-name := $_.result-name;
                    $enter-setup.push(
                      QAST::Op.new(
                        :op<bind>,
                        QAST::Var.new( :name($result-name), :scope<lexical>, :decl<var> ),
                        $_.IMPL-CALLISH-QAST($context)
                      )
                    );
                    %seen{nqp::objectid($_.meta-object)} := 1;
                }
            }
            if nqp::isconcrete($block) && nqp::ishash($phasers) && nqp::existskey($phasers, 'ENTER') {
                my $enter-phasers := nqp::atkey($phasers, 'ENTER');
                if nqp::isconcrete($enter-phasers) {
                    my int $i := 0;
                    my int $n := nqp::elems($enter-phasers);
                    while $i < $n {
                        my $p := nqp::atpos($enter-phasers, $i);
                        unless %seen{nqp::objectid($p)} {
                            $context.ensure-sc($p);
                            $enter-setup.push(QAST::Op.new(:op<call>, QAST::WVal.new(:value($p))));
                        }
                        $i := $i + 1;
                    }
                }
            }
            self.IMPL-ADD-ENTER-PHASERS-TO-QAST($qast, $enter-setup);
        }

        if $!let {
            self.IMPL-ADD-PHASER-QAST($context, $!let, '!LET-RESTORE', $qast);
        }
        if $!temp {
            self.IMPL-ADD-PHASER-QAST($context, $!temp, '!TEMP-RESTORE', $qast);
        }

        if $!LAST || $!NEXT || $!QUIT || $!CLOSE
            || $phasers && nqp::ishash($phasers) && (
                   nqp::existskey($phasers, 'LAST')
                || nqp::existskey($phasers, 'NEXT')
                || nqp::existskey($phasers, 'QUIT')
                || nqp::existskey($phasers, 'CLOSE')
            )
        {
            $qast[0].push(
              QAST::Op.new(
                :op('callmethod'),
                :name('!capture_phasers'),
                QAST::Op.new(
                  :op('getcodeobj'),
                  QAST::Op.new(:op('curcode'))
                )
              )
            );
        }

        if $!LEAVE || $!KEEP || $!UNDO || $!POST
            || $phasers && (nqp::istype($phasers, Code) || nqp::ishash($phasers) && (
                   nqp::existskey($phasers, 'LEAVE')
                || nqp::existskey($phasers, 'KEEP')
                || nqp::existskey($phasers, 'UNDO')
                || nqp::existskey($phasers, 'POST')
            ))
        {
            $qast.annotate('WANTMEPLEASE',1);
        }
    }

    method IMPL-ADD-ENTER-PHASERS-TO-QAST(QAST::Node $qast, QAST::Node $enter-setup) {
        $qast[0].push($enter-setup);
    }

    method IMPL-STUB-PHASERS(RakuAST::Resolver $resolver, RakuAST::IMPL::Context $context) {
        if $!let {
            $!let.IMPL-BEGIN($resolver, $context);
            $!let.IMPL-STUB-CODE($resolver, $context);
        }
        if $!temp {
            $!temp.IMPL-BEGIN($resolver, $context);
            $!temp.IMPL-STUB-CODE($resolver, $context);
        }
    }

    method IMPL-ADD-PHASER-QAST(
      RakuAST::IMPL::Context $context,
      RakuAST::Block         $phaser,
      Str                    $value_stash,
      QAST::Block            $block
    ) {
        $block[0].push(QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :name($value_stash), :scope('lexical'), :decl('var') ),
            QAST::Op.new(
              :op('create'),
              QAST::WVal.new( :value(IterationBuffer)))));
        $block.symbol($value_stash, :scope('lexical'));

        # The phaser's cached block survives a re-formation of the routine
        # that owns it, so this method can run against the same block
        # twice. The restore loop must go in once, while the attachment
        # below must land in every formation's children.
        my $phaser-block := $phaser.IMPL-QAST-BLOCK($context, :blocktype('declaration_static'));
        unless $phaser-block.ann('phaser-restore-added') {
            $phaser-block.annotate('phaser-restore-added', 1);
            $phaser-block.push(QAST::Op.new(
                :op('while'),
                QAST::Op.new(
                    :op('elems'),
                    QAST::Var.new( :name($value_stash), :scope('lexical') )),
                QAST::Op.new(
                    :op('if'),
                    QAST::Op.new(
                        :op('iscont'),
                        QAST::Op.new(
                            :op('atpos'),
                            QAST::Var.new( :name($value_stash), :scope('lexical') ),
                            QAST::IVal.new( :value(0) ))),
                    QAST::Op.new( # p6store is for Scalar
                        :op('p6store'),
                        QAST::Op.new(
                            :op('shift'),
                            QAST::Var.new( :name($value_stash), :scope('lexical') )),
                        QAST::Op.new(
                            :op('shift'),
                            QAST::Var.new( :name($value_stash), :scope('lexical') ))),
                    QAST::Op.new( # Otherwise we restore by means of the container itself
                        :op('callmethod'),
                        :name('TEMP-LET-RESTORE'),
                        QAST::Op.new(
                            :op('shift'),
                            QAST::Var.new( :name($value_stash), :scope('lexical') )),
                        QAST::Op.new(
                            :op('shift'),
                            QAST::Var.new( :name($value_stash), :scope('lexical') ))))));
        }

        # Add as phaser.
        $block[0].push($phaser-block);
    }

    method has-phaser(str $phaser-name) {
        # TOOD: Also check '$!phasers' hash on the meta-object
        nqp::elems(nqp::getattr(self, RakuAST::ScopePhaser, '$!' ~ $phaser-name) // []) > 0
    }
}

# A block, either without signature or with only a placeholder signature.
class RakuAST::Block
  is RakuAST::LexicalScope
  is RakuAST::Term
  is RakuAST::Code
  is RakuAST::StubbyMeta
  is RakuAST::BlockStatementSensitive
  is RakuAST::SinkPropagator
  is RakuAST::Blorst
  is RakuAST::ImplicitDeclarations
  is RakuAST::ImplicitLookups
  is RakuAST::AttachTarget
  is RakuAST::PlaceholderParameterOwner
  is RakuAST::ParseTime
  is RakuAST::BeginTime
  is RakuAST::ScopePhaser
  is RakuAST::Doc::DeclaratorTarget
{
    has RakuAST::Blockoid $.body;

    # Should this block have an implicit topic, in the absence of a (perhaps
    # placeholder) signature?
    # 0 = no implicit topic
    # 1 = optional implicit topic
    # 2 = required implicit topic
    # 3 = required implicit topic populated from exception
    has int $!implicit-topic-mode;

    # Should this block declare a fresh implicit `$/`?
    has int $!fresh-match;

    # Set on blocks whose QAST merges into a frame that already declares
    # the implicit specials, such as the compilation unit mainline, so
    # they must not declare an implicit `$/` of their own.
    has int $!no-implicit-match;

    # Should this block declare a fresh implicit `$!`?
    has int $!fresh-exception;

    has int $!is-in-method;
    has int $!may-have-signature;

    # Set by the lexical-to-local lowering analysis on the sunk body of a
    # loop statement when everything the block does is provably
    # frame-independent: every declaration it makes is a lowered local or
    # an unused implicit, nothing reaches its lexicals by name, and it
    # has no phasers or handlers. The loop then emits the body's
    # statements inline instead of calling the block each iteration.
    has int $!flatten-approved;

    method IMPL-SET-FLATTEN-APPROVED() {
        nqp::bindattr_i(self, RakuAST::Block, '$!flatten-approved', 1)
    }

    method IMPL-FLATTEN-APPROVED() { $!flatten-approved }

    # The declaration receiving a flattened invocation's argument. A
    # plain block has none: its topic is only eligible for flattening
    # when unused, so the argument is discarded.
    method IMPL-FLATTEN-ARG-DECLARATION() { nqp::null }

    # The parameter owning that declaration.
    method IMPL-FLATTEN-ARG-PARAMETER() { nqp::null }

    # As IMPL-QAST-FLATTENED, but binding the given argument QAST into
    # the block's argument declaration, the way calling the block with
    # one positional would have.
    method IMPL-QAST-FLATTENED-WITH-ARG(RakuAST::IMPL::QASTContext $context, Mu $arg-qast) {
        my $param := self.IMPL-FLATTEN-ARG-PARAMETER;
        my $nominal := nqp::isnull($param)
            ?? Mu
            !! nqp::getattr($param.meta-object, Parameter, '$!type');

        # A parameter of nominal type Mu binds any value, so no check is
        # needed. Bind as the parameter binder would have. Hllize, since
        # an iterator driven from nqp code can hand out unboxed values.
        # Wrap in a fresh read-only Scalar so an Iterable value stays a
        # single item and assignment to the parameter dies.
        if $nominal =:= Mu {
            return self.IMPL-QAST-FLATTENED-ENTRY($context, QAST::Op.new(
                :op('p6bindattrinvres'),
                QAST::Op.new(
                    :op('create'),
                    QAST::WVal.new( :value(Scalar) )
                ),
                QAST::WVal.new( :value(Scalar) ),
                QAST::SVal.new( :value('$!value') ),
                QAST::Op.new( :op('hllize'),
                    QAST::Op.new( :op('decont'), $arg-qast ) )
            ));
        }

        # An '@', '%' or '&' parameter carries a nominal type that the
        # binder checks, and a concrete Junction argument autothreads
        # over its eigenstates instead of binding. Check inline, running
        # the body once per value off a worklist that a Junction expands
        # into, depth first, the way the autothreader recurses. A value
        # that is neither throws the binder's type error. The worklist
        # loop carries no handlers, so a loop control in the body
        # reaches the loop the body belongs to.
        my str $val-name   := QAST::Node.unique('flat_arg');
        my str $queue-name := QAST::Node.unique('flat_arg_queue');
        my $param-obj := $param.meta-object;
        $context.ensure-sc($param-obj);
        $context.ensure-sc($nominal);
        my str $var-name := nqp::getattr_s($param-obj, Parameter, '$!variable_name');
        my $dispatch := QAST::Op.new(
            :op('if'),
            QAST::Op.new(
                :op('istype'),
                QAST::Var.new( :name($val-name), :scope('local') ),
                QAST::WVal.new( :value($nominal) )
            ),
            self.IMPL-QAST-FLATTENED-ENTRY($context,
                QAST::Var.new( :name($val-name), :scope('local') )),
            QAST::Op.new(
                :op('if'),
                QAST::Op.new(
                    :op('if'),
                    QAST::Op.new(
                        :op('istype'),
                        QAST::Var.new( :name($val-name), :scope('local') ),
                        QAST::WVal.new( :value(Junction) )
                    ),
                    QAST::Op.new(
                        :op('isconcrete'),
                        QAST::Var.new( :name($val-name), :scope('local') )
                    ),
                    QAST::IVal.new( :value(0) )
                ),
                QAST::Stmts.new(
                    QAST::Op.new(
                        :op('if'),
                        QAST::Op.new(
                            :op('isnull'),
                            QAST::Var.new( :name($queue-name), :scope('local') )
                        ),
                        QAST::Op.new(
                            :op('bind'),
                            QAST::Var.new( :name($queue-name), :scope('local') ),
                            QAST::Op.new( :op('list') )
                        )
                    ),
                    QAST::Op.new(
                        :op('splice'),
                        QAST::Var.new( :name($queue-name), :scope('local') ),
                        QAST::Op.new(
                            :op('getattr'),
                            QAST::Var.new( :name($val-name), :scope('local') ),
                            QAST::WVal.new( :value(Junction) ),
                            QAST::SVal.new( :value('$!eigenstates') )
                        ),
                        QAST::IVal.new( :value(0) ),
                        QAST::IVal.new( :value(0) )
                    )
                ),
                QAST::Op.new(
                    :op('callmethod'), :name('throw_or_die'),
                    QAST::WVal.new( :value(Perl6::Metamodel::Configuration) ),
                    QAST::SVal.new( :value('X::TypeCheck::Binding::Parameter') ),
                    QAST::SVal.new( :value(
                        "Nominal type check failed for parameter '" ~ $var-name ~ "'"
                    ) ),
                    QAST::Var.new( :name($val-name), :scope('local'), :named('got') ),
                    QAST::WVal.new( :value($nominal), :named('expected') ),
                    QAST::SVal.new( :value($var-name), :named('symbol') ),
                    QAST::WVal.new( :value($param-obj), :named('parameter') )
                )
            )
        );
        # The binder gives a Positional parameter one failover ahead
        # of the nominal type check: a value that does
        # PositionalBindFailover binds as its cached List, which is
        # how a Seq binds to an '@' parameter.
        my $lookups := $param.IMPL-UNWRAP-LIST($param.get-implicit-lookups);
        if $nominal =:= $lookups[0].resolution.compile-time-value {
            my $failover := $lookups[1].resolution.compile-time-value;
            $context.ensure-sc($failover);
            $dispatch := QAST::Stmts.new(
                QAST::Op.new(
                    :op('if'),
                    QAST::Op.new(
                        :op('istype'),
                        QAST::Var.new( :name($val-name), :scope('local') ),
                        QAST::WVal.new( :value($failover) )
                    ),
                    QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :name($val-name), :scope('local') ),
                        QAST::Op.new(
                            :op('decont'),
                            QAST::Op.new(
                                :op('callmethod'), :name('cache'),
                                QAST::Var.new( :name($val-name), :scope('local') )
                            )
                        )
                    )
                ),
                $dispatch
            );
        }
        my $advance := QAST::Op.new(
            :op('if'),
            QAST::Op.new(
                :op('if'),
                QAST::Op.new(
                    :op('not_i'),
                    QAST::Op.new(
                        :op('isnull'),
                        QAST::Var.new( :name($queue-name), :scope('local') )
                    )
                ),
                QAST::Op.new(
                    :op('elems'),
                    QAST::Var.new( :name($queue-name), :scope('local') )
                ),
                QAST::IVal.new( :value(0) )
            ),
            QAST::Stmts.new(
                QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :name($val-name), :scope('local') ),
                    QAST::Op.new(
                        :op('shift'),
                        QAST::Var.new( :name($queue-name), :scope('local') )
                    )
                ),
                QAST::IVal.new( :value(1) )
            ),
            QAST::IVal.new( :value(0) )
        );
        QAST::Stmts.new(
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name($val-name), :scope('local'), :decl('var') ),
                QAST::Op.new( :op('hllize'),
                    QAST::Op.new( :op('decont'), $arg-qast ) )
            ),
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name($queue-name), :scope('local'), :decl('var') ),
                QAST::Op.new( :op('null') )
            ),
            QAST::Op.new(
                :op('repeat_while'),
                $advance,
                $dispatch,
                QAST::IVal.new( :value(1), :named('nohandler') )
            )
        )
    }

    # The statements run on each entry of a flattened invocation: the
    # argument declaration bound to the given value, the other lowered
    # declarations with fresh containers, and the body.
    method IMPL-QAST-FLATTENED-ENTRY(RakuAST::IMPL::QASTContext $context, Mu $value-qast) {
        my $arg-decl := self.IMPL-FLATTEN-ARG-DECLARATION;
        my $stmts := QAST::Stmts.new();
        for self.IMPL-UNWRAP-LIST(self.ast-lexical-declarations()) {
            if nqp::istype($_, RakuAST::VarDeclaration::Simple)
                && $_.IMPL-LOWERED-LOCAL-NAME {
                if !nqp::isnull($arg-decl) && nqp::eqaddr($_, $arg-decl) {
                    my str $local-name := $_.IMPL-LOWERED-LOCAL-NAME;
                    $stmts.push(QAST::Var.new(
                        :scope('local'), :decl('var'), :name($local-name) ));
                    $stmts.push(QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :name($local-name), :scope('local') ),
                        $value-qast
                    ));
                }
                else {
                    $stmts.push($_.IMPL-QAST-DECL-FLATTENED($context));
                }
            }
        }
        my $nested-blocks := self.IMPL-QAST-NESTED-BLOCK-DECLS($context);
        $stmts.push($nested-blocks) if nqp::elems($nested-blocks.list);
        $stmts.push($!body.IMPL-TO-QAST($context));
        $stmts.annotate('flattened-body', 1);
        $stmts
    }

    # The body emitted for inlining into the frame of the block's user:
    # declarations of nested code objects, the lowered locals with a
    # fresh container clone on every entry (a per-iteration frame would
    # have provided a fresh container the same way), and the statement
    # list. The by-name sentinel lexicals are deliberately absent, since
    # the enclosing frame's symbols are not this block's to declare.
    method IMPL-QAST-FLATTENED(RakuAST::IMPL::QASTContext $context) {
        my $stmts := QAST::Stmts.new();
        for self.IMPL-UNWRAP-LIST(self.ast-lexical-declarations()) {
            if nqp::istype($_, RakuAST::VarDeclaration::Simple)
                && $_.IMPL-LOWERED-LOCAL-NAME {
                $stmts.push($_.IMPL-QAST-DECL-FLATTENED($context));
            }
        }
        my $nested-blocks := self.IMPL-QAST-NESTED-BLOCK-DECLS($context);
        $stmts.push($nested-blocks) if nqp::elems($nested-blocks.list);
        $stmts.push($!body.IMPL-TO-QAST($context));
        $stmts.annotate('flattened-body', 1);
        $stmts
    }

    method new(RakuAST::Blockoid :$body,
                            Bool :$implicit-topic,
                            Bool :$required-topic,
                            Bool :$exception,
                            Bool :$may-have-signature,
        RakuAST::Doc::Declarator :$WHY,
              # ignored, just for compatability with Routine
              RakuAST::Signature :$signature,
              # ignored for now, see #5997
                             str :$multiness
    ) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Block, '$!body', $body // RakuAST::Blockoid.new);
        nqp::bindattr_i($obj, RakuAST::Block, '$!is-in-method', 0);
        nqp::bindattr_i($obj, RakuAST::Block, '$!may-have-signature', $may-have-signature ?? 1 !! 0);
        $obj.set-implicit-topic($implicit-topic // 1, :required($required-topic), :$exception);
        $obj.set-WHY($WHY);
        $obj
    }

    # Helper method to return if there are any whenevers in this block,
    # either directly, or in any embedded blocks.
    method any-whenevers() { self.body.statement-list.any-whenevers }

    method may-have-signature() {
        $!may-have-signature ?? True !! False
    }

    method set-may-have-signature(Bool $may-have-signature) {
        nqp::bindattr_i(self, RakuAST::Block, '$!may-have-signature', $may-have-signature ?? 1 !! 0);
    }

    method replace-body(RakuAST::Blockoid $new-body) {
        nqp::bindattr(self, RakuAST::Block, '$!body', $new-body);
        Nil
    }

    method set-implicit-topic(Bool $implicit, Bool :$required, Bool :$exception, Bool :$local) {
        nqp::bindattr_i(self, RakuAST::Block, '$!implicit-topic-mode', $implicit
            ?? ($exception ?? 3 !!
                $required  ?? 2 !!
                              1)
            !! $local ?? -1 !! 0);
        Nil
    }

    method implicit-topic() { $!implicit-topic-mode == 1 ?? Bool !! $!implicit-topic-mode > 1 }
    method required-topic() { $!implicit-topic-mode > 1 || Bool }
    method exception()      { $!implicit-topic-mode > 2 || Bool }

    method set-fresh-variables(Bool :$match, Bool :$exception) {
        nqp::bindattr_i(self, RakuAST::Block, '$!fresh-match', $match ?? 1 !! 0);
        nqp::bindattr_i(self, RakuAST::Block, '$!fresh-exception', $exception ?? 1 !! 0);
    }

    method set-no-implicit-match() {
        nqp::bindattr_i(self, RakuAST::Block, '$!no-implicit-match', 1);
        Nil
    }

    method attach-target-names() {
        self.IMPL-WRAP-LIST(['block'])
    }

    method propagate-sink(Bool $is-sunk) {
        my $body-sunk := $is-sunk && !self.needs-result;
        self.set-nil-on-succeed() if $body-sunk;
        $!body.apply-sink($body-sunk);
    }

    method PRODUCE-IMPLICIT-DECLARATIONS() {
        my @implicit;
        unless self.IMPL-HAS-PARAMETER('$_') {
            if $!implicit-topic-mode == 1 {
                @implicit[0] := RakuAST::VarDeclaration::Implicit::BlockTopic.new:
                    parameter => self.signature ?? False !! True;
            }
            elsif $!implicit-topic-mode == -1 {
                @implicit[0] := RakuAST::VarDeclaration::Implicit::BlockTopic.new:
                    parameter => False, loop => True;
            }
            elsif $!implicit-topic-mode == 2 {
                @implicit[0] := RakuAST::VarDeclaration::Implicit::BlockTopic.new:
                    :required,
                    parameter => self.signature ?? False !! True;
            }
            elsif $!implicit-topic-mode == 3 {
                @implicit[0] := RakuAST::VarDeclaration::Implicit::BlockTopic.new(:required,
                    :exception);
            }
        }
        if nqp::getcomp('Raku').language_revision >= 3 && !$!no-implicit-match {
            nqp::push(@implicit, RakuAST::VarDeclaration::Implicit::BlockMatch.new(:name('$/')))
                unless self.IMPL-HAS-PARAMETER('$/');
        }
        elsif $!fresh-match {
            nqp::push(@implicit, RakuAST::VarDeclaration::Implicit::Special.new(:name('$/')));
        }
        if $!fresh-exception {
            nqp::push(@implicit, RakuAST::VarDeclaration::Implicit::Special.new(:name('$!')));
        }
        # Declare a parameter's type captures block-local, so they shadow
        # rather than rebind an outer same-named capture. A sub-signature can
        # declare captures too, so descend into it.
        my $signature := self.signature;
        $signature.IMPL-COLLECT-TYPE-CAPTURES(@implicit) if $signature;
        @implicit
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        [
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Code')),
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('&FATALIZE')),
        ]
    }

    method IMPL-FATALIZE() {
        self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[1].resolution.compile-time-value;
    }

    method IMPL-FATALIZE-RESOLVED() {
        self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[1].is-resolved
    }

    method IMPL-IS-IN-METHOD() {
        $!is-in-method
    }

    method PERFORM-PARSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        if $resolver.find-attach-target('method') {
            nqp::bindattr_i(self, RakuAST::Block, '$!is-in-method', True);
        }
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        $!body.to-begin-time($resolver, $context); # In case it's the default we created in the ctor.

        # Make sure that our placeholder signature has resolutions performed,
        # and that we don't produce a topic parameter.
        my $placeholder-signature := self.placeholder-signature;
        if $placeholder-signature {
            $placeholder-signature.IMPL-BEGIN($resolver, $context);
            if $!implicit-topic-mode > 0 {
                my $topic := self.IMPL-UNWRAP-LIST(self.get-implicit-declarations)[0];
                $topic.set-parameter(False);
            }
        }

        self.IMPL-STUB-PHASERS($resolver, $context);

        self.IMPL-STUB-CODE($resolver, $context);

        Nil
    }

    method PRODUCE-STUBBED-META-OBJECT(:$resolver, :$context) {
        nqp::create(Block);
    }

    method PRODUCE-META-OBJECT(:$resolver, :$context) {
        self.IMPL-PRODUCE-META-OBJECT
    }

    method IMPL-PRODUCE-META-OBJECT() {
        my $block := self.stubbed-meta-object;

        # Create block object and install signature. If it doesn't have one, then
        # we can create it based upon the implicit topic it may or may not have.
        my $signature := self.signature || self.placeholder-signature;
        if $signature {
            nqp::bindattr($block, Code, '$!signature', $signature.meta-object);
            nqp::bindattr($signature.meta-object, Signature, '$!code', $block);
        }
        elsif $!implicit-topic-mode > 0 {
            my constant REQUIRED-TOPIC-PARAM := -> {
                my $param := nqp::create(Parameter);
                nqp::bindattr_s($param, Parameter, '$!variable_name', '$_');
                nqp::bindattr($param, Parameter, '$!type', Mu);
                nqp::bindattr_i($param, Parameter, '$!flags',
                    nqp::const::SIG_ELEM_IS_RAW);
                $param
            }();
            my constant OPTIONAL-TOPIC-PARAM := -> {
                my $param := nqp::create(Parameter);
                nqp::bindattr_s($param, Parameter, '$!variable_name', '$_');
                nqp::bindattr($param, Parameter, '$!type', Mu);
                nqp::bindattr_i($param, Parameter, '$!flags',
                    nqp::const::SIG_ELEM_IS_RAW
                    +| nqp::const::SIG_ELEM_IS_OPTIONAL
                    +| nqp::const::SIG_ELEM_DEFAULT_FROM_OUTER);
                $param
            }();
            # The Parameter holds no per-block state and can be shared, but
            # each block needs its own Signature: the Binder's trial bind
            # invokes under the signature's $!code, so the backlink must
            # point at this block.
            my int $optional := $!implicit-topic-mode == 1;
            my $sig := nqp::create(Signature);
            nqp::bindattr($sig, Signature, '@!params',
                [$optional ?? OPTIONAL-TOPIC-PARAM !! REQUIRED-TOPIC-PARAM]);
            nqp::bindattr_i($sig, Signature, '$!arity', $optional ?? 0 !! 1);
            nqp::bindattr($sig, Signature, '$!count', nqp::box_i(1, Int));
            nqp::bindattr($sig, Signature, '$!code', $block);
            nqp::bindattr($block, Code, '$!signature', $sig);
        }
        self.add-phasers-to-code-object($block);
        $block
    }

    method IMPL-CHECK-DOUBLE-CLOSURE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $stmts := self.body.statement-list;
        if $stmts.IMPL-IS-SINGLE-EXPRESSION
            && $stmts.code-statements[0].expression.IMPL-PRIMED
        {
            return $resolver.build-exception: 'X::Syntax::Malformed',
                :what('double closure; WhateverCode is already a closure without curlies, so either remove the curlies or use valid parameter syntax instead of *');
        }
        Nil
    }

    method IMPL-QAST-FORM-BLOCK(RakuAST::IMPL::QASTContext $context, str :$blocktype,
            RakuAST::Expression :$expression) {
        self.IMPL-MAYBE-FATALIZE-QAST(
            self.IMPL-QAST-FORM-BLOCK-FOR-BODY($context, :$blocktype, :$expression,
                self.IMPL-APPEND-SIGNATURE-RETURN($context,
                $!body.IMPL-TO-QAST($context))));
    }
    method IMPL-QAST-FORM-BLOCK-FOR-BODY(RakuAST::IMPL::QASTContext $context, Mu $body-qast,
            str :$blocktype, RakuAST::Expression :$expression) {
        # Form block with declarations.
        my $block := QAST::Block.new(
            :$blocktype,
            self.IMPL-QAST-DECLS($context)
        );
        self.IMPL-ADD-LOWERED-DEBUG-MAPPINGS($block);

        # Compile body and, if needed, a signature, and set up arity and any
        # exception rethrow logic.
        my $signature := self.signature || self.placeholder-signature;
        if $signature {
            $block.push($signature.IMPL-QAST-BINDINGS($context, :needs-full-binder(self.custom-args)));
            $block.custom_args(1) if self.custom-args;
            $block.arity($signature.arity);
            $block.annotate('count', $signature.count);
        }
        elsif $!implicit-topic-mode == 1 {
            $block.arity(0);
            $block.annotate('count', 1);
        }
        elsif $!implicit-topic-mode >= 2 {
            $block.arity(1);
            $block.annotate('count', 1);
        }

        my $is-handler := $!implicit-topic-mode == 3 ?? True !! False;
        $block.push(self.IMPL-WRAP-SCOPE-HANDLER-QAST($context, $body-qast, :$is-handler));

        self.add-phasers-handling-code($context, $block);

        $block
    }

    method IMPL-QAST-DECL-CODE(RakuAST::IMPL::QASTContext $context) {
        # Form the block itself and link it with the meta-object.
        self.IMPL-QAST-BLOCK($context, :blocktype('declaration_static'));
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context, :$immediate) {
        if $immediate {
            # The QAST compiler gives an immediate block correct closure
            # semantics itself, so unlike the branch below, no closure clone
            # of the code object is emitted. The meta-object is still linked:
            # constructs like &?BLOCK reach it through getcodeobj at run time.
            my $block := self.IMPL-QAST-FORM-BLOCK($context, :blocktype<immediate>);
            self.IMPL-LINK-META-OBJECT($context, $block);
            $block
        }
        else {
            # A frame-independent bare block statement runs inline, with
            # no closure clone and no call.
            return self.IMPL-QAST-FLATTENED($context)
                if self.bare-block && self.IMPL-FLATTEN-APPROVED;

            # Not immediate, so already produced as a declaration above; just
            # closure clone it. Only invoke if it's a bare block.
            # Ensure the block is linked when our outer block gets cloned before
            # our IMPL-QAST-DECL-CODE is called.
            self.IMPL-QAST-BLOCK($context, :blocktype('declaration_static'));
            my $ast := self.IMPL-CLOSURE-QAST($context);
            self.bare-block
                ?? QAST::Op.new( :op('call'), $ast )
                !! $ast
        }
    }

    method bare-block() {
        self.is-block-statement
    }

    method visit-children(Code $visitor) {
        $visitor($!body);
        $visitor(self.WHY) if self.WHY;
    }

    method IMPL-CAN-INTERPRET() {
        True
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.meta-object
    }

    method as-block {
        self
    }
}

# A pointy block (-> $foo { ... }).
class RakuAST::PointyBlock
  is RakuAST::Block
  is RakuAST::ImplicitLookups
  is RakuAST::Doc::DeclaratorTarget
{
    has RakuAST::Signature $.signature;

    # The single plain positional parameter, when the signature is
    # simple enough that binding a flattened invocation's argument to
    # the parameter's local matches what the call would have done: no
    # explicit type, no default, no where, no adverbs, no traits.
    method IMPL-FLATTEN-ARG-PARAMETER() {
        my @params := self.IMPL-UNWRAP-LIST($!signature.parameters);
        return nqp::null unless nqp::elems(@params) == 1;
        my $param := @params[0];
        return nqp::null if nqp::isconcrete($param.type)
            || nqp::isconcrete($param.default)
            || nqp::isconcrete($param.where)
            || nqp::isconcrete($param.array-shape)
            || nqp::isconcrete($param.sub-signature)
            || $param.optional
            || $param.invocant
            || $param.default-rw
            || $param.default-raw
            || !($param.slurpy =:= RakuAST::Parameter::Slurpy)
            || nqp::elems($param.IMPL-UNWRAP-LIST($param.names))
            || nqp::elems($param.IMPL-UNWRAP-LIST($param.traits));
        my $target := $param.target;
        return nqp::null unless nqp::istype($target, RakuAST::ParameterTarget::Var)
            && nqp::isconcrete($target.declaration)
            && !nqp::elems($target.declaration.IMPL-UNWRAP-LIST($target.declaration.traits));
        $param
    }

    method IMPL-FLATTEN-ARG-DECLARATION() {
        my $param := self.IMPL-FLATTEN-ARG-PARAMETER;
        nqp::isnull($param) ?? nqp::null() !! $param.target.declaration
    }

    method new(RakuAST::Signature :$signature,
                RakuAST::Blockoid :$body,
         RakuAST::Doc::Declarator :$WHY
    ) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::PointyBlock, '$!signature',
          $signature // RakuAST::Signature.new);
        nqp::bindattr($obj, RakuAST::Block, '$!body',
          $body // RakuAST::Blockoid.new);
        nqp::bindattr_i($obj, RakuAST::Block, '$!is-in-method', 0);
        $obj.set-WHY($WHY);
        $obj
    }

    method replace-signature(RakuAST::Signature $new-signature) {
        nqp::bindattr(self, RakuAST::PointyBlock, '$!signature', $new-signature);
        Nil
    }

    method may-have-signature() { True }

    method bare-block() { False }

    method propagate-sink(Bool $is-sunk) {
        my $body-sunk := $is-sunk && !self.needs-result;
        self.set-nil-on-succeed() if $body-sunk;
        self.body.apply-sink($body-sunk);
        $!signature.apply-sink(True);
    }

    method visit-children(Code $visitor) {
        $visitor($!signature);
        my $placeholder-signature := self.placeholder-signature;
        $visitor($placeholder-signature) if $placeholder-signature;
        $visitor(self.body);
        $visitor(self.WHY) if self.WHY;
    }

    method IMPL-HAS-PARAMETER(Str $name) {
        $!signature.IMPL-HAS-PARAMETER($name)
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        [
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Callable')),
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('&FATALIZE')),
        ]
    }

    method IMPL-FATALIZE() {
        self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[1].resolution.compile-time-value;
    }

    method PRODUCE-META-OBJECT(:$resolver, :$context) {
        my $block := self.IMPL-PRODUCE-META-OBJECT();
        my $signature := self.signature || self.placeholder-signature;

        if $signature.meta-object.has_returns {
            my $Callable :=
              self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].compile-time-value;
            # Parameterizations intern by argument identity, so a constant
            # return value would create a distinct Callable parameterization
            # and mixin type per literal. Use its type instead.
            my $returns := $signature.meta-object.returns;
            $returns := nqp::what($returns) if nqp::isconcrete($returns);
            {
                $block.HOW.mixin(
                  $block,
                  $Callable.HOW.parameterize($Callable, $returns)
                );
                CATCH {
                    if $*COMPILING_CORE_SETTING != 1 {
                        nqp::die($_);
                    }
                }
            }
        }
        $block
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.body.to-begin-time($resolver, $context); # In case it's the default we created in the ctor.

        if $!signature {
            $!signature.set-parameters-initialized;
            $!signature.PERFORM-PARSE($resolver, $context);
            self.add-generated-lexical-declaration($_) for $!signature.IMPL-ENSURE-IMPLICITS($resolver, $context);
            $!signature.to-begin-time($resolver, $context);
        }
        my $placeholder-signature := self.placeholder-signature;
        $placeholder-signature.to-begin-time($resolver, $context) if $placeholder-signature;

        self.IMPL-STUB-PHASERS($resolver, $context);

        self.IMPL-STUB-CODE($resolver, $context);

        Nil
    }

    method IMPL-WRAP-RETURN-HANDLER(RakuAST::IMPL::QASTContext $context, QAST::Node $body) {
        my $result := $body;
        my $block := self.compile-time-value;
        my $signature := nqp::getattr($block, Code, '$!signature');
        $context.ensure-sc($block);

        # Add return type check if needed.
        my $returns := nqp::ifnull($signature.returns, Mu);
        unless $returns =:= Mu || $returns =:= Nil || nqp::isconcrete($returns) {
            $context.ensure-sc($returns);
            $result := QAST::Op.new(
                :op('p6typecheckrv'),
                $result,
                QAST::WVal.new( :value($block) ),
                QAST::WVal.new( :value(Nil) )
            );
        }

        $result
    }

    method IMPL-QAST-FORM-BLOCK(RakuAST::IMPL::QASTContext $context, str :$blocktype,
            RakuAST::Expression :$expression) {
        self.IMPL-MAYBE-FATALIZE-QAST(
            self.IMPL-QAST-FORM-BLOCK-FOR-BODY($context, :$blocktype, :$expression,
                self.IMPL-WRAP-RETURN-HANDLER($context,
                    self.IMPL-APPEND-SIGNATURE-RETURN($context, self.body.IMPL-TO-QAST($context)))))
    }
}

# Done by all kinds of Routine.
class RakuAST::Routine
  is RakuAST::LexicalScope
  is RakuAST::Term
  is RakuAST::Code
  is RakuAST::StubbyMeta
  is RakuAST::Declaration
  is RakuAST::Declaration::Mergeable
  is RakuAST::ImplicitDeclarations
  is RakuAST::AttachTarget
  is RakuAST::PlaceholderParameterOwner
  is RakuAST::ImplicitLookups
  is RakuAST::ParseTime
  is RakuAST::BeginTime
  is RakuAST::TraitTarget
  is RakuAST::ScopePhaser
  is RakuAST::Doc::DeclaratorTarget
{
    has RakuAST::Name $.name;
    has RakuAST::Signature $.signature;
    has str $!multiness;
    has RakuAST::Package $!package;
    has RakuAST::Code $!outer;
    has Bool $.need-routine-variable;
    has Bool $!replace-stub;
    has Bool $!may-use-return;

    # Set when the `soft` pragma is in effect where the routine is declared.
    # The pragma promises the routine stays wrappable at run time, so no
    # inline info may be recorded for it. Captured at begin time because the
    # pragma is a scope property, and code generation, where the info is
    # recorded, no longer has the scope stack.
    has int $!in-soft-scope;

    method IMPL-REBUILD-ELIGIBLE() { 1 }

    method multiness() {
        my $multiness := $!multiness;
        nqp::isnull_s($multiness) ?? '' !! $multiness
    }

    method replace-name(RakuAST::Name $new-name) {
        nqp::bindattr(self, RakuAST::Routine, '$!name', $new-name);
        Nil
    }

    method replace-signature(RakuAST::Signature $new-signature) {
        nqp::bindattr(self, RakuAST::Routine, '$!signature', $new-signature);
        Nil
    }

    method set-replace-stub(Bool $replace-stub) {
        nqp::bindattr(self, RakuAST::Routine, '$!replace-stub', $replace-stub ?? True !! False);
    }

    method set-may-use-return(Bool $may-use-return) {
        nqp::bindattr(self, RakuAST::Routine, '$!may-use-return', $may-use-return ?? True !! False);
    }

    method declaration-kind() { 'routine' }

    # RakuAST::Code answers this too, but the method resolution order
    # reaches RakuAST::Expression first for routines.
    method needs-sink-call() { False }

    method attach-target-names() {
        self.IMPL-WRAP-LIST(['routine', 'block'])
    }

    method is-stub() {
        False
    }

    method PERFORM-PARSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $package := $resolver.find-attach-target('package');
        nqp::bindattr(self, RakuAST::Routine, '$!package', $package // $resolver.global-package);
        my $block := $resolver.find-attach-target('block', :skip-first);
        nqp::bindattr(self, RakuAST::Routine, '$!outer', $block);
    }

    method set-need-routine-variable() {
        nqp::bindattr(self, RakuAST::Routine, '$!need-routine-variable', True);
    }

    method build-bind-exception(RakuAST::Resolver $resolver) {
        $resolver.build-exception: 'X::Bind::Rebind',
            :target(self.lexical-name)
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        [
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Callable')),
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('&FATALIZE')),
        ]
    }

    method IMPL-FATALIZE() {
        self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[1].resolution.compile-time-value;
    }

    method PRODUCE-STUBBED-META-OBJECT(:$resolver, :$context) {
        nqp::create(self.IMPL-META-OBJECT-TYPE)
    }

    method PRODUCE-META-OBJECT(:$resolver, :$context) {
        my $routine := self.stubbed-meta-object;
        my $signature := self.placeholder-signature || self.signature;
        nqp::bindattr($routine, Code, '$!signature', $signature.meta-object);
        nqp::bindattr($signature.meta-object, Signature, '$!code', $routine);

        if $signature.meta-object.has_returns {
            my $Callable :=
              self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].compile-time-value;
            # Parameterizations intern by argument identity, so a constant
            # return value would create a distinct Callable parameterization
            # and mixin type per literal. Use its type instead.
            my $returns := $signature.meta-object.returns;
            $returns := nqp::what($returns) if nqp::isconcrete($returns);
            {
                $routine.HOW.mixin(
                  $routine,
                  $Callable.HOW.parameterize($Callable, $returns)
                );
                CATCH {
                    if $*COMPILING_CORE_SETTING != 1 {
                        nqp::die($_);
                    }
                }
            }
        }

        if $!package {
            nqp::bindattr($routine, Routine, '$!package', $!package.compile-time-value);
        }

        if nqp::istype(self.body, RakuAST::OnlyStar) && !nqp::istype(self, RakuAST::RegexDeclaration) {
            $routine.set_onlystar;
        }

        # Make sure that any OperatorProperties are set on the meta-object
        # if it is some kind of operator. The category of an operator sub is
        # only expressed in its name, so reading it from the name is not a
        # workaround: a dedicated node class per category would still have
        # to parse the name to know which class to construct.
        if $!name {
            my @parts;
            for $!name.IMPL-UNWRAP-LIST($!name.colonpairs) {
                @parts.push($_.canonicalize);
            }
            my str $op := nqp::join(' ',@parts);
            if $op {
                $op := nqp::substr($op,1,nqp::chars($op) - 2);
                my str $name := $!name.canonicalize;
                my $op_props := nqp::eqat($name,'infix:',0)
                  ?? OperatorProperties.infix($op)
                  !! nqp::eqat($name,'prefix:',0)
                    ?? OperatorProperties.prefix($op)
                    !! nqp::eqat($name,'postfix:',0)
                      ?? OperatorProperties.postfix($op)
                      !! nqp::eqat($name,'postcircumfix:',0)
                        ?? OperatorProperties.postcircumfix($op)
                        !! nqp::eqat($name,'circumfix:',0)
                          ?? OperatorProperties.circumfix($op)
                          !! Mu;
                nqp::bindattr($routine,Routine,'$!op_props',$op_props)
                  if $op_props;
            }
        }

        self.add-phasers-to-code-object($routine);

        $routine
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        nqp::bindattr_i(self, RakuAST::Routine, '$!in-soft-scope', 1)
            if self.IMPL-IN-SOFT-SCOPE($resolver);
        self.body.to-begin-time($resolver, $context); # In case it's the default created in the ctor.

        # Make sure that our placeholder signature has resolutions performed.
        my $placeholder-signature := self.placeholder-signature;
        if $placeholder-signature {
            $placeholder-signature.PERFORM-PARSE($resolver, $context);
            self.add-generated-lexical-declaration($_) for $placeholder-signature.IMPL-ENSURE-IMPLICITS($resolver, $context);
            $placeholder-signature.to-begin-time($resolver, $context);
        }

        unless $placeholder-signature || $!signature {
            nqp::bindattr(self, RakuAST::Routine, '$!signature', RakuAST::Signature.new);
        }

        # Make sure that our signature has resolutions performed.
        if $!signature {
            $!signature.set-parameters-initialized;
            $!signature.set-default-type(
                RakuAST::Type::Setting.new(
                    RakuAST::Name.from-identifier('Any'),
                ).to-begin-time($resolver, $context)
            ) unless nqp::istype(self, RakuAST::RoleBody);
            $!signature.PERFORM-PARSE($resolver, $context);
            self.add-generated-lexical-declaration($_) for $!signature.IMPL-ENSURE-IMPLICITS($resolver, $context);
            $!signature.to-begin-time($resolver, $context);
        }

        my $routine := self.meta-object;
        if $!package && self.lexical-name && self.scope eq 'our' && self.multiness ne 'multi' {

            my $stash := $!package.stubbed-meta-object.WHO;

            if nqp::existskey($stash, self.lexical-name) {
                self.add-sorry:
                    $resolver.build-exception: 'X::Redeclaration', :symbol(self.lexical-name);
            }

            $stash{self.lexical-name} := $routine;
        }

        if self.multiness eq 'multi' && self.name {
            my $name := '&' ~ self.name.canonicalize;
            my $proto := $resolver.resolve-lexical($name, :current-scope-only);
            if $proto && nqp::can($proto.compile-time-value, 'is_dispatcher') && $proto.compile-time-value.is_dispatcher {
                $proto := $proto.compile-time-value;
            }
            else {
                unless self.scope eq '' || self.scope eq 'my' {
                    $resolver.build-exception('X::Declaration::Scope::Multi', scope => self.scope, declaration => 'multi').throw;
                }

                my $scope := $resolver.current-scope;

                if (   ($proto := $resolver.resolve-lexical-constant($name))
                    || ($proto := $resolver.resolve-lexical-constant-in-outer($name))
                   ) && nqp::can($proto.compile-time-value, 'is_dispatcher') && $proto.compile-time-value.is_dispatcher
                {
                    $proto := $proto.compile-time-value.derive_dispatcher;
                    $scope.add-generated-lexical-declaration(
                        RakuAST::VarDeclaration::Implicit::Constant.new(:$name, :value($proto))
                    );
                }
                else {
                    my $proto-ast := RakuAST::Sub.new(
                        :scope<my>,
                        :name(self.name),
                        :signature(RakuAST::Signature.new(
                            :parameters([
                                RakuAST::Parameter.new(
                                    :slurpy(RakuAST::Parameter::Slurpy::Capture),
                                )
                            ]),
                        )),
                        :body(RakuAST::OnlyStar.new),
                        :multiness<proto>,
                    );

                    $proto-ast.ensure-begin-performed($resolver, $context);
                    $proto := $proto-ast.meta-object;

                    $resolver.declare-lexical($proto-ast) if nqp::istype($resolver, RakuAST::Resolver::Compile);
                    $scope.add-generated-lexical-declaration(
                        RakuAST::VarDeclaration::Implicit::Block.new(:block($proto-ast))
                    );
                }
            }
            $proto.add_dispatchee($routine);
        }
        elsif self.multiness eq 'proto' {
            nqp::bindattr($routine, Routine, '@!dispatchees', []);
            $resolver.current-scope.add-generated-lexical-declaration(self);
        }

        self.IMPL-STUB-PHASERS($resolver, $context);

        my $stub := self.IMPL-STUB-CODE($resolver, $context);
        nqp::setcodename($stub, $!name.canonicalize) if $!name;

        self.meta-object.set_yada if self.is-stub;

        # Apply any traits, with the routine's own scope visible: a trait
        # argument can declare into it, as in `is memoized(my %h)`.
        $resolver.push-scope(self);
        self.apply-traits($resolver, $context, self);
        $resolver.pop-scope();
    }

    method set-value(Mu $value) {
        nqp::bindattr(self, RakuAST::StubbyMeta, '$!cached-stubbed-meta-object', $value);
        nqp::bindattr(self, RakuAST::Meta, '$!cached-meta-object', $value);
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.add-trait-sorries;

        nqp::findmethod(RakuAST::LexicalScope, 'PERFORM-CHECK')(self, $resolver, $context);

        if $!multiness && !$!name {
            self.add-sorry:
              $resolver.build-exception: 'X::Anon::Multi', multiness => $!multiness, routine-type => self.declaration-kind;
        }

        if $!multiness eq 'proto' {
            my $meta-object := self.meta-object;
            if nqp::can($meta-object, 'sort_dispatchees') {
                $meta-object.sort_dispatchees();
            }
        }
    }

    method PRODUCE-IMPLICIT-DECLARATIONS() {
        my int $slash := 1;
        my int $exclamation-mark := 1;
        my int $underscore := 1;
        my @declarations;
        if $!signature {
            my $implicit-invocant := $!signature.implicit-invocant;
            if $implicit-invocant {
                my $type-captures := self.IMPL-UNWRAP-LIST($implicit-invocant.type-captures);
                for $type-captures {
                    nqp::push(@declarations, $_);
                }
            }
            for self.IMPL-UNWRAP-LIST($!signature.parameters) {
                if ($_.target) {
                    my $name := $_.target.lexical-name;
                    $slash := 0            if $name eq '$/';
                    $exclamation-mark := 0 if $name eq '$!';
                    $underscore := 0       if $name eq '$_';
                }
            }
            $!signature.IMPL-COLLECT-TYPE-CAPTURES(@declarations);
            # A special variable bound inside a sub-signature, such as the $_ in
            # `(:value($_))`, is not a top-level parameter, so catch it too.
            $slash := 0            if $slash && $!signature.IMPL-HAS-PARAMETER('$/');
            $exclamation-mark := 0 if $exclamation-mark && $!signature.IMPL-HAS-PARAMETER('$!');
            $underscore := 0       if $underscore && $!signature.IMPL-HAS-PARAMETER('$_');
        }

        # An onlystar body only ever runs the dispatcher, so it has no use
        # for the special variables. A regex declaration's {*} instead calls
        # the proto regex machinery, which does need them.
        my int $cursor := 1;
        if nqp::istype(self.body, RakuAST::OnlyStar)
          && !nqp::istype(self, RakuAST::RegexDeclaration) {
            $slash            := 0;
            $exclamation-mark := 0;
            $underscore       := 0;
            $cursor           := 0;
        }

        nqp::push(@declarations, RakuAST::VarDeclaration::Implicit::Special.new(:name('$/'))) if $slash;
        nqp::push(@declarations, RakuAST::VarDeclaration::Implicit::Special.new(:name('$!'))) if $exclamation-mark;
        nqp::push(@declarations, RakuAST::VarDeclaration::Implicit::Special.new(:name('$_'))) if $underscore;
        nqp::push(@declarations, RakuAST::VarDeclaration::Implicit::Routine.new()) if $!need-routine-variable;
        nqp::push(@declarations, RakuAST::VarDeclaration::Implicit::Cursor.new()) if $cursor;
        @declarations
    }

    method IMPL-HAS-PARAMETER(Str $name) {
        $!signature && $!signature.IMPL-HAS-PARAMETER($name)
    }

    # The implicit self declaration of this routine, or null for a
    # routine without one.
    method IMPL-SELF-DECLARATION() {
        if nqp::istype(self, RakuAST::ImplicitDeclarations) {
            for self.IMPL-UNWRAP-LIST(self.get-implicit-declarations()) {
                return $_ if nqp::istype($_, RakuAST::VarDeclaration::Implicit::Self);
            }
        }
        nqp::null
    }

    method IMPL-QAST-FORM-BLOCK(RakuAST::IMPL::QASTContext $context, str :$blocktype,
            RakuAST::Expression :$expression) {
        # RegexThunk needs the body compiled first
        my $body := self.IMPL-COMPILE-BODY($context);

        my $block :=
            self.IMPL-SET-NODE(
                QAST::Block.new(
                    :name(self.name ?? self.name.canonicalize !! ''),
                    :blocktype('declaration_static'),
                    self.IMPL-QAST-DECLS($context)
                ), :key);
        self.IMPL-ADD-LOWERED-DEBUG-MAPPINGS($block);
        my $signature := self.placeholder-signature || $!signature;
        $block.push($signature.IMPL-QAST-BINDINGS($context, :needs-full-binder(self.custom-args), :multi(self.multiness eq 'multi'), :invocant-decl(self.IMPL-SELF-DECLARATION)));
        $block.custom_args(1) if self.custom-args;
        $block.arity($signature.arity);
        $block.annotate('count', $signature.count);
        $block.push($body);
        self.add-phasers-handling-code($context, $block);
        my $formed := self.IMPL-MAYBE-FATALIZE-QAST($block);
        self.IMPL-MAYBE-SET-INLINE-INFO($formed);
        $formed
    }

    # A named sub whose body reduces to a tree of inlinable ops over its
    # parameters records that tree on its code object, with each parameter
    # use replaced by a positional placeholder. A caller that decides the
    # dispatch at compile time splices the tree in place of the call, with
    # the argument code standing in for the placeholders. Substituting
    # argument code for a parameter is only identity-preserving when the
    # binding is: every parameter must be native, required, positional, and
    # by-value, with no constraints beyond its type. The routine itself must
    # be a plain one: a custom invocation protocol, phasers, or the `soft`
    # pragma's wrappability promise all keep the call.
    method IMPL-MAYBE-SET-INLINE-INFO(Mu $block) {
        return Nil unless nqp::istype(self, RakuAST::Sub)
            && self.name
            && !$!in-soft-scope;
        my $code := self.meta-object;
        return Nil unless nqp::isconcrete($code);
        return Nil if nqp::can($code, 'CALL-ME');
        return Nil if nqp::can($code, 'soft') && $code.soft;
        return Nil if nqp::isconcrete(nqp::getattr($code, Block, '$!phasers'));
        return Nil unless nqp::elems($block.list) == 3;

        my $signature := nqp::getattr($code, Code, '$!signature');
        my @params := nqp::getattr($signature, Signature, '@!params');
        my int $n := nqp::elems(@params);
        return Nil unless $n;
        my %placeholders;
        my int $i := -1;
        while ++$i < $n {
            my $param := nqp::atpos(@params, $i);
            # Only a full-width signed int, full-width num, or str parameter
            # binds any argument the trial bind accepts unchanged. An
            # unsigned or narrower native wraps or truncates on binding,
            # which argument code standing in for the parameter would skip.
            my $param-type := nqp::getattr($param, Parameter, '$!type');
            my int $ps := nqp::objprimspec($param-type);
            return Nil unless $ps == 3
                || ($ps == 1 || $ps == 2) && nqp::objprimbits($param-type) == 64;
            my int $flags := nqp::getattr_i($param, Parameter, '$!flags');
            return Nil if $flags +& (nqp::const::SIG_ELEM_IS_OPTIONAL
                +| nqp::const::SIG_ELEM_IS_COPY
                +| nqp::const::SIG_ELEM_BIND_PRIVATE_ATTR
                +| nqp::const::SIG_ELEM_BIND_PUBLIC_ATTR);
            return Nil if nqp::isconcrete(nqp::getattr($param, Parameter, '@!named_names'))
                || nqp::isconcrete(nqp::getattr($param, Parameter, '@!type_captures'))
                || nqp::isconcrete(nqp::getattr($param, Parameter, '@!post_constraints'))
                || nqp::isconcrete(nqp::getattr($param, Parameter, '$!sub_signature'))
                || nqp::isconcrete(nqp::getattr($param, Parameter, '$!default_value'))
                || nqp::isconcrete(nqp::getattr($param, Parameter, '$!signature_constraint'));
            my str $name := nqp::getattr_s($param, Parameter, '$!variable_name');
            %placeholders{$name} := QAST::InlinePlaceholder.new(:position($i))
                unless nqp::isnull_s($name) || $name eq '';
        }

        # The block's declarations may hold only the implicits a routine
        # always gets and the parameters themselves. Anything else, and in
        # particular any nested block, means the body depends on its frame.
        for $block.list[0].list {
            if nqp::istype($_, QAST::Var) && $_.scope eq 'lexical' {
                my str $name := $_.name;
                return Nil unless $name eq '$_' || $name eq '$/' || $name eq '$!'
                    || $name eq '$¢' || $name eq '$*DISPATCHER'
                    || nqp::existskey(%placeholders, $name);
            }
            elsif nqp::istype($_, QAST::Block) {
                return Nil;
            }
            elsif (nqp::istype($_, QAST::Stmt) || nqp::istype($_, QAST::Stmts))
                && nqp::elems($_.list) && nqp::istype($_.list[0], QAST::Block) {
                return Nil;
            }
        }

        # A body that spliced other routines' inline info is itself a
        # candidate, so trees can compound through chains of small helpers.
        # The node budget bounds that growth: a routine whose body walk
        # exceeds it keeps the call, and the amount of code any single
        # call site can splice stays small.
        my $info;
        my int $walked := 0;
        my @budget := [64];
        try {
            $info := self.IMPL-INLINE-INFO-NODE($block.list[2], %placeholders, @budget);
            $walked := 1;
        }
        if $walked && nqp::istype($info, QAST::Node) {
            RakuAST::IMPL::VarLowering.IMPL-NOTE("INLINE-INFO " ~ self.name.canonicalize)
                if nqp::existskey(nqp::getenvhash(), 'RAKUDO_INLINE_DEBUG');
            nqp::bindattr($code, Routine, '$!inline_info', $info);
        }
        Nil
    }

    method IMPL-INLINE-INFO-CLEAR(Mu $node) {
        $node.node(nqp::null());
        $node.clear_annotations();
        $node
    }

    # Rebuild a body node as inline info, dying when the body is not
    # expressible independent of its frame: only literal values, object
    # references other than pseudo-stashes, inlinable ops, statement and
    # want wrappers, and parameter reads, which become placeholders, can
    # appear. Source locations and annotations are stripped from the
    # copies, as they describe the routine, not the call site the tree
    # will be spliced into.
    method IMPL-INLINE-INFO-NODE(Mu $node, %placeholders, @budget) {
        nqp::bindpos(@budget, 0, nqp::atpos(@budget, 0) - 1);
        nqp::die('Body too large to inline') if nqp::atpos(@budget, 0) < 0;
        if nqp::istype($node, QAST::IVal) || nqp::istype($node, QAST::SVal)
            || nqp::istype($node, QAST::NVal) {
            $node.node ?? self.IMPL-INLINE-INFO-CLEAR($node.shallow_clone) !! $node
        }
        elsif nqp::istype($node, QAST::WVal) {
            nqp::die('Routines using pseudo-stashes are not inlinable')
                if $node.value.HOW.name($node.value) eq 'PseudoStash';
            $node.node ?? self.IMPL-INLINE-INFO-CLEAR($node.shallow_clone) !! $node
        }
        elsif nqp::istype($node, QAST::Op) {
            nqp::die('Non-inlinable op encountered')
                unless nqp::getcomp('QAST').operations.is_inlinable('Raku', $node.op);
            my $replacement := $node.shallow_clone;
            my int $n := nqp::elems($node.list);
            my int $i := -1;
            while ++$i < $n {
                nqp::bindpos($replacement.list, $i,
                    self.IMPL-INLINE-INFO-NODE($node.list[$i], %placeholders, @budget));
            }
            self.IMPL-INLINE-INFO-CLEAR($replacement)
        }
        elsif nqp::istype($node, QAST::Var) && ($node.scope eq 'lexical' || $node.scope eq '') {
            nqp::die('Cannot inline with non-argument variables')
                unless nqp::existskey(%placeholders, $node.name);
            my $replacement := %placeholders{$node.name};
            if $node.named || $node.flat {
                $replacement := $replacement.shallow_clone;
                $replacement.named($node.named) if $node.named;
                $replacement.flat($node.flat) if $node.flat;
            }
            $replacement
        }
        elsif nqp::istype($node, QAST::Stmt) || nqp::istype($node, QAST::Stmts) {
            my $replacement := $node.shallow_clone;
            my int $n := nqp::elems($node.list);
            my int $i := -1;
            while ++$i < $n {
                nqp::bindpos($replacement.list, $i,
                    self.IMPL-INLINE-INFO-NODE($node.list[$i], %placeholders, @budget));
            }
            self.IMPL-INLINE-INFO-CLEAR($replacement)
        }
        elsif nqp::istype($node, QAST::Want) {
            my $replacement := $node.shallow_clone;
            my int $n := nqp::elems($node.list);
            my int $i := 0;
            while $i < $n {
                nqp::bindpos($replacement.list, $i,
                    self.IMPL-INLINE-INFO-NODE($node.list[$i], %placeholders, @budget));
                $i := $i + 2;
            }
            self.IMPL-INLINE-INFO-CLEAR($replacement)
        }
        else {
            nqp::die('Unhandled node type; will not inline')
        }
    }

    method IMPL-COMPILE-BODY(RakuAST::IMPL::QASTContext $context) {
        nqp::die('RakuAST::Routine subclass must implement IMPL-COMPILE-BODY')
    }

    # Set by the optimize pass, allowing the return decontainerization to be
    # skipped when the body's result is provably container-free.
    has int $!elide-return-decont;

    method IMPL-SET-ELIDE-RETURN-DECONT() {
        nqp::bindattr_i(self, RakuAST::Routine, '$!elide-return-decont', 1)
    }

    # The op to decontainerize the fall-through return value with: the given
    # full one, a plain native decont for a native assignment result, or none
    # when the result is provably container-free: a boolification, a native
    # or bigint operation, a native variable read (downgraded here from a
    # reference to a plain read, which boxes a fresh value), the invocant, or
    # an uncontainerized constant. Descends statement wrappers to the node
    # whose value is the body's. An explicit return does not pass through
    # this op either way, so only the fall-through value is at stake.
    method IMPL-RETURN-DECONT-OP(Mu $body, str $full) {
        my $node := $body;
        while nqp::istype($node, QAST::Stmts) || nqp::istype($node, QAST::Stmt) {
            my int $n := nqp::elems($node.list);
            return $full unless $n;
            my $rc := $node.resultchild;
            $node := $node[nqp::defined($rc) ?? $rc !! $n - 1];
        }
        $node := $node[0]
            if nqp::istype($node, QAST::Want) && nqp::elems($node.list);
        if nqp::istype($node, QAST::Op) {
            my str $op := $node.op;
            return '' if $op eq 'hllbool' || nqp::eqat($op, 'I', -1);
            if nqp::eqat($op, 'assign_', 0) {
                return 'decont_i' if $op eq 'assign_i';
                return 'decont_n' if $op eq 'assign_n';
                return 'decont_s' if $op eq 'assign_s';
                return 'decont_u' if $op eq 'assign_u';
            }
            return '' if nqp::eqat($op, '_i', -2) || nqp::eqat($op, '_u', -2)
                || nqp::eqat($op, '_n', -2) || nqp::eqat($op, '_s', -2);
        }
        elsif nqp::istype($node, QAST::Var) {
            my str $scope := $node.scope;
            if $scope eq 'lexicalref' {
                $node.scope('lexical');
                return '';
            }
            if $scope eq 'attributeref' {
                $node.scope('attribute');
                return '';
            }
            return '' if $scope eq 'lexical' && $node.name eq 'self';
            if $scope eq 'local' {
                my $self-decl := self.IMPL-SELF-DECLARATION;
                return '' if nqp::isconcrete($self-decl)
                    && $node.name eq $self-decl.IMPL-LOWERED-LOCAL-NAME;
            }
        }
        elsif nqp::istype($node, QAST::WVal) {
            return '' unless nqp::iscont($node.value);
        }
        elsif nqp::istype($node, QAST::IVal) || nqp::istype($node, QAST::NVal)
            || nqp::istype($node, QAST::SVal) {
            return '';
        }
        $full
    }

    method IMPL-WRAP-RETURN-HANDLER(RakuAST::IMPL::QASTContext $context, QAST::Node $body) {
        my $result := $body;
        my $routine := self.compile-time-value;
        my $signature := nqp::getattr($routine, Code, '$!signature');
        $context.ensure-sc($routine);

        # Add return exception and decont handler if needed.
        my str $decont-rv-op := $context.lang-version lt 'd' && $context.is-moar
            ?? 'p6decontrv_6c'
            !! 'p6decontrv';
        unless $routine.rw {
            my str $decont-op := $!elide-return-decont
                ?? self.IMPL-RETURN-DECONT-OP($body, $decont-rv-op)
                !! $decont-rv-op;
            if $decont-op eq $decont-rv-op {
                $result := QAST::Op.new( :op($decont-rv-op),
                    QAST::WVal.new( :value($routine) ), $result );
            }
            elsif $decont-op {
                $result := QAST::Op.new( :op($decont-op), $result );
            }
        }
        if $!may-use-return {
            $result := QAST::Op.new(
                :op<handlepayload>,
                $result,
                'RETURN',
                QAST::Op.new( :op<lastexpayload> )
            );
        }

        # Add return type check if needed.
        my $returns := nqp::ifnull($signature.returns, Mu);
        unless $returns =:= Mu || $returns =:= Nil || nqp::isconcrete($returns) {
            $context.ensure-sc($returns);
            $result := QAST::Op.new(
                :op('p6typecheckrv'),
                $result,
                QAST::WVal.new( :value($routine) ),
                QAST::WVal.new( :value(Nil) )
            );
        }

        $result
    }

    method IMPL-QAST-DECL-CODE(RakuAST::IMPL::QASTContext $context) {
        # Form the QAST block itself and link it with the meta-object.
        my $block := self.IMPL-QAST-BLOCK($context);

        # Set a name, if there is one.
        if $!name {
            my $canon-name := $!name.canonicalize;
            $block.name($canon-name);
        }

        my $name := self.lexical-name;
        if $name && (self.scope eq 'our' || self.scope eq 'unit') && self.multiness ne 'multi' {
            my $stmts := self.IMPL-SET-NODE(QAST::Stmts.new(), :key);
            $stmts.push($block);
            $stmts.push(QAST::Op.new(
                :op('bindkey'),
                QAST::Op.new( :op('who'), QAST::WVal.new( :value($!package.meta-object) ) ),
                QAST::SVal.new( :value($name) ),
                QAST::Var.new( :name($name), :scope('lexical') )
            ));
            return $stmts;
        }

        # A multi candidate has no lexical of its own, so its placement in
        # the enclosing block is where its do gets bound to the running
        # compilation.
        if self.multiness eq 'multi'
          && nqp::getattr_i(self, RakuAST::Code, '$!dynamically-compiled')
          && !$context.is-precompilation-mode {
            return QAST::Stmts.new($block, self.IMPL-DYNAMIC-DO-REBIND-QAST($context));
        }

        $block
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        # If we're a named lexical thing, install us in the block.
        my $name := self.lexical-name;
        if $name && self.multiness ne 'multi' {
            if $!replace-stub {
                QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :scope<lexical>, :$name ),
                    self.IMPL-CLOSURE-QAST($context)
                )
            }
            else {
                if $!outer { # Ensure each block invocation gets its own closure clone of this routine
                    QAST::Stmts.new(
                        QAST::Var.new( :decl<static>, :scope<lexical>, :$name, :value(self.meta-object) ),
                        QAST::Op.new(
                            :op('bind'),
                            QAST::Var.new( :scope<lexical>, :$name ),
                            self.IMPL-CLOSURE-QAST($context)
                        )
                    )
                }
                else {
                    # The comp unit's frame runs once per load, so the
                    # serialized routine works as the lexical's value as is.
                    $context.ensure-sc(self.meta-object);
                    my $decl := QAST::Var.new( :decl<static>, :scope<lexical>, :$name, :value(self.meta-object) );
                    nqp::getattr_i(self, RakuAST::Code, '$!dynamically-compiled')
                      && !$context.is-precompilation-mode
                        ?? QAST::Stmts.new($decl, self.IMPL-DYNAMIC-DO-REBIND-QAST($context))
                        !! $decl
                }
            }
        }
        else {
            QAST::Op.new( :op('null') )
        }
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        self.IMPL-CLOSURE-QAST($context)
    }

    method lexical-name() {
        my $name := self.name;
        if $name {
            '&' ~ $name.canonicalize
        }
        else {
            Nil
        }
    }

    method declaration-name() {
        self.name.canonicalize
    }

    method is-lexical() {
        my str $scope := self.scope;
        $scope eq 'my' || $scope eq 'state' || $scope eq 'our' || $scope eq 'unit'
    }

    method is-simple-lexical-declaration() {
        self.is-lexical && self.multiness ne 'multi' && self.multiness ne 'proto'
    }

    method generate-lookup() {
        if self.is-lexical {
            my $lookup := RakuAST::Var::Lexical.new(self.lexical-name);
            $lookup.set-resolution(self);
            $lookup
        }
        else {
            nqp::die('Cannot generate lookup of a routine for scope ' ~ self.scope);
        }
    }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context, Mu :$rvalue) {
        QAST::Var.new( :scope('lexical'), :name(self.lexical-name) )
    }

    method visit-children(Code $visitor) {
        $visitor($!name) if $!name;
        $visitor(self.WHY) if self.WHY;  # needs to be before signature
        $visitor($!signature) if $!signature;
        self.visit-traits($visitor);
        $visitor(self.body);
    }

    method IMPL-CAN-INTERPRET() {
        True
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.meta-object
    }
}

# A subroutine.
class RakuAST::Sub
  is RakuAST::Routine
  is RakuAST::SinkBoundary
{
    has RakuAST::Blockoid $.body;

    method new(          str :$scope,
                         str :$multiness,
               RakuAST::Name :$name,
          RakuAST::Signature :$signature,
                        List :$traits,
           RakuAST::Blockoid :$body,
    RakuAST::Doc::Declarator :$WHY
    ) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', $scope);
        nqp::bindattr_s($obj, RakuAST::Routine, '$!multiness', $multiness //'');
        nqp::bindattr($obj, RakuAST::Routine, '$!name', $name // RakuAST::Name);
        nqp::bindattr($obj, RakuAST::Routine, '$!signature', $signature);
        $obj.set-traits($traits);
        nqp::bindattr($obj, RakuAST::Sub, '$!body',
          $body // RakuAST::Blockoid.new);
        $obj.set-WHY($WHY);
        $obj
    }

    method declarator() { 'sub' }

    method replace-body(RakuAST::Blockoid $new-body) {
        nqp::bindattr(self, RakuAST::Sub, '$!body', $new-body);
        # The implicit declarations were produced and cached when this
        # routine's scope was entered during parsing, before the body was
        # known. An onlystar body prunes the special variables from them,
        # so drop the cache to have them produced anew.
        nqp::bindattr(self, RakuAST::ImplicitDeclarations,
          '$!implicit-declarations-cache', Mu)
          if nqp::istype($new-body, RakuAST::OnlyStar);
        Nil
    }

    method IMPL-META-OBJECT-TYPE() { Sub }

    method default-scope() {
        self.name ?? 'my' !! 'anon'
    }

    method allowed-scopes() {
        self.IMPL-WRAP-LIST(['my', 'anon', 'our', 'unit'])
    }

    method get-boundary-sink-propagator() {
        $!body.statement-list
    }

    method is-boundary-sunk() {
        return False if self.needs-result;
        my $signature := self.signature;
        $signature ?? $signature.provides-return-value !! False
    }

    method is-stub() {
        my @code := self.body.statement-list.code-statements;
        nqp::elems(@code) == 1
            && nqp::istype(@code[0], RakuAST::Statement::Expression)
            && nqp::istype(@code[0].expression, RakuAST::Stub)
    }

    method PERFORM-CHECK(Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        nqp::findmethod(RakuAST::Routine, 'PERFORM-CHECK')(self, $resolver, $context);

        self.check-scope($resolver, 'sub');

        # Anonymous multis will already have been reported and would lead to compiler
        # error in the next check.
        return Nil if self.multiness ne 'multi' || !self.name;

        self.IMPL-CHECK-FOR-DUPLICATE-MULTI-SIGNATURES($resolver);
    }

    method IMPL-COMPILE-BODY(RakuAST::IMPL::QASTContext $context) {
        # A stub body sets the yada bit so .yada and .raku report it as a stub.
        self.meta-object.set_yada if self.is-stub;
        self.IMPL-CALCULATE-SINK unless self.sink-calculated;
        self.IMPL-WRAP-RETURN-HANDLER($context,
            self.IMPL-WRAP-SCOPE-HANDLER-QAST($context,
                self.IMPL-APPEND-SIGNATURE-RETURN($context, $!body.IMPL-TO-QAST($context))))
    }

    method IMPL-CHECK-FOR-DUPLICATE-MULTI-SIGNATURES(Resolver $resolver) {
        my $proto := self.meta-object.dispatcher;
        my $signature := (self.placeholder-signature || self.signature).compile-time-value;
        my $meta := self.meta-object;

        # If we ourselves can default, there is no need to check further
        return Nil if nqp::can($meta, 'default');
        return Nil if $*COMPILING_CORE_SETTING == 1; # No chance in early bootstrap

        # Checking two candidates for equivalence smartmatches their signatures,
        # which runs each parameter type's ACCEPTS. A where-clause constraint or
        # a type whose ACCEPTS is defined outside the setting would run user code
        # at compile time, so a signature carrying either is left out of the check.
        return Nil unless self.IMPL-SIGNATURE-STATICALLY-COMPARABLE($signature);

        # Crawl the candidates and ensure that none of them double up. For a
        # revision gated proto, equivalent signatures gated at different
        # revisions are how a routine evolves per language version, so only
        # candidates competing at the same effective revision are compared.
        # A candidate without its own gate defaults to the proto's revision,
        # as it does at dispatch time.
        my $proto-revision := nqp::can($proto, 'REQUIRED-REVISION') && $proto.REQUIRED-REVISION;
        my $self-revision := $proto-revision
            ?? (nqp::can($meta, 'REQUIRED-REVISION') ?? $meta.REQUIRED-REVISION !! $proto-revision)
            !! Nil;
        my @seen-accepts;
        for $proto.dispatchees {
            last if $_.can("default");
            next if $_ =:= $meta;

            if $proto-revision {
                my $other-revision := nqp::can($_, 'REQUIRED-REVISION')
                    ?? $_.REQUIRED-REVISION
                    !! $proto-revision;
                next unless $other-revision == $self-revision;
            }

            my $other-signature := $_.signature;
            next unless $signature.arity == $other-signature.arity;
            next unless self.IMPL-SIGNATURE-STATICALLY-COMPARABLE($other-signature);

            @seen-accepts.push($_)
                if try $other-signature.ACCEPTS($signature)
                    && try $signature.ACCEPTS($other-signature);

            if @seen-accepts > 0 {
                my %args;
                if my $origin := self.origin {
                    my $origin-match := self.origin.as-match;
                    %args<filename>   := $origin-match.file;
                    %args<line> := $origin-match.line
                }
                self.add-worry: $resolver.build-exception:
                        'X::Redeclaration::Multi',
                        :symbol(self.name.canonicalize),
                        :ambiguous(@seen-accepts),
                        |%args;
            }
        }
    }

    method IMPL-SIGNATURE-STATICALLY-COMPARABLE($signature) {
        for self.IMPL-UNWRAP-LIST($signature.params) {
            return False if $_.constraint_list;
            my $type := $_.type;
            unless nqp::isnull($type) {
                # A coercion type erases to its target under the smartmatch,
                # so distinct coercions would compare as equivalent.
                return False if $type.HOW.archetypes.coercive;
                my $accepts := nqp::tryfindmethod($type, 'ACCEPTS');
                return False
                  if nqp::defined($accepts)
                  && nqp::istype($accepts, Code)
                  && !$accepts.file.starts-with('SETTING::');
            }
            # Apply the same checks through a sub-signature destructure.
            my $sub-signature := $_.sub_signature;
            return False
              if nqp::isconcrete($sub-signature)
              && !self.IMPL-SIGNATURE-STATICALLY-COMPARABLE($sub-signature);
        }
        True
    }
}

class RakuAST::RoleBody
  is RakuAST::Sub
{
    has RakuAST::LexicalFixup $.fixup;

    # The lexical fixup nodes IMPL-FINISH-ROLE-BODY appended to the
    # formed block. The throwaway block's outer annotation names the
    # block object the graft keeps.
    has Mu $!fixup-nodes;

    # A re-formation reproduces the body's statements only, so the fixup
    # nodes go back, and the accessor QAST the package splices in is
    # spliced again once its marker, which the graft kept, is cleared.
    method IMPL-REBUILD-BEGIN-TIME-CACHED-BLOCK(RakuAST::IMPL::QASTContext $context) {
        nqp::findmethod(RakuAST::Code, 'IMPL-REBUILD-BEGIN-TIME-CACHED-BLOCK')(self, $context);
        my $block := nqp::getattr(self, RakuAST::Code, '$!qast-block');
        for $!fixup-nodes {
            $block[1].push($_);
        }
        $block.annotate('accessor-qast-added', 0);
        Nil
    }

    method new(          str :$scope,
                         str :$multiness,
               RakuAST::Name :$name,
          RakuAST::Signature :$signature,
                        List :$traits,
           RakuAST::Blockoid :$body,
    RakuAST::Doc::Declarator :$WHY
    ) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', $scope);
        nqp::bindattr_s($obj, RakuAST::Routine, '$!multiness', $multiness //'');
        nqp::bindattr($obj, RakuAST::Routine, '$!name', $name // RakuAST::Name);
        $signature := RakuAST::Signature.new unless nqp::isconcrete($signature);
        $signature.set-is-on-role-body(True);
        nqp::bindattr($obj, RakuAST::Routine, '$!signature', $signature);
        $obj.set-traits($traits);
        nqp::bindattr($obj, RakuAST::Sub, '$!body',
          $body // RakuAST::Blockoid.new);
        nqp::bindattr($obj, RakuAST::RoleBody, '$!fixup', RakuAST::LexicalFixup);
        nqp::bindattr($obj, RakuAST::RoleBody, '$!fixup-nodes', []);
        $obj.set-WHY($WHY);
        $obj
    }

    method set-fixup(RakuAST::LexicalFixup $fixup) {
        nqp::bindattr(self, RakuAST::RoleBody, '$!fixup', $fixup);
    }

    method replace-signature(RakuAST::Signature $new-signature) {
        $new-signature.set-is-on-role-body(True);
        nqp::bindattr(self, RakuAST::Routine, '$!signature', $new-signature);
        Nil
    }

    method PERFORM-PARSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        nqp::findmethod(RakuAST::Routine, 'PERFORM-PARSE')(self, $resolver, $context);
        nqp::findmethod(RakuAST::Routine, 'PERFORM-BEGIN')(self, $resolver, $context);
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        # Everything already done at parse time
        Nil
    }

    method IMPL-FINISH-ROLE-BODY(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        unless self.is-stub {
            self.IMPL-RESOLVE-FORWARD-LEXICALS($resolver);
            my $*IMPL-COMPILE-DYNAMICALLY := 1;
            # The body compiles here ahead of the unit, so it takes the
            # optimize walk and the lowering a BEGIN-time routine takes
            # in its compiler thunk.
            unless nqp::isconcrete(nqp::getattr(self, RakuAST::Code, '$!qast-block')) {
                self.IMPL-OPTIMIZE-AHEAD-OF-UNIT($resolver, $context);
                RakuAST::IMPL::VarLowering.analyze-routine(self, $resolver);
            }
            my $body-qast := self.IMPL-QAST-BLOCK($context, :blocktype<immediate>);
            nqp::bindattr(self, RakuAST::RoleBody, '$!fixup-nodes',
                self.IMPL-BEGIN-TIME-LEXICAL-FIXUP($context, $body-qast, $!fixup));
        }
    }

    # The body is code-generated here, ahead of the compilation unit's check
    # pass, so a forward reference inside it (for example a parameter or
    # attribute default naming a `sub` declared later in the body) is still
    # unresolved from its parse-time lookup. Resolve those now, pushing each
    # lexical scope as we descend so a lookup is re-resolved against its own
    # scope chain. This reaches declarations hoisted into a method, a nested
    # block, or a package nested in the body, not only body-level ones. Only
    # lookups that are still unresolved are touched, so an outer or CORE match
    # is never replaced and a role-local declaration still shadows an outer one.
    method IMPL-RESOLVE-FORWARD-LEXICALS(RakuAST::Resolver $resolver) {
        self.IMPL-RESOLVE-FORWARD-LEXICALS-IN($resolver, self);
    }

    method IMPL-RESOLVE-FORWARD-LEXICALS-IN(RakuAST::Resolver $resolver, $node) {
        my int $is-scope := nqp::istype($node, RakuAST::LexicalScope);
        $resolver.push-scope($node) if $is-scope;
        $node.visit-children(-> $child {
            if nqp::istype($child, RakuAST::Var::Lexical)
              && $child.needs-resolution && !$child.is-resolved {
                my $resolved := $resolver.resolve-lexical($child.name);
                $child.set-resolution($resolved) if $resolved;
            }
            self.IMPL-RESOLVE-FORWARD-LEXICALS-IN($resolver, $child);
        });
        $resolver.pop-scope() if $is-scope;
    }
}

# The commonalities of method-like things, whichever language their body is in
# (be it the main Raku language or the regex language).
class RakuAST::Methodish
  is RakuAST::Routine
{
    # A %_ in the body is the implicit slurpy parameter rather than a
    # placeholder that builds a signature. The 'method' target says so.
    method attach-target-names() {
        self.IMPL-WRAP-LIST(['method', 'routine', 'block'])
    }

    method default-scope() {
        self.name ?? 'has' !! 'anon'
    }

    method allowed-scopes() {
        self.IMPL-WRAP-LIST(['has', 'my', 'anon', 'our'])
    }

    method PERFORM-PARSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $package := $resolver.find-attach-target('package');
        if self.scope eq 'has' || self.scope eq 'our' {
            if $package {
                nqp::bindattr(self, RakuAST::Routine, '$!package', $package);
            }
        }
        else {
            # An anonymous or lexical method literal still records its enclosing
            # package, matching a sub and the legacy frontend. It is not attached
            # as a method of that package (see PERFORM-BEGIN).
            nqp::bindattr(self, RakuAST::Routine, '$!package',
                $package // $resolver.global-package);
        }
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.body.to-begin-time($resolver, $context); # In case it's the default created in the ctor.

        my $package := nqp::getattr(self, RakuAST::Routine, '$!package');
        # `has`/`our` are the scopes that make this a method of the package;
        # `my`/`anon` merely record the enclosing package for `.package`.
        my $is-package-method := self.scope eq 'has' || self.scope eq 'our';
        my $package-is-role := $package && $is-package-method && $package.declarator eq 'role';
        my $placeholder-signature := self.placeholder-signature;
        if $placeholder-signature {
            $placeholder-signature.set-is-on-method(True);
            $placeholder-signature.set-is-on-named-method(True) if self.name;
            $placeholder-signature.set-is-on-meta-method(True) if nqp::can(self, 'meta') && self.meta;
            $placeholder-signature.set-is-on-role-method(True) if $package-is-role;
            $placeholder-signature.set-invocant-type-check(self.IMPL-INVOCANT-TYPE-CHECK);
            $placeholder-signature.to-parse-time($resolver, $context);
            self.add-generated-lexical-declaration($_) for $placeholder-signature.IMPL-ENSURE-IMPLICITS($resolver, $context);
            $placeholder-signature.to-begin-time($resolver, $context);
        }

        unless $placeholder-signature || self.signature {
            nqp::bindattr(self, RakuAST::Routine, '$!signature', RakuAST::Signature.new);
        }

        # Make sure that our signature has resolutions performed.
        my $signature := self.signature;
        if $signature {
            $signature.set-parameters-initialized;
            $signature.set-default-type(
                RakuAST::Type::Setting.new(
                    RakuAST::Name.from-identifier('Any'),
                ).to-begin-time($resolver, $context)
            );
            $signature.set-is-on-method(True);
            $signature.set-is-on-named-method(True) if self.name;
            $signature.set-is-on-meta-method(True) if nqp::can(self, 'meta') && self.meta;
            $signature.set-is-on-role-method(True) if $package-is-role;
            $signature.set-invocant-type-check(self.IMPL-INVOCANT-TYPE-CHECK);
            $signature.to-parse-time($resolver, $context);
            self.add-generated-lexical-declaration($_) for $signature.IMPL-ENSURE-IMPLICITS($resolver, $context);
            $signature.to-begin-time($resolver, $context);
        }

        my str $name := self.name ?? self.name.canonicalize !! '';

        if $package && nqp::can($package, 'can-have-methods') {
            if $package.can-have-methods {
                $package.ATTACH-METHOD(self) if self.scope eq 'has';
            }
            elsif self.scope eq 'has' {
                self.add-worry:
                  $resolver.build-exception: 'X::Useless::Declaration',
                    name  => $name,
                    where => "a " ~ $package.parsed-declarator
            }
        }
        elsif self.scope eq 'has' {
            self.add-worry:
              $resolver.build-exception: 'X::Useless::Declaration',
                name  => $name,
                where => 'the mainline';
        }

        if self.multiness eq 'proto' {
            nqp::bindattr(self.meta-object, Routine, '@!dispatchees', []);
            $resolver.outer-scope.add-generated-lexical-declaration(self) if self.scope ne 'has';
        }

        # Install `our` multi/proto methods into the package's OUR stash
        # with duplicate detection.  Routine.PERFORM-BEGIN's stash install
        # excludes multi, and Methodish doesn't chain through it; non-multi
        # `our method` is handled by LexicalScope.PERFORM-CHECK.  Skip
        # roles and non-method-capable packages: their existing role-scope
        # / useless-declaration errors already report the issue.
        if self.lexical-name && self.scope eq 'our'
                && (self.multiness eq 'multi' || self.multiness eq 'proto') {
            my $install-pkg := $package;
            # Mainline (no enclosing package) falls back to GLOBAL.  Skip
            # during CORE.setting compile: GLOBAL's `.WHO` isn't wired up
            # that early in bootstrap.
            if !$install-pkg && !($*COMPILING_CORE_SETTING // 0) {
                $install-pkg := $resolver.global-package;
            }
            # GLOBAL is wrapped in a non-Package Implicit::Constant; only
            # real RakuAST::Package nodes carry `can-have-methods`.
            if $install-pkg
                    && !$package-is-role
                    && (!nqp::istype($install-pkg, RakuAST::Package)
                            || $install-pkg.can-have-methods) {
                my $stash := $install-pkg.stubbed-meta-object.WHO;
                if nqp::existskey($stash, self.lexical-name) {
                    self.add-sorry:
                        $resolver.build-exception: 'X::Redeclaration',
                            :symbol(self.declaration-name),
                            :what(self.declaration-kind);
                }
                else {
                    $stash{self.lexical-name} := self.meta-object;
                }
            }
        }

        self.IMPL-STUB-PHASERS($resolver, $context);

        my $stub := self.IMPL-STUB-CODE($resolver, $context);
        nqp::setcodename($stub, $name) if $name;

        self.meta-object.set_yada if self.is-stub;

        # Apply any traits.
        self.apply-traits($resolver, $context, self)
    }

    method PERFORM-CHECK(Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        nqp::findmethod(RakuAST::Routine, 'PERFORM-CHECK')(self, $resolver, $context);

        self.check-scope($resolver, self.declarator);
    }

    method IMPL-INVOCANT-TYPE-CHECK() {
        True
    }
}

# A method.
class RakuAST::Method
  is RakuAST::Methodish
  is RakuAST::SinkBoundary
{
    has RakuAST::Blockoid $.body;
    has Bool              $.meta;
    has Bool              $.private;
    has Mu                $!self-declaration;

    method new(          str :$scope,
                         str :$multiness,
                        Bool :$private,
                        Bool :$meta,
               RakuAST::Name :$name,
          RakuAST::Signature :$signature,
                        List :$traits,
           RakuAST::Blockoid :$body,
    RakuAST::Doc::Declarator :$WHY
    ) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', $scope);
        nqp::bindattr_s($obj, RakuAST::Routine, '$!multiness', $multiness //'');
        nqp::bindattr($obj, RakuAST::Method, '$!private',
          $private ?? True !! False);
        nqp::bindattr($obj, RakuAST::Method, '$!meta', $meta ?? True !! False);
        nqp::bindattr($obj, RakuAST::Routine, '$!name', $name // RakuAST::Name);
        nqp::bindattr($obj, RakuAST::Routine, '$!signature', $signature);
        # Doesn't look like we can find out whether we actually need &?ROUTINE
        # in time, so better be safe than sorry.
        nqp::bindattr($obj, RakuAST::Routine, '$!need-routine-variable', True);
        $obj.set-traits($traits);
        nqp::bindattr($obj, RakuAST::Method, '$!body',
          $body // RakuAST::Blockoid.new);
        $obj.set-WHY($WHY);
        $obj
    }

    method declarator() { 'method' }
    method declaration-kind() { 'method' }

    method replace-body(RakuAST::Blockoid $new-body) {
        nqp::bindattr(self, RakuAST::Method, '$!body', $new-body);
        # See RakuAST::Sub::replace-body for why the cache is dropped.
        nqp::bindattr(self, RakuAST::ImplicitDeclarations,
          '$!implicit-declarations-cache', Mu)
          if nqp::istype($new-body, RakuAST::OnlyStar);
        Nil
    }

    method set-meta(Bool $meta) {
        nqp::bindattr(self, RakuAST::Method, '$!meta', $meta ?? True !! False);
    }

    method set-private(Bool $private) {
        nqp::bindattr(self, RakuAST::Method, '$!private', $private ?? True !! False);
    }

    method IMPL-META-OBJECT-TYPE() { Method }

    # The one self declaration of this method. The grammar declares it
    # into scope ahead of the signature parse and the implicit list
    # carries it, so a resolution of self anywhere in the method
    # reaches the instance its frame declares.
    method IMPL-SELF-DECLARATION() {
        $!self-declaration
            // nqp::bindattr(self, RakuAST::Method, '$!self-declaration',
                RakuAST::VarDeclaration::Implicit::Self.new)
    }

    method PRODUCE-IMPLICIT-DECLARATIONS() {
        my $list := nqp::findmethod(RakuAST::Routine, 'PRODUCE-IMPLICIT-DECLARATIONS')(self);
        self.IMPL-UNWRAP-LIST($list).push: self.IMPL-SELF-DECLARATION;
        $list
    }

    method get-boundary-sink-propagator() {
        $!body.statement-list
    }

    method is-boundary-sunk() {
        return False if self.needs-result;
        my $signature := self.signature;
        $signature ?? $signature.provides-return-value !! False
    }

    method is-stub() {
        my @code := self.body.statement-list.code-statements;
        nqp::elems(@code) == 1
            && nqp::istype(@code[0], RakuAST::Statement::Expression)
            && nqp::istype(@code[0].expression, RakuAST::Stub)
    }

    method IMPL-COMPILE-BODY(RakuAST::IMPL::QASTContext $context) {
        # If our first expression is a stub object (!!!, ..., ???),
        # set the yada bit on the Method itself
        if (my $first-statement := nqp::atpos($!body.statement-list.code-statements, 0))
            && nqp::istype($first-statement, RakuAST::Statement::Expression)
            && nqp::istype($first-statement.expression, RakuAST::Stub)
        {
            self.meta-object.set_yada;
        }

        self.IMPL-CALCULATE-SINK unless self.sink-calculated;
        self.IMPL-WRAP-RETURN-HANDLER($context,
            self.IMPL-WRAP-SCOPE-HANDLER-QAST($context,
                self.IMPL-APPEND-SIGNATURE-RETURN($context, $!body.IMPL-TO-QAST($context))))
    }
}

# Just exists so we know this method is an attribute initializer, for better error messages.
class RakuAST::Method::Initializer
  is RakuAST::Method
{
}

# A submethod.
class RakuAST::Submethod
  is RakuAST::Method
{
    method IMPL-META-OBJECT-TYPE() { Submethod }

    method declarator() { 'submethod' }
}

class RakuAST::Method::AttributeAccessor
  is RakuAST::Method
{
    has str $!attr-name;
    has Mu $!type;
    has Mu $!package-type;
    has Bool $!rw;

    method new(RakuAST::Name :$name, str :$attr-name, Mu :$type, Mu :$package-type, Bool :$rw) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', 'has');
        nqp::bindattr_s($obj, RakuAST::Routine, '$!multiness', '');
        nqp::bindattr($obj, RakuAST::Method, '$!private', False);
        nqp::bindattr($obj, RakuAST::Method, '$!meta', False);
        nqp::bindattr($obj, RakuAST::Routine, '$!need-routine-variable', False);
        nqp::bindattr($obj, RakuAST::Method, '$!body', RakuAST::Blockoid.new);
        nqp::bindattr($obj, RakuAST::Routine, '$!signature', RakuAST::Signature.new);
        nqp::die('Accessor needs a name') unless $name;
        nqp::bindattr($obj, RakuAST::Routine, '$!name', $name);
        nqp::bindattr_s($obj, RakuAST::Method::AttributeAccessor, '$!attr-name', $attr-name);
        nqp::bindattr($obj, RakuAST::Method::AttributeAccessor, '$!type', $type);
        nqp::bindattr($obj, RakuAST::Method::AttributeAccessor, '$!package-type', $package-type);
        nqp::bindattr($obj, RakuAST::Method::AttributeAccessor, '$!rw', $rw // 0);
        $obj
    }

    method declarator() { 'submethod' }

    # The body is fully synthetic: nothing in it can reach the special
    # variables, so only self is declared.
    method PRODUCE-IMPLICIT-DECLARATIONS() {
        [ self.IMPL-SELF-DECLARATION ]
    }

    method PRODUCE-META-OBJECT(:$resolver, :$context) {
        my $meta := nqp::findmethod(RakuAST::Routine, 'PRODUCE-META-OBJECT')(self);
        $meta.set_rw if $!rw;
        $meta
    }

    # The block takes the shape the legacy frontend installs: the
    # invocant as a raw local parameter and a discarded named slurpy,
    # with no binder run. A Raku level caller passes hllized values
    # already, and an nqp level caller gets exactly what the legacy
    # frontend has always exposed. A type object invocant reaches the
    # body, where the attribute access dies. The lexical %_ stays
    # declared without per call setup, as the legacy frontend leaves
    # it.
    method IMPL-QAST-FORM-BLOCK(RakuAST::IMPL::QASTContext $context, str :$blocktype,
            RakuAST::Expression :$expression) {
        my $block := QAST::Block.new(
            :name(self.name.canonicalize), :blocktype('declaration_static'),
            QAST::Stmts.new(
                QAST::Var.new( :decl<param>, :scope<local>, :name('self') ),
                QAST::Var.new( :decl<param>, :scope<local>, :name('_'),
                    :slurpy, :named ),
                QAST::Var.new( :decl<static>, :scope<lexical>, :name('%_') )
            ),
            self.IMPL-COMPILE-BODY($context)
        );
        $block.arity(1);
        $block.annotate('count', 1);
        $block
    }

    method IMPL-COMPILE-BODY(RakuAST::IMPL::QASTContext $context) {
        # Is it a native attribute? (primpspec != 0)
        my $native := nqp::objprimspec($!type);
        $context.ensure-sc($!package-type);

        # Set up the actual statements, starting with "self"
        # nqp::attribute(self, $package_type, $attr_name)
        my $accessor := QAST::Var.new(
            :scope($native && $!rw ?? 'attributeref' !! 'attribute'),
            :name($!attr-name),
            :returns($!type),
            QAST::Op.new( :op<decont>, QAST::Var.new( :scope<local>, :name('self') ) ),
            QAST::WVal.new( :value($!package-type) ),
        );

        # Opaque and read-only accessors need a decont
        unless $native || $!rw {
            $accessor := QAST::Op.new( :op<decont>, $accessor );
        }

        $accessor
    }
}

# The generated POPULATE method: a flattened compile of the class's
# BUILDALLPLAN, which Mu.POPULATE otherwise interprets per object
# construction. Created by RakuAST::CompilerServices when the
# metamodel composes a class, mirroring what the legacy frontend's
# generate_buildplan_executor installs. The setting values the plan
# needs arrive resolved through the constructor, since this file
# cannot name them and composition may run before they exist.
class RakuAST::Submethod::BuildPlanExecutor
  is RakuAST::Submethod
{
    has Mu $!package-type;
    has Mu $!build-plan;
    has Mu $!True;
    has Mu $!Failure;
    has Mu $!X-Attribute-Required;
    has Mu $!return-routine;

    method new(Mu :$package-type, Mu :$build-plan, Mu :$True,
            Mu :$Failure, Mu :$X-Attribute-Required, Mu :$return-routine) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', 'has');
        nqp::bindattr_s($obj, RakuAST::Routine, '$!multiness', '');
        nqp::bindattr($obj, RakuAST::Method, '$!private', False);
        nqp::bindattr($obj, RakuAST::Method, '$!meta', False);
        nqp::bindattr($obj, RakuAST::Routine, '$!need-routine-variable', False);
        nqp::bindattr($obj, RakuAST::Method, '$!body', RakuAST::Blockoid.new);
        nqp::bindattr($obj, RakuAST::Routine, '$!signature', RakuAST::Signature.new);
        nqp::bindattr($obj, RakuAST::Routine, '$!name',
            RakuAST::Name.from-identifier('POPULATE'));
        nqp::bindattr($obj, RakuAST::Submethod::BuildPlanExecutor, '$!package-type', $package-type);
        nqp::bindattr($obj, RakuAST::Submethod::BuildPlanExecutor, '$!build-plan', $build-plan);
        nqp::bindattr($obj, RakuAST::Submethod::BuildPlanExecutor, '$!True', $True);
        nqp::bindattr($obj, RakuAST::Submethod::BuildPlanExecutor, '$!Failure', $Failure);
        nqp::bindattr($obj, RakuAST::Submethod::BuildPlanExecutor, '$!X-Attribute-Required', $X-Attribute-Required);
        nqp::bindattr($obj, RakuAST::Submethod::BuildPlanExecutor, '$!return-routine', $return-routine);
        $obj
    }

    # The body is fully synthetic: nothing in it can reach the special
    # variables, so only self is declared.
    method PRODUCE-IMPLICIT-DECLARATIONS() {
        [ RakuAST::VarDeclaration::Implicit::Self.new() ]
    }

    # The block binds the invocant and the initialization hash as raw
    # locals with no binder run, the shape the legacy frontend
    # installs. The definite invocant type on the signature informs
    # introspection, not a run time check. The lexical %_ stays
    # declared without per call setup.
    method IMPL-QAST-FORM-BLOCK(RakuAST::IMPL::QASTContext $context, str :$blocktype,
            RakuAST::Expression :$expression) {
        # The statements carry the composing class's file and line. A
        # setting class's frame then reads as setting code, which keeps
        # a callframe walk that skips setting frames moving past it,
        # and a user class's frame reports the user's own file. The
        # origin comes from the package via the compiler services.
        my $block := QAST::Block.new(
            :name<POPULATE>, :blocktype('declaration_static'),
            self.IMPL-SET-NODE(QAST::Stmts.new(
                QAST::Var.new( :decl<param>, :scope<local>, :name('self') ),
                QAST::Var.new( :decl<param>, :scope<local>, :name('%init') ),
                QAST::Var.new( :decl<var>, :scope<local>, :name('init') ),
                QAST::Var.new( :decl<var>, :scope<local>, :name('return') ),
                QAST::Var.new( :decl<static>, :scope<lexical>, :name('%_') )
            )),
            self.IMPL-COMPILE-BODY($context)
        );
        self.IMPL-SET-NODE($block);
        $block.arity(2);
        $block.annotate('count', 2);
        $block
    }

    method IMPL-COMPILE-BODY(RakuAST::IMPL::QASTContext $context) {
        # Mapping of primspec to attribute op postfix
        my @psp := ('','_i','_n','_s','','','','','','','_u');
        my $build-plan := $!build-plan;
        my $self := QAST::Var.new( :scope<local>, :name('self') );
        my $init := QAST::Var.new( :scope<local>, :name('init') );

        my $stmts := self.IMPL-SET-NODE(QAST::Stmts.new);

        # An empty plan produces the shared do nothing POPULATE, which
        # never touches the initialization hash.
        my int $count := nqp::elems($build-plan);
        unless $count {
            $stmts.push($self);
            return $stmts;
        }

        my int $needs-wrapping;

        # my $init := nqp::getattr(%init,Map,'$!storage')
        $context.ensure-sc(Map);
        $stmts.push(QAST::Op.new( :op<bind>,
            $init,
            QAST::Op.new( :op<getattr>,
                QAST::Var.new( :scope<local>, :name('%init') ),
                QAST::WVal.new( :value(Map) ),
                QAST::SVal.new( :value('$!storage') )
            )
        ));

        my int $i := -1;
        while nqp::islt_i($i := nqp::add_i($i, 1), $count) {
            if nqp::islist(my $task := nqp::atpos($build-plan, $i)) {
                $context.ensure-sc(nqp::atpos($task, 1));
                my $class := QAST::WVal.new( :value(nqp::atpos($task, 1)) );
                my $attr := QAST::SVal.new( :value(nqp::atpos($task, 2)) );

                my int $code := nqp::atpos($task, 0);

                # 0,1100,1200,1300 = initialize opaque from %init
                if $code == 0 || $code == 1100 || $code == 1200 || $code == 1300 {
                    my $getattr := QAST::Op.new( :op<getattr>, $self, $class, $attr );

                    # nqp::unless(
                    #   nqp::isnull(my \tmp = nqp::atkey($init,'a')),
                    my $tmp := QAST::Node.unique('buildall_tmp_');
                    my $if := QAST::Op.new( :op<unless>,
                        QAST::Op.new( :op<isnull>,
                            QAST::Op.new( :op<bind>,
                                QAST::Var.new( :name($tmp), :scope<local>, :decl<var> ),
                                QAST::Op.new( :op<atkey>,
                                    $init,
                                    QAST::SVal.new( :value(nqp::atpos($task, 3)) )
                                )
                            )
                        )
                    );

                    my str $sigil := nqp::substr(nqp::atpos($task, 2), 0, 1);

                    # nqp::getattr(self,Foo,'$!a').STORE(tmp, :INITIALIZE)
                    if $sigil eq '@' || $sigil eq '%' {
                        $context.ensure-sc($!True);
                        $if.push(
                            QAST::Op.new( :op<callmethod>, :name<STORE>,
                                $getattr,
                                QAST::Var.new( :name($tmp), :scope<local> ),
                                QAST::WVal.new( :value($!True), :named('INITIALIZE') )
                            )
                        );
                    }

                    # nqp::bindattr(self,Foo,'$!a',tmp)
                    elsif $code == 1300 {
                        my $arg := QAST::Var.new( :name($tmp), :scope<local> );
                        if nqp::elems($task) == 5 {
                            $context.ensure-sc(nqp::atpos($task, 4));
                            $arg := QAST::Op.new( :op('p6bindassert'),
                                $arg,
                                QAST::WVal.new( :value(nqp::atpos($task, 4)) )
                            );
                        }
                        $if.push(
                            QAST::Op.new( :op('bindattr'), $self, $class, $attr, $arg )
                        );
                    }

                    # nqp::getattr(self,Foo,'$!a') = tmp
                    else {
                        $if.push(
                            QAST::Op.new(
                                :op( $sigil eq '$' || $sigil eq '&'
                                       ?? 'p6assign' !! 'p6store' ),
                                $getattr,
                                QAST::Var.new( :name($tmp), :scope<local> )
                            )
                        );
                    }

                    # 1100,1200: bindattr(self,Foo,'$!a',nqp::list or hash)
                    # when the key is absent
                    if $code == 1100 || $code == 1200 {
                        $if.push(
                            QAST::Op.new( :op<bindattr>,
                                $self, $class, $attr,
                                QAST::Op.new( :op($code == 1100 ?? 'list' !! 'hash') )
                            )
                        );
                    }

                    $stmts.push($if);
                }

                # 1,2,3,10 = initialize native from %init
                elsif $code < 100 {
                    my $tmp := QAST::Node.unique('buildall_tmp_');
                    $stmts.push(
                        QAST::Op.new( :op<unless>,
                            QAST::Op.new( :op<isnull>,
                                QAST::Op.new( :op<bind>,
                                    QAST::Var.new( :decl<var>, :name($tmp), :scope<local> ),
                                    QAST::Op.new( :op<atkey>,
                                        $init,
                                        QAST::SVal.new( :value(nqp::atpos($task, 3)) )
                                    )
                                )
                            ),
                            QAST::Op.new( :op('bindattr' ~ @psp[$code]),
                                $self, $class, $attr,
                                QAST::Op.new( :op<decont>,
                                    QAST::Var.new( :name($tmp), :scope<local> ) )
                            )
                        )
                    );
                }

                # 400,1400 = set opaque with default if not set yet
                elsif $code == 400 || $code == 1400 {
                    my $getattr := QAST::Op.new( :op<getattr>, $self, $class, $attr );
                    my $unless := QAST::Op.new( :op<unless>,
                        QAST::Op.new( :op<p6attrinited>, $getattr )
                    );

                    $context.ensure-sc(nqp::atpos($task, 3));
                    my $initializer := nqp::istype(nqp::atpos($task, 3), Block)
                        ?? QAST::Op.new( :op<call>,
                             QAST::WVal.new( :value(nqp::atpos($task, 3)) ),
                             $self, $getattr )
                        !! QAST::WVal.new( :value(nqp::atpos($task, 3)) );

                    my str $sigil := nqp::substr(nqp::atpos($task, 2), 0, 1);
                    if $sigil eq '@' || $sigil eq '%' {
                        $context.ensure-sc($!True);
                        $unless.push(
                            QAST::Op.new( :op<callmethod>, :name<STORE>,
                                $getattr, $initializer,
                                QAST::WVal.new( :value($!True), :named('INITIALIZE') )
                            )
                        );
                    }
                    elsif $code == 1400 {
                        if nqp::elems($task) == 5 {
                            $context.ensure-sc(nqp::atpos($task, 4));
                            $initializer := QAST::Op.new( :op('p6bindassert'),
                                $initializer,
                                QAST::WVal.new( :value(nqp::atpos($task, 4)) )
                            );
                        }
                        $unless.push(
                            QAST::Op.new( :op('bindattr'),
                                $self, $class, $attr, $initializer )
                        );
                    }
                    else {
                        $unless.push(
                            QAST::Op.new(
                                :op( $sigil eq '$' || $sigil eq '&'
                                       ?? 'p6assign' !! 'p6store' ),
                                $getattr, $initializer
                            )
                        );
                    }

                    $stmts.push($unless);
                }

                # 401,402,410 = set native numeric with default if not set
                elsif $code == 401 || $code == 402 || $code == 410 {
                    my $getattr := QAST::Op.new(
                        :op('getattr' ~ @psp[$code - 400]),
                        $self, $class, $attr
                    );
                    $context.ensure-sc(nqp::atpos($task, 3));
                    $stmts.push(
                        QAST::Op.new( :op<if>,
                            QAST::Op.new(
                                :op('iseq' ~ ($code == 410 ?? '_i' !! @psp[$code - 400])),
                                $getattr,
                                $code == 402
                                    ?? QAST::NVal.new( :value(0e0) )
                                    !! QAST::IVal.new( :value(0) )
                            ),
                            QAST::Op.new( :op('bindattr' ~ @psp[$code - 400]),
                                $self, $class, $attr,
                                nqp::istype(nqp::atpos($task, 3), Block)
                                    ?? QAST::Op.new( :op<call>,
                                         QAST::WVal.new( :value(nqp::atpos($task, 3)) ),
                                         $self, $getattr )
                                    !! ($code == 402
                                         ?? QAST::NVal.new( :value(nqp::atpos($task, 3)) )
                                         !! QAST::IVal.new( :value(nqp::atpos($task, 3)) ))
                            )
                        )
                    );
                }

                # 403 = set native string with default if not set
                elsif $code == 403 {
                    my $getattr := QAST::Op.new( :op<getattr_s>, $self, $class, $attr );
                    $context.ensure-sc(nqp::atpos($task, 3));
                    $stmts.push(
                        QAST::Op.new( :op<if>,
                            QAST::Op.new( :op<isnull_s>, $getattr ),
                            QAST::Op.new( :op<bindattr_s>,
                                $self, $class, $attr,
                                nqp::istype(nqp::atpos($task, 3), Block)
                                    ?? QAST::Op.new( :op<call>,
                                         QAST::WVal.new( :value(nqp::atpos($task, 3)) ),
                                         $self, $getattr )
                                    !! QAST::SVal.new( :value(nqp::atpos($task, 3)) )
                            )
                        )
                    );
                }

                # 800 = die if opaque not yet initialized
                # 1501,1502,1510 = die if int, num, uint is zero
                # 1503 = die if str is null_s
                elsif $code == 800 || $code > 1500 && $code < 1600 {
                    my $check;
                    if $code == 1501 {
                        $check := QAST::Op.new( :op<getattr_i>, $self, $class, $attr );
                    }
                    elsif $code == 1502 {
                        $check := QAST::Op.new( :op<getattr_n>, $self, $class, $attr );
                    }
                    elsif $code == 1503 {
                        $check := QAST::Op.new( :op<not_i>,
                            QAST::Op.new( :op<isnull_s>,
                                QAST::Op.new( :op<getattr_s>, $self, $class, $attr )
                            )
                        );
                    }
                    elsif $code == 1510 {
                        $check := QAST::Op.new( :op<getattr_u>, $self, $class, $attr );
                    }
                    else {
                        $check := QAST::Op.new( :op<p6attrinited>,
                            QAST::Op.new( :op<getattr>, $self, $class, $attr )
                        );
                    }
                    $context.ensure-sc($!X-Attribute-Required);
                    $context.ensure-sc(nqp::atpos($task, 3));
                    $stmts.push(
                        QAST::Op.new( :op<unless>,
                            $check,
                            QAST::Op.new( :op<callmethod>, :name<throw>,
                                QAST::Op.new( :op<callmethod>, :name<new>,
                                    QAST::WVal.new( :value($!X-Attribute-Required) ),
                                    QAST::SVal.new( :named('name'),
                                        :value(nqp::atpos($task, 2)) ),
                                    QAST::WVal.new( :named('why'),
                                        :value(nqp::atpos($task, 3)) )
                                )
                            )
                        )
                    );
                }

                # 900 = run attribute container initializer
                elsif $code == 900 {
                    $context.ensure-sc(nqp::atpos($task, 3));
                    $stmts.push(
                        QAST::Op.new( :op<bindattr>,
                            $self, $class, $attr,
                            QAST::Op.new( :op<call>,
                                QAST::WVal.new( :value(nqp::atpos($task, 3)) ) )
                        )
                    );
                }

                # 1000 = vivify a mixin attribute for the side effect
                elsif $code == 1000 {
                    $stmts.push(
                        QAST::Op.new( :op<getattr>, $self, $class, $attr )
                    );
                }

                else {
                    nqp::die('Invalid '
                      ~ $!package-type.HOW.name($!package-type)
                      ~ '.POPULATE plan: ' ~ $code);
                }
            }

            # BUILD or TWEAK submethod: call it with the original nameds
            # and return any Failure it produces
            else {
                $needs-wrapping := 1;
                $context.ensure-sc($task);
                $context.ensure-sc($!Failure);
                $context.ensure-sc($!return-routine);
                my $return := QAST::Var.new( :scope<local>, :name<return> );
                $stmts.push(
                    QAST::Op.new( :op<if>,
                        QAST::Op.new( :op<istype>,
                            QAST::Op.new( :op<bind>,
                                $return,
                                QAST::Op.new( :op<if>,
                                    QAST::Op.new( :op<elems>, $init ),
                                    QAST::Op.new( :op<call>,
                                        QAST::WVal.new( :value($task) ),
                                        $self,
                                        QAST::Var.new( :scope<local>, :name<init>,
                                            :flat(1), :named(1) )
                                    ),
                                    QAST::Op.new( :op<call>,
                                        QAST::WVal.new( :value($task) ),
                                        $self
                                    )
                                )
                            ),
                            QAST::WVal.new( :value($!Failure) )
                        ),
                        QAST::Op.new( :op<call>,
                            QAST::WVal.new( :value($!return-routine) ),
                            $return
                        )
                    )
                );
            }
        }

        $stmts.push($self);

        # The return call for a Failure producing BUILD or TWEAK is a
        # control exception, which needs its catching counterpart in
        # this frame.
        if $needs-wrapping {
            $stmts := QAST::Op.new( :op<handlepayload>,
                $stmts,
                'RETURN',
                QAST::Op.new( :op<lastexpayload> )
            );
        }

        $stmts
    }
}

class RakuAST::Method::ClassAccessor
  is RakuAST::Method
{
    method IMPL-WRAP-RETURN-HANDLER(RakuAST::IMPL::QASTContext $context, QAST::Node $qast) {
        $qast
    }
}

# Base class for regex declaration, such as `token foo { bar }`. This
# implies its own lexical scope.
class RakuAST::RegexDeclaration
  is RakuAST::Methodish
{
    has RakuAST::Regex $.body;
    has            str $.source;

    method new(          str :$scope,
                         str :$multiness,
               RakuAST::Name :$name,
          RakuAST::Signature :$signature,
                        List :$traits,
              RakuAST::Regex :$body,
                         str :$source,
    RakuAST::Doc::Declarator :$WHY
    ) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', $scope);
        nqp::bindattr_s($obj, RakuAST::Routine, '$!multiness', $multiness //'');
        nqp::bindattr($obj, RakuAST::Routine, '$!name', $name // RakuAST::Name);
        nqp::bindattr($obj, RakuAST::Routine, '$!signature',
            $signature // RakuAST::Signature.new);
        $obj.set-traits($traits);
        nqp::bindattr($obj, RakuAST::RegexDeclaration, '$!body',
            $body // RakuAST::Regex::Assertion::Fail.new);
        $obj.set-source($source);
        $obj.set-WHY($WHY);
        $obj
    }

    method declarator() { 'regex' }

    method replace-body(RakuAST::Regex $new-body) {
        nqp::bindattr(self, RakuAST::RegexDeclaration, '$!body', $new-body);
        Nil
    }

    method set-source($source) {
        nqp::bindattr_s(self, RakuAST::RegexDeclaration, '$!source',
          $source // '');
        Nil
    }

    method PRODUCE-META-OBJECT(:$resolver, :$context) {
        my $meta := nqp::findmethod(RakuAST::Routine, 'PRODUCE-META-OBJECT')(self);
        nqp::bindattr_s($meta, $meta.WHAT, '$!source',
          self.declarator ~ ' ' ~ $!source
        );
        $meta
    }

    method IMPL-META-OBJECT-TYPE() { Regex }

    method IMPL-INVOCANT-TYPE-CHECK() {
        self.scope ne 'my' && self.scope ne 'our'
    }

    method PRODUCE-IMPLICIT-DECLARATIONS() {
        my @declarations := [
            RakuAST::VarDeclaration::Implicit::Special.new(:name('$/')),
            RakuAST::VarDeclaration::Implicit::Special.new(:name('$!')),
            RakuAST::VarDeclaration::Implicit::Special.new(:name('$_')),
            RakuAST::VarDeclaration::Implicit::Self.new(),
            RakuAST::VarDeclaration::Implicit::Cursor.new(),
        ];
        nqp::push(@declarations, RakuAST::VarDeclaration::Implicit::Routine.new())
            if nqp::getattr(self, RakuAST::Routine, '$!need-routine-variable');
        @declarations
    }

    method IMPL-COMPILE-BODY(RakuAST::IMPL::QASTContext $context) {
        my %mods;
        %mods<s> := 1 if self.declarator eq 'rule';
        %mods<r> := 1 if self.declarator ne 'regex';

        my $name := self.name;
        $name := $name ?? $name.canonicalize !! "";

        self.IMPL-SET-NODE(
            QAST::Stmts.new(
                # Regex compiler wants a local named "self"
                QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :decl('var'), :scope('local'), :name('self') ),
                    QAST::Var.new( :scope('lexical'), :name('self') )
                ),
                $!body.IMPL-REGEX-TOP-LEVEL-QAST(
                  $context, self.meta-object, %mods, :$name
                )
            ), :key)
    }
}

class RakuAST::TokenDeclaration
  is RakuAST::RegexDeclaration
{
    method declarator() { 'token' }
}

class RakuAST::RuleDeclaration
  is RakuAST::RegexDeclaration
{
    method declarator() { 'rule' }
}

# Done by things that "thunk" a regex - that is to say, they want to compile as
# a separate regex code object but without introducing a new lexical scope. This
# includes quoted regexes like /.../, capturing groups, and calls of the form
# `<?before foo>`, where `foo` is the thunked regex.
class RakuAST::RegexThunk
  is RakuAST::Code
  is RakuAST::Meta
  is RakuAST::BeginTime
{
    has int $!decls-placed-inline;

    method IMPL-PLACE-DECLS-INLINE() {
        nqp::bindattr_i(self, RakuAST::RegexThunk, '$!decls-placed-inline', 1);
        Nil
    }
    method IMPL-DECLS-PLACED-INLINE() { $!decls-placed-inline }

    # Gather the regex-thunk declarations of this regex's subtree so the
    # regex carries them in its own frame. A dynamically compiled regex
    # serializes as a value, and a declaration left to an enclosing frame
    # is only ever bound in the compile-time frame instance, which does
    # not survive precompilation.
    method IMPL-NESTED-REGEX-THUNK-DECLS(RakuAST::IMPL::QASTContext $context) {
        my $stmts := QAST::Stmts.new;
        my @todo := [self];
        while @todo {
            my $visit := @todo.shift;
            $visit.visit-children: -> $node {
                if nqp::istype($node, RakuAST::RegexThunk) {
                    $node.IMPL-PLACE-DECLS-INLINE();
                    $stmts.push($node.IMPL-QAST-DECL-CODE($context));
                }
                elsif nqp::istype($node, RakuAST::Code) {
                    # A plain code block keeps its usual placement.
                }
                else {
                    nqp::push(@todo, $node);
                }
            }
        }
        $stmts
    }

    method PRODUCE-META-OBJECT(:$resolver, :$context) {
        # Create default signature, receiving invocant only.
        my $signature := nqp::create(Signature);
        my $parameter := nqp::create(Parameter);
        nqp::bindattr($parameter, Parameter, '$!type', Mu);
        nqp::bindattr($signature, Signature, '@!params', nqp::list($parameter));
        nqp::bindattr_i($signature, Signature, '$!arity', 1);
        nqp::bindattr($signature, Signature, '$!count', nqp::box_i(1, Int));
        nqp::bindattr_i($signature, Signature, '$!readonly', 1);

        # Create Regex object.
        my $regex := nqp::create(Regex);
        nqp::bindattr($regex, Code, '$!signature', $signature);
        nqp::bindattr_s($regex, Regex, '$!source', self.origin ?? self.origin.Str !! self.DEPARSE);
        nqp::bindattr($signature, Signature, '$!code', $regex);
        $regex
    }

    method IMPL-QAST-FORM-BLOCK(RakuAST::IMPL::QASTContext $context, str :$blocktype,
            RakuAST::Expression :$expression) {
        my $slash := RakuAST::VarDeclaration::Implicit::Special.new(:name('$/'));
        my $thunk := self.IMPL-THUNKED-REGEX-QAST($context); # must be before nested blocks
        my $nested-decls := $*IMPL-COMPILE-DYNAMICALLY || $*EMIT-BEGIN-SHAPE
            ?? self.IMPL-NESTED-REGEX-THUNK-DECLS($context)
            !! QAST::Stmts.new;
        QAST::Block.new(
            :blocktype('declaration_static'),
            QAST::Var.new( :decl('var'), :scope('local'), :name('self') ),
            QAST::Var.new( :decl('var'), :scope('lexical'), :name('$¢') ),
            QAST::Op.new(
              :op('bind'),
              QAST::Var.new(:name('$?REGEX'), :scope<lexical>, :decl('var')),
              QAST::Op.new(
                  :op('getcodeobj'),
                  QAST::Op.new( :op('curcode') )
              )
            ),
            $slash.IMPL-QAST-DECL($context),
            QAST::Var.new(
                :decl('param'), :scope('local'), :name('__lowered_param'),
                QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :scope('local'), :name('self') ),
                    QAST::Op.new(
                        :op('decont'),
                        QAST::Var.new( :scope('local'), :name('__lowered_param') )
                    )
                )
            ),
            $nested-decls,
            $thunk
        )
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.IMPL-STUB-CODE($resolver, $context);

        Nil
    }
}

# A language construct that does some kind of pattern matching. These all have
# adverbs in common.
class RakuAST::QuotedMatchConstruct
  is RakuAST::Term
  is RakuAST::BeginTime
{
    has List $.adverbs;

    method replace-adverbs(List $adverbs) {
        my @checked-adverbs;
        if $adverbs {
            for self.IMPL-UNWRAP-LIST($adverbs) {
                unless nqp::istype($_, RakuAST::QuotePair) {
                    nqp::die('A regex adverb may only be a RakuAST::QuotePair');
                }
                nqp::push(@checked-adverbs, $_);
            }
        }
        nqp::bindattr(self, RakuAST::QuotedMatchConstruct, '$!adverbs',
            @checked-adverbs);
        Nil
    }

    method IMPL-NORMALIZE-ADVERB(str $adverb) {
        my constant NORMS := nqp::hash(
            'ignorecase',   'i',
            'ignoremark',   'm',
            'ratchet',      'r',
            'sigspace',     's',
            'continue',     'c',
            'pos',          'p',
            'th',           'nth',
            'st',           'nth',
            'nd',           'nth',
            'rd',           'nth',
            'global',       'g',
            'overlap',      'ov',
            'exhaustive',   'ex',
            'Perl5',        'P5',
            'samecase',     'ii',
            'samespace',    'ss',
            'samemark',     'mm',
            'squash',       's',
            'complement',   'c',
            'delete',       'd'
        );
        NORMS{$adverb} // $adverb
    }

    method IMPL-IS-COMPILATION-ADVERB(str $norm-adverb) {
        my constant COMPS := nqp::hash('i', 1, 'm', 1, 'r', 1, 's', 1, 'P5', 1);
        nqp::existskey(COMPS, $norm-adverb)
    }

    method IMPL-IS-POSITION-ADVERB(str $norm-adverb) {
        my constant POS := nqp::hash('c', 1, 'p', 1);
        nqp::existskey(POS, $norm-adverb)
    }

    method IMPL-IS-MULTIPLE-ADVERB(str $norm-adverb) {
        my constant POS := nqp::hash('x', 1, 'g', 1, 'ov', 1, 'ex', 1);
        nqp::existskey(POS, $norm-adverb)
    }

    method IMPL-SUBST-TO-MATCH-ADVERB(str $adverb) {
        my constant S2M := nqp::hash('ii', 'i', 'ss', 's', 'mm', 'm');
        S2M{$adverb} // $adverb
    }

    method IMPL-ADVERBS-TO-COMPILATION-MODS() {
        # Obtain adverbs that affect compilation and install them into
        # the %mods hash.
        my %mods;
        for self.IMPL-UNWRAP-LIST(self.adverbs) {
            my str $norm := self.IMPL-SUBST-TO-MATCH-ADVERB(self.IMPL-NORMALIZE-ADVERB($_.key));
            if self.IMPL-IS-COMPILATION-ADVERB($norm) {
                %mods{$norm} := $_.simple-compile-time-quote-value() ?? 1 !! 0;
            }
        }
        %mods
    }

    method IMPL-VISIT-ADVERBS(Code $visitor) {
        for self.IMPL-UNWRAP-LIST($!adverbs) {
            $visitor($_);
        }
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.IMPL-STUB-CODE($resolver, $context);
        Nil
    }

    method IMPL-IS-CONSTANT() {
        False
    }
}

# A quoted regex, such as `/abc/` or `rx/def/` or `m/ghi/`. Does not imply a
# new lexical scope.
class RakuAST::QuotedRegex
  is RakuAST::RegexThunk
  is RakuAST::QuotedMatchConstruct
  is RakuAST::Sinkable
  is RakuAST::ImplicitLookups
  is RakuAST::CheckTime
{
    has RakuAST::Regex $.body;
    has Bool $.match-immediately;

    method new(RakuAST::Regex :$body, Bool :$match-immediately, List :$adverbs) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::QuotedRegex, '$!body',
            $body // RakuAST::Regex::Assertion::Fail.new);
        nqp::bindattr($obj, RakuAST::QuotedRegex, '$!match-immediately',
            $match-immediately ?? True !! False);
        $obj.replace-adverbs($adverbs // List);
        $obj
    }

    method replace-body(RakuAST::Regex $new-body) {
        nqp::bindattr(self, RakuAST::QuotedRegex, '$!body', $new-body);
        Nil
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        [
            RakuAST::Var::Lexical.new('$_'),
            RakuAST::Var::Lexical.new('$/'),
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier-parts(
                'Rakudo', 'Internals', 'RegexBoolification6cMarker'
            ))
        ]
    }

    method IMPL-IS-IMMEDIATE-MATCH-ADVERB(str $norm-adverb) {
        $norm-adverb eq 'nth' || self.IMPL-IS-POSITION-ADVERB($norm-adverb) ||
            self.IMPL-IS-MULTIPLE-ADVERB($norm-adverb)
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        # Check adverbs
        for self.IMPL-UNWRAP-LIST(self.adverbs) {
            my str $key := $_.key;
            my str $norm := self.IMPL-NORMALIZE-ADVERB($key);
            if self.IMPL-IS-COMPILATION-ADVERB($norm) {
                # Compile-time adverbs must have a simple compile time value.
                unless nqp::isconcrete($_.simple-compile-time-quote-value()) {
                    self.add-sorry:
                      $resolver.build-exception: 'X::Value::Dynamic',
                        what => "Adverb $key";
                }
            }
            elsif !($!match-immediately && self.IMPL-IS-IMMEDIATE-MATCH-ADVERB($norm)) {
                # Not applicable to the construct, so report.
                self.add-sorry:
                  $resolver.build-exception: 'X::Syntax::Regex::Adverb',
                    adverb    => $key,
                    construct => $!match-immediately ?? 'm' !! 'rx'
            }
        }
    }

    method IMPL-THUNKED-REGEX-QAST(RakuAST::IMPL::QASTContext $context) {
        $!body.IMPL-REGEX-TOP-LEVEL-QAST($context, self.meta-object,
            self.IMPL-ADVERBS-TO-COMPILATION-MODS())
    }

    method IMPL-QAST-DECL-CODE(RakuAST::IMPL::QASTContext $context) {
        # Form the block itself and link it with the meta-object.
        self.IMPL-QAST-BLOCK($context, :blocktype('declaration_static'));
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $closure := self.IMPL-CLOSURE-QAST($context, :regex);
        if $!match-immediately {
            my $lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
            my $topic   := $lookups[0].IMPL-TO-QAST($context);
            my $slash   := $lookups[1].IMPL-TO-QAST($context);

            my $match-qast := QAST::Op.new(
              :op('callmethod'), :name('match'), $topic, $closure
            );
            my int $is-multiple-match;
            for self.IMPL-UNWRAP-LIST(self.adverbs) {
                my str $norm := self.IMPL-NORMALIZE-ADVERB($_.key);
                if nqp::istype($_,RakuAST::ColonPair::True)
                  && self.IMPL-IS-POSITION-ADVERB($norm) {
                    # These need to be passed the end of the last match.
                    $match-qast.push: QAST::Op.new:
                        :named($norm), :op<if>,
                        $slash,
                        QAST::Op.new( :op<callmethod>, :name<to>, $slash ),
                        QAST::IVal.new( :value(0) )
                }
                else {
                    # Pass the value of the pair.
                    my $arg := $_.value.IMPL-TO-QAST($context);
                    $arg.named($_.key);
                    $match-qast.push($arg);
                    $is-multiple-match := 1 if self.IMPL-IS-MULTIPLE-ADVERB($norm);
                }
            }
            if $is-multiple-match {
                # Don't update $/ in the list case
                $match-qast
            }
            else {
                QAST::Op.new(
                  :op('decont'),
                  QAST::Op.new(:op('p6store'), $slash, $match-qast)
                )
            }
        }
        else {
            self.sunk
                ?? QAST::Op.new( :op('callmethod'), :name('Bool'), $closure )
                !! $closure
        }
    }

    method IMPL-TWEAK-REGEX-CLONE(RakuAST::IMPL::QASTContext $context, Mu $clone) {
        my $lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
        if $context.lang-version lt 'd' {
            my $topic := $lookups[2].IMPL-TO-QAST($context);
            $topic.named('topic');
            $clone.push($topic);
        }
        else {
            my $topic := $lookups[0].IMPL-TO-QAST($context);
            $topic.named('topic');
            $clone.push($topic);
            my $slash := $lookups[1].IMPL-TO-QAST($context);
            $slash.named('slash');
            $clone.push($slash);
        }
        Nil
    }

    method visit-children(Code $visitor) {
        self.IMPL-VISIT-ADVERBS($visitor);
        $visitor($!body);
    }
}

# A substitution, such as `s/abc/def/`, `S/not_in/place/`, or `s/abc/ = 'def'`.
class RakuAST::Substitution
  is RakuAST::RegexThunk
  is RakuAST::QuotedMatchConstruct
  is RakuAST::ImplicitLookups
  is RakuAST::CheckTime
{
    has Bool $.immutable;
    has Bool $.samespace;
    has RakuAST::Regex $.pattern;
    has RakuAST::Infixish $.infix;
    has RakuAST::Expression $.replacement;

    method new(Bool :$immutable, Bool :$samespace, List :$adverbs,
            RakuAST::Regex :$pattern!, RakuAST::Infixish :$infix,
            RakuAST::Expression :$replacement!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Substitution, '$!immutable',
            $immutable ?? True !! False);
        nqp::bindattr($obj, RakuAST::Substitution, '$!samespace',
            $samespace ?? True !! False);
        $obj.replace-adverbs($adverbs // List);
        nqp::bindattr($obj, RakuAST::Substitution, '$!pattern', $pattern);
        nqp::bindattr($obj, RakuAST::Substitution, '$!infix', $infix);
        nqp::bindattr($obj, RakuAST::Substitution, '$!replacement', $replacement);
        $obj
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        [
            RakuAST::Var::Lexical.new('$_'),
            RakuAST::Var::Lexical.new('$/'),
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Positional')),
        ]
    }

    method IMPL-IS-SUBST-MATCH-ADVERB(str $norm-adverb) {
        my constant SUBST_OK := nqp::hash(
            'x', 1, 'g', 1, 'nth', 1,
            'ii', 1, 'ss', 1, 'mm', 1);
        self.IMPL-IS-POSITION-ADVERB($norm-adverb) || nqp::existskey(SUBST_OK, $norm-adverb)
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        # Check adverbs
        for self.IMPL-UNWRAP-LIST(self.adverbs) {
            my str $key := $_.key;
            my str $norm := self.IMPL-NORMALIZE-ADVERB($key);
            if self.IMPL-IS-COMPILATION-ADVERB(self.IMPL-SUBST-TO-MATCH-ADVERB($norm)) {
                # Compile-time adverbs must have a simple compile time value.
                unless nqp::isconcrete($_.simple-compile-time-quote-value()) {
                    self.add-sorry:
                      $resolver.build-exception: 'X::Value::Dynamic',
                        what => "Adverb $key";
                }
            }
            elsif !self.IMPL-IS-SUBST-MATCH-ADVERB($norm) {
                # Not applicable to the construct, so report.
                self.add-sorry:
                  $resolver.build-exception: 'X::Syntax::Regex::Adverb',
                    adverb    => $key,
                    construct => $!immutable ?? 'S' !! 's';
            }
        }

        # Thunk the replacement part.
        $!replacement.wrap-with-thunk: RakuAST::SubstitutionReplacementThunk.new:
            :infix($!infix);
        $!replacement.visit-thunks(-> $thunk { $thunk.ensure-begin-performed($resolver, $context) });

        self.IMPL-QAST-BLOCK($context, :blocktype<declaration_static>);
    }

    method IMPL-ADVERBS-TO-COMPILATION-MODS() {
        my %mods := nqp::findmethod(RakuAST::QuotedMatchConstruct, 'IMPL-ADVERBS-TO-COMPILATION-MODS')(self);
        %mods<s> := 1 if $!samespace;
        %mods
    }

    method IMPL-THUNKED-REGEX-QAST(RakuAST::IMPL::QASTContext $context) {
        $!pattern.IMPL-REGEX-TOP-LEVEL-QAST($context, self.meta-object,
            self.IMPL-ADVERBS-TO-COMPILATION-MODS())
    }

    method IMPL-QAST-DECL-CODE(RakuAST::IMPL::QASTContext $context) {
        # Form the block itself and link it with the meta-object.
        self.IMPL-QAST-BLOCK($context, :blocktype('declaration_static'));
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        # Coerce the topic into a Str before we start (we need to do that for
        # applying the match results anyway, so may as well avoid a double
        # coercion in the call to .match also).
        my $lookups    := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
        my $topic      := $lookups[0];
        my $slash      := $lookups[1];
        my $Positional := $lookups[2].compile-time-value;

        my $topic-str-var := QAST::Node.unique('subst_topic_str');
        my $result := self.IMPL-SET-NODE(
            QAST::Stmts.new(
                QAST::Op.new:
                    :op('bind'),
                    QAST::Var.new( :decl('var'), :scope('local'), :name($topic-str-var) ),
                    QAST::Op.new:
                        :op('callmethod'), :name('Str'),
                        $topic.IMPL-TO-QAST($context) ), :key);

        # Compile the call to match the regex against the stringified topic,
        # binding it into a result variable. While emitting adverbs, take
        # note of those needed for the replacement also.
        my $regex-closure := self.IMPL-CLOSURE-QAST($context);
        my $match-qast := QAST::Op.new:
            :op('callmethod'), :name('match'),
            QAST::Var.new( :scope('local'), :name($topic-str-var) ),
            $regex-closure;
        my $match-lookup := $slash.IMPL-TO-QAST($context);
        my int $samespace := $!samespace;
        my int $sigspace := $samespace;
        my int $samecase;
        my int $samemark;
        if $!samespace {
            my $arg := QAST::IVal.new(:value(1));
            $arg.named('samespace');
            $match-qast.push($arg);
        }
        for self.IMPL-UNWRAP-LIST(self.adverbs) {
            my str $norm := self.IMPL-NORMALIZE-ADVERB($_.key);
            if nqp::istype($_,RakuAST::ColonPair::True)
              && self.IMPL-IS-POSITION-ADVERB($norm) {
                # These need to be passed the end of the last match.
                $match-qast.push: QAST::Op.new:
                    :named($norm), :op<if>,
                    $match-lookup,
                    QAST::Op.new( :op<callmethod>, :name<to>, $match-lookup ),
                    QAST::IVal.new( :value(0) ) unless $norm eq 'ss' && $!samespace;
            }
            else {
                # Pass the value of the pair.
                my $arg := $_.value.IMPL-TO-QAST($context);
                $arg.named($_.key);
                $match-qast.push($arg);

                # Take note of interesting ones for the replacement.
                if $norm eq 'ii' {
                    $samecase := 1;
                }
                elsif $norm eq 'mm' {
                    $samemark := 1;
                }
                elsif $norm eq 'ss' {
                    $samespace := 1;
                    $sigspace := 1;
                }
                elsif $norm eq 's' {
                    $sigspace := 1;
                }
            }
        }
        my $match-result-var := QAST::Node.unique('subst_match');
        my $list-result      := QAST::Node.unique('subst_list_result');
        $result.push: QAST::Op.new:
            :op('bind'),
            QAST::Var.new( :decl('var'), :scope('local'), :name($match-result-var) ),
            $match-qast;

        # Assign the result to $/.
        $result.push: QAST::Op.new:
            :op('p6store'),
            $match-lookup,
            QAST::Var.new( :scope('local'), :name($match-result-var) );

        # Obtain the replacement part and build the call to apply it to
        # the matches.
        my $replacement-closure := $!replacement.IMPL-TO-QAST($context);
        my $apply-matches-meth := Str.HOW.find_private_method(Str, 'APPLY-MATCHES');
        my $apply-call := QAST::Op.new:
            :op('call'),
            QAST::WVal.new( :value($apply-matches-meth) ),
            QAST::Var.new( :scope('local'), :name($topic-str-var) ),
            QAST::Var.new( :scope('local'), :name($match-result-var) ),
            $replacement-closure,
            $match-lookup,                      # $/
            QAST::WVal.new( :value(True) ),     # Flag to update $/
            QAST::IVal.new( :value($sigspace) ),
            QAST::IVal.new( :value($samespace) ),
            QAST::IVal.new( :value($samecase) ),
            QAST::IVal.new( :value($samemark) );

        # We only want to apply matches if we really did match. The pre-RakuAST
        # compiler frontend explicitly checked if it got a Match object or a
        # non-empty List. However, those are both truthy, and all the non-match
        # cases would be falsey, so we can just emit a truth test.
        $result.push: QAST::Op.new:
            :op('if'),
            QAST::Var.new( :scope('local'), :name($match-result-var) ),
            # If we matched...
            $!immutable
                # For the S/// form, we evaluate to the result of the call to
                # APPLY-MATCHES
                ?? $apply-call
                # For the s/// form, we assign the result of APPLY-MATCHES
                # into the topic, and evaluate to the match result.
                !! QAST::Stmts.new(
                    QAST::Op.new(
                        :op('assign'),
                        $lookups[0].IMPL-TO-QAST($context),
                        $apply-call
                    ),
                    # If we have a list of matches, then put them into $/,
                    # otherwise, $/ already has the Match object we want it to have.
                    # Not entirely sure, why we need to do this. Guess $/ gets
                    # clobbered by the APPLY-MATCHES call.
                    QAST::Op.new( :op('p6store'),
                        $match-lookup,
                        QAST::Var.new( :name($match-result-var), :scope('local') ),
                    ),
                    QAST::Var.new( :scope('local'), :name($match-result-var) )
                ),
            # If we didn't match...
            $!immutable
                # For the S/// form, evaluate to topic Str
                ?? QAST::Var.new( :scope('local'), :name($topic-str-var) )
                # For the s/// form, evaluate to the match variable
                !! $match-lookup;

        $result.push:
            # If we have a list of matches, then put them into $/,
            # otherwise, $/ already has the Match object we want it to have
            QAST::Op.new( :op('if'),
                QAST::Op.new( :op('istype'),
                    QAST::Var.new( :name($match-result-var), :scope('local') ),
                    QAST::WVal.new( :value($Positional) )
                ),
                QAST::Op.new( :op('p6store'),
                    QAST::Var.new( :name('$/'), :scope('lexical') ),
                    QAST::Stmts.new(
                        QAST::Op.new( :op('bind'),
                            QAST::Var.new( :name($list-result), :scope('local'), :decl('var') ),
                            QAST::Op.new( :op('create'),
                                QAST::WVal.new( :value(List) )
                            )
                        ),
                        QAST::Op.new( :op('bindattr'),
                            QAST::Var.new( :name($list-result), :scope('local') ),
                            QAST::WVal.new( :value(List) ),
                            QAST::SVal.new( :value('$!reified') ),
                            QAST::Op.new( :op('getattr'),
                                QAST::Var.new( :name($match-result-var), :scope('local') ),
                                QAST::WVal.new( :value(List) ),
                                QAST::SVal.new( :value('$!reified') )
                            )
                        ),
                        QAST::Var.new( :name($list-result), :scope('local') )
                    )
                ),
            );
        if $!immutable {
            $result.resultchild(nqp::elems($result.list) - 2);
        }
        else {
            $result.push: QAST::Var.new( :name('$/'), :scope('lexical') );
        }

        $result
    }

    method visit-children(Code $visitor) {
        self.IMPL-VISIT-ADVERBS($visitor);
        $visitor($!pattern);
        $visitor($!infix) if $!infix;
        $visitor($!replacement);
    }
}

class RakuAST::Transliteration
  is RakuAST::ImplicitLookups
  is RakuAST::QuotedMatchConstruct
{
    has Bool $.destructive;
    has RakuAST::Expression $.left;
    has RakuAST::Expression $.right;

    method new(Bool :$destructive!, RakuAST::Expression :$left!, RakuAST::Expression :$right!, List :$adverbs) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Transliteration, '$!destructive', $destructive ?? True !! False);
        nqp::bindattr($obj, RakuAST::Transliteration, '$!left', $left);
        nqp::bindattr($obj, RakuAST::Transliteration, '$!right', $right);
        $obj.replace-adverbs($adverbs // List);
        $obj
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        [
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Pair')),
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('StrDistance')),
        ]
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $Pair := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].resolution.compile-time-value;
        my $trans := QAST::Op.new:
            QAST::Var.new(:name<$_>, :scope<lexical>),
            :op<callmethod>, :name<trans>,
                QAST::Op.new:
                    :op<callmethod>, :name<new>, :returns($Pair),
                    QAST::WVal.new( :value($Pair) ),
                    $!left.IMPL-TO-QAST($context),  # key
                    $!right.IMPL-TO-QAST($context); # value
        for self.IMPL-UNWRAP-LIST(self.adverbs) {
            my $arg := $_.value.IMPL-TO-QAST($context);
            $arg.named($_.key);
            $trans.push($arg);
        }
        if $!destructive {
            my $StrDistance := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[1].resolution.compile-time-value;
            my $original := QAST::Node.unique: 'original_value_to_trans';

            QAST::Stmt.new(
                QAST::Op.new( # save original $_ into our temp var
                    QAST::Var.new(:name($original), :scope<lexical>, :decl<var>),
                    :op<bind>, QAST::Op.new: :op<decont>,
                    QAST::Var.new(:name<$_>, :scope<lexical>)
                ),
                QAST::Op.new( # call .trans() and assign result to $_
                    QAST::Var.new(:name<$_>, :scope<lexical>),
                    :op<call>, :name('&infix:<=>'),
                    $trans,
                ),
                QAST::Op.new: # our return value: the StrDistance object
                    :returns($StrDistance),
                    QAST::Var.new(
                      :name<StrDistance>, :scope<lexical> ),
                    :op<callmethod>, :name<new>,
                        QAST::Var.new(
                          :named<before>, :name($original), :scope<lexical>),
                        QAST::Var.new:
                          :named<after>,  :name<$_>, :scope<lexical>).annotate_self('regex_match_code', 1)
        }
        else {
            $trans
        }
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        Nil
    }
}

# Thunk handle for substitution replacement.
class RakuAST::SubstitutionReplacementThunk
  is RakuAST::ExpressionThunk
{
    has RakuAST::Infixish $.infix;

    method new(RakuAST::Infixish :$infix) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::SubstitutionReplacementThunk, '$!infix', $infix);
        $obj
    }

    method IMPL-THUNK-TWEAK-EXPRESSION(RakuAST::IMPL::QASTContext $context, Mu $qast) {
        # We only need to really do the assignment if it's not a plain `=`;
        # if it's just that, we can avoid that work.
        if $!infix && !(nqp::istype($!infix, RakuAST::Infix) && $!infix.operator eq '=') {
            my $temp-var := QAST::Op.new:
                :op('p6assign'),
                QAST::Op.new( :op('p6scalarfromdesc'), QAST::Op.new( :op('null') ) ),
                QAST::Var.new( :name('$/'), :scope('lexical') );
            $!infix.IMPL-INFIX-QAST($context, $temp-var, $qast)
        }
        else {
            $qast
        }
    }

    method visit-children(Code $visitor) {
        $visitor($!infix) if $!infix;
    }
}

# Thunk for a primed Whatever expression.
class RakuAST::PrimeThunk
  is RakuAST::ExpressionThunk
  is RakuAST::ImplicitLookups
{
    has Mu $!parameters;
    has Str $!original-expression;

    method new(Str $original-expression, @args) {
        my $obj := nqp::create(self);
        my @params := [];
        nqp::bindattr($obj, RakuAST::PrimeThunk, '$!parameters', @params);
        for @args {
            # $name will usually be undefined, but sometimes we re-use references to existing * targets
            my $target := RakuAST::ParameterTarget::Whatever.new($_.name);
            $_.set-resolution($target);
            my $param := RakuAST::Parameter.new(
                target => $target
            );
            nqp::push(@params, $param);
        }
        nqp::bindattr($obj, RakuAST::PrimeThunk, '$!original-expression', nqp::hllizefor($original-expression, 'Raku'));
        $obj
    }

    method thunk-kind() {
        'WhateverCode'
    }

    method thunk-details() {
        '⋐' ~ nqp::x('🔆', self.IMPL-NUM-PARAMS)  ~ '⋑'
    }

    method IMPL-THUNK-OBJECT-TYPE() {
        self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].compile-time-value
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        [
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('WhateverCode'))
        ]
    }

    method IMPL-NUM-PARAMS() {
        nqp::elems($!parameters)
    }

    method IMPL-THUNK-SIGNATURE() {
        RakuAST::Signature.new(parameters => self.IMPL-WRAP-LIST($!parameters))
    }

    method IMPL-THUNK-META-OBJECT-PRODUCED(Mu $code) {
        nqp::bindattr($code, self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].compile-time-value, '$!original-expression', $!original-expression)
    }
}

class RakuAST::HyperPrimeThunk
  is RakuAST::PrimeThunk
{
    method IMPL-THUNK-VALUE-QAST(RakuAST::IMPL::QASTContext $context) {
        my $qast := self.IMPL-CLOSURE-QAST($context);
        $qast.annotate('thunked', 1);
        QAST::Op.new(:op<call>, :name<&HYPERWHATEVER>, $qast)
    }
}

class RakuAST::BlockThunk
  is RakuAST::ExpressionThunk
  is RakuAST::ImplicitDeclarations
{
    has RakuAST::Expression $!expression;

    method new(RakuAST::Expression :$expression) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::BlockThunk, '$!expression', $expression) if $expression;
        $obj
    }

    method thunk-kind() {
        'Block thunk'
    }

    method thunk-details() {
        ''
    }

    method PRODUCE-IMPLICIT-DECLARATIONS() {
        [
            RakuAST::VarDeclaration::Implicit::BlockTopic.new:
                parameter => self.signature ?? False !! True
        ];
    }

    method IMPL-THUNK-OBJECT-TYPE() {
        Block
    }

    method IMPL-QAST-DECL-CODE(RakuAST::IMPL::QASTContext $context) {
        # Form the block itself and link it with the meta-object.
        self.IMPL-QAST-BLOCK($context, :blocktype('declaration_static'), :expression($!expression));
    }

    method PRODUCE-META-OBJECT(:$resolver, :$context) {
        my $code := nqp::create(self.IMPL-THUNK-OBJECT-TYPE);
        my $param := nqp::create(Parameter);
        nqp::bindattr_s($param, Parameter, '$!variable_name', '$_');
        nqp::bindattr_i($param, Parameter, '$!flags', 2048 + 16384); # Optional + default from outer
        my $sig := nqp::create(Signature);
        nqp::bindattr($sig, Signature, '@!params', [$param]);
        nqp::bindattr_i($sig, Signature, '$!arity', 0);
        nqp::bindattr($sig, Signature, '$!count', nqp::box_i(1, Int));
        nqp::bindattr($code, Code, '$!signature', $sig);
        nqp::bindattr($sig, Signature, '$!code', $code);
        self.IMPL-THUNK-META-OBJECT-PRODUCED($code);
        $code
    }
}

class FixupList {
    has Mu $!list;
    has Mu $!resolved;
    has Mu $!resolver;

    method new(Str $sc-handle) {
        my $obj := nqp::create(FixupList);
        nqp::bindattr($obj, FixupList, '$!list', nqp::list());
        nqp::bindattr($obj, FixupList, '$!resolved', Mu);
        nqp::bindattr($obj, FixupList, '$!resolver', $sc-handle);
        $obj
    }

    method add_unresolved($code) {
        nqp::scwbdisable();
        nqp::push($!list, $code);
        nqp::scwbenable();
        if nqp::isconcrete($!resolved) {
            my $CU := $*CU;
            if nqp::can($CU, 'context') && nqp::can($CU.context, "sc-handle") && $CU.context.sc-handle ne $!resolver {
                $CU.context.ensure-sc($code);
                $CU.context.add-deserialize-task(-> { QAST::Op.new(
                    :op('callmethod'), :name('update'),
                    QAST::WVal.new( :value(self) ),
                    QAST::WVal.new( :value($code) )
                ) });
            }
            else {
                my $do := nqp::getattr($code, Code, '$!do');
                nqp::p6captureouters2([$do], $!resolved);
            }
        }
    }
    method resolve($resolved) {
        nqp::scwbdisable();
        nqp::bindattr(self, FixupList, '$!resolved', $resolved);
        nqp::scwbenable();
        my $do-list := nqp::list();
        my int $i := 0;
        my int $n := nqp::elems($!list);
        while $i < $n {
            nqp::bindpos($do-list, $i, nqp::getattr(nqp::atpos($!list, $i), Code, '$!do'));
            $i++;
        }
        nqp::p6captureouters2($do-list, $resolved);
    }
    method update($code) {
        if !nqp::isnull($!resolved) && !nqp::istype($!resolved, Mu) {
            my $do := nqp::getattr($code, Code, '$!do');
            nqp::p6captureouters2([$do],
                nqp::getcomp('Raku').backend.name eq 'moar'
                    ?? nqp::getstaticcode($!resolved)
                    !! $!resolved);
        }
    }
}
