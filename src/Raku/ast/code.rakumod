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
        QAST::Op.new(
            :op('dispatch'),
            QAST::SVal.new( :value('boot-resume') ),
            QAST::IVal.new( :value(nqp::const::DISP_ONLYSTAR) ));
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
    # Only for use by the fallback resolver in dynamically compiled code!
    has RakuAST::Resolver $!resolver;

    method PERFORM-PARSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        nqp::bindattr(self, RakuAST::Code, '$!resolver', $resolver.clone);
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
        QAST::Op.new( :op('p6capturelex'), $clone )
    }

    method IMPL-QAST-BLOCK(RakuAST::IMPL::QASTContext $context, str :$blocktype,
            RakuAST::Expression :$expression) {
        unless ($!qast-block) {
            self.IMPL-FINISH-CODE-OBJECT($context, :$blocktype, :$expression);
        }
        $!qast-block
    }

    method IMPL-FINISH-CODE-OBJECT(RakuAST::IMPL::QASTContext $context, str :$blocktype,
            RakuAST::Expression :$expression) {
        my $block := self.IMPL-QAST-FORM-BLOCK($context, :$blocktype, :$expression);
        self.IMPL-LINK-META-OBJECT($context, $block);
        nqp::bindattr(self, RakuAST::Code, '$!qast-block', $block);
    }

    method IMPL-STUB-CODE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        # Not all objects of certain subclasses will be attached properly,
        # so get that resolver here
        nqp::bindattr(self, RakuAST::Code, '$!resolver', $resolver.clone)
            unless $!resolver;

        my $code-obj := self.meta-object;
        nqp::bindattr_s(self, RakuAST::Code, '$!cuid', QAST::Block.next-cuid());

        # Stash it under the QAST block unique ID.
        my str $cuid := $!cuid;
        $context.sub-id-to-code-object(){$cuid} := $code-obj;

        my $precomp;
        my $stub := nqp::freshcoderef(sub (*@pos, *%named) {
            my $code-obj := nqp::getcodeobj(nqp::curcode());
            unless $precomp {
                my $*IMPL-COMPILE-DYNAMICALLY := 1;
                my $block := self.IMPL-QAST-BLOCK($context, :blocktype<declaration_static>);
                $precomp := self.IMPL-COMPILE-DYNAMICALLY($resolver, $context, $block);
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
            nqp::bindattr(self, RakuAST::Code, '$!resolver', RakuAST::Resolver) if $context.is-precompilation-mode;
        });

        @compstuff[2] := sub ($orig, $clone) {
            my $do := nqp::getattr($clone, Code, '$!do');
            nqp::markcodestub($do);
            $context.add-cleanup-task(sub () {
                nqp::bindattr($clone, Code, '@!compstuff', nqp::null());
            });
            $context.add-clone-for-cuid($clone, $cuid);
        }

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
        my $cuid := $!cuid;
        $block.set-cuid($!cuid);

        my $fixups := QAST::Stmts.new();
        unless $context.is-precompilation-mode || $context.is-nested {
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
            @compstuff[2] := sub ($orig, $clone) {
                $context.ensure-sc($clone);
                $context.add-cleanup-task(sub () {
                    nqp::bindattr($clone, Code, '@!compstuff', nqp::null());
                });
                $context.add-clone-for-cuid($clone, $cuid);

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
            }
            @compstuff[3] := $fixups;
        }

        @compstuff[0] := $block;

        $context.add-code-ref(nqp::getattr($code-obj, Code, '$!do'), $block);

        $context.add-fixup-task(-> {
            $fixups
        });
    }

    method IMPL-FIXUP-DYNAMICALLY-COMPILED-BLOCK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context, Mu $block) {

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

        my $declared-in-cu := sub ($name) {
            for @blocks {
                return 1 if nqp::existskey($_, $name);
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
                if $scope eq 'lexical' && ! $declared-in-cu($name) {
                    my $value := $var.ann('compile-time-value');
                    if !($value =:= NQPMu) {
                        $var := QAST::WVal.new(:$value);
                    }
                    elsif $name ne '$_' { #TODO figure out why we specifially don't declare $_ in ExpressionThunks
                        my $decl := $!resolver.resolve-lexical-constant($name);
                        if $decl {
                            $decl.to-begin-time($resolver, $context); # Ensure any required lookups are resolved
                            my $value := $decl.compile-time-value;
                            $context.ensure-sc($value);
                            $var := QAST::WVal.new(:$value);
                        }
                        else {
                            nqp::die("Could not find a compile-time-value for lexical $name");
                        }
                    }
                }
            }
            $var
        }

        $visit-children := sub ($node) {
            my int $i := 0;
            my int $n := nqp::elems($node);
            while $i < $n {
                my $visit := $node[$i];
                $visit := $visit.shallow_clone if nqp::istype($visit, QAST::Node);
                $node[$i] := $visit;
                if nqp::istype($visit, QAST::Op) {
                    my $op := $visit.op;
                    if ($op eq 'call' || $op eq 'callstatic' || $op eq 'chain') && $visit.name {
                        my $routine := $!resolver.resolve-lexical-constant($visit.name);
                        my $value := $routine.compile-time-value;
                        if ! $declared-in-cu($visit.name) {
                            $context.ensure-sc($value);
                            $visit.name(nqp::null);
                            $visit.unshift(QAST::WVal.new(:$value));
                        }
                    }
                    $visit-children($visit)
                }
                elsif nqp::istype($visit, QAST::Block) {
                    $visit-block($visit);
                }
                elsif nqp::istype($visit, QAST::Stmt) || nqp::istype($visit, QAST::Stmts) || nqp::istype($visit, QAST::ParamTypeCheck) {
                    $visit-children($visit);
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

    method IMPL-COMPILE-DYNAMICALLY(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context, Mu $block) {
        my $wrapper := QAST::Block.new(QAST::Stmts.new(), nqp::clone($block));
        $wrapper.annotate('DYN_COMP_WRAPPER', 1);
        $wrapper[0].push(QAST::Var.new(
            :name('$_'), :scope('lexical'),
            :decl('contvar'), :value(Mu)
        ));
        $wrapper[0].push(QAST::Var.new(
            :name('$/'), :scope('lexical'),
            :decl('contvar'), :value(Nil)
        ));
        my $package := $!resolver.current-package;
        $context.ensure-sc($package);
        $wrapper[0].push(QAST::Var.new(
            :name('$?PACKAGE'), :scope('lexical'),
            :decl('static'), :value($package)
        ));

        self.IMPL-FIXUP-DYNAMICALLY-COMPILED-BLOCK($resolver, $context, $wrapper);

        my $compunit := QAST::CompUnit.new(
            :hll('Raku'),
            :sc($context.sc()),
            :compilation_mode(0),
            $wrapper
        );
        my $comp := $*HLL-COMPILER // nqp::getcomp("Raku");
        my $precomp := $comp.compile($compunit, :from<qast>, :compunit_ok(1));
        my $mainline := $comp.backend.compunit_mainline($precomp);
        $mainline();

        # Fix up Code object associations (including nested blocks).
        # We un-stub any code objects for already-compiled inner blocks
        # to avoid wasting re-compiling them, and also to help make
        # parametric role outer chain work out. Also set up their static
        # lexpads, if they have any.
        my @coderefs := $comp.backend.compunit_coderefs($precomp);
        my int $num-subs := nqp::elems(@coderefs);
        my int $i;
        my $result;
        while $i < $num-subs {
            my $subid := nqp::getcodecuid(@coderefs[$i]);

            # un-stub code objects for blocks we just compiled:
            my %sub-id-to-code-object := $context.sub-id-to-code-object();
            if nqp::existskey(%sub-id-to-code-object, $subid) {
                my $code-obj := %sub-id-to-code-object{$subid};
                nqp::setcodeobj(@coderefs[$i], $code-obj);
                nqp::bindattr($code-obj, Code, '$!do', @coderefs[$i]);
                my $fixups := nqp::getattr($code-obj, Code, '@!compstuff')[3];
                if $fixups {
                    $fixups.pop() while $fixups.list;
                }
                nqp::bindattr($code-obj, Code, '@!compstuff', nqp::null());
            }

            # un-stub clones of code objects for blocks we just compiled:
            my %sub-id-to-cloned-code-objects := $context.sub-id-to-cloned-code-objects();
            if nqp::existskey(%sub-id-to-cloned-code-objects, $subid) {
                for %sub-id-to-cloned-code-objects{$subid} -> $code-obj {
                    my $clone := nqp::clone(@coderefs[$i]);
                    nqp::setcodeobj($clone, $code-obj);
                    nqp::bindattr($code-obj, Code, '$!do', $clone);
                    my $fixups := nqp::getattr($code-obj, Code, '@!compstuff')[3];
                    if $fixups {
                        $fixups.pop() while $fixups.list;
                    }
                    nqp::bindattr($code-obj, Code, '@!compstuff', nqp::null());
                }
            }

            my %sub-id-to-sc-idx := $context.sub-id-to-sc-idx();
            if nqp::existskey(%sub-id-to-sc-idx, $subid) {
                nqp::markcodestatic(@coderefs[$i]);
                nqp::scsetcode($context.sc, %sub-id-to-sc-idx{$subid}, @coderefs[$i]);
            }

            if $subid eq $block.cuid {
                # Remember the VM coderef that maps to the thing we were originally
                # asked to compile.
                $result := @coderefs[$i];
            }
            ++$i;
        }
        $result
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

# The base of all expression thunks, which produce a code object of some kind
# that wraps the thunk.
class RakuAST::ExpressionThunk
  is RakuAST::Code
  is RakuAST::Meta
  is RakuAST::BeginTime
{
    has RakuAST::ExpressionThunk $.next;
    has RakuAST::Signature $!signature;

    method set-next(RakuAST::ExpressionThunk $next) {
        nqp::bindattr(self, RakuAST::ExpressionThunk, '$!next', $next);
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
    method IMPL-THUNK-CODE-QAST(RakuAST::IMPL::QASTContext $context, Mu $target,
            RakuAST::Expression $expression) {

        my $block := self.IMPL-QAST-BLOCK($context, :$expression);
        # Link and push the produced code block.
        $target.push($block);
    }

    method IMPL-QAST-FORM-BLOCK(RakuAST::IMPL::QASTContext $context,
            str :$blocktype, RakuAST::Expression :$expression!) {
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
                unless nqp::istype($node, RakuAST::LexicalScope) {
                    @code-todo.push($node);
                }
            }
        }

        my $nested-blocks := $expression.IMPL-QAST-NESTED-BLOCK-DECLS($context);
        $stmts.push($nested-blocks) if nqp::elems($nested-blocks.list);

        $block.push($stmts) if $stmts.list;
        $block.arity($signature.arity);

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

    method PRODUCE-META-OBJECT() {
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

    method add-placeholder-parameter(RakuAST::VarDeclaration::Placeholder $placeholder) {
        unless nqp::islist($!attached-placeholder-parameters) {
            nqp::bindattr(self, RakuAST::PlaceholderParameterOwner,
                '$!attached-placeholder-parameters', []);
        }
        my $name := $placeholder.lexical-name;
        if self.IMPL-HAS-PARAMETER($name) {
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

    method clear-placeholder-attachments() {
        if nqp::islist($!attached-placeholder-parameters) {
            for $!attached-placeholder-parameters {
                # reset declared state on parameters,
                # will be re-set when they are attached again
                $_.IMPL-ALREADY-DECLARED(False);
            }
            nqp::bindattr(self, RakuAST::PlaceholderParameterOwner,
                '$!attached-placeholder-parameters', nqp::null());
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

    method add-enter-phaser(RakuAST::StatementPrefix::Phaser $phaser) {
        self.add-phaser('ENTER', $phaser);
        my $result-name := '__enter_phaser_result_' ~ $!next-enter-phaser-result;
        nqp::bindattr_i(self, RakuAST::ScopePhaser, '$!next-enter-phaser-result', $!next-enter-phaser-result + 1);
        $result-name
    }

    method needs-result() {
        nqp::istype(self, RakuAST::Meta) && (
            self.meta-object.has-phaser('UNDO')
            || self.meta-object.has-phaser('KEEP')
            || self.meta-object.has-phaser('POST')
        )
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

    method add-phasers-handling-code(RakuAST::IMPL::Context $context, Mu $qast) {
        my $block := nqp::istype(self, RakuAST::Code) ?? self.meta-object !! NQPMu;

        if $!has-exit-handler || self.needs-result || $block && ($block.has-phaser('LEAVE') || $block.has-phaser('POST')) {
            $qast.has_exit_handler(1);
        }

        if $!PRE || $block && $block.has-phaser('PRE') {
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

        if $!FIRST || $block && $block.has-phaser('FIRST') {
            my $first-setup := QAST::Stmts.new;
            my $calls := QAST::Stmts.new(
                QAST::Op.new(:op<call>, :name<&infix:<=>>,
                    QAST::Var.new(:scope<lexical>, :name<!phaser_first_triggered>),
                    QAST::WVal.new(:value(True))
                )
            );
            my $descriptor := ContainerDescriptor.new(:of(Bool), :name('!phaser_first_triggered'), :default(False), :dynamic(0));
            my $container := nqp::create(Scalar);
            nqp::bindattr($container, Scalar, '$!descriptor', $descriptor);
            nqp::bindattr($container, Scalar, '$!value', False);
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

        if $!ENTER || $block && $block.has-phaser('ENTER') {
            my $enter-setup := QAST::Stmts.new;
            my %seen;
            if $!ENTER {
                for $!ENTER {
                    my $result-name := $_.IMPL-RESULT-NAME;
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
            if $block {
                my $enter-phasers := $block.phasers('ENTER');
                if nqp::isconcrete($enter-phasers) {
                    for $enter-phasers.FLATTENABLE_LIST {
                        unless %seen{nqp::objectid($_)} {
                            $context.ensure-sc($_);
                            $enter-setup.push(QAST::Op.new(:op<call>, QAST::WVal.new(:value($_))));
                        }
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
            || $block && (
                   $block.has-phaser('LAST')
                || $block.has-phaser('NEXT')
                || $block.has-phaser('QUIT')
                || $block.has-phaser('CLOSE')
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
            || $block && (
                   $block.has-phaser('LEAVE')
                || $block.has-phaser('KEEP')
                || $block.has-phaser('UNDO')
                || $block.has-phaser('POST')
            )
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

        my $phaser-block := $phaser.IMPL-QAST-BLOCK($context, :blocktype('declaration_static'));
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

    # Should this block declare a fresh implicit `$!`?
    has int $!fresh-exception;

    method new(RakuAST::Blockoid :$body,
                            Bool :$implicit-topic,
                            Bool :$required-topic,
                            Bool :$exception,
        RakuAST::Doc::Declarator :$WHY,
              RakuAST::Signature :$signature # ignored, just for compatability with Routine
    ) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Block, '$!body', $body // RakuAST::Blockoid.new);
        $obj.set-implicit-topic($implicit-topic // 1, :required($required-topic), :$exception);
        $obj.set-WHY($WHY);
        $obj
    }

    # Helper method to return if there are any whenevers in this block,
    # either directly, or in any embedded blocks.
    method any-whenevers() { self.body.statement-list.any-whenevers }

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

    method attach-target-names() {
        self.IMPL-WRAP-LIST(['block'])
    }

    method propagate-sink(Bool $is-sunk) {
        $!body.apply-sink($is-sunk && !self.needs-result);
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
        if $!fresh-match {
            nqp::push(@implicit, RakuAST::VarDeclaration::Implicit::Special.new(:name('$/')));
        }
        if $!fresh-exception {
            nqp::push(@implicit, RakuAST::VarDeclaration::Implicit::Special.new(:name('$!')));
        }
        self.IMPL-WRAP-LIST(@implicit)
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Code')),
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('&FATALIZE')),
        ])
    }

    method IMPL-FATALIZE() {
        self.get-implicit-lookups.AT-POS(1).resolution.compile-time-value;
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

    method PRODUCE-STUBBED-META-OBJECT() {
        nqp::create(Block);
    }

    method PRODUCE-META-OBJECT() {
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
            my constant REQUIRED-TOPIC-SIG := -> {
                my $param := nqp::create(Parameter);
                nqp::bindattr_s($param, Parameter, '$!variable_name', '$_');
                my $sig := nqp::create(Signature);
                nqp::bindattr($sig, Signature, '@!params', [$param]);
                nqp::bindattr_i($sig, Signature, '$!arity', 1);
                nqp::bindattr($sig, Signature, '$!count', nqp::box_i(1, Int));
                $sig
            }();
            my constant OPTIONAL-TOPIC-SIG := -> {
                my $param := nqp::create(Parameter);
                nqp::bindattr_s($param, Parameter, '$!variable_name', '$_');
                nqp::bindattr_i($param, Parameter, '$!flags', 2048 + 16384); # Optional + default from outer
                my $sig := nqp::create(Signature);
                nqp::bindattr($sig, Signature, '@!params', [$param]);
                nqp::bindattr_i($sig, Signature, '$!arity', 0);
                nqp::bindattr($sig, Signature, '$!count', nqp::box_i(1, Int));
                $sig
            }();
            nqp::bindattr($block, Code, '$!signature', $!implicit-topic-mode == 1
                ?? OPTIONAL-TOPIC-SIG
                !! REQUIRED-TOPIC-SIG);
        }
        self.add-phasers-to-code-object($block);
        $block
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
            # For now, assume we never need a code object for such a block. The
            # closure clone is done for us by the QAST compiler.
            my $block := self.IMPL-QAST-FORM-BLOCK($context, :blocktype<immediate>);
            self.IMPL-LINK-META-OBJECT($context, $block);
            $block
        }
        else {
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

    # Applies the block or hash disambiguation algorithm, to determine whether
    # a circumfix:<{ }> should compile as a block or a hash. If it should
    # compile as a block, returns identity. Otherwise, returns an instance of
    # RakuAST::Circumfix::HashComposer. Note that this method - nor the block
    # vs hash distinction - does NOT impact upon the compilation of this node
    # if it is in the AST. This method is for use by a compiler that may have
    # parsed things tentatively as a block to obtain the actual node to use.
    # If constructing AST synthetically, just make the correct thing in the
    # first place.
    method block-or-hash(int :$object-hash) {
        my @statements := self.body.statement-list.code-statements;
        my int $num-statements := nqp::elems(@statements);

        # Empty block is always an empty hash composer
        if $num-statements == 0 {
            return RakuAST::Circumfix::HashComposer.new(:$object-hash)
        }

        # Multiple statements is always a block
        elsif $num-statements > 1 {
            return self
        }

        # Not a statement always means block
        my $statement := @statements[0];
        unless nqp::istype($statement, RakuAST::Statement::Expression) {
            return self;
        }

        # If it's a comma list, then obtain the first element. Otherwise,
        # we have the thing to test already.
        my $expression := $statement.expression;
        my int $is-comma := nqp::istype($expression, RakuAST::ApplyListInfix) &&
            nqp::istype($expression.infix, RakuAST::Infix) &&
            $expression.infix.operator eq ',';
        my $test := $is-comma
            ?? self.IMPL-UNWRAP-LIST($expression.operands)[0]
            !! $expression;

        # A fatarrow is ok
        if nqp::istype($test,RakuAST::FatArrow) {
        }
        # A colonpair is ok
        elsif nqp::istype($test,RakuAST::ColonPair) {
        }
        # A hash sigil'd variable is ok
        elsif nqp::istype($test,RakuAST::Var) && $test.sigil eq '%' {
        }
        # Some kind of infix may be ok
        elsif nqp::istype($test,RakuAST::ApplyInfix) {

            # Get the proper infix to check
            my $infix := $test.infix;
            if nqp::istype($infix,RakuAST::Infix) {
            }
            elsif nqp::istype($infix,RakuAST::MetaInfix::Reverse) {
                $infix := $infix.infix;
            }

            # It's a block as no valid infix found
            else {
                return self
            }

            # It's a block if not a fat arrow
            return self unless $infix.operator eq '=>';
        }

        # It's a block as nothing recognizable
        else {
            return self;
        }

        # Looks like a hash, but check for declarations or $_ usage.
        my int $seen-decl-or-topic;
        $expression.visit: -> $node {
            # Don't walk into other scopes
            if nqp::istype($node, RakuAST::LexicalScope) {
                0
            }
            # If it's a declaration, it blocks; walk no futher
            elsif nqp::istype($node, RakuAST::Declaration) {
                $seen-decl-or-topic := 1;
                0
            }
            # If it's a usage of the topic, it also blocks; walk no further
            elsif nqp::istype($node, RakuAST::Var::Lexical) && $node.name eq '$_' ||
                nqp::istype($node, RakuAST::Term::TopicCall) {
                $seen-decl-or-topic := 1;
                0
            }
            # Otherwise, keep looking
            else {
                1
            }
        }
        $seen-decl-or-topic
            ?? self
            !! RakuAST::Circumfix::HashComposer.new($expression, :$object-hash)
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

    method new(RakuAST::Signature :$signature,
                RakuAST::Blockoid :$body,
         RakuAST::Doc::Declarator :$WHY
    ) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::PointyBlock, '$!signature',
          $signature // RakuAST::Signature.new);
        nqp::bindattr($obj, RakuAST::Block, '$!body',
          $body // RakuAST::Blockoid.new);
        $obj.set-WHY($WHY);
        $obj
    }

    method replace-signature(RakuAST::Signature $new-signature) {
        nqp::bindattr(self, RakuAST::PointyBlock, '$!signature', $new-signature);
        Nil
    }

    method bare-block() { False }

    method propagate-sink(Bool $is-sunk) {
        self.body.apply-sink($is-sunk && !self.needs-result);
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
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Callable')),
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('&FATALIZE')),
        ])
    }

    method IMPL-FATALIZE() {
        self.get-implicit-lookups.AT-POS(1).resolution.compile-time-value;
    }

    method PRODUCE-META-OBJECT() {
        my $block := self.IMPL-PRODUCE-META-OBJECT();
        my $signature := self.signature || self.placeholder-signature;

        if $signature.meta-object.has_returns {
            my $Callable :=
              self.get-implicit-lookups.AT-POS(0).compile-time-value;
            $block.HOW.mixin(
              $block,
              $Callable.HOW.parameterize(
                $Callable,
                $signature.meta-object.returns
              )
            );
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
        # TODO also infer body type
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
    has Bool $.need-routine-variable;
    has Bool $!replace-stub;

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

    method declaration-kind() { 'routine' }

    method attach-target-names() {
        self.IMPL-WRAP-LIST(['routine', 'block'])
    }

    method PERFORM-PARSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $package := $resolver.find-attach-target('package');
        nqp::bindattr(self, RakuAST::Routine, '$!package', $package // $resolver.global-package);
        nqp::bindattr(self, RakuAST::Code, '$!resolver', $resolver.clone);
    }

    method set-need-routine-variable() {
        nqp::bindattr(self, RakuAST::Routine, '$!need-routine-variable', True);
    }

    method build-bind-exception(RakuAST::Resolver $resolver) {
        $resolver.build-exception: 'X::Bind::Rebind',
            :target(self.lexical-name)
    }

    method is-stub() {
        my $statement-list := self.body.statement-list;
        $statement-list.IMPL-IS-SINGLE-EXPRESSION && nqp::istype($statement-list.statements.AT-POS(0).expression, RakuAST::Stub)
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Callable')),
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('&FATALIZE')),
        ])
    }

    method IMPL-FATALIZE() {
        self.get-implicit-lookups.AT-POS(1).resolution.compile-time-value;
    }

    method PRODUCE-STUBBED-META-OBJECT() {
        nqp::create(self.IMPL-META-OBJECT-TYPE)
    }

    method PRODUCE-META-OBJECT() {
        my $routine := self.stubbed-meta-object;
        my $signature := self.placeholder-signature || self.signature;
        nqp::bindattr($routine, Code, '$!signature', $signature.meta-object);
        nqp::bindattr($signature.meta-object, Signature, '$!code', $routine);

        if $signature.meta-object.has_returns {
            my $Callable :=
              self.get-implicit-lookups.AT-POS(0).compile-time-value;
            $routine.HOW.mixin(
              $routine,
              $Callable.HOW.parameterize(
                $Callable,
                $signature.meta-object.returns
              )
            );
        }

        if nqp::istype(self.body, RakuAST::OnlyStar) && !nqp::istype(self, RakuAST::RegexDeclaration) {
            $routine.set_onlystar;
        }

        if $!package {
            nqp::bindattr($routine,Routine,'$!package',$!package.compile-time-value);
            ($!package.stubbed-meta-object.WHO){self.lexical-name} := $routine
                if self.lexical-name && self.scope eq 'our';
        }

        # Make sure that any OperatorProperties are set on the meta-object
        # if it is some kind of operator.  This feels pretty hackish way
        # to do this, perhaps better done with dedicated subclasses of
        # RakuAST::Sub.  But it will do for now.
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
            );
            $!signature.PERFORM-PARSE($resolver, $context);
            self.add-generated-lexical-declaration($_) for $!signature.IMPL-ENSURE-IMPLICITS($resolver, $context);
            $!signature.to-begin-time($resolver, $context);
        }

        if self.multiness eq 'multi' {
            my $name := '&' ~ self.name.canonicalize;
            my $proto := $resolver.resolve-lexical($name, :current-scope-only);
            if $proto {
                $proto := $proto.compile-time-value;
            }
            else {
                my $scope := $resolver.current-scope;

                if $proto := $resolver.resolve-lexical-constant($name) {
                    $proto := $proto.compile-time-value.derive_dispatcher;
                }
                elsif $proto := $resolver.resolve-lexical-constant-in-outer($name) {
                    $proto := $proto.compile-time-value.derive_dispatcher;
                }
                else {
                    my $proto-ast := RakuAST::Sub.new(
                        :scope<my>,
                        :name(self.name),
                        :signature(RakuAST::Signature.new(
                            :parameters(self.IMPL-WRAP-LIST([
                                RakuAST::Parameter.new(
                                    :slurpy(RakuAST::Parameter::Slurpy::Capture),
                                )
                            ])),
                        )),
                        :body(RakuAST::OnlyStar.new),
                        :multiness<proto>,
                    );

                    $proto-ast.ensure-begin-performed($resolver, $context);
                    $proto := $proto-ast.meta-object;

                    $scope.add-generated-lexical-declaration(
                        RakuAST::VarDeclaration::Implicit::Block.new(:block($proto-ast))
                    );
                }
                $scope.add-generated-lexical-declaration(
                    RakuAST::VarDeclaration::Implicit::Constant.new(:$name, :value($proto))
                );
            }
            $proto.add_dispatchee(self.meta-object);
        }
        elsif self.multiness eq 'proto' {
            nqp::bindattr(self.meta-object, Routine, '@!dispatchees', []);
        }

        self.IMPL-STUB-PHASERS($resolver, $context);

        my $stub := self.IMPL-STUB-CODE($resolver, $context);
        nqp::setcodename($stub, $!name.canonicalize) if $!name;

        # Apply any traits.
        self.apply-traits($resolver, $context, self)
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.add-trait-sorries;

        nqp::findmethod(RakuAST::LexicalScope, 'PERFORM-CHECK')(self, $resolver, $context);
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
                    my $name := $_.target.name;
                    $slash := 0            if $name eq '$/';
                    $exclamation-mark := 0 if $name eq '$!';
                    $underscore := 0       if $name eq '$_';
                }
                my $type-captures := self.IMPL-UNWRAP-LIST($_.type-captures);
                for $type-captures {
                    nqp::push(@declarations, $_);
                }
            }
        }
        nqp::push(@declarations, RakuAST::VarDeclaration::Implicit::Special.new(:name('$/'))) if $slash;
        nqp::push(@declarations, RakuAST::VarDeclaration::Implicit::Special.new(:name('$!'))) if $exclamation-mark;
        nqp::push(@declarations, RakuAST::VarDeclaration::Implicit::Special.new(:name('$_'))) if $underscore;
        nqp::push(@declarations, RakuAST::VarDeclaration::Implicit::Routine.new()) if $!need-routine-variable;
        self.IMPL-WRAP-LIST(@declarations)
    }

    method IMPL-HAS-PARAMETER(Str $name) {
        $!signature && $!signature.IMPL-HAS-PARAMETER($name)
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
        my $signature := self.placeholder-signature || $!signature;
        $block.push($signature.IMPL-QAST-BINDINGS($context, :needs-full-binder(self.custom-args), :multi(self.multiness eq 'multi')));
        $block.custom_args(1) if self.custom-args;
        $block.arity($signature.arity);
        $block.annotate('count', $signature.count);
        $block.push($body);
        self.add-phasers-handling-code($context, $block);
        self.IMPL-MAYBE-FATALIZE-QAST($block)
    }

    method IMPL-COMPILE-BODY(RakuAST::IMPL::QASTContext $context) {
        nqp::die('RakuAST::Routine subclass must implement IMPL-COMPILE-BODY')
    }

    method IMPL-WRAP-RETURN-HANDLER(RakuAST::IMPL::QASTContext $context, QAST::Node $body) {
        my $result := $body;
        my $routine := self.compile-time-value;
        my $signature := nqp::getattr($routine, Code, '$!signature');
        $context.ensure-sc($routine);

        # Add return exception and decont handler if needed.
        # TODO optimize out if provably no return call
        my str $decont-rv-op := $context.lang-version lt 'd' && $context.is-moar
            ?? 'p6decontrv_6c'
            !! 'p6decontrv';
        $result := QAST::Op.new(
            :op<handlepayload>,
            QAST::Op.new( :op($decont-rv-op), QAST::WVal.new( :value($routine) ), $result ),
            'RETURN',
            QAST::Op.new( :op<lastexpayload> )
        );

        # Add return type check if needed.
        # TODO also infer body type
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
        if $name && (self.scope eq 'our' || self.scope eq 'unit') {
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
                QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :decl<var>, :scope<lexical>, :$name ),
                    self.IMPL-CLOSURE-QAST($context)
                )
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

    method is-lexical() {
        my str $scope := self.scope;
        $scope eq 'my' || $scope eq 'state' || $scope eq 'our' || $scope eq 'unit'
    }

    method is-simple-lexical-declaration() {
        self.is-lexical && self.multiness ne 'multi'
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

    method replace-body(RakuAST::Blockoid $new-body) {
        nqp::bindattr(self, RakuAST::Sub, '$!body', $new-body);
        Nil
    }

    method IMPL-META-OBJECT-TYPE() { Sub }

    method default-scope() {
        self.name ?? 'my' !! 'anon'
    }

    method allowed-scopes() {
        self.IMPL-WRAP-LIST(['my', 'anon', 'our'])
    }

    method get-boundary-sink-propagator() {
        $!body.statement-list
    }

    method is-boundary-sunk() {
        return False if self.needs-result;
        my $signature := self.signature;
        $signature ?? $signature.provides-return-value !! False
    }

    method PERFORM-CHECK(Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.add-trait-sorries;

        nqp::findmethod(RakuAST::LexicalScope, 'PERFORM-CHECK')(self, $resolver, $context);

        return Nil unless self.multiness eq 'multi';

        self.IMPL-CHECK-FOR-DUPLICATE-MULTI-SIGNATURES($resolver);
    }

    method IMPL-COMPILE-BODY(RakuAST::IMPL::QASTContext $context) {
        self.IMPL-WRAP-RETURN-HANDLER($context,
            self.IMPL-WRAP-SCOPE-HANDLER-QAST($context,
                self.IMPL-APPEND-SIGNATURE-RETURN($context, $!body.IMPL-TO-QAST($context))))
    }

    method IMPL-CHECK-FOR-DUPLICATE-MULTI-SIGNATURES(Resolver $resolver) {
        my $proto := self.meta-object.dispatcher;
        my $signature := (self.placeholder-signature || self.signature).compile-time-value;
        my $meta := self.meta-object;

        # If we ourselves can default, there is no need to check further
        return Nil if $meta.can("default");

        my int $has-post-constraints;
        for self.IMPL-UNWRAP-LIST($signature.params) {
            $has-post-constraints := 1 if $_.constraint_list;
        }

        my @seen-accepts;
        for $proto.dispatchees {
            last if $_.can("default");
            next if $_ =:= $meta;

            my $other-signature := $_.signature;
            next unless $signature.arity == $other-signature.arity;

            my $other-has-post-constraints;
            for self.IMPL-UNWRAP-LIST($other-signature.params) {
                $other-has-post-constraints := 1 if $_.constraint_list
            }

            @seen-accepts.push($_)
                if !($has-post-constraints || $other-has-post-constraints)
                        && (try $other-signature.ACCEPTS($signature) && try $signature.ACCEPTS($other-signature));

            if @seen-accepts > 0 {
                my %args;
                if my $origin := self.origin {
                    my $origin-match := self.origin.as-match;
                    %args<filename>  := $origin-match.file;
                    %args<line>      := $origin-match.line
                }
                self.add-worry: $resolver.build-exception:
                        'X::Redeclaration::Multi',
                        :symbol(self.name.canonicalize),
                        :ambiguous(@seen-accepts),
                        |%args;
            }
        }
    }
}

class RakuAST::RoleBody
  is RakuAST::Sub
{
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
        $obj.set-WHY($WHY);
        $obj
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
}

# The commonalities of method-like things, whichever language their body is in
# (be it the main Raku language or the regex language).
class RakuAST::Methodish
  is RakuAST::Routine
{
    method default-scope() {
        self.name ?? 'has' !! 'anon'
    }

    method allowed-scopes() {
        self.IMPL-WRAP-LIST(['has', 'my', 'anon', 'our'])
    }

    method PERFORM-PARSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        if self.scope eq 'has' || self.scope eq 'our' {
            my $package := $resolver.find-attach-target('package');
            if $package {
                nqp::bindattr(self, RakuAST::Routine, '$!package', $package);
            }
        }
        nqp::bindattr(self, RakuAST::Code, '$!resolver', $resolver.clone);
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.body.to-begin-time($resolver, $context); # In case it's the default created in the ctor.

        my $package := nqp::getattr(self, RakuAST::Routine, '$!package');
        my $package-is-role := $package && $package.declarator eq 'role';
        my $placeholder-signature := self.placeholder-signature;
        if $placeholder-signature {
            $placeholder-signature.set-is-on-method(True);
            $placeholder-signature.set-is-on-named-method(True) if self.name;
            $placeholder-signature.set-is-on-meta-method(True) if nqp::can(self, 'meta') && self.meta;
            $placeholder-signature.set-is-on-role-method(True) if $package-is-role;
            $placeholder-signature.set-invocant-type-check(self.IMPL-INVOCANT-TYPE-CHECK);
            $placeholder-signature.attach($resolver);
            $placeholder-signature.PERFORM-PARSE($resolver, $context);
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
            $signature.PERFORM-PARSE($resolver, $context);
            self.add-generated-lexical-declaration($_) for $signature.IMPL-ENSURE-IMPLICITS($resolver, $context);
            $signature.to-begin-time($resolver, $context);
        }

        my str $name := self.name ?? self.name.canonicalize !! '';

        if $package {
            if $package.can-have-methods {
                $package.ATTACH-METHOD(self) unless self.scope eq 'our';
            }
            else {
                $resolver.add-worry:  # XXX should be self.add-worry
                  $resolver.build-exception: 'X::Useless::Declaration',
                    name  => $name,
                    where => "a " ~ $package.declarator
            }
        }
        elsif self.scope eq 'has' {
            $resolver.add-worry:  # XXX should be self.add-worry
              $resolver.build-exception: 'X::Useless::Declaration',
                name  => $name,
                where => 'the mainline';
        }

        if self.multiness eq 'proto' {
            nqp::bindattr(self.meta-object, Routine, '@!dispatchees', []);
        }
        self.IMPL-STUB-PHASERS($resolver, $context);

        my $stub := self.IMPL-STUB-CODE($resolver, $context);
        nqp::setcodename($stub, $name) if $name;

        # Apply any traits.
        self.apply-traits($resolver, $context, self)
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

    method replace-body(RakuAST::Blockoid $new-body) {
        nqp::bindattr(self, RakuAST::Method, '$!body', $new-body);
        Nil
    }

    method set-meta(Bool $meta) {
        nqp::bindattr(self, RakuAST::Method, '$!meta', $meta ?? True !! False);
    }

    method set-private(Bool $private) {
        nqp::bindattr(self, RakuAST::Method, '$!private', $private ?? True !! False);
    }

    method IMPL-META-OBJECT-TYPE() { Method }

    method PRODUCE-IMPLICIT-DECLARATIONS() {
        my $list := nqp::findmethod(RakuAST::Routine, 'PRODUCE-IMPLICIT-DECLARATIONS')(self);
        self.IMPL-UNWRAP-LIST($list).push:
            RakuAST::VarDeclaration::Implicit::Self.new();
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

    method IMPL-COMPILE-BODY(RakuAST::IMPL::QASTContext $context) {
        # If our first expression is a stub object (!!!, ..., ???),
        # set the yada bit on the Method itself
        if (my $first-statement := $!body.statement-list.statements.AT-POS(0))
            && nqp::istype($first-statement, RakuAST::Statement::Expression)
            && nqp::istype($first-statement.expression, RakuAST::Stub)
        {
            self.meta-object.set_yada;
        }

        self.IMPL-WRAP-RETURN-HANDLER($context,
            self.IMPL-WRAP-SCOPE-HANDLER-QAST($context,
                self.IMPL-APPEND-SIGNATURE-RETURN($context, $!body.IMPL-TO-QAST($context))))
    }
}

# A submethod.
class RakuAST::Submethod
  is RakuAST::Method
{
    method IMPL-META-OBJECT-TYPE() { Submethod }
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

    method PRODUCE-META-OBJECT() {
        my $meta := nqp::findmethod(RakuAST::Routine, 'PRODUCE-META-OBJECT')(self);
        nqp::bindattr_s($meta, $meta.WHAT, '$!source',
          self.declarator ~ ' ' ~ $!source
        );
        $meta
    }

    method IMPL-META-OBJECT-TYPE() { Regex }

    method IMPL-INVOCANT-TYPE-CHECK() {
        self.scope ne 'my'
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
        self.IMPL-WRAP-LIST(@declarations)
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

    method PRODUCE-META-OBJECT() {
        # Create default signature, receiving invocant only.
        my $signature := nqp::create(Signature);
        my $parameter := nqp::create(Parameter);
        nqp::bindattr($parameter, Parameter, '$!type', Mu);
        nqp::bindattr($signature, Signature, '@!params', nqp::list($parameter));

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
            self.IMPL-WRAP-LIST(@checked-adverbs));
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
        self.IMPL-WRAP-LIST([
            RakuAST::Var::Lexical.new('$_'),
            RakuAST::Var::Lexical.new('$/'),
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier-parts(
                'Rakudo', 'Internals', 'RegexBoolification6cMarker'
            ))
        ])
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
            my $lookups := self.get-implicit-lookups;
            my $topic   := $lookups.AT-POS(0).IMPL-TO-QAST($context);
            my $slash   := $lookups.AT-POS(1).IMPL-TO-QAST($context);

            my $match-qast := QAST::Op.new(
              :op('callmethod'), :name('match'), $topic, $closure
            );
            my int $is-multiple-match;
            for self.IMPL-UNWRAP-LIST(self.adverbs) {
                my str $norm := self.IMPL-NORMALIZE-ADVERB($_.key);
                if self.IMPL-IS-POSITION-ADVERB($norm) {
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
        my $lookups := self.get-implicit-lookups;
        if $context.lang-version lt 'd' {
            my $topic := $lookups.AT-POS(2).IMPL-TO-QAST($context);
            $topic.named('topic');
            $clone.push($topic);
        }
        else {
            my $topic := $lookups.AT-POS(0).IMPL-TO-QAST($context);
            $topic.named('topic');
            $clone.push($topic);
            my $slash := $lookups.AT-POS(1).IMPL-TO-QAST($context);
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
        self.IMPL-WRAP-LIST([
            RakuAST::Var::Lexical.new('$_'),
            RakuAST::Var::Lexical.new('$/'),
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Positional')),
        ])
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
        my $lookups    := self.get-implicit-lookups;
        my $topic      := $lookups.AT-POS(0);
        my $slash      := $lookups.AT-POS(1);
        my $Positional := $lookups.AT-POS(2).compile-time-value;

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
            if self.IMPL-IS-POSITION-ADVERB($norm) {
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
                        $lookups.AT-POS(0).IMPL-TO-QAST($context),
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
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Pair')),
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('StrDistance')),
        ])
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $Pair := self.get-implicit-lookups.AT-POS(0).resolution.compile-time-value;
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
            my $StrDistance := self.get-implicit-lookups.AT-POS(1).resolution.compile-time-value;
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

# Thunk for a curried Whatever expression.
class RakuAST::CurryThunk
  is RakuAST::ExpressionThunk
  is RakuAST::ImplicitLookups
{
    has Mu $!parameters;
    has Str $!original-expression;

    method new(Str $original-expression, @args) {
        my $obj := nqp::create(self);
        my @params := [];
        nqp::bindattr($obj, RakuAST::CurryThunk, '$!parameters', @params);
        for @args {
            # $name will usually be undefined, but sometimes we re-use references to existing * targets
            my $target := RakuAST::ParameterTarget::Whatever.new($_.name);
            $_.set-resolution($target);
            my $param := RakuAST::Parameter.new(
                target => $target
            );
            nqp::push(@params, $param);
        }
        nqp::bindattr($obj, RakuAST::CurryThunk, '$!original-expression', nqp::hllizefor($original-expression, 'Raku'));
        $obj
    }

    method thunk-kind() {
        'WhateverCode'
    }

    method thunk-details() {
        '⋐' ~ nqp::x('🔆', self.IMPL-NUM-PARAMS)  ~ '⋑'
    }

    method IMPL-THUNK-OBJECT-TYPE() {
        self.get-implicit-lookups.AT-POS(0).compile-time-value
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('WhateverCode'))
        ])
    }

    method IMPL-NUM-PARAMS() {
        nqp::elems($!parameters)
    }

    method IMPL-THUNK-SIGNATURE() {
        RakuAST::Signature.new(parameters => self.IMPL-WRAP-LIST($!parameters))
    }

    method IMPL-THUNK-META-OBJECT-PRODUCED(Mu $code) {
        nqp::bindattr($code, self.get-implicit-lookups.AT-POS(0).compile-time-value, '$!original-expression', $!original-expression)
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
        self.IMPL-WRAP-LIST([
            RakuAST::VarDeclaration::Implicit::BlockTopic.new:
                parameter => self.signature ?? False !! True
        ]);
    }

    method IMPL-THUNK-OBJECT-TYPE() {
        Block
    }

    method IMPL-QAST-DECL-CODE(RakuAST::IMPL::QASTContext $context) {
        # Form the block itself and link it with the meta-object.
        self.IMPL-QAST-BLOCK($context, :blocktype('declaration_static'), :expression($!expression));
    }

    method PRODUCE-META-OBJECT() {
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
