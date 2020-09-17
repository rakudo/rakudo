# A blockoid represents the block part of some kind of code declaration.
class RakuAST::Blockoid is RakuAST::SinkPropagator {
    has RakuAST::StatementList $.statement-list;

    method new(RakuAST::StatementList $statement-list?) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Blockoid, '$!statement-list',
            $statement-list // RakuAST::StatementList.new);
        $obj
    }

    method DEPARSE() { "\{\n" ~ $!statement-list.DEPARSE ~ '}' }

    method propagate-sink(Bool $is-sunk) {
        $!statement-list.propagate-sink($is-sunk, :has-block-parent(True))
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        $!statement-list.IMPL-TO-QAST($context)
    }

    method visit-children(Code $visitor) {
        $visitor($!statement-list);
    }
}

# Marker for all code-y things.
class RakuAST::Code is RakuAST::Node {
    method IMPL-CLOSURE-QAST() {
        my $code-obj := self.meta-object;
        QAST::Op.new(
            :op('p6capturelex'),
            QAST::Op.new(
                :op('callmethod'), :name('clone'),
                QAST::WVal.new( :value($code-obj) )
            )
        )
    }

    method IMPL-LINK-META-OBJECT(RakuAST::IMPL::QASTContext $context, Mu $block) {
        # Obtain the meta-object and connect it to the code block.
        my $code-obj := self.meta-object;
        $context.ensure-sc($code-obj);
        $block.code_object($code-obj);

        # We need to do a fixup of the code block for the non-precompiled case.
        $context.add-fixup-task(-> {
            QAST::Op.new(
                :op('bindattr'),
                QAST::WVal.new( :value($code-obj) ),
                QAST::WVal.new( :value(Code) ),
                QAST::SVal.new( :value('$!do') ),
                QAST::BVal.new( :value($block) )
            )
        });
    }
}

# A block, either without signature or with only a placeholder signature.
class RakuAST::Block is RakuAST::LexicalScope is RakuAST::Term is RakuAST::Code is RakuAST::Meta
                     is RakuAST::BlockStatementSensitive is RakuAST::SinkPropagator
                     is RakuAST::Blorst is RakuAST::ImplicitDeclarations
                     is RakuAST::AttachTarget {
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

    method new(RakuAST::Blockoid :$body, Bool :$implicit-topic, Bool :$required-topic) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Block, '$!body', $body // RakuAST::Blockoid.new);
        nqp::bindattr_i($obj, RakuAST::Block, '$!implicit-topic-mode', 1);
        $obj
    }

    method DEPARSE() {
        '{' ~ $!body.DEPARSE ~ '}'  # XXX needs topic inspection?
    }

    method replace-body(RakuAST::Blockoid $new-body) {
        nqp::bindattr(self, RakuAST::Block, '$!body', $new-body);
        Nil
    }

    method set-implicit-topic(Bool $implicit, Bool :$required, Bool :$exception) {
        nqp::bindattr_i(self, RakuAST::Block, '$!implicit-topic-mode', $implicit
            ?? ($exception ?? 3 !!
                $required  ?? 2 !!
                              1)
            !! 0);
        Nil
    }

    method set-fresh-variables(Bool :$match, Bool :$exception) {
        nqp::bindattr_i(self, RakuAST::Block, '$!fresh-match', $match ?? 1 !! 0);
        nqp::bindattr_i(self, RakuAST::Block, '$!fresh-exception', $exception ?? 1 !! 0);
    }

    method attach-target-names() {
        self.IMPL-WRAP-LIST(['block'])
    }

    method clear-attachments() {
        self.clear-handler-attachments();
        Nil
    }

    method signature() { Nil }

    method propagate-sink(Bool $is-sunk) {
        $!body.apply-sink($is-sunk);
    }

    method PRODUCE-IMPLICIT-DECLARATIONS() {
        my @implicit;
        unless self.signature {
            if $!implicit-topic-mode == 1 {
                @implicit[0] := RakuAST::VarDeclaration::Implicit::TopicParameter.new;
            }
            elsif $!implicit-topic-mode == 2 {
                @implicit[0] := RakuAST::VarDeclaration::Implicit::TopicParameter.new(:required);
            }
            elsif $!implicit-topic-mode == 3 {
                @implicit[0] := RakuAST::VarDeclaration::Implicit::TopicParameter.new(:required,
                    :exception);
            }
            if $!fresh-match {
                nqp::push(@implicit, RakuAST::VarDeclaration::Implicit::Special.new(:name('$/')));
            }
            if $!fresh-exception {
                nqp::push(@implicit, RakuAST::VarDeclaration::Implicit::Special.new(:name('$!')));
            }
        }
        self.IMPL-WRAP-LIST(@implicit)
    }

    method PRODUCE-META-OBJECT() {
        # Create block object and install signature. If it doesn't have one, then
        # we can create it based upon the implicit topic it may or may not have.
        my $block := nqp::create(Block);
        my $signature := self.signature;
        if $signature {
            nqp::bindattr($block, Code, '$!signature', $signature.meta-object);
        }
        elsif $!implicit-topic-mode {
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
        $block
    }

    method IMPL-QAST-FORM-BLOCK(RakuAST::IMPL::QASTContext $context, str $blocktype) {
        # Form block with declarations.
        my $block := QAST::Block.new(
            :$blocktype,
            self.IMPL-QAST-DECLS($context)
        );

        # Compile body and, if needed, a signature, and set up arity and any
        # exception rethrow logic.
        my $body-qast := $!body.IMPL-TO-QAST($context);
        my $signature := self.signature;
        if $signature {
            $block.push($signature.IMPL-TO-QAST($context));
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
        $block
    }

    method IMPL-QAST-DECL-CODE(RakuAST::IMPL::QASTContext $context) {
        # Form the block itself and link it with the meta-object.
        my $block := self.IMPL-QAST-FORM-BLOCK($context, 'declaration_static');
        self.IMPL-LINK-META-OBJECT($context, $block);
        $block
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context, :$immediate) {
        if $immediate {
            # For now, assume we never need a code object for such a block. The
            # closure clone is done for us by the QAST compiler.
            self.IMPL-QAST-FORM-BLOCK($context, 'immediate')
        }
        else {
            # Not immediate, so already produced as a declaration above; just
            # closure clone it. Only invoke if it's a bare block.
            my $ast := self.IMPL-CLOSURE-QAST();
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
    method block-or-hash() {
        my @statements := self.IMPL-UNWRAP-LIST(self.body.statement-list.statements);
        my int $num-statements := nqp::elems(@statements);
        if $num-statements == 0 {
            # Empty block is always an empty hash composer
            RakuAST::Circumfix::HashComposer.new
        }
        elsif $num-statements > 1 {
            # Multiple statements is always a block
            self
        }
        else {
            # Must be an expression statement.
            my $statement := @statements[0];
            unless nqp::istype($statement, RakuAST::Statement::Expression) {
                return self;
            }
            my $expression := $statement.expression;

            # If it's a comma list, then obtain the first element. Otherwise,
            # we have the thing to test already.
            my int $is-comma := nqp::istype($expression, RakuAST::ApplyListInfix) &&
                nqp::istype($expression.infix, RakuAST::Infix) &&
                $expression.infix.operator eq ',';
            my $test := $is-comma
                ?? self.IMPL-UNWRAP-LIST($expression.operands)[0]
                !! $expression;

            # It's a block unless we have:
            return self unless
                # A fatarrow or colonpair
                nqp::istype($test, RakuAST::FatArrow) ||
                nqp::istype($test, RakuAST::ColonPair) ||
                # The Pair infix operator
                nqp::istype($test, RakuAST::ApplyInfix) &&
                    nqp::istype($test.infix, RakuAST::Infix) &&
                    $test.infix.operator eq '=>' ||
                # A hash sigil'd variable
                nqp::istype($test, RakuAST::Var) && $test.sigil eq '%';

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
                !! RakuAST::Circumfix::HashComposer.new($expression)
        }
    }

    method visit-children(Code $visitor) {
        $visitor($!body);
    }
}

# A pointy block (-> $foo { ... }).
class RakuAST::PointyBlock is RakuAST::Block {
    has RakuAST::Signature $.signature;

    method new(RakuAST::Signature :$signature, RakuAST::Blockoid :$body) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::PointyBlock, '$!signature', $signature
            // RakuAST::Signature.new);
        nqp::bindattr($obj, RakuAST::Block, '$!body', $body // RakuAST::Blockoid.new);
        $obj
    }

    method DEPARSE() {
        '-> ' ~ $!signature.DEPARSE ~ "\{\n" ~ self.body.DEPARSE ~ '}'
    }

    method replace-signature(RakuAST::Signature $new-signature) {
        nqp::bindattr(self, RakuAST::PointyBlock, '$!signature', $new-signature);
        Nil
    }

    method bare-block() { False }

    method propagate-sink(Bool $is-sunk) {
        self.body.apply-sink($is-sunk);
        $!signature.apply-sink(True);
    }

    method visit-children(Code $visitor) {
        $visitor($!signature);
        $visitor(self.body);
    }
}

# Done by all kinds of Routine.
class RakuAST::Routine is RakuAST::LexicalScope is RakuAST::Term is RakuAST::Code is RakuAST::Meta
                       is RakuAST::SinkBoundary is RakuAST::Declaration
                       is RakuAST::ImplicitDeclarations is RakuAST::AttachTarget
                       is RakuAST::BeginTime is RakuAST::TraitTarget {
    has RakuAST::Name $.name;
    has RakuAST::Signature $.signature;
    has RakuAST::Blockoid $.body;

    method new(str :$scope, RakuAST::Name :$name, RakuAST::Signature :$signature,
            List :$traits, RakuAST::Blockoid :$body) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration, '$!scope', $scope);
        nqp::bindattr($obj, RakuAST::Routine, '$!name', $name // RakuAST::Name);
        nqp::bindattr($obj, RakuAST::Routine, '$!signature', $signature
            // RakuAST::Signature.new);
        nqp::bindattr($obj, RakuAST::Routine, '$!body', $body // RakuAST::Blockoid.new);
        $obj.set-traits($traits);
        $obj
    }

    method IMPL-DEPARSE-ROUTINE() {
        my $sig := '(' ~ $!signature.DEPARSE ~ ") \{\n" ~ $!body.DEPARSE ~ '}';
        $!name ?? $!name ~ $sig !! $sig
    }

    method replace-name(RakuAST::Name $new-name) {
        nqp::bindattr(self, RakuAST::Routine, '$!name', $new-name);
        Nil
    }

    method replace-body(RakuAST::Blockoid $new-body) {
        nqp::bindattr(self, RakuAST::Routine, '$!body', $new-body);
        Nil
    }

    method replace-signature(RakuAST::Signature $new-signature) {
        nqp::bindattr(self, RakuAST::Routine, '$!signature', $new-signature);
        Nil
    }

    method attach-target-names() {
        self.IMPL-WRAP-LIST(['routine', 'block'])
    }

    method clear-attachments() {
        self.clear-handler-attachments();
        Nil
    }

    method PRODUCE-META-OBJECT() {
        my $routine := nqp::create(self.IMPL-META-OBJECT-TYPE);
        my $signature := self.signature;
        nqp::bindattr($routine, Code, '$!signature', $signature.meta-object);
        $routine
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver) {
        self.apply-traits($resolver, self)
    }

    method PRODUCE-IMPLICIT-DECLARATIONS() {
        self.IMPL-WRAP-LIST([
            RakuAST::VarDeclaration::Implicit::Special.new(:name('$/')),
            RakuAST::VarDeclaration::Implicit::Special.new(:name('$!')),
            RakuAST::VarDeclaration::Implicit::Special.new(:name('$_')),
        ])
    }

    method IMPL-QAST-FORM-BLOCK(RakuAST::IMPL::QASTContext $context) {
        my $block := QAST::Block.new(
            :blocktype('declaration_static'),
            self.IMPL-QAST-DECLS($context)
        );
        $block.push($!signature.IMPL-TO-QAST($context));
        $block.arity($!signature.arity);
        $block.annotate('count', $!signature.count);
        $block.push(self.IMPL-WRAP-RETURN-HANDLER($context,
            self.IMPL-WRAP-SCOPE-HANDLER-QAST($context, $!body.IMPL-TO-QAST($context))));
        $block
    }

    method IMPL-WRAP-RETURN-HANDLER(RakuAST::IMPL::QASTContext $context, RakuAST::Node $body) {
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
        unless $returns =:= Mu {
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
        my $block := self.IMPL-QAST-FORM-BLOCK($context);
        self.IMPL-LINK-META-OBJECT($context, $block);

        # Set a name, if there is one.
        if $!name {
            my $canon-name := $!name.canonicalize;
            $block.name($canon-name);
        }

        $block
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        # If we're a named lexical thing, install us in the block.
        my $name := self.lexical-name;
        if $name && self.scope eq 'my' {
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :decl<var>, :scope<lexical>, :$name ),
                self.IMPL-CLOSURE-QAST()
            )
        }
        else {
            QAST::Op.new( :op('null') )
        }
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        if self.scope eq 'my' {
            my $canon-name := $!name.canonicalize;
            QAST::Var.new( :scope<lexical>, :name('&' ~ $canon-name) )
        }
        else {
            self.IMPL-CLOSURE-QAST
        }
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

    method get-boundary-sink-propagator() {
        $!body.statement-list
    }

    method is-boundary-sunk() {
        True
    }

    method visit-children(Code $visitor) {
        $visitor($!name);
        $visitor($!signature);
        self.visit-traits($visitor);
        $visitor($!body);
    }
}

# A subroutine.
class RakuAST::Sub is RakuAST::Routine is RakuAST::Declaration {

    method DEPARSE() { 'sub ' ~ self.IMPL-DEPARSE-ROUTINE }

    method IMPL-META-OBJECT-TYPE() { Sub }

    method default-scope() {
        self.name ?? 'my' !! 'anon'
    }

    method allowed-scopes() {
        self.IMPL-WRAP-LIST(['my', 'anon', 'our'])
    }
}

# A method.
class RakuAST::Method is RakuAST::Routine is RakuAST::Attaching {
    method IMPL-META-OBJECT-TYPE() { Method }

    method DEPARSE() { 'method ' ~ self.IMPL-DEPARSE-ROUTINE }

    method default-scope() {
        self.name ?? 'has' !! 'anon'
    }

    method allowed-scopes() {
        self.IMPL-WRAP-LIST(['has', 'my', 'anon', 'our'])
    }

    method PRODUCE-IMPLICIT-DECLARATIONS() {
        self.IMPL-WRAP-LIST([
            RakuAST::VarDeclaration::Implicit::Special.new(:name('$/')),
            RakuAST::VarDeclaration::Implicit::Special.new(:name('$!')),
            RakuAST::VarDeclaration::Implicit::Special.new(:name('$_')),
            RakuAST::VarDeclaration::Implicit::Self.new(),
        ])
    }

    method attach(RakuAST::Resolver $resolver) {
        if self.scope eq 'has' {
            my $package := $resolver.find-attach-target('package');
            if $package {
                $package.ATTACH-METHOD(self);
            }
            else {
                # TODO check-time problem
            }
        }
    }
}

# A submethod.
class RakuAST::Submethod is RakuAST::Method {

    method DEPARSE() { 'submethod' ~ self.IMPL-DEPARSE-ROUTINE }

    method IMPL-META-OBJECT-TYPE() { Submethod }
}

# A regex declaration, such as `token foo { bar }`. This implies its own
# lexical scope.
class RakuAST::RegexDeclaration is RakuAST::Code is RakuAST::LexicalScope is RakuAST::Term {
    has RakuAST::Signature $.signature;
    has RakuAST::Regex $.body;

    method new(RakuAST::Signature :$signature, RakuAST::Regex :$body) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::RegexDeclaration, '$!signature',
            $signature // RakuAST::Signature.new);
        nqp::bindattr($obj, RakuAST::RegexDeclaration, '$!body',
            $body // RakuAST::Regex::Assertion::Fail.new);
        $obj
    }

    method replace-body(RakuAST::Regex $new-body) {
        nqp::bindattr(self, RakuAST::RegexDeclaration, '$!body', $new-body);
        Nil
    }

    method replace-signature(RakuAST::Signature $new-signature) {
        nqp::bindattr(self, RakuAST::RegexDeclaration, '$!signature', $new-signature);
        Nil
    }

    method visit-children(Code $visitor) {
        $visitor($!signature);
        $visitor($!body);
    }
}

# Done by things that "thunk" a regex - that is to say, they want to compile as
# a separate regex code object but without introducing a new lexical scope. This#
# includes quoted regexes like /.../, capturing groups, and calls of the  form
# `<?before foo>`, where `foo` is the thunked regex.
class RakuAST::RegexThunk is RakuAST::Code is RakuAST::Meta {
    method PRODUCE-META-OBJECT() {
        # Create default signature, receiving invocant only.
        my $signature := nqp::create(Signature);
        my $parameter := nqp::create(Parameter);
        nqp::bindattr($parameter, Parameter, '$!nominal_type', Mu);
        nqp::bindattr($signature, Signature, '@!params', nqp::list($parameter));

        # Create Regex object.
        my $regex := nqp::create(Regex);
        nqp::bindattr($regex, Code, '$!signature', $signature);
        $regex
    }

    method IMPL-QAST-FORM-BLOCK(RakuAST::IMPL::QASTContext $context, str $blocktype) {
        QAST::Block.new(
            :blocktype('declaration_static'),
            QAST::Var.new( :decl('var'), :scope('local'), :name('self') ),
            QAST::Var.new( :decl('var'), :scope('lexical'), :name('$¢') ),
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
            self.IMPL-THUNKED-REGEX-QAST($context)
        )
    }
}

# A quoted regex, such as `/abc/` or `rx/def/` or `m/ghi/`. Does not imply a
# new lexical scope.
class RakuAST::QuotedRegex is RakuAST::RegexThunk is RakuAST::Term
                           is RakuAST::Sinkable {
    has RakuAST::Regex $.body;

    method new(RakuAST::Regex :$body) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::QuotedRegex, '$!body',
            $body // RakuAST::Regex::Assertion::Fail.new);
        $obj
    }

    method replace-body(RakuAST::Regex $new-body) {
        nqp::bindattr(self, RakuAST::QuotedRegex, '$!body', $new-body);
        Nil
    }

    method IMPL-THUNKED-REGEX-QAST(RakuAST::IMPL::QASTContext $context) {
        $!body.IMPL-REGEX-TOP-LEVEL-QAST($context, self.meta-object, nqp::hash())
    }

    method IMPL-QAST-DECL-CODE(RakuAST::IMPL::QASTContext $context) {
        # Form the block itself and link it with the meta-object.
        my $block := self.IMPL-QAST-FORM-BLOCK($context, 'declaration_static');
        self.IMPL-LINK-META-OBJECT($context, $block);
        $block
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        # TODO topic/slash capture and sink context
        self.IMPL-CLOSURE-QAST
    }

    method visit-children(Code $visitor) {
        $visitor($!body);
    }
}
