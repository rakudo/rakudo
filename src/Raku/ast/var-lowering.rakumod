# Escape analysis for lexical-to-local lowering. Walks a checked and
# optimized compilation unit and decides, per `my` declaration, whether
# every access is confined to the frame that declares it. A confined
# declaration is marked so QAST emission can use a frame-local register
# slot instead of a by-name lexical entry. The walk mirrors the frame
# structure QAST emission produces: every node that creates a block (a
# lexical scope, or an expression carrying thunks) is a frame boundary.

# Per-frame bookkeeping made while the walk is inside that frame.
class RakuAST::IMPL::VarLoweringFrame {
    has RakuAST::Node $!node;
    has int $!is-scope;
    has int $!poisoned;
    has Mu $!candidates;
    has Mu $!candidate-ids;
    has Mu $!candidate-names;
    has Mu $!implicit-ids;
    has Mu $!implicit-names;
    has int $!implicit-used;
    has int $!flatten-candidate;
    has int $!flatten-arg;
    has int $!flatten-blocked;
    has Mu $!deferred-uses;
    has str $!implicit-slurpy-id;

    method new(RakuAST::Node $node, int $is-scope) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::IMPL::VarLoweringFrame, '$!node', $node);
        nqp::bindattr_i($obj, RakuAST::IMPL::VarLoweringFrame, '$!is-scope', $is-scope);
        nqp::bindattr($obj, RakuAST::IMPL::VarLoweringFrame, '$!candidates', []);
        nqp::bindattr($obj, RakuAST::IMPL::VarLoweringFrame, '$!candidate-ids', nqp::hash());
        nqp::bindattr($obj, RakuAST::IMPL::VarLoweringFrame, '$!candidate-names', nqp::hash());
        nqp::bindattr($obj, RakuAST::IMPL::VarLoweringFrame, '$!implicit-ids', nqp::hash());
        nqp::bindattr($obj, RakuAST::IMPL::VarLoweringFrame, '$!implicit-names', nqp::hash());
        nqp::bindattr($obj, RakuAST::IMPL::VarLoweringFrame, '$!deferred-uses', []);
        $obj
    }

    method node() { $!node }
    method is-scope() { $!is-scope }
    method poison() {
        nqp::bindattr_i(self, RakuAST::IMPL::VarLoweringFrame, '$!poisoned', 1);
        Nil
    }
    method is-poisoned() { $!poisoned }

    method register-implicit(str $id, Mu $decl) {
        my $record := nqp::hash('decl', $decl);
        nqp::bindkey($!implicit-ids, $id, $record);
        nqp::bindkey($!implicit-names, $decl.lexical-name, $record);
        Nil
    }
    method implicit-record-for-id(str $id) { nqp::atkey($!implicit-ids, $id) }
    method implicit-record-for-name(str $name) { nqp::atkey($!implicit-names, $name) }
    method implicit-records() { $!implicit-ids }
    method mark-implicit-used(Mu $record) {
        nqp::bindkey($record, 'used', 1);
        nqp::bindattr_i(self, RakuAST::IMPL::VarLoweringFrame, '$!implicit-used', 1);
        Nil
    }
    method implicit-used() { $!implicit-used }

    method mark-flatten-candidate() {
        nqp::bindattr_i(self, RakuAST::IMPL::VarLoweringFrame, '$!flatten-candidate', 1);
        Nil
    }
    method mark-flatten-arg() {
        nqp::bindattr_i(self, RakuAST::IMPL::VarLoweringFrame, '$!flatten-arg', 1);
        Nil
    }
    method flatten-arg() { $!flatten-arg }
    method is-flatten-candidate() { $!flatten-candidate }
    method block-flatten() {
        nqp::bindattr_i(self, RakuAST::IMPL::VarLoweringFrame, '$!flatten-blocked', 1);
        Nil
    }
    method flatten-blocked() { $!flatten-blocked }

    method set-implicit-slurpy-id(str $id) {
        nqp::bindattr_s(self, RakuAST::IMPL::VarLoweringFrame, '$!implicit-slurpy-id', $id);
        Nil
    }
    method implicit-slurpy-id() { $!implicit-slurpy-id }

    method add-deferred(str $id) {
        nqp::push($!deferred-uses, $id);
        Nil
    }
    method deferred-uses() { $!deferred-uses }

    method register(Mu $decl, str $declined) {
        my $record := nqp::hash('decl', $decl, 'declined', $declined);
        # Redeclarations of one name in one scope share a runtime symbol,
        # so neither copy can own a private register slot.
        my str $name := $decl.lexical-name;
        my $clash := nqp::atkey($!candidate-names, $name);
        if nqp::isnull($clash) {
            nqp::bindkey($!candidate-names, $name, $record);
        }
        else {
            nqp::bindkey($clash, 'declined', 'duplicate')
                unless nqp::atkey($clash, 'declined');
            nqp::bindkey($record, 'declined', 'duplicate')
                unless $declined;
        }
        nqp::push($!candidates, $record);
        nqp::bindkey($!candidate-ids, ~nqp::objectid($decl), $record);
        $record
    }

    # An extra identity a use-site's resolution may carry for this
    # candidate, such as the parameter target wrapping a declaration.
    method alias(str $id, Mu $record) {
        nqp::bindkey($!candidate-ids, $id, $record);
        Nil
    }

    method record-for-id(str $id) {
        nqp::atkey($!candidate-ids, $id)
    }

    method candidates() { $!candidates }
}

# Drives the analysis. One instance analyzes one compilation unit.
class RakuAST::IMPL::VarLowering {
    has Mu $!frames;
    has Mu $!sentinel;
    has int $!debug;
    has int $!begin-context;
    has int $!topic-not-dynamic;

    method analyze-compunit(RakuAST::CompUnit $compunit, RakuAST::Resolver $resolver, :$interactive?) {
        # Escape hatch while the lowering is young: skip the analysis
        # entirely, leaving every lexical emitted by name.
        return Nil if nqp::atkey(nqp::getenvhash(), 'RAKUDO_NO_LEX2LOCAL');

        my $analyzer := nqp::create(self);
        nqp::bindattr($analyzer, RakuAST::IMPL::VarLowering, '$!frames', []);
        nqp::bindattr_i($analyzer, RakuAST::IMPL::VarLowering, '$!debug',
            nqp::atkey(nqp::getenvhash(), 'RAKUDO_LOWERING_DEBUG') ?? 1 !! 0);
        # From 6.d the topic is not a dynamic variable, so a callee
        # cannot reach an unused one and its declaration can go.
        nqp::bindattr_i($analyzer, RakuAST::IMPL::VarLowering, '$!topic-not-dynamic',
            nqp::getcomp('Raku').language_revision >= 2 ?? 1 !! 0);

        # The sentinel type that stands in for a lowered lexical's
        # by-name symbol. Without it (very early bootstrap) nothing is
        # marked, since the emitted placeholder could not be produced.
        my $sentinel := nqp::null;
        my $sentinel-name := RakuAST::Name.from-identifier-parts(
            'Rakudo', 'Internals', 'LoweredAwayLexical');
        my $found := $resolver.resolve-name-constant-in-setting($sentinel-name);
        $found := $resolver.resolve-name-constant($sentinel-name)
            unless nqp::isconcrete($found);
        $sentinel := $found.compile-time-value
            if nqp::isconcrete($found) && nqp::can($found, 'compile-time-value');
        nqp::bindattr($analyzer, RakuAST::IMPL::VarLowering, '$!sentinel', $sentinel);

        # The mainline Block is an empty shell for the mainline code
        # object; the compilation unit's statements live in the statement
        # list, which QAST emission places directly in the mainline frame.
        # So the compilation unit and the mainline share one frame here,
        # and the shell is skipped to visit each node exactly once.
        my $mainline := $compunit.mainline;
        $analyzer.IMPL-ENTER($compunit, 1);
        # Each interactive (REPL) line must leave its lexicals reachable
        # by name for the lines compiled after it.
        $analyzer.IMPL-POISON-ALL() if $interactive;
        $compunit.visit-children(-> $child {
            $analyzer.IMPL-WALK($child) unless nqp::eqaddr($child, $mainline);
        });
        $analyzer.IMPL-LEAVE();
        Nil
    }

    method IMPL-WALK(RakuAST::Node $node) {
        self.IMPL-CHECK-NAME-REACHERS($node);
        self.IMPL-CHECK-FLATTEN-BLOCKERS($node);

        # Trait arguments, type parameterizations, and constant
        # initializers are evaluated at BEGIN time by dynamically
        # compiled code that reaches lexicals by name, which neither the
        # frame nesting nor the resolutions here can show. Every use in
        # such a subtree escapes.
        my int $begin-entered;
        if nqp::istype($node, RakuAST::Trait)
            || nqp::istype($node, RakuAST::Type::Parameterized)
            || nqp::istype($node, RakuAST::VarDeclaration::Constant) {
            nqp::bindattr_i(self, RakuAST::IMPL::VarLowering, '$!begin-context',
                $!begin-context + 1);
            $begin-entered := 1;
        }
        if $begin-entered {
            self.IMPL-WALK-INNER($node);
            nqp::bindattr_i(self, RakuAST::IMPL::VarLowering, '$!begin-context',
                $!begin-context - 1);
            return Nil;
        }
        self.IMPL-WALK-INNER($node)
    }

    method IMPL-WALK-INNER(RakuAST::Node $node) {

        # The sunk body of a loop statement is a flatten candidate: if
        # everything it does proves frame-independent, the loop emits its
        # statements inline and uses of enclosing lexicals from it do not
        # count as captures.
        if nqp::istype($node, RakuAST::Statement::Loop) && $node.IMPL-DISCARD-RESULT {
            my $body := $node.body;
            if nqp::isconcrete($body) {
                self.IMPL-REGISTER-IMPLICIT-LOOKUPS($node);
                $node.visit-children(-> $child {
                    self.IMPL-WALK($child) unless nqp::eqaddr($child, $body);
                });
                self.IMPL-WALK-FLATTEN-CANDIDATE($body);
                return Nil;
            }
        }

        # A conditional statement's branch bodies flatten the same way,
        # sunk or value-producing: an inlined branch evaluates to its
        # last statement just as the immediate block call did. A branch
        # that topicalizes, as with and orwith do, keeps its frame,
        # which its required topic enforces in the verdict.
        if nqp::istype($node, RakuAST::Statement::IfWith) {
            self.IMPL-REGISTER-IMPLICIT-LOOKUPS($node);
            self.IMPL-WALK($node.condition);
            self.IMPL-WALK-FLATTEN-CANDIDATE($node.then);
            for $node.IMPL-UNWRAP-LIST($node.elsifs) {
                self.IMPL-WALK($_.condition);
                self.IMPL-WALK-FLATTEN-CANDIDATE($_.then);
            }
            my $else := $node.else;
            self.IMPL-WALK-FLATTEN-CANDIDATE($else) if nqp::isconcrete($else);
            $node.visit-labels(-> $label { self.IMPL-WALK($label) });
            return Nil;
        }
        if nqp::istype($node, RakuAST::Statement::Unless) {
            self.IMPL-REGISTER-IMPLICIT-LOOKUPS($node);
            self.IMPL-WALK($node.condition);
            self.IMPL-WALK-FLATTEN-CANDIDATE($node.body);
            $node.visit-labels(-> $label { self.IMPL-WALK($label) });
            return Nil;
        }

        # A sunk serial for statement drives its body directly, and a
        # given calls its body with the topicalized value, so both walk
        # their bodies as argument-taking flatten candidates: a plain
        # body flattens when its topic goes unused, and a pointy body
        # with one plain parameter has the iteration value bound to the
        # parameter's local instead.
        if nqp::istype($node, RakuAST::Statement::For)
            && $node.mode eq 'serial'
            && $node.IMPL-DISCARD-RESULT
            && !nqp::isconcrete($node.otherwise)
            && $node.IMPL-CAN-USE-STATEMENT-FORM($node.body) {
            self.IMPL-REGISTER-IMPLICIT-LOOKUPS($node);
            self.IMPL-WALK($node.source);
            self.IMPL-WALK-FLATTEN-CANDIDATE($node.body, :arg);
            $node.visit-labels(-> $label { self.IMPL-WALK($label) });
            return Nil;
        }
        if nqp::istype($node, RakuAST::Statement::Given) {
            self.IMPL-REGISTER-IMPLICIT-LOOKUPS($node);
            self.IMPL-WALK($node.source);
            self.IMPL-WALK-FLATTEN-CANDIDATE($node.body, :arg);
            $node.visit-labels(-> $label { self.IMPL-WALK($label) });
            return Nil;
        }

        # A bare block statement is called where it stands, so it
        # flattens under the same rules as a loop body. A loop modifier
        # keeps the block a per-iteration frame, and a topicalizing
        # condition modifier passes the block an argument.
        if nqp::istype($node, RakuAST::Statement::Expression)
            && !nqp::isconcrete($node.loop-modifier) {
            my $expression := $node.expression;
            my $cond := $node.condition-modifier;
            if nqp::eqaddr($expression.WHAT, RakuAST::Block)
                && $expression.bare-block
                && (!nqp::isconcrete($cond)
                    || nqp::istype($cond, RakuAST::StatementModifier::If)
                    || nqp::istype($cond, RakuAST::StatementModifier::Unless)) {
                self.IMPL-REGISTER-IMPLICIT-LOOKUPS($node);
                self.IMPL-WALK($cond) if nqp::isconcrete($cond);
                self.IMPL-WALK-FLATTEN-CANDIDATE($expression);
                $node.visit-labels(-> $label { self.IMPL-WALK($label) });
                return Nil;
            }
        }

        # A variable declaration belongs to the enclosing scope's frame,
        # not to any frame the node itself creates. A parameter's uses
        # resolve to the target node while emission delegates to the
        # declaration it wraps, so the target registers the declaration
        # under both identities, and the wrapped declaration itself is
        # skipped when the walk reaches it as a child.
        # A %_ appearing in a method body is a slurpy hash placeholder
        # node rather than a variable lookup, and it is what makes the
        # signature's implicit slurpy hash matter.
        if nqp::istype($node, RakuAST::VarDeclaration::Placeholder::SlurpyHash) {
            my int $i := nqp::elems($!frames);
            while --$i >= 0 {
                my $frame := nqp::atpos($!frames, $i);
                if $frame.implicit-slurpy-id {
                    my $record := $frame.record-for-id($frame.implicit-slurpy-id);
                    nqp::bindkey($record, 'used', 1) unless nqp::isnull($record);
                    last;
                }
            }
        }

        if nqp::istype($node, RakuAST::ParameterTarget::Var) {
            my $decl := $node.declaration;
            self.IMPL-REGISTER-DECL($decl, ~nqp::objectid($node))
                if nqp::isconcrete($decl)
                && nqp::istype($decl, RakuAST::VarDeclaration::Simple);
        }
        elsif nqp::istype($node, RakuAST::VarDeclaration::Simple) {
            self.IMPL-REGISTER-DECL($node)
                unless nqp::getattr($node, RakuAST::VarDeclaration::Simple, '$!is-parameter');
        }

        # A list declaration bound with := goes through the runtime
        # signature binder, which writes into the frame's lexicals by
        # name.
        if nqp::istype($node, RakuAST::VarDeclaration::Signature)
            && nqp::isconcrete($node.initializer)
            && $node.initializer.is-binding {
            self.IMPL-POISON-CURRENT-SCOPE();
        }

        # Feed stages are emitted inside blocks the tree does not show,
        # so anything a stage references stays a by-name lexical.
        if nqp::istype($node, RakuAST::ApplyListInfix)
            && nqp::istype($node.infix, RakuAST::Feed) {
            $node.visit-children(-> $child {
                self.IMPL-ENTER($child, 0);
                self.IMPL-WALK($child);
                self.IMPL-LEAVE();
            });
            return Nil;
        }

        # An nqp::handle handler is code-generated in an implicit block
        # of its own, so a lexical referenced from a handler must stay
        # addressable by name. The protected expression, the first
        # argument, runs in the current frame.
        if nqp::istype($node, RakuAST::Nqp)
            && ($node.op eq 'handle' || $node.op eq 'handlepayload') {
            my int $first := 1;
            $node.visit-children(-> $child {
                if $first {
                    $first := 0;
                    self.IMPL-WALK($child);
                }
                else {
                    self.IMPL-ENTER($child, 0);
                    self.IMPL-WALK($child);
                    self.IMPL-LEAVE();
                }
            });
            return Nil;
        }

        # A loop statement modifier's thunk wraps the statement
        # expression, and a condition modifier's test is compiled inside
        # that same thunk, while the loop source stays outside. Mirror
        # that frame placement rather than walking the modifiers in the
        # statement's own frame.
        if nqp::istype($node, RakuAST::Statement::Expression)
            && nqp::isconcrete($node.loop-modifier) {
            my $expression := $node.expression;
            if nqp::istype($expression, RakuAST::Expression) && $expression.creates-block {
                self.IMPL-ENTER($node, 0);
                self.IMPL-WALK($expression);
                self.IMPL-WALK($node.condition-modifier)
                    if nqp::isconcrete($node.condition-modifier);
                self.IMPL-LEAVE();
                self.IMPL-WALK($node.loop-modifier);
                $node.visit-labels(-> $label { self.IMPL-WALK($label) });
                return Nil;
            }
        }

        my int $pushed;
        if nqp::istype($node, RakuAST::MayCreateBlock) && $node.creates-block {
            self.IMPL-ENTER($node, nqp::istype($node, RakuAST::LexicalScope) ?? 1 !! 0);
            $pushed := 1;
            # A signature that needs the runtime binder binds by name
            # into the frame, so its lexicals must stay addressable.
            nqp::atpos($!frames, nqp::elems($!frames) - 1).poison()
                if nqp::istype($node, RakuAST::Code) && $node.custom-args;
        }

        self.IMPL-REGISTER-USE($node)
            if nqp::istype($node, RakuAST::Lookup) && $node.is-resolved;
        self.IMPL-REGISTER-IMPLICIT-LOOKUPS($node);
        self.IMPL-CHECK-POISON($node);

        $node.visit-children(-> $child { self.IMPL-WALK($child) });

        self.IMPL-LEAVE() if $pushed;
        Nil
    }

    method IMPL-ENTER(RakuAST::Node $node, int $is-scope) {
        my $frame := RakuAST::IMPL::VarLoweringFrame.new($node, $is-scope);
        nqp::push($!frames, $frame);
        # Uses can resolve to a scope's implicit declarations, whose
        # identities decide whether the implicits go unused.
        if $is-scope && nqp::istype($node, RakuAST::ImplicitDeclarations) {
            for $node.IMPL-UNWRAP-LIST($node.get-implicit-declarations()) {
                $frame.register-implicit(~nqp::objectid($_), $_)
                    if nqp::istype($_, RakuAST::VarDeclaration::Implicit);
            }
        }
        if $is-scope && nqp::istype($node, RakuAST::Routine)
            && nqp::isconcrete($node.signature) {
            my $slurpy := nqp::getattr($node.signature, RakuAST::Signature,
                '$!implicit-slurpy-hash');
            $frame.set-implicit-slurpy-id(
                ~nqp::objectid($slurpy.target.declaration))
                if nqp::isconcrete($slurpy)
                && nqp::isconcrete($slurpy.target)
                && nqp::isconcrete($slurpy.target.declaration);
        }
        $frame
    }

    method IMPL-LEAVE() {
        my $frame := nqp::pop($!frames);
        if $frame.is-scope {
            self.IMPL-DECIDE($frame);
            my int $flattened;
            if $frame.is-flatten-candidate {
                my int $approved := self.IMPL-FLATTEN-VERDICT($frame);
                $flattened := $approved;
                if $approved {
                    $frame.node.IMPL-SET-FLATTEN-APPROVED();
                    if $!debug {
                        my str $where := '';
                        my $origin := $frame.node.origin;
                        if nqp::isconcrete($origin) {
                            my $source := $origin.source;
                            $where := ' at ' ~ $source.original-file
                                ~ ':' ~ $source.original-line($origin.from);
                        }
                        self.IMPL-NOTE('lex2local: flatten '
                            ~ $frame.node.HOW.name($frame.node) ~ $where);
                    }
                }
                # A use that crossed only this pending body settles now:
                # an approved body is part of its parent's frame, so the
                # use re-evaluates from the parent's viewpoint, while a
                # declined body was a real frame boundary after all.
                for $frame.deferred-uses -> $id {
                    $approved
                        ?? self.IMPL-REGISTER-USE-ID($id)
                        !! self.IMPL-MARK-CAPTURED-ID($id);
                }
            }
            self.IMPL-DECIDE-IMPLICITS($frame) unless $flattened;
        }
        Nil
    }

    # An unused implicit whose declaring scope nothing reaches by name
    # need not be set up at all. The topic only goes from 6.d, where it
    # is not dynamic, so a callee cannot reach it either. A kept topic
    # that aliases the enclosing one still emits its getlexouter, which
    # is a by-name use of the enclosing topic, so elimination cascades
    # outward only through scopes whose own topic went unused.
    method IMPL-DECIDE-IMPLICITS(Mu $frame) {
        my int $poisoned := $frame.is-poisoned;
        my str $slurpy-id := $frame.implicit-slurpy-id;
        if $slurpy-id && !$poisoned {
            my $record := $frame.record-for-id($slurpy-id);
            nqp::atkey($record, 'decl').IMPL-SET-UNUSED-SLURPY()
                unless nqp::isnull($record) || nqp::existskey($record, 'used');
        }
        my $it := nqp::iterator($frame.implicit-records);
        while $it {
            my $record := nqp::iterval(nqp::shift($it));
            my $decl := nqp::atkey($record, 'decl');
            my int $used := nqp::existskey($record, 'used');
            if nqp::istype($decl, RakuAST::VarDeclaration::Implicit::BlockTopic) {
                if $decl.exception {
                    self.IMPL-MARK-MAGICAL-USED('$!');
                }
                elsif $decl.parameter {
                    # A parameter-form topic cannot go: without its param
                    # declaration the block would no longer accept the
                    # argument its callers pass. Only a non-required
                    # parameter defaults from the enclosing topic.
                    self.IMPL-MARK-MAGICAL-USED('$_') unless $decl.required;
                }
                elsif !$used && !$poisoned && $!topic-not-dynamic {
                    # A non-parameter topic binds from the enclosing one
                    # whether or not it is marked required, so an unused
                    # one is dropped whole.
                    $decl.IMPL-SET-UNUSED();
                }
                else {
                    # And a kept one reads the enclosing topic by name.
                    self.IMPL-MARK-MAGICAL-USED('$_');
                }
            }
            elsif nqp::istype($decl, RakuAST::VarDeclaration::Implicit::Special) {
                $decl.IMPL-SET-UNUSED()
                    if $decl.name eq '$_'
                    && !$used && !$poisoned && $!topic-not-dynamic;
            }
            elsif nqp::istype($decl, RakuAST::VarDeclaration::Implicit::Cursor) {
                $decl.IMPL-SET-UNUSED() if !$used && !$poisoned;
            }
        }
        Nil
    }

    # Whether a flatten-candidate body proved frame-independent: not
    # poisoned, no construct that reaches lexicals by name, no handlers,
    # unused implicit topic only, and every declaration of its own a
    # lowered local.
    method IMPL-FLATTEN-VERDICT(Mu $frame) {
        return 0 if $frame.is-poisoned
            || $frame.flatten-blocked
            || $frame.implicit-used;
        my $block := $frame.node;
        return 0 if nqp::getattr($block, RakuAST::LexicalScope, '$!catch-handlers')
            || nqp::getattr($block, RakuAST::LexicalScope, '$!control-handlers');
        return 0 if nqp::elems($block.IMPL-UNWRAP-LIST(
            $block.generated-lexical-declarations()));
        for $block.IMPL-UNWRAP-LIST($block.ast-lexical-declarations()) {
            # The emission's own predicate is the deciding one: the
            # analysis mark alone is not enough, since emission declines
            # some marked declarations, natives among them, and such a
            # declaration still needs the frame.
            if nqp::istype($_, RakuAST::VarDeclaration::Simple)
                && $_.IMPL-LOWERED-LOCAL-NAME {
            }
            else {
                # Before 6.d the topic is dynamic, so a called routine can
                # reach it by name with no use this analysis could see,
                # and a body declaring one must stay a frame.
                return 0 if nqp::isnull($frame.implicit-record-for-id(~nqp::objectid($_)))
                    || !nqp::istype($_, RakuAST::VarDeclaration::Implicit::BlockTopic)
                    || ($_.required && !$frame.flatten-arg)
                    || !$!topic-not-dynamic
                    || $_.exception;
            }
        }
        1
    }

    method IMPL-MARK-CAPTURED-ID(str $id) {
        my int $i := nqp::elems($!frames);
        while --$i >= 0 {
            my $record := nqp::atpos($!frames, $i).record-for-id($id);
            unless nqp::isnull($record) {
                nqp::bindkey($record, 'captured', 1);
                return Nil;
            }
        }
        Nil
    }

    # Register a `my` declaration with the frame of its declaring scope,
    # along with any reason it must stay a lexical that is knowable from
    # the declaration alone.
    method IMPL-REGISTER-DECL(RakuAST::VarDeclaration::Simple $decl, str $alias-id?) {
        return Nil unless $decl.scope eq 'my';

        my int $i := nqp::elems($!frames);
        my $scope-frame;
        my int $scope-index := -1;
        while --$i >= 0 {
            my $frame := nqp::atpos($!frames, $i);
            if $frame.is-scope {
                $scope-frame := $frame;
                $scope-index := $i;
                last;
            }
        }
        return Nil if $scope-index < 0;

        my str $declined := '';
        my str $sigil := $decl.sigil;
        if $decl.twigil ne '' {
            $declined := 'twigil';
        }
        elsif $decl.forced-dynamic {
            $declined := 'dynamic';
        }
        elsif nqp::isconcrete($decl.container-descriptor)
            && $decl.container-descriptor.dynamic {
            # An `is dynamic` trait reaches the descriptor by mutation,
            # not through the declaration's own attributes.
            $declined := 'dynamic';
        }
        elsif $sigil ne '$' && $sigil ne '@' && $sigil ne '%' {
            # A callable is looked up by name as the callee of emitted
            # call ops, so a `&`-sigiled lexical must keep its symbol.
            $declined := 'callable';
        }
        elsif !nqp::istype($decl, RakuAST::VarDeclaration::Anonymous)
            && !nqp::iscclass(nqp::const::CCLASS_ALPHABETIC,
                $decl.desigilname.canonicalize, 0) {
            # This keeps punctuation variables such as $/ addressable
            # by name. An anonymous variable reports an empty
            # desigilname, but nothing can look one up, so it is
            # exempt.
            $declined := 'name';
        }
        elsif nqp::isconcrete($decl.shape) {
            $declined := 'shaped';
        }
        elsif nqp::elems($decl.IMPL-UNWRAP-LIST($decl.traits)) {
            # A variable trait applies through a Variable meta-object
            # whose phaser helper looks the variable up by name in the
            # calling frame at run time (Variable.willdo), and a custom
            # trait_mod can do the same with any variable it is handed.
            $declined := 'trait';
        }
        elsif $scope-index != nqp::elems($!frames) - 1 || $decl.creates-block {
            # The declaration is emitted under a thunk of its scope, so
            # its accesses cross a frame boundary the scope nesting does
            # not show.
            $declined := 'thunked';
        }
        my $record := $scope-frame.register($decl, $declined);
        $scope-frame.alias($alias-id, $record) if $alias-id;
        Nil
    }

    # Some nodes reach lexicals through implicit lookups that
    # visit-children does not hand out, such as the topic a bare method
    # call resolves. They count as uses like any other.
    method IMPL-REGISTER-IMPLICIT-LOOKUPS(RakuAST::Node $node) {
        if nqp::istype($node, RakuAST::ImplicitLookups) {
            for $node.IMPL-UNWRAP-LIST($node.get-implicit-lookups()) {
                self.IMPL-CHECK-NAME-REACHERS($_);
                self.IMPL-REGISTER-USE($_)
                    if nqp::istype($_, RakuAST::Lookup) && $_.is-resolved;
            }
        }
        Nil
    }

    # Walk the body of a sunk loop statement as a flatten candidate when
    # its shape allows flattening at all: a plain block, no signature or
    # placeholders, no phasers. Everything else about eligibility is
    # decided from what the walk observes, when the frame pops.
    method IMPL-WALK-FLATTEN-CANDIDATE(RakuAST::Node $body, :$arg?) {
        my int $shape-ok := nqp::eqaddr($body.WHAT, RakuAST::Block);
        $shape-ok := 1 if $arg
            && nqp::eqaddr($body.WHAT, RakuAST::PointyBlock)
            && !nqp::isnull($body.IMPL-FLATTEN-ARG-DECLARATION);
        unless $shape-ok
            && !nqp::isconcrete($body.placeholder-signature)
            && !$body.has-any-phasers {
            self.IMPL-WALK($body);
            return Nil;
        }
        my $frame := self.IMPL-ENTER($body, 1);
        $frame.mark-flatten-candidate();
        $frame.mark-flatten-arg() if $arg;
        self.IMPL-REGISTER-IMPLICIT-LOOKUPS($body);
        $body.visit-children(-> $child { self.IMPL-WALK($child) });
        self.IMPL-LEAVE();
        Nil
    }

    # Constructs that reach the topic, or other lexicals, by name at run
    # time without a resolved lookup this walk could see. Any of them in
    # a candidate body's flat extent keeps the body a real frame.
    method IMPL-CHECK-FLATTEN-BLOCKERS(RakuAST::Node $node) {
        my int $n := nqp::elems($!frames);
        return Nil unless $n;
        my $top := nqp::atpos($!frames, $n - 1);
        return Nil unless $top.is-flatten-candidate && !$top.flatten-blocked;
        if nqp::istype($node, RakuAST::ApplyPrefix) {
            # temp and let register their restore on the frame of the
            # code that runs them.
            my $prefix := $node.prefix;
            if nqp::istype($prefix, RakuAST::Prefix) {
                my str $op := $prefix.operator;
                $top.block-flatten() if $op eq 'temp' || $op eq 'let';
            }
        }
        elsif nqp::istype($node, RakuAST::Nqp) {
            # Ops that observe the identity of the running frame, or of
            # its caller or outer, mean something else once the frame is
            # gone.
            my constant FRAME-OPS := nqp::hash(
                'ctx', 1, 'ctxcaller', 1, 'ctxcallerskipthunks', 1,
                'ctxouter', 1, 'ctxouterskipthunks', 1,
                'curcode', 1, 'callercode', 1,
                'getlexouter', 1, 'getlexcaller', 1, 'getlexdyn', 1,
                'getlexrel', 1, 'getlexreldyn', 1, 'getlexrelcaller', 1,
                'savecapture', 1, 'usecapture', 1, 'takedispatcher', 1,
            );
            $top.block-flatten() if nqp::existskey(FRAME-OPS, $node.op);
        }
        elsif nqp::istype($node, RakuAST::Var::Dynamic) {
            # A dynamic lookup that did not resolve lexically walks the
            # caller chain at run time, and that walk starts at the
            # frame's caller, so removing the frame skips one link.
            $top.block-flatten();
        }
        Nil
    }

    # Reaching a magical by name, or running a construct that reads and
    # writes the topic by name, counts as a use of the innermost
    # declaration of that name: magicals are emitted as by-name
    # lexicals, so at run time they bind innermost even when their
    # compile-time resolution points at an outer declaration.
    method IMPL-CHECK-NAME-REACHERS(RakuAST::Node $node) {
        if nqp::istype($node, RakuAST::Var::Lexical) {
            my str $name := $node.name;
            self.IMPL-MARK-MAGICAL-USED($name)
                if $name eq '$_' || $name eq '$/' || $name eq '$!' || $name eq '$¢';
        }
        elsif nqp::istype($node, RakuAST::Statement::When)
            || nqp::istype($node, RakuAST::Statement::Default) {
            self.IMPL-MARK-MAGICAL-USED('$_');
        }
        elsif nqp::istype($node, RakuAST::Statement::Expression) {
            my $cond := $node.condition-modifier;
            my $loop := $node.loop-modifier;
            self.IMPL-MARK-MAGICAL-USED('$_')
                if (nqp::isconcrete($cond)
                    && !nqp::istype($cond, RakuAST::StatementModifier::If)
                    && !nqp::istype($cond, RakuAST::StatementModifier::Unless))
                || (nqp::isconcrete($loop)
                    && !nqp::istype($loop, RakuAST::StatementModifier::WhileUntil));
        }
        elsif nqp::istype($node, RakuAST::ApplyInfix)
            || nqp::istype($node, RakuAST::ApplyListInfix) {
            my $infix := $node.infix;
            if nqp::istype($infix, RakuAST::Infix) {
                my str $op := $infix.operator;
                self.IMPL-MARK-MAGICAL-USED('$_')
                    if $op eq '~~' || $op eq '!~~'
                    || $op eq 'andthen' || $op eq 'orelse' || $op eq 'notandthen';
            }
        }
        Nil
    }

    method IMPL-MARK-MAGICAL-USED(str $name) {
        my int $i := nqp::elems($!frames);
        while --$i >= 0 {
            my $frame := nqp::atpos($!frames, $i);
            if $frame.is-scope {
                my $record := $frame.implicit-record-for-name($name);
                unless nqp::isnull($record) {
                    $frame.mark-implicit-used($record);
                    return Nil;
                }
            }
        }
        Nil
    }

    # A resolved lookup whose resolution is a tracked declaration marks
    # that declaration as captured when the lookup lives in any frame
    # other than the declaring one. A use separated from its declaration
    # only by pending flatten-candidate bodies is deferred instead, since
    # an approved body dissolves into its parent's frame.
    method IMPL-REGISTER-USE(RakuAST::Node $node) {
        self.IMPL-REGISTER-USE-ID(~nqp::objectid($node.resolution));
    }

    method IMPL-REGISTER-USE-ID(str $id) {
        my int $top := nqp::elems($!frames) - 1;
        my int $i := $top + 1;
        while --$i >= 0 {
            my $frame := nqp::atpos($!frames, $i);
            my $record := $frame.record-for-id($id);
            unless nqp::isnull($record) {
                nqp::bindkey($record, 'used', 1);
                if $!begin-context {
                    nqp::bindkey($record, 'captured', 1);
                }
                elsif $i != $top {
                    my int $j := $top;
                    my int $only-candidates := 1;
                    while $j > $i {
                        unless nqp::atpos($!frames, $j).is-flatten-candidate {
                            $only-candidates := 0;
                            last;
                        }
                        $j--;
                    }
                    $only-candidates
                        ?? nqp::atpos($!frames, $top).add-deferred($id)
                        !! nqp::bindkey($record, 'captured', 1);
                }
                return Nil;
            }
            my $implicit := $frame.implicit-record-for-id($id);
            unless nqp::isnull($implicit) {
                $frame.mark-implicit-used($implicit);
                return Nil;
            }
        }
        Nil
    }

    # Constructs that can reach lexicals by name at runtime. Any of them
    # poisons every frame the walk is currently inside: the lexicals of
    # those frames must stay addressable by name.
    method IMPL-CHECK-POISON(RakuAST::Node $node) {
        my constant POISON-CALLS := nqp::hash(
            'EVAL', 1, 'EVALFILE', 1,
            'callwith', 1, 'callsame', 1,
            'nextwith', 1, 'nextsame', 1,
            'samewith', 1,
            'throws-like', 1, 'repl', 1,
        );
        my constant POISON-VARS := nqp::hash(
            '&EVAL', 1, '&EVALFILE', 1,
            '&callwith', 1, '&callsame', 1,
            '&nextwith', 1, '&nextsame', 1,
            '&samewith', 1,
            '&throws-like', 1, '&repl', 1,
        );

        if nqp::istype($node, RakuAST::Call::Name) {
            my $name := $node.name;
            if $name.is-pseudo-package || $name.is-indirect-lookup
                || nqp::existskey(POISON-CALLS, $name.canonicalize) {
                self.IMPL-POISON-ALL();
            }
        }
        elsif nqp::istype($node, RakuAST::Var::Lexical) {
            if $node.desigilname.is-indirect-lookup
                || nqp::existskey(POISON-VARS, $node.name) {
                self.IMPL-POISON-ALL();
            }
        }
        elsif nqp::istype($node, RakuAST::Call::Method) {
            # A method named EVAL reaches enclosing lexicals the same way
            # the sub form does, for example a string or AST's .EVAL.
            my $name := $node.name;
            if nqp::istype($name, RakuAST::Name)
                && !$name.is-indirect-lookup
                && nqp::existskey(POISON-CALLS, $name.canonicalize) {
                self.IMPL-POISON-ALL();
            }
        }
        elsif nqp::istype($node, RakuAST::Var::Package)
            || nqp::istype($node, RakuAST::Term::Name)
            || nqp::istype($node, RakuAST::Type::Simple) {
            my $name := $node.name;
            if nqp::istype($name, RakuAST::Name)
                && ($name.is-pseudo-package || $name.is-indirect-lookup) {
                self.IMPL-POISON-ALL();
            }
        }
        elsif nqp::istype($node, RakuAST::QuotedRegex)
            || nqp::istype($node, RakuAST::RegexDeclaration)
            || nqp::istype($node, RakuAST::Substitution)
            || nqp::istype($node, RakuAST::Transliteration) {
            # Regex code reaches lexicals of its enclosing frames late,
            # through the cursor, not through resolved lookups this walk
            # can see. Substitutions and transliterations carry regex
            # and replacement code the same way.
            self.IMPL-POISON-ALL();
        }
        Nil
    }

    # This code runs where the NQP setting's IO subs are not reachable,
    # so stderr output is spelled out in ops, including the byte buffer
    # type to encode into.
    method IMPL-NOTE(str $message) {
        my $u8 := nqp::newtype(nqp::knowhow(), 'P6int');
        nqp::composetype($u8, nqp::hash('integer', nqp::hash('unsigned', 1, 'bits', 8)));
        my $buf-type := nqp::newtype(nqp::knowhow(), 'VMArray');
        nqp::composetype($buf-type, nqp::hash('array', nqp::hash('type', $u8)));
        nqp::writefh(nqp::getstderr(),
            nqp::encode($message ~ "\n", 'utf8', nqp::create($buf-type)));
        Nil
    }

    method IMPL-POISON-ALL() {
        my int $i := nqp::elems($!frames);
        while --$i >= 0 {
            nqp::atpos($!frames, $i).poison();
        }
        Nil
    }

    method IMPL-POISON-CURRENT-SCOPE() {
        my int $i := nqp::elems($!frames);
        while --$i >= 0 {
            my $frame := nqp::atpos($!frames, $i);
            if $frame.is-scope {
                $frame.poison();
                last;
            }
        }
        Nil
    }

    method IMPL-DECIDE(Mu $frame) {
        my $candidates := $frame.candidates;
        return Nil unless nqp::elems($candidates);
        my int $poisoned := $frame.is-poisoned;
        for $candidates -> $record {
            my $decl := nqp::atkey($record, 'decl');
            my str $declined := nqp::atkey($record, 'declined');
            if $declined eq '' && $poisoned {
                $declined := 'poisoned';
            }
            if $declined eq '' && nqp::existskey($record, 'captured') {
                $declined := 'captured';
            }
            nqp::bindkey($record, 'final', $declined);
            if $declined eq '' {
                $decl.IMPL-SET-LOWERED-TO-LOCAL($!sentinel);
            }
            if $!debug {
                my str $where := $frame.node.HOW.name($frame.node);
                self.IMPL-NOTE($declined eq ''
                    ?? "lex2local: lower '" ~ $decl.lexical-name ~ "' in " ~ $where
                    !! "lex2local: keep '" ~ $decl.lexical-name ~ "' in "
                        ~ $where ~ " (" ~ $declined ~ ")");
            }
        }
        Nil
    }
}
