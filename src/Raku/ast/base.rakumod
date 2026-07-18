# The base of all RakuAST nodes.
class RakuAST::Node {
    has RakuAST::Origin $.origin;

    # What type does evaluating this node produce, if known?
    method return-type() { Mu }

    # Is evaluating this pure (that is, if its evaluation is elided due to
    # not being used, then the program will behave the same)?
    method pure() { False }

    # Visits all child nodes of this one, applying the selected block.
    # This is a non-recursive operation.
    method visit-children($visitor) {
        # Default is that we have no children to visit.
        Nil
    }

    # Recursively applies sinking up until a sink boundary.
    method apply-sink(Bool $is-sunk) {
        # If we are sunk and this is a sinkable node, apply that.
        if $is-sunk && nqp::istype(self, RakuAST::Sinkable) {
            self.mark-sunk();
        }

        # If this node knows how to propagate sinks itself, ask it to do so.
        if nqp::istype(self, RakuAST::SinkPropagator) {
            self.propagate-sink($is-sunk);
        }

        # Otherwise, we assume it's a wanted child, and just walk its children,
        # unless it is a sink boundary.
        elsif !nqp::istype(self, RakuAST::SinkBoundary) {
            self.visit-children: -> $child {
                $child.apply-sink(False);
            }
        }
    }

    # Checks if this node needs the sink method calling on it in the event
    # that it appears in a sink context.
    method needs-sink-call() { True }

    # Returns True if the expression is something that can be bound to,
    # and False otherwise.
    method can-be-bound-to() { False }

    # Builds the exception thrown when this cannot be bound to, but someone
    # tries to do so anyway.
    method build-bind-exception(RakuAST::Resolver $resolver) {
        $resolver.build-exception: 'X::Bind'
    }

    method set-origin(RakuAST::Origin $origin) {
        nqp::bindattr(self, RakuAST::Node, '$!origin', $origin);
    }

    # Find the narrowest key origin node for an original position
    method locate-node(int $pos, int $to?, :$key) {
        return Nil unless nqp::isconcrete($!origin)
                            && $pos >= $!origin.from && $pos < $!origin.to
                            && (!nqp::isconcrete($to) || $to <= $!origin.to);

        if $key && !$!origin.is-key {
            nqp::die("Only a key node can search for key nodes")
        }
        if $key {
            my @nestings := $!origin.nestings;
            for @nestings {
                my $cand := $_.locate-node($pos, $to, :key);
                return $cand if nqp::isconcrete($cand);
            }
        }
        else {
            self.visit-children(-> $child {
                my $cand := $child.locate-node($pos, $to);
                return $cand if nqp::isconcrete($cand);
            });
        }
        # If no nested key node gave a match then we are the one.
        self
    }

    # Bring the node up to parse time. Returns the node itself.
    method to-parse-time(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        if nqp::istype(self, RakuAST::ImplicitLookups) {
            self.implicit-lookups-to-begin-time($resolver, $context);
        }
        if nqp::istype(self, RakuAST::ParseTime) {
            self.ensure-parse-performed($resolver, $context);
        }
        self
    }

    # Bring the node up to begin time. Returns the node itself.
    method to-begin-time(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        if nqp::istype(self, RakuAST::ImplicitLookups) {
            self.implicit-lookups-to-begin-time($resolver, $context);
        }
        if nqp::istype(self, RakuAST::ParseTime) {
            self.ensure-parse-performed($resolver, $context);
        }
        # Apply implicit block semantics.
        if nqp::istype(self, RakuAST::ImplicitBlockSemanticsProvider) {
            self.apply-implicit-block-semantics(:$resolver, :$context);
        }
        if nqp::istype(self, RakuAST::BeginTime) {
            self.ensure-begin-performed($resolver, $context);
        }
        self
    }

    # Drive parse-time and BEGIN-time actitivites on this node and its children. In the context of
    # the compiler, this is done while parsing takes place. For a synthetic AST, however, it needs
    # to be performed.
    method IMPL-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        # Ensure implicit lookups are driven to their begin state ahead of the node's parse
        # time (in that sense, they are a bit like implicit children of the node).
        if nqp::istype(self, RakuAST::ImplicitLookups) {
            self.implicit-lookups-to-begin-time($resolver, $context);
        }
        # Apply implicit block semantics.
        if nqp::istype(self, RakuAST::ImplicitBlockSemanticsProvider) {
            self.apply-implicit-block-semantics(:$resolver, :$context);
        }

        # Ensure parse time was performed already before visiting children, when it is a
        # lexical scope that we are entering.
        my int $is-scope := nqp::istype(self, RakuAST::LexicalScope);
        my int $is-parse-time := nqp::istype(self, RakuAST::ParseTime);
        if $is-scope && $is-parse-time {
            self.ensure-parse-performed($resolver, $context);
            $is-parse-time := 0;
        }

        # Visit children.
        my int $is-package := nqp::istype(self, RakuAST::Package);
        $resolver.push-scope(self) if $is-scope;
        $resolver.push-package(self) if $is-package;
        self.visit-children(-> $child { $child.IMPL-BEGIN($resolver, $context) });
        $resolver.pop-scope() if $is-scope;
        $resolver.pop-package() if $is-package;

        # Perform parse time and BEGIN time as needed.
        my int $is-begin-time := nqp::istype(self, RakuAST::BeginTime);
        if $is-parse-time {
            self.ensure-parse-performed($resolver, $context);
        }
        if $is-begin-time {
            self.ensure-begin-performed($resolver, $context);
        }

        Nil
    }

    # Drive CHECK-time activities on this node and its children. Assumes that BEGIN time and
    # parse time has already completely happened.
    method IMPL-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        if nqp::istype(self, RakuAST::SinkBoundary) && !self.sink-calculated {
            self.calculate-sink();
        }

        # Visit children and do their CHECK time.
        my int $is-scope := nqp::istype(self, RakuAST::LexicalScope);
        my int $is-package := nqp::istype(self, RakuAST::Package);
        $resolver.push-scope(self) if $is-scope;
        $resolver.push-package(self) if $is-package;
        self.visit-children(-> $child { $child.IMPL-CHECK($resolver, $context) });
        $resolver.pop-scope() if $is-scope;
        $resolver.pop-package() if $is-package;

        if nqp::istype(self, RakuAST::CheckTime) {
            self.PERFORM-CHECK($resolver, $context);
            if self.has-check-time-problems {
                if $resolver.find-scope-property(-> $scope { $scope.fatal }) {
                    self.promote-worries-to-sorries;
                }
                my $worries := $resolver.find-scope-property(-> $scope { $scope.tell-worries });
                if nqp::isconcrete($worries) && !$worries {
                    self.clear-worries;
                }
                $resolver.add-node-with-check-time-problems(self) if self.has-check-time-problems;
            }
        }
        if nqp::istype(self, RakuAST::Lookup) && !self.is-resolved && self.needs-resolution {
            $resolver.add-node-unresolved-after-check-time(self);
        }

        Nil
    }

    method IMPL-CALCULATE-SINK() {
        if nqp::istype(self, RakuAST::SinkBoundary) && !self.sink-calculated {
            self.calculate-sink();
        }

        self.visit-children(-> $child { $child.IMPL-CALCULATE-SINK() });

        Nil
    }

    # Drives the optimize phase, an AST-to-AST pass that runs after check (which
    # only analyses) and before QAST generation. The walk is post-order: a
    # node's children are optimized before the node itself. Each child the node
    # visits is then offered to IMPL-OPTIMIZE-EXPRESSION and a differing result
    # replaces the child in place, so every node that exposes a child through
    # visit-children gets the expression-level optimizations without any code
    # of its own. A node may be replaced by its parent this way, never by
    # itself. The resolver tracks scopes and packages the same way the check
    # walk does, so a rewrite may resolve names in the scope of the node it is
    # looking at. The replacement pass for a node's children runs in the scope
    # the node lives in, not the scope it defines.
    method IMPL-OPTIMIZE(RakuAST::Resolver $resolver) {
        my int $is-scope := nqp::istype(self, RakuAST::LexicalScope);
        my int $is-package := nqp::istype(self, RakuAST::Package);
        $resolver.push-scope(self) if $is-scope;
        $resolver.push-package(self) if $is-package;
        self.visit-children(-> $child { $child.IMPL-OPTIMIZE($resolver) });
        $resolver.pop-scope() if $is-scope;
        $resolver.pop-package() if $is-package;

        my @children;
        self.visit-children(-> $child { nqp::push(@children, $child) });
        for @children -> $child {
            my $result := self.IMPL-OPTIMIZE-EXPRESSION($resolver, $child);
            self.IMPL-REPLACE-CHILD($child, $result) unless $result =:= $child;
        }
        Nil
    }

    # Replace a directly held child node with another node, locating the slot
    # that holds it by identity: any object attribute bound to the child, and
    # any element of any list attribute. All occurrences are replaced. A child
    # that is visited but not stored on the node itself (for example one a
    # visit reaches through a computed value) has no slot to find, and the
    # replacement is then quietly skipped, which only costs the rewrite.
    method IMPL-REPLACE-CHILD(Mu $old, Mu $new) {
        # The scan introspects every node class the walk meets, so a surprise
        # from an unusual meta-object must not break compilation. Skipping the
        # replacement only costs the rewrite.
        CATCH {
            return Nil;
        }
        for self.IMPL-UNWRAP-LIST(self.HOW.mro(self)) -> $class {
            for self.IMPL-UNWRAP-LIST($class.HOW.attributes($class, :local)) -> $attr {
                next if nqp::objprimspec($attr.type);
                my $value := nqp::getattr(self, $class, $attr.name);
                if nqp::eqaddr($value, $old) {
                    nqp::bindattr(self, $class, $attr.name, $new);
                }
                elsif nqp::islist($value) {
                    my int $i := 0;
                    my int $n := nqp::elems($value);
                    while $i < $n {
                        nqp::bindpos($value, $i, $new)
                            if nqp::eqaddr(nqp::atpos($value, $i), $old);
                        $i++;
                    }
                }
            }
        }
        Nil
    }

    method IMPL-QAST-NESTED-BLOCK-DECLS(RakuAST::IMPL::QASTContext $context) {
        my $stmts := QAST::Stmts.new;
        my @code-todo := [self];
        while @code-todo {
            my $visit := @code-todo.shift;
            $visit.visit-children: -> $node {
                if nqp::istype($node, RakuAST::Code) {
                    if nqp::istype($visit, RakuAST::IMPL::ImmediateBlockUser) &&
                            $visit.IMPL-IMMEDIATELY-USES($node) {
                    }
                    elsif nqp::istype($node, RakuAST::Block) && $node.IMPL-FLATTEN-APPROVED {
                        # A flattened loop body's statements are emitted
                        # inline by its loop; a block declaration for it
                        # would re-emit them referencing locals of the
                        # frame it flattened into.
                    }
                    else {
                        my $code := $node.IMPL-QAST-DECL-CODE($context);
                        $stmts.push($code);
                    }
                }
                if nqp::istype($node, RakuAST::Expression) {
                    $node.IMPL-QAST-ADD-THUNK-DECL-CODE($context, $stmts);
                }

                if nqp::istype($node, RakuAST::LexicalScope) {
                    # A code object emits declarations for the blocks among
                    # its children itself, including those in its traits.
                    # Emitting a trait's block here as well would nest the
                    # same block twice, and this frame's prologue would then
                    # capture it against the wrong frame. A package produces
                    # no frame of its own for a trait's block to nest in, so
                    # its traits' blocks do belong here.
                    if nqp::istype($node, RakuAST::TraitTarget) && !nqp::istype($node, RakuAST::Code) {
                        $node.visit-traits(-> $trait { @code-todo.push($trait) });
                    }
                }
                elsif nqp::istype($node, RakuAST::MayCreateBlock) && $node.creates-block {
                }
                else {
                    @code-todo.push($node);
                }
            }
        }
        $stmts
    }

    # Recursively walks the tree finding nodes of the specified type that are
    # beneath this one. A node that matches the stopper type will be returned
    # if it satisfies the specified type, but its children shall not be
    # visited. The search is strict - that is to say, it starts at the children
    # of the current node, but doesn't consider the current one.
    method find-nodes(
        Mu  $type,       # type to select on
      Code :$condition,  # condition to perform (if concrete)
        Mu :$stopper     # type/code to prevent going deeper
    ) {

        # Variables that need visibility from collectors
        my @visit-queue := [self];
        my @result;

        # Different types of collectors
        my sub collector-no-stopper-condition($node) {
            nqp::push(@result, $node)
              if nqp::istype($node, $type)
              && $condition($node);
            nqp::push(@visit-queue, $node);
        }
        my sub collector-no-stopper-no-condition($node) {
            nqp::push(@result, $node)
              if nqp::istype($node, $type);
            nqp::push(@visit-queue, $node);
        }
        my sub collector-run-stopper-condition($node) {
            unless $stopper($node) {
                nqp::push(@result, $node)
                  if nqp::istype($node, $type)
                  && $condition($node);
                nqp::push(@visit-queue, $node);
            }
        }
        my sub collector-run-stopper-no-condition($node) {
            unless $stopper($node) {
                nqp::push(@result, $node)
                  if nqp::istype($node, $type);
                nqp::push(@visit-queue, $node);
            }
        }
        my sub collector-is-stopper-condition($node) {
            nqp::push(@result, $node)
              if nqp::istype($node, $type)
              && $condition($node);
            nqp::push(@visit-queue, $node)
              unless nqp::istype($node, $stopper);
        }
        my sub collector-is-stopper-no-condition($node) {
            nqp::push(@result, $node)
              if nqp::istype($node, $type);
            nqp::push(@visit-queue, $node)
              unless nqp::istype($node, $stopper);
        }

        # Set up the collector
        my $collector := nqp::eqaddr($stopper,Mu)
          ?? $condition
            ?? &collector-no-stopper-condition
            !! &collector-no-stopper-no-condition
          !! nqp::isconcrete($stopper)
            ?? $condition
              ?? &collector-run-stopper-condition
              !! &collector-run-stopper-no-condition
            !! $condition
              ?? &collector-is-stopper-condition
              !! &collector-is-stopper-no-condition;

        # Walk the tree, also handling any elements added on the way
        while @visit-queue {
            nqp::shift(@visit-queue).visit-children($collector);
        }

        # Return the result in HLL format
        self.IMPL-WRAP-LIST(@result)
    }

    # Visit the AST starting at the current node. Call the callback for each
    # AST node. If the callback returns a true value, then its children will
    # also be walked. The strict option, if set, will not visit the current
    # node.
    method visit(Code $callback, Bool :$strict) {
        my @visit-queue;
        if $strict || $callback(self) {
            @visit-queue[0] := self;
        }
        my $visitor := -> $node {
            if $callback($node) {
                nqp::push(@visit-queue, $node);
            }
        }
        while @visit-queue {
            nqp::shift(@visit-queue).visit-children($visitor);
        }
        Nil
    }

    method visit-dfs(Code $callback, Bool :$strict) {
        my $visitor;
        $visitor := -> $node {
            if $callback($node) {
                $node.visit-children($visitor);
            }
        }
        self.visit-children($visitor) if $strict || $callback(self);
        Nil
    }

    method IMPL-CAN-INTERPRET() { False }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        nqp::die('Missing IMPL-INTERPRET implementation on ' ~ self.HOW.name(self))
    }

    method IMPL-WRAP-LIST(Mu $vm-array) {
        if nqp::istype($vm-array, List) {
            # It already is a list
            $vm-array
        }
        else {
            my $result := nqp::create(List);
            nqp::bindattr($result, List, '$!reified', $vm-array);
            $result
        }
    }

    method IMPL-UNWRAP-LIST(Mu $list) {
        if nqp::islist($list) {
            # Wasn't wrapped anyway
            $list
        }
        elsif nqp::istype($list, List) {
            my $todo := nqp::getattr($list, List, '$!todo');
            if nqp::isconcrete($todo) {
                $todo.reify-all;
                nqp::getattr($list, List, '$!reified')
            }
            else {
                nqp::isconcrete(nqp::getattr($list, List, '$!reified'))
                    ?? nqp::getattr($list, List, '$!reified')
                    !! nqp::bindattr($list, List, '$!reified', nqp::create(IterationBuffer));
            }
        }
        else {
            nqp::list($list)
        }
    }

    method IMPL-WRAP-MAP(Mu $vm-hash) {
        if nqp::istype($vm-hash, Map) {
            # It already is a map
            $vm-hash
        }
        else {
            my $result := nqp::create(Map);
            nqp::bindattr($result, Map, '$!storage', $vm-hash);
            $result
        }
    }

    method IMPL-UNWRAP-MAP(Mu $map) {
        if nqp::ishash($map) {
            # Wasn't wrapped anyway
            $map
        }
        elsif nqp::istype($map, Map) {
            my $storage := nqp::getattr($map, Map, '$!storage');
            nqp::isconcrete($storage)
                ?? $storage
                !! $map.FLATTENABLE_HASH
        }
        else {
            nqp::die("Cannot hashify " ~ $map.HOW.name($map));
        }
    }

    method dump-markers() {
        my @markers;
        @markers.push('⚓') if nqp::istype(self, RakuAST::Sinkable) && self.sunk;
        @markers.push('▪') if nqp::istype(self, RakuAST::BlockStatementSensitive) && self.is-block-statement;
        if nqp::isconcrete($!origin) {
            @markers.push('𝄞') if $!origin.is-key();
        }
        nqp::join('', @markers)
    }

    # Dump any extra information about the node if there is any and when it doesn't fit into the primary line. Extras
    # are placed below the line and are expected to respect the indentation level provided with $indent and be
    # terminated with a new line. See RakuAST::Expression.dump-extras() as a reference implementation.
    method dump-extras(int $indent) { '' }

    method dump-children(int $indent) {
        my @chunks;
        self.visit-children(-> $child {
            @chunks.push($child.dump($indent));
        });
        nqp::join('', @chunks)
    }

    method dump-origin() {
        my @chunks;
        if nqp::isconcrete($!origin) {
            my $from := $!origin.from;
            my $orig-source := $!origin.source;
            if $!origin.is-key {
                my @location := $orig-source.location-of-pos($from);
                @chunks.push(@location[2] ~ ':' ~ @location[0]);
            }

            my $src := nqp::escape(nqp::substr($orig-source.orig, $from, $!origin.to - $from));
            if nqp::chars($src) > 50 {
                $src := nqp::substr($src, 0, 49) ~ '…';
            }
            @chunks.push(' ⎡');
            @chunks.push($src ~ '⎤');
        }
        nqp::join('', @chunks)
    }

    method dump(int $indent?) {
        my @chunks := [
            nqp::x(' ', $indent),
            nqp::substr(self.HOW.name(self), nqp::chars('RakuAST::'))
        ];

        if (my $markers := self.dump-markers()) {
            @chunks.push(' ' ~ $markers);
        }

        if (my $origin := self.dump-origin()) {
            @chunks.push(' ' ~ $origin);
        }

        @chunks.push("\n");
        if (my $extras := self.dump-extras($indent + 2)) {
            @chunks.push($extras);
        }
        if (my $children := self.dump-children($indent + 2)) {
            @chunks.push($children);
        }
        nqp::join('', @chunks)
    }

    method mixin-role($base, $role) {
        my $class := nqp::clone($base);
        $class.HOW.mixin($class, $role).BUILD_LEAST_DERIVED({})
    }

    # Hook into the Raku RakuAST::Deparse class (by default) or any other
    # class that has been put into the hllsym hash for 'Raku'
    method DEPARSE(*@roles) {
        my $class := my $core := nqp::gethllsym('Raku','DEPARSE');
        for @roles {
            if nqp::istype($_, Str) {
                $class := self.mixin-role($class, $core.slang($_));
            }
            elsif nqp::can($_.HOW,'pun') {  # it's a role
                $class := self.mixin-role($class, $_);
            }
            else {
                $class := $_;
            }
        }

        $class.deparse(self)
    }

    method IMPL-SORTED-KEYS(Mu $hash) {
        # Due to these classes being pieced together at compile time we can't
        # reach the sorted_hash sub in the NQP setting, so it's copied here.
        my @keys;
        for $hash {
            nqp::push(@keys, $_.key);
        }

        my int $count := +@keys;
        my int $start := $count / 2 - 1;
        while $start >= 0 {
            self.IMPL-SIFT-DOWN(@keys, $start, $count - 1);
            $start := $start - 1;
        }

        my int $end := +@keys - 1;
        while $end > 0 {
            my str $swap := @keys[$end];
            @keys[$end] := @keys[0];
            @keys[0] := $swap;
            $end := $end - 1;
            self.IMPL-SIFT-DOWN(@keys, 0, $end);
        }

        return @keys;
    }

    method IMPL-SIFT-DOWN(Mu $a, int $start, int $end) {
        my @a := $a;
        my int $root := $start;

        while 2*$root + 1 <= $end {
            my $child := 2*$root + 1;
            my $swap := $root;

            if @a[$swap] gt @a[$child] {
                $swap := $child;
            }
            if $child + 1 <= $end && @a[$swap] ge @a[$child + 1] {
                $swap := $child + 1;
            }
            if $swap == $root {
                return;
            } else {
                my str $tmp := @a[$root];
                @a[$root] := @a[$swap];
                @a[$swap] := $tmp;
                $root := $swap;
            }
        }
    }

    method IMPL-TEMPORARIZE-TOPIC(Mu $new-topic-qast, Mu $with-topic-qast) {
        my $temporary := QAST::Node.unique('save_topic');
        QAST::Stmt.new(
            :resultchild(2),
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name($temporary), :scope('local'), :decl('var') ),
                QAST::Var.new( :name('$_'), :scope('lexical') )
            ),
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name('$_'), :scope('lexical') ),
                $new-topic-qast
            ),
            $with-topic-qast,
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name('$_'), :scope('lexical') ),
                QAST::Var.new( :name($temporary), :scope('local') )
            )
        )
    }

    # Set QAST .node() from the origin. With :key named argument the narrowest parent key node would be used instead
    # of node's own .origin.
    # Origin information is not critical to the overall compilation process. Therefore no exceptions are thrown and any
    # absence of information is treated as irrelevant. The only possible case when this dies is when key node is not
    # found. But this is only possible as a side effect of a worse error somewhere else.
    method IMPL-SET-NODE(Mu $qast, :$key) {
        my $orig := self.origin;
        if nqp::isconcrete($orig) {
            if $key && !$orig.is-key {
                my $comp-unit := $*CU;
                if nqp::isconcrete($comp-unit) {
                    my $key-node := $comp-unit.locate-node($orig.from, $orig.to, :key);
                    $orig := $key-node.origin if nqp::isconcrete($key-node);
                }
            }
            $qast.node($orig.as-match);
        }
        $qast
    }

    # If has-compile-time-value is True, the node must also have a maybe-compile-time-value method.
    method has-compile-time-value() {
        False
    }

    # Optimize a child expression, returning a node to use in its place (the
    # same node if nothing applies). The optimize walk offers every visited
    # child to this method, and this is where the expression-level
    # optimizations are registered. Each is tried in turn and declines by
    # returning its input unchanged. The walk is post-order, so a child's own
    # subexpressions are already optimized when this runs.
    # A rewrite added here must honour three rules so it stays correct in every
    # context the phase runs in, including CORE setting compilation. It declines,
    # returning its input, whenever it is not certain the change preserves the
    # value. If it evaluates anything at compile time it resolves a setting type
    # with IMPL-OPTIMIZE-SETTING-TYPE first and declines when that is null, since
    # during early bootstrap the machinery it relies on is not ready. And it
    # removes a subtree only when IMPL-DROPPABLE is true of it, so a
    # declaration's lexical effect is never lost.
    method IMPL-OPTIMIZE-EXPRESSION(RakuAST::Resolver $resolver, Mu $expr) {
        return $expr unless nqp::isconcrete($expr);

        my $result := self.IMPL-COLLAPSE-TERNARY($resolver, $expr);

        if $result =:= $expr {
            $result := self.IMPL-COLLAPSE-SHORT-CIRCUIT($resolver, $expr);
        }

        if $result =:= $expr {
            $result := self.IMPL-FOLD-CONSTANT($resolver, $expr);
        }

        if $result =:= $expr {
            $result := self.IMPL-COLLAPSE-TYPEMATCH($resolver, $expr);
        }

        if $result =:= $expr {
            $result := self.IMPL-REWRITE-SQUARE($resolver, $expr);
        }

        if $result =:= $expr {
            $result := self.IMPL-UNROLL-SLICE($resolver, $expr);
        }

        # Lowerings that direct code generation rather than replacing the
        # node register their marks here, gated on the optimize pass running.
        # They each drop a layer of operator dispatch or pin down a routine
        # lookup, so the `soft` pragma, which keeps routines wrappable, turns
        # them off.
        if $result =:= $expr && !self.IMPL-IN-SOFT-SCOPE($resolver) {
            self.IMPL-MARK-NATIVE-INCDEC($resolver, $expr);
            self.IMPL-MARK-NATIVE-METAOP($resolver, $expr);
            self.IMPL-MARK-SCALAR-METAOP($resolver, $expr);
            self.IMPL-MARK-DOT-ASSIGN($resolver, $expr);
            self.IMPL-MARK-RANGE-FOR($resolver, $expr);
            self.IMPL-MARK-STATIC-CALL($resolver, $expr);
            self.IMPL-MARK-STATIC-CHAIN($resolver, $expr);
            self.IMPL-MARK-RETURN-DECONT($resolver, $expr);
            self.IMPL-MARK-ARRAY-INIT($resolver, $expr);
            self.IMPL-MARK-CT-DISPATCH($resolver, $expr);
            self.IMPL-MARK-NATIVE-CONDITION($resolver, $expr);
            self.IMPL-MARK-WHEN-TYPEMATCH($resolver, $expr);
        }

        # A replacement stands where the original stood, so it must carry the
        # original's sunk state for any sink-sensitive code generation.
        if !($result =:= $expr)
          && nqp::istype($expr, RakuAST::Sinkable) && $expr.sunk
          && nqp::istype($result, RakuAST::Sinkable) && !$result.sunk {
            $result.mark-sunk();
        }
        $result
    }

    # Mark a native int or num increment or decrement on a simple lexical for
    # lowering to a raw op at code generation. Both the postfix (`$i++`) and
    # prefix (`++$i`) forms qualify. Doing it here, in the optimize pass, gates
    # it on optimization being on. Only the CORE operator is lowered; a
    # user-redefined one must still run.
    method IMPL-MARK-NATIVE-INCDEC(RakuAST::Resolver $resolver, Mu $expr) {
        my int $is-postfix := nqp::istype($expr, RakuAST::ApplyPostfix);
        my $op-node;
        if $is-postfix {
            $op-node := $expr.postfix;
            return Nil unless nqp::istype($op-node, RakuAST::Postfix);
        }
        elsif nqp::istype($expr, RakuAST::ApplyPrefix) {
            $op-node := $expr.prefix;
            return Nil unless nqp::istype($op-node, RakuAST::Prefix);
        }
        else {
            return Nil;
        }
        my str $op := $op-node.operator;
        return Nil unless $op eq '++' || $op eq '--';
        my $operand := $expr.operand;
        return Nil unless nqp::istype($operand, RakuAST::Var::Lexical) && $operand.is-resolved;
        my int $spec := nqp::objprimspec($operand.return-type);
        return Nil unless $spec == 1 || $spec == 2;
        # A post-increment recovers the original by reversing the step on the
        # assigned value, which round-trips only at the full native width. A
        # narrower type (int8, int16, num32) truncates on assignment, so the
        # reverse would not give the original back; leave those to the routine.
        # A prefix yields the stepped value directly, so it needs no such guard.
        return Nil if $is-postfix && nqp::objprimbits($operand.return-type) != 64;
        $expr.IMPL-SET-NATIVE-INCDEC($spec) if self.IMPL-OPERATOR-IS-CORE($resolver, $op-node);
        Nil
    }

    # Mark a native int or num add, subtract, or multiply compound assignment
    # on a simple lexical with a native operand for lowering to a raw op. Gated
    # in the optimize pass like the increment case. Only the CORE operator is
    # lowered.
    method IMPL-MARK-NATIVE-METAOP(RakuAST::Resolver $resolver, Mu $expr) {
        return Nil unless nqp::istype($expr, RakuAST::ApplyInfix);
        my $infix := $expr.infix;
        return Nil unless nqp::istype($infix, RakuAST::MetaInfix::Assign);
        my $base := $infix.infix;
        return Nil unless nqp::istype($base, RakuAST::Infix);
        my str $op := $base.operator;
        return Nil unless $op eq '+' || $op eq '-' || $op eq '*';
        my $left := $expr.left;
        return Nil unless nqp::istype($left, RakuAST::Var::Lexical) && $left.is-resolved;
        my int $spec := nqp::objprimspec($left.return-type);
        return Nil unless $spec == 1 || $spec == 2;
        # The right operand must be a native value: a native variable of the
        # same flavour, or a float literal. An integer literal is an `Int`, so
        # `$i += 1` is an int + Int step that overflows to a bignum the native
        # cannot hold and throws, like `my int $r = $i + 1`; leave it to the
        # metaop. A float literal never overflows that way.
        my $right := $expr.right;
        my int $rhs-ok := 0;
        if nqp::istype($right, RakuAST::Var::Lexical) && $right.is-resolved
          && nqp::objprimspec($right.return-type) == $spec {
            $rhs-ok := 1;
        }
        elsif $spec == 2 && nqp::istype($right, RakuAST::NumLiteral) {
            $rhs-ok := 1;
        }
        return Nil unless $rhs-ok;
        $infix.IMPL-SET-NATIVE-STEP($spec) if self.IMPL-OPERATOR-IS-CORE($resolver, $base);
        Nil
    }

    # Mark a compound assignment on a boxed scalar lexical for inlining to an
    # assignment of the operator's result, dropping the metaop dispatch. The
    # left may also be another compound assignment, so chains inline in full;
    # code generation binds the left to a temporary, so it is evaluated once.
    method IMPL-MARK-SCALAR-METAOP(RakuAST::Resolver $resolver, Mu $expr) {
        return Nil unless nqp::istype($expr, RakuAST::ApplyInfix);
        my $infix := $expr.infix;
        return Nil unless nqp::istype($infix, RakuAST::MetaInfix::Assign);
        return Nil if $infix.IMPL-WRAPS-LIST-META;
        my $base := $infix.infix;
        return Nil unless nqp::istype($base, RakuAST::Infix);
        # `orelse`, `andthen`, and `notandthen` compile to a call with a thunked
        # right, so the inline, which evaluates the right eagerly, cannot keep
        # them lazy. Leave them to the metaop.
        my str $op := $base.operator;
        return Nil if $op eq 'orelse' || $op eq 'andthen' || $op eq 'notandthen';
        # `^^` and `xor` compile to the `xor` QAST op, which yields a VMNull
        # when neither operand is the result. The metaop calls the routine,
        # which returns a Nil there, so leave them to it as well.
        return Nil if $op eq '^^' || $op eq 'xor';
        return Nil unless self.IMPL-SCALAR-METAOP-LHS-OK($expr.left);
        $infix.IMPL-SET-INLINE if self.IMPL-OPERATOR-IS-CORE($resolver, $base);
        Nil
    }

    # True when the left of a compound assignment is a boxed scalar the inline
    # may assign through: a plain scalar lexical, or another compound assignment
    # whose result is itself such a scalar. Grouping parentheses are seen
    # through, so a parenthesized chain qualifies.
    method IMPL-SCALAR-METAOP-LHS-OK(Mu $lhs) {
        my $node := $lhs;
        while nqp::istype($node, RakuAST::Circumfix::Parentheses)
          && $node.semilist.IMPL-IS-SINGLE-EXPRESSION {
            my $stmt := self.IMPL-UNWRAP-LIST($node.semilist.code-statements)[0];
            return False if $stmt.condition-modifier || $stmt.loop-modifier;
            $node := $stmt.expression;
        }
        (nqp::istype($node, RakuAST::Var::Lexical) && $node.is-resolved
          && nqp::eqat($node.name, '$', 0) && nqp::objprimspec($node.return-type) == 0)
        || (nqp::istype($node, RakuAST::ApplyInfix)
          && nqp::istype($node.infix, RakuAST::MetaInfix::Assign)
          && self.IMPL-SCALAR-METAOP-LHS-OK($node.left))
    }

    # Mark a dot-assignment for inlining the dispatcher away. It is always a
    # method call whose result is stored back, with no operator routine to
    # shadow, so unlike the numeric marks no core-operator guard applies.
    # Two forms reach it. An explicit target is an ApplyDottyInfix whose infix is
    # the call-assign operator. A bare call on the topic is a Term::TopicCall
    # whose call carries the `.=` dispatcher; only a plain Call::Method honors
    # the inline at code generation, so a quoted or private method is left out.
    method IMPL-MARK-DOT-ASSIGN(RakuAST::Resolver $resolver, Mu $expr) {
        if nqp::istype($expr, RakuAST::ApplyDottyInfix) {
            my $infix := $expr.infix;
            $infix.IMPL-SET-INLINE if nqp::istype($infix, RakuAST::DottyInfix::CallAssign);
        }
        elsif nqp::istype($expr, RakuAST::Term::TopicCall) {
            my $call := $expr.call;
            if nqp::istype($call, RakuAST::Call::Method) {
                my $dispatcher := $call.dispatcher;
                $call.IMPL-SET-INLINE if $dispatcher && $dispatcher eq 'dispatch:<.=>';
            }
        }
        Nil
    }

    # Mark a for loop whose source is an integer range built by a CORE range
    # constructor (`..` and its exclusive variants, prefix `^`, or `reverse`
    # of one) for lowering to a native counting loop at code generation. Both
    # the statement form and the statement modifier form qualify. Only the
    # source shape and the operator's origin are decided here; code generation
    # checks the loop itself (sunk, serial, simple body) and the bounds, and
    # falls back to the ordinary compilation when those disqualify.
    method IMPL-MARK-RANGE-FOR(RakuAST::Resolver $resolver, Mu $expr) {
        my $for;
        if nqp::istype($expr, RakuAST::Statement::For) {
            $for := $expr;
        }
        elsif nqp::istype($expr, RakuAST::Statement::Expression)
            && nqp::isconcrete($expr.loop-modifier)
            && nqp::istype($expr.loop-modifier, RakuAST::StatementModifier::For) {
            $for := $expr.loop-modifier;
        }
        else {
            return Nil;
        }
        my $source := nqp::istype($for, RakuAST::Statement::For)
            ?? $for.source
            !! $for.expression;
        my $operator := $for.IMPL-RANGE-FOR-OPERATOR($source);
        $for.IMPL-SET-CAN-LOWER-RANGE()
            if nqp::isconcrete($operator)
            && self.IMPL-OPERATOR-IS-CORE($resolver, $operator);
        Nil
    }

    # Mark a call to a named sub whose callee lexical is bound once for a
    # static callee lookup at code generation.
    method IMPL-MARK-STATIC-CALL(RakuAST::Resolver $resolver, Mu $expr) {
        return Nil unless nqp::istype($expr, RakuAST::Call::Name)
            && $expr.name.is-identifier
            && $expr.is-resolved;
        $expr.IMPL-SET-CALLSTATIC()
            if self.IMPL-RESOLUTION-BOUND-ONCE($resolver, $expr.resolution,
                '&' ~ $expr.name.canonicalize);
        Nil
    }

    # Mark a chaining comparison whose operator's lexical is bound once for a
    # static callee lookup at code generation.
    method IMPL-MARK-STATIC-CHAIN(RakuAST::Resolver $resolver, Mu $expr) {
        return Nil unless nqp::istype($expr, RakuAST::ApplyInfix);
        my $infix := $expr.infix;
        return Nil unless nqp::istype($infix, RakuAST::Infix)
            && $infix.is-resolved
            && $infix.properties.chain;
        $infix.IMPL-SET-CHAINSTATIC()
            if self.IMPL-RESOLUTION-BOUND-ONCE($resolver, $infix.resolution,
                '&infix' ~ $resolver.IMPL-CANONICALIZE-PAIR($infix.operator));
        Nil
    }

    # Allow a routine to skip its return decontainerization when code
    # generation can see the body's result is container-free. The shape
    # check happens at code generation, where the body QAST exists; the mark
    # only carries that the optimize pass ran.
    method IMPL-MARK-RETURN-DECONT(RakuAST::Resolver $resolver, Mu $expr) {
        $expr.IMPL-SET-ELIDE-RETURN-DECONT()
            if nqp::istype($expr, RakuAST::Routine);
        Nil
    }

    # Mark the initialization of a plain array from a comma list, both the
    # declaration form and the assignment form, for lowering to a direct
    # build of the list internals at code generation, skipping the STORE
    # dispatch and the intermediate list the comma call builds.
    method IMPL-MARK-ARRAY-INIT(RakuAST::Resolver $resolver, Mu $expr) {
        if nqp::istype($expr, RakuAST::ApplyInfix) {
            # The assignment form emits a runtime type guard with a STORE
            # fallback, and both branches compile the operands, so an operand
            # declaring a lexical must keep the plain STORE.
            my $infix := $expr.infix;
            $infix.IMPL-SET-LOWERED-ARRAY-INIT()
                if nqp::istype($infix, RakuAST::Infix)
                && $infix.operator eq '='
                && nqp::istype($expr.left, RakuAST::Var::Lexical)
                && $expr.left.is-resolved
                && self.IMPL-PLAIN-ARRAY-DECL($expr.left.resolution)
                && self.IMPL-CORE-COMMA-LIST($resolver, $expr.right)
                && self.IMPL-DROPPABLE($expr.right);
        }
        elsif nqp::istype($expr, RakuAST::VarDeclaration::Simple) {
            $expr.IMPL-SET-LOWERED-ARRAY-INIT()
                if self.IMPL-PLAIN-ARRAY-DECL($expr)
                && nqp::istype($expr.initializer, RakuAST::Initializer::Assign)
                && self.IMPL-CORE-COMMA-LIST($resolver, $expr.initializer.expression);
        }
        Nil
    }

    # A plain lexical array declaration: no type, shape, where, twigil, or
    # traits, so its container is the stock Array whose STORE the lowering
    # replicates. A bind initializer disqualifies it, since the bound value
    # replaces that container with anything, like a native array.
    method IMPL-PLAIN-ARRAY-DECL(Mu $decl) {
        nqp::istype($decl, RakuAST::VarDeclaration::Simple)
          && $decl.sigil eq '@'
          && $decl.twigil eq ''
          && $decl.scope eq 'my'
          && !nqp::isconcrete($decl.type)
          && !nqp::isconcrete($decl.shape)
          && !nqp::isconcrete($decl.where)
          && !nqp::istype($decl.initializer, RakuAST::Initializer::Bind)
          && nqp::elems(self.IMPL-UNWRAP-LIST($decl.traits)) == 0
            ?? 1 !! 0
    }

    # An application of the core comma operator with plain operands.
    method IMPL-CORE-COMMA-LIST(RakuAST::Resolver $resolver, Mu $expr) {
        nqp::istype($expr, RakuAST::ApplyListInfix)
          && nqp::istype($expr.infix, RakuAST::Infix)
          && $expr.infix.operator eq ','
          && nqp::elems(nqp::getattr($expr, RakuAST::ApplyListInfix, '$!adverbs')) == 0
          && nqp::elems(self.IMPL-UNWRAP-LIST($expr.operands)) > 0
          && self.IMPL-OPERATOR-IS-CORE($resolver, $expr.infix)
            ?? 1 !! 0
    }

    # Whether a QAST subtree tolerates being compiled twice in one frame.
    # The guarded array initialization places the same operand nodes in
    # both branches, and compiling a declaration a second time collides
    # ("Local ... already declared" for the locals a with modifier or an
    # inlined metaop declares), while a block registers its frame per
    # compilation. Such operands keep the plain STORE call.
    method IMPL-QAST-SAFE-TO-RECOMPILE(Mu $qast) {
        return 0 if nqp::istype($qast, QAST::Block);
        return 0 if nqp::istype($qast, QAST::Var) && $qast.decl;
        if nqp::istype($qast, QAST::Node) {
            for @($qast) {
                return 0 unless self.IMPL-QAST-SAFE-TO-RECOMPILE($_);
            }
        }
        1
    }

    # The lowered initialization of a plain array from a comma list: bind a
    # fresh reification buffer and a reifier whose future is the elements,
    # then reify, the same layout the STORE method builds through dispatch.
    # Null when the QAST is not the expected variable and core comma call
    # shape, or when an operand cannot compile in both guard branches, and
    # the caller then keeps the STORE call. A fallback, when given, guards
    # the build behind a runtime check that the variable holds the stock
    # Array: the declared container can be replaced by a bind, so the
    # assignment form cannot rely on the declaration's shape.
    method IMPL-ARRAY-INIT-QAST(Mu $var-qast, Mu $comma-qast, Mu $fallback?) {
        return nqp::null() if $*COMPILING_CORE_SETTING;
        return nqp::null() unless nqp::istype($var-qast, QAST::Var)
            && nqp::istype($comma-qast, QAST::Op)
            && ($comma-qast.op eq 'call' || $comma-qast.op eq 'callstatic')
            && $comma-qast.name eq '&infix:<,>';
        for @($comma-qast) {
            return nqp::null() if $_.named;
            return nqp::null()
                if nqp::isconcrete($fallback)
                && !self.IMPL-QAST-SAFE-TO-RECOMPILE($_);
        }

        my $Reifier := nqp::atkey(nqp::who(List), 'Reifier');
        my $future := QAST::Op.new( :op<list> );
        $future.set_children(@($comma-qast));
        my $init := QAST::Stmts.new(
            QAST::Op.new(
                :op<callmethod>, :name('reify-until-lazy'),
                QAST::Op.new(
                    :op<getattr>,
                    QAST::Op.new(
                        :op<p6bindattrinvres>,
                        QAST::Op.new(
                            :op<p6bindattrinvres>,
                            $var-qast,
                            QAST::WVal.new(:value(List)),
                            QAST::SVal.new(:value('$!reified')),
                            QAST::Op.new(:op<create>,
                                QAST::WVal.new(:value(IterationBuffer))),
                        ),
                        QAST::WVal.new(:value(List)),
                        QAST::SVal.new(:value('$!todo')),
                        QAST::Op.new(
                            :op<p6bindattrinvres>,
                            QAST::Op.new(
                                :op<p6bindattrinvres>,
                                QAST::Op.new(
                                    :op<p6bindattrinvres>,
                                    QAST::Op.new(:op<create>,
                                        QAST::WVal.new(:value($Reifier))),
                                    QAST::WVal.new(:value($Reifier)),
                                    QAST::SVal.new(:value('$!reified')),
                                    QAST::Op.new(
                                        :op<getattr>,
                                        $var-qast,
                                        QAST::WVal.new(:value(List)),
                                        QAST::SVal.new(:value('$!reified')),
                                    )
                                ),
                                QAST::WVal.new(:value($Reifier)),
                                QAST::SVal.new(:value('$!reification-target')),
                                QAST::Op.new(
                                    :op<callmethod>,
                                    :name('reification-target'),
                                    $var-qast,
                                )
                            ),
                            QAST::WVal.new(:value($Reifier)),
                            QAST::SVal.new(:value('$!future')),
                            $future,
                        ),
                    ),
                    QAST::WVal.new(:value(List)),
                    QAST::SVal.new(:value('$!todo')),
                ),
            ),
            $var-qast
        );
        $init.nosink(1);
        if nqp::isconcrete($fallback) {
            $init := QAST::Op.new(
                :op<if>,
                QAST::Op.new(
                    :op<eqaddr>,
                    QAST::Op.new( :op<what_nd>, $var-qast ),
                    QAST::WVal.new(:value(Array)),
                ),
                $init,
                $fallback,
            );
            $init.nosink(1);
        }
        $init
    }

    # Mark a conditional or loop statement so code generation may test a
    # native-int condition directly: the boolification over a native-int
    # computation is dropped, and a bare native-int variable is compared
    # against zero. Registering the mark here gates the rewrite on the
    # optimize pass running. A with or when part tests something other
    # than truth, so code generation only rewrites plain if parts.
    method IMPL-MARK-NATIVE-CONDITION(RakuAST::Resolver $resolver, Mu $expr) {
        if nqp::istype($expr, RakuAST::Statement::Loop)
            || nqp::istype($expr, RakuAST::Statement::IfWith)
            || nqp::istype($expr, RakuAST::Statement::Unless) {
            $expr.IMPL-SET-NATIVE-CONDITION();
        }
        elsif nqp::istype($expr, RakuAST::Statement::Expression) {
            my $loop := $expr.loop-modifier;
            $loop.IMPL-SET-NATIVE-CONDITION()
                if nqp::isconcrete($loop)
                && nqp::istype($loop, RakuAST::StatementModifier::WhileUntil);
            my $cond := $expr.condition-modifier;
            $cond.IMPL-SET-NATIVE-CONDITION()
                if nqp::isconcrete($cond)
                && (nqp::istype($cond, RakuAST::StatementModifier::If)
                    || nqp::istype($cond, RakuAST::StatementModifier::Unless));
        }
        Nil
    }

    # A condition rewritten to branch on the native int it computes. A
    # boolified native-int computation loses the boolification, and with
    # it a return-type check the boolification satisfied statically. A
    # bare native-int variable reference is read as a value and compared
    # against zero, which is what its boolification comes down to.
    method IMPL-NATIVE-CONDITION-QAST(Mu $cond) {
        my $stripped := self.IMPL-STRIP-BOOL-CONDITION($cond);
        return $stripped unless nqp::isnull($stripped);
        if nqp::istype($cond, QAST::Var)
            && nqp::objprimspec($cond.returns) == 1 {
            my str $scope := $cond.scope;
            if $scope eq 'lexicalref' {
                $cond.scope('lexical');
            }
            elsif $scope eq 'attributeref' {
                $cond.scope('attribute');
            }
            else {
                return $cond unless $scope eq 'lexical' || $scope eq 'local';
            }
            return QAST::Op.new( :op('isne_i'), $cond, QAST::IVal.new( :value(0) ) );
        }
        $cond
    }

    # Descend a condition's value path through statement wrappers, and
    # through a return-type check, to a boolification of a provably
    # native-int child, and return the condition with the boolification
    # dropped, or null when the shape is anything else. Only a child the
    # compiler already treats as a native int qualifies: the
    # boolification coerces any other operand itself, and dropping it
    # would change what the branch tests.
    method IMPL-STRIP-BOOL-CONDITION(Mu $node) {
        if nqp::istype($node, QAST::Op) {
            my str $op := $node.op;
            if $op eq 'hllbool' {
                my $child := nqp::atpos($node.list, 0);
                return $child
                    if nqp::istype($child, QAST::IVal)
                    || nqp::istype($child, QAST::Op) && nqp::eqat($child.op, '_i', -2);
            }
            elsif $op eq 'p6typecheckrv' {
                return self.IMPL-STRIP-BOOL-CONDITION(nqp::atpos($node.list, 0));
            }
        }
        elsif nqp::istype($node, QAST::Stmts) || nqp::istype($node, QAST::Stmt) {
            my int $n := nqp::elems($node.list);
            if $n {
                my $rc := $node.resultchild;
                my int $idx := nqp::defined($rc) ?? $rc !! $n - 1;
                my $inner := self.IMPL-STRIP-BOOL-CONDITION(nqp::atpos($node.list, $idx));
                unless nqp::isnull($inner) {
                    nqp::bindpos($node.list, $idx, $inner);
                    return $node;
                }
            }
        }
        nqp::null()
    }

    # Mark a call whose argument types decide the dispatch at compile time.
    # A named sub call and an infix operator application both qualify when
    # the callee resolves to a compile-time routine whose lexical is bound
    # once, and every argument is a plain positional with a known nominal
    # type. For an onlystar multi, the proto must trial-bind and the
    # candidate analysis must land on exactly one candidate; for a plain
    # sub, its own signature must trial-bind. The chosen routine is recorded
    # on the node, and code generation splices its inline info, when it has
    # any, in place of the call. A chaining operator only qualifies standing
    # alone: a link inside a longer chain takes part in the chain op
    # protocol, which an inlined body no longer would.
    method IMPL-MARK-CT-DISPATCH(RakuAST::Resolver $resolver, Mu $expr) {
        return Nil if nqp::istrue(nqp::ifnull(nqp::getlexdyn('$*NO-CT-DISPATCH'), 0));
        my $target;
        my @args;
        my str $lexname;
        if nqp::istype($expr, RakuAST::Call::Name) {
            return Nil unless $expr.name.is-identifier
                && $expr.is-resolved
                && !$expr.feed-stage;
            return Nil if nqp::isconcrete($expr.args.invocant);
            my $arg-list := $expr.args;
            for self.IMPL-UNWRAP-LIST($arg-list.args) {
                return Nil if nqp::istype($_, RakuAST::NamedArg)
                    || $arg-list.IMPL-IS-FLATTENING($_);
                nqp::push(@args, $_);
            }
            $target := $expr;
            $lexname := '&' ~ $expr.name.canonicalize;
        }
        elsif nqp::istype($expr, RakuAST::ApplyInfix) {
            my $infix := $expr.infix;
            return Nil unless nqp::istype($infix, RakuAST::Infix) && $infix.is-resolved;
            return Nil if nqp::elems($expr.colonpairs);
            my $left := $expr.left;
            my $right := $expr.right;
            if $infix.properties.chain {
                # An operand that is itself a chain link makes this a longer
                # chain. Its links take part in the chain op protocol, so
                # none of them may be inlined. The operands were visited,
                # and possibly marked, before this node was offered, so the
                # decision they took is withdrawn here.
                my int $linked := 0;
                if nqp::istype($left, RakuAST::ApplyInfix)
                    && nqp::istype($left.infix, RakuAST::Infix)
                    && $left.infix.properties.chain {
                    $left.infix.IMPL-CLEAR-CT-INLINE-CANDIDATE();
                    $linked := 1;
                }
                if nqp::istype($right, RakuAST::ApplyInfix)
                    && nqp::istype($right.infix, RakuAST::Infix)
                    && $right.infix.properties.chain {
                    $right.infix.IMPL-CLEAR-CT-INLINE-CANDIDATE();
                    $linked := 1;
                }
                return Nil if $linked;
            }
            nqp::push(@args, $left);
            nqp::push(@args, $right);
            $target := $infix;
            $lexname := '&infix' ~ $resolver.IMPL-CANONICALIZE-PAIR($infix.operator);
        }
        else {
            return Nil;
        }

        my $resolution := $target.resolution;
        return Nil unless self.IMPL-RESOLUTION-BOUND-ONCE($resolver, $resolution, $lexname);
        my $routine := nqp::istype($resolution, RakuAST::CompileTimeValue)
            ?? $resolution.compile-time-value
            !! $resolution.maybe-compile-time-value;
        return Nil unless nqp::isconcrete($routine)
            && nqp::istype($routine, Code)
            && nqp::can($routine, 'signature');

        my @info := self.IMPL-CT-ARG-TYPES($resolver, @args);
        return Nil unless nqp::elems(@info);
        my @types := @info[0];
        my @flags := @info[1];

        my $chosen;
        if nqp::can($routine, 'is_dispatcher') && $routine.is_dispatcher {
            return Nil unless nqp::can($routine, 'onlystar') && $routine.onlystar;
            my int $proto-ok := 0;
            my @multi-result;
            try {
                $proto-ok := nqp::p6trialbind($routine.signature, @types, @flags);
                @multi-result := $routine.analyze_dispatch(@types, @flags);
            }
            return Nil unless $proto-ok == 1
                && nqp::elems(@multi-result)
                && nqp::atpos(@multi-result, 0) == 1;
            $chosen := nqp::atpos(@multi-result, 1);
        }
        else {
            my int $ct-result := 0;
            try $ct-result := nqp::p6trialbind($routine.signature, @types, @flags);
            return Nil unless $ct-result == 1;
            $chosen := $routine;
        }
        return Nil if nqp::can($chosen, 'soft') && $chosen.soft;
        $target.IMPL-SET-CT-INLINE-CANDIDATE($chosen);
        Nil
    }

    # The nominal types and native flags of the given arguments, as trial
    # binding and candidate analysis expect them, or an empty list when any
    # argument's type is not known well enough for the answer to be final.
    # A native type is passed as its boxed counterpart carrying the native
    # flag. A literal of a boxed type prefers a native candidate when it is
    # alone or paired with a native argument of matching kind, as the same
    # allomorphic argument would at run time.
    method IMPL-CT-ARG-TYPES(RakuAST::Resolver $resolver, Mu @args) {
        my int $ARG_IS_LITERAL := 32;
        my @types;
        my @flags;
        my @allo;
        my int $num-prim := 0;
        my int $num-allo := 0;
        for @args {
            my $type := $_.return-type;
            # A parameter read reports no type of its own; the declared type
            # of the parameter's variable is the read's type. Kept local to
            # this analysis so the trial-bind diagnostic's coverage does not
            # change underneath existing code.
            if $type =:= Mu
                && nqp::istype($_, RakuAST::Var::Lexical)
                && $_.is-resolved
                && nqp::istype($_.resolution, RakuAST::ParameterTarget::Var)
                && nqp::isconcrete($_.resolution.declaration) {
                try $type := $_.resolution.declaration.return-type;
            }
            return [] if $type =:= Mu;
            my int $ok := 0;
            try $ok := $type.HOW.archetypes.nominal && !$type.HOW.archetypes.generic;
            return [] unless $ok;
            return [] if nqp::istype($type.HOW, Perl6::Metamodel::SubsetHOW);
            my int $ps := nqp::objprimspec($type);
            if $ps == 1 || $ps == 2 || $ps == 3 {
                $type := self.IMPL-OPTIMIZE-SETTING-TYPE($resolver,
                    $ps == 1 ?? 'Int' !! $ps == 2 ?? 'Num' !! 'Str');
                return [] if nqp::isnull($type);
                $num-prim++;
            }
            elsif $ps {
                return [];
            }
            nqp::push(@types, $type);
            nqp::push(@flags, $ps);
            my int $allo-flag := 0;
            if nqp::istype($_, RakuAST::Literal) {
                my $native := $_.native-type-flag;
                # An integer too wide for the native representation is not
                # allomorphic: only its boxed form holds the value.
                if nqp::defined($native)
                    && !($native == 1
                        && nqp::isbig_I(nqp::decont($_.compile-time-value))) {
                    $allo-flag := $native;
                    $num-allo++;
                }
            }
            nqp::push(@allo, $allo-flag);
        }
        if nqp::elems(@types) == 2 && $num-prim == 1 && $num-allo == 1 {
            my int $prim := nqp::atpos(@flags, 0) || nqp::atpos(@flags, 1);
            my int $allo-idx := nqp::atpos(@allo, 0) ?? 0 !! 1;
            nqp::bindpos(@flags, $allo-idx, $prim +| $ARG_IS_LITERAL)
                if nqp::atpos(@allo, $allo-idx) == $prim;
        }
        elsif nqp::elems(@types) == 1 && $num-allo == 1 {
            nqp::bindpos(@flags, 0, nqp::atpos(@allo, 0) +| $ARG_IS_LITERAL);
        }
        [@types, @flags]
    }

    # Whether a resolution's lexical is bound once, so the VM may resolve a
    # lookup of $name a single time and treat the result as a constant. Two
    # kinds of binding qualify. A setting routine: the setting binds each
    # routine name once and user code cannot rebind it. And a
    # compile-time-valued binding in the outermost scope of the compilation
    # unit (a sub declaration or an import), since that scope's frame is
    # entered once per load and a sub declaration cannot be rebound. A
    # declaration without a compile-time value, like `my &foo`, does not
    # qualify: its binding is free to be rebound at runtime. Nor does a
    # routine declared in a nested scope, since its enclosing frame is
    # entered many times and each entry may bind a fresh clone. Nor does a
    # callee compiled under the soft pragma, so it stays wrappable.
    method IMPL-RESOLUTION-BOUND-ONCE(RakuAST::Resolver $resolver, Mu $decl, str $name) {
        return 1 if nqp::istype($decl, RakuAST::Declaration::External::Setting);

        return 0 unless nqp::istype($decl, RakuAST::CompileTimeValue)
            || nqp::can($decl, 'maybe-compile-time-value');
        my $routine := nqp::istype($decl, RakuAST::CompileTimeValue)
            ?? $decl.compile-time-value
            !! $decl.maybe-compile-time-value;
        return 0 unless nqp::isconcrete($routine)
            && nqp::istype($routine, Code)
            && nqp::can($routine, 'soft') && !$routine.soft;

        # The nearest scope declaring the name must be the outermost one, and
        # the declaration found there must be the resolution itself, so a
        # shadowing declaration the resolution predates turns the mark off.
        my $nearest := nqp::null();
        my $outermost := nqp::null();
        $resolver.find-scope-property(-> $scope {
            $outermost := $scope;
            $nearest := $scope
                if nqp::isnull($nearest)
                && nqp::isconcrete($scope.find-lexical($name));
            Nil
        });
        !nqp::isnull($nearest)
            && nqp::eqaddr($nearest, $outermost)
            && nqp::eqaddr($outermost.find-lexical($name), $decl)
    }

    # A smartmatch against a compile-time-known type object reduces to a type
    # check. The setting's ACCEPTS for a type object is nqp::istype of the
    # topic against it, and istype itself runs a subset's refinement and
    # honors definite and coercion types, so the reduction preserves the
    # match. It is only valid when no user ACCEPTS candidate could dispatch
    # instead, which the setting's own IS-SETTING-ONLY answers, ignoring
    # candidates that need a defined invocant since the matcher here is a
    # type object. A concrete Junction topic autothreads over ACCEPTS, so
    # code generation guards the fast path with a runtime Junction test,
    # unless the matcher is Junction itself, which istype answers directly.
    # A topic that is itself a compile-time value decides the match here and
    # the whole expression becomes that constant; a subset is excluded from
    # that, since its refinement may depend on runtime state. Only a lone
    # match qualifies: a link of a longer comparison chain must stay a chain
    # op. The soft pragma turns the reduction off, since it bypasses the
    # operator and ACCEPTS routines that wrapping relies on.
    # The compile-time type object a matcher node reduces to a type check
    # against, or null when it is anything else: the matcher must carry a
    # compile-time type-object value, non-generic, whose ACCEPTS no user
    # candidate can intercept.
    method IMPL-TYPEMATCH-MATCHER-TYPE(Mu $matcher) {
        CATCH {
            return nqp::null();
        }
        return nqp::null() unless $matcher.has-compile-time-value;
        my $type := $matcher.maybe-compile-time-value;
        return nqp::null() if nqp::isconcrete($type);
        my $how := $type.HOW;
        return nqp::null() unless nqp::can($how, 'archetypes');
        return nqp::null() if $how.archetypes($type).generic;
        my $accepts := nqp::tryfindmethod($type, 'ACCEPTS');
        return nqp::null() unless nqp::isconcrete($accepts)
            && nqp::can($accepts, 'IS-SETTING-ONLY')
            && nqp::istrue($accepts.IS-SETTING-ONLY(
                nqp::const::SIG_ELEM_DEFINED_ONLY));
        $type
    }

    # Mark a when statement, or when statement modifier, whose matcher is
    # a compile-time type object for reduction to a type check on the
    # topic. A when only ever tests plain truth of its match, so the
    # check feeds the branch directly, with the same runtime guard a
    # reduced smartmatch takes for a topic that turns out to be a
    # concrete Junction.
    method IMPL-MARK-WHEN-TYPEMATCH(RakuAST::Resolver $resolver, Mu $expr) {
        CATCH {
            return Nil;
        }
        my $when;
        if nqp::istype($expr, RakuAST::Statement::When) {
            $when := $expr;
        }
        elsif nqp::istype($expr, RakuAST::Statement::Expression) {
            my $cond := $expr.condition-modifier;
            $when := $cond
                if nqp::isconcrete($cond)
                && nqp::istype($cond, RakuAST::StatementModifier::When);
        }
        return Nil unless nqp::isconcrete($when);
        my $matcher := nqp::istype($when, RakuAST::Statement::When)
            ?? $when.condition
            !! $when.expression;
        my $type := self.IMPL-TYPEMATCH-MATCHER-TYPE($matcher);
        return Nil if nqp::isnull($type);
        my $Junction := self.IMPL-OPTIMIZE-SETTING-TYPE($resolver, 'Junction');
        return Nil if nqp::isnull($Junction);
        $when.IMPL-SET-TYPEMATCH($type,
            nqp::istype($type, $Junction) ?? nqp::null() !! $Junction);
        Nil
    }

    # A when construct's reduced condition: the topic, bound to a local so
    # the guard and the check evaluate it once, tested with a plain istype
    # the branch reads directly. A topic that turns out to be a concrete
    # Junction autothreads over the matcher's ACCEPTS instead, as the full
    # match would.
    method IMPL-WHEN-TYPEMATCH-QAST(RakuAST::IMPL::QASTContext $context, Mu $topic-qast, Mu $type, Mu $junction) {
        $context.ensure-sc($type);
        my str $tmp := QAST::Node.unique('when_topic');
        my $topic := QAST::Var.new( :name($tmp), :scope<local> );
        my $check := QAST::Op.new( :op<istype>, $topic,
            QAST::WVal.new( :value($type) ) );
        unless nqp::isnull($junction) {
            $context.ensure-sc($junction);
            my $negate-value := nqp::hllboolfor(0, 'Raku');
            $context.ensure-sc($negate-value);
            $check := QAST::Op.new(
                :op<if>,
                QAST::Op.new(
                    :op<if>,
                    QAST::Op.new( :op<istype>, $topic,
                        QAST::WVal.new( :value($junction) ) ),
                    QAST::Op.new( :op<isconcrete>, $topic )),
                QAST::Op.new(
                    :op<callmethod>, :name<BOOLIFY-ACCEPTS>,
                    $topic,
                    QAST::WVal.new( :value($type) ),
                    QAST::WVal.new( :value($negate-value) )),
                $check);
        }
        QAST::Stmts.new(
            QAST::Op.new( :op<bind>,
                QAST::Var.new( :name($tmp), :scope<local>, :decl<var> ),
                $topic-qast),
            $check)
    }

    method IMPL-COLLAPSE-TYPEMATCH(RakuAST::Resolver $resolver, Mu $expr) {
        # The checks introspect meta-objects the walk has no say over, so a
        # surprise from an unusual one declines rather than breaks the build.
        CATCH {
            return $expr;
        }

        # A lone application of the smartmatch operator.
        return $expr unless nqp::istype($expr, RakuAST::ApplyInfix);
        my $infix := $expr.infix;
        return $expr unless nqp::istype($infix, RakuAST::Infix)
            && $infix.is-resolved;
        my str $op := $infix.operator;
        my int $negated := $op eq '!~~';
        return $expr unless $negated || $op eq '~~';
        my $left := $expr.left;
        return $expr if nqp::istype($left, RakuAST::ApplyInfix)
            && nqp::istype($left.infix, RakuAST::Infix)
            && $left.infix.properties.chain;
        return $expr if nqp::istype(self, RakuAST::ApplyInfix)
            && nqp::istype(self.infix, RakuAST::Infix)
            && self.infix.properties.chain
            && nqp::eqaddr(self.left, $expr);

        # A matcher that reduces to a compile-time type check.
        my $right := $expr.right;
        my $type := self.IMPL-TYPEMATCH-MATCHER-TYPE($right);
        return $expr if nqp::isnull($type);
        my $how := $type.HOW;
        my $archetypes := $how.archetypes($type);

        my $Junction := self.IMPL-OPTIMIZE-SETTING-TYPE($resolver, 'Junction');
        my $Bool     := self.IMPL-OPTIMIZE-SETTING-TYPE($resolver, 'Bool');
        return $expr if nqp::isnull($Junction) || nqp::isnull($Bool);

        return $expr unless self.IMPL-OPERATOR-IS-CORE($resolver, $infix);
        return $expr if self.IMPL-IN-SOFT-SCOPE($resolver);

        # A native topic would need boxing the plain call already provides.
        return $expr if nqp::objprimspec($left.return-type);

        # A topic known to be a concrete Junction keeps the full smartmatch.
        my int $left-known := $left.has-compile-time-value;
        my $left-value;
        if $left-known {
            $left-value := $left.maybe-compile-time-value;
            return $expr if nqp::isconcrete($left-value)
                && nqp::istype($left-value, $Junction);
        }

        my int $is-subset := $archetypes.nominalizable
            && nqp::can($how, 'wrappee-lookup')
            && !nqp::isnull($how.wrappee-lookup($type, :subset));

        # A known topic decides the match now, unless a subset's refinement
        # would need to run. The topic must be a foldable operand, the same
        # bound constant folding uses: a name lookup may claim a compile-time
        # value that differs from what evaluating it produces, so only a
        # literal-like topic is trusted.
        if $left-known && !$is-subset
            && self.IMPL-FOLDABLE-OPERAND($left)
            && nqp::can($left-value.HOW, 'archetypes')
            && !$left-value.HOW.archetypes($left-value).generic
            && self.IMPL-DROPPABLE($left) && self.IMPL-DROPPABLE($right) {
            my int $matches := nqp::istype($left-value, $type);
            $matches := nqp::not_i($matches) if $negated;
            return RakuAST::Literal.from-value(nqp::hllboolfor($matches, 'Raku'));
        }

        # Mark for code generation; the matcher being a Junction needs no
        # runtime guard.
        $infix.IMPL-SET-TYPEMATCH($type,
            nqp::istype($type, $Junction) ?? nqp::null() !! $Junction);
        $expr
    }

    # Squaring by the core power operator becomes a multiply of the operand
    # with itself: the power routine handles any exponent, bottoming out in a
    # bignum power or libm pow, where the multiply is a single operation.
    # Only a plain resolved variable qualifies, so no side effect is
    # duplicated, and only one whose type rules out a Junction: a junction
    # squares each eigenstate, but autothreads over both sides of a multiply,
    # which builds a different junction. A native can never hold one; a boxed
    # variable qualifies when its declared type and Junction are unrelated.
    # The multiply emitted must itself be the core one in the node's scope.
    # The soft pragma turns the rewrite off, since it bypasses the power
    # routine that wrapping relies on.
    method IMPL-REWRITE-SQUARE(RakuAST::Resolver $resolver, Mu $expr) {
        CATCH {
            return $expr;
        }

        return $expr unless nqp::istype($expr, RakuAST::ApplyInfix);
        my $infix := $expr.infix;
        return $expr unless nqp::istype($infix, RakuAST::Infix)
            && $infix.is-resolved
            && $infix.operator eq '**';

        # An exponent that is the literal integer 2.
        my $right := $expr.right;
        return $expr unless nqp::istype($right, RakuAST::IntLiteral);
        my $exp := $right.compile-time-value;
        return $expr if nqp::isbig_I($exp);
        return $expr unless nqp::iseq_i(nqp::unbox_i($exp), 2);

        # A plain resolved variable whose type rules out a Junction.
        my $left := $expr.left;
        return $expr unless nqp::istype($left, RakuAST::Var::Lexical)
            && $left.is-resolved;
        my $type := $left.return-type;
        unless nqp::objprimspec($type) {
            my $Junction := self.IMPL-OPTIMIZE-SETTING-TYPE($resolver, 'Junction');
            return $expr if nqp::isnull($Junction);
            return $expr if $type =:= Mu;
            return $expr unless nqp::can($type.HOW, 'archetypes')
                && !$type.HOW.archetypes($type).generic;
            return $expr if nqp::istype($type, $Junction)
                || nqp::istype($Junction, $type);
        }

        return $expr if self.IMPL-IN-SOFT-SCOPE($resolver);
        return $expr unless self.IMPL-OPERATOR-IS-CORE($resolver, $infix);
        my $mul := $resolver.resolve-lexical('&infix:<*>');
        return $expr unless nqp::isconcrete($mul)
            && nqp::istype($mul, RakuAST::Declaration::External::Setting);

        my $mul-op := RakuAST::Infix.new('*');
        $mul-op.set-resolution($mul);
        my $product := RakuAST::ApplyInfix.new(
            :left($left), :infix($mul-op), :right($left));
        $product.set-origin($expr.origin) if nqp::isconcrete($expr.origin);
        $product
    }

    # A slice of a plain variable by literal integer indexes becomes the
    # list of the AT-POS calls those indexes dispatch to, dropping the
    # postcircumfix call and the index list it builds. Only a value use
    # qualifies: as an assignment target the postcircumfix can extend the
    # array for an index past the end, where AT-POS cannot, so a slice that
    # is the left operand of any parent keeps the call. The comma and the
    # postcircumfix operator must be the core ones in the node's scope.
    method IMPL-UNROLL-SLICE(RakuAST::Resolver $resolver, Mu $expr) {
        CATCH {
            return $expr;
        }

        return $expr unless nqp::istype($expr, RakuAST::ApplyPostfix);
        my $postfix := $expr.postfix;
        return $expr unless nqp::istype($postfix, RakuAST::Postcircumfix::ArrayIndex);
        return $expr if nqp::isconcrete($postfix.assignee);
        return $expr if nqp::elems(self.IMPL-UNWRAP-LIST($postfix.colonpairs));
        return $expr if nqp::can(self, 'left')
            && nqp::eqaddr(self.left, $expr);
        my $operand := $expr.operand;
        return $expr unless nqp::istype($operand, RakuAST::Var::Lexical)
            && $operand.is-resolved;

        my $semilist := $postfix.index;
        return $expr unless nqp::istype($semilist, RakuAST::SemiList)
            && $semilist.IMPL-IS-SINGLE-EXPRESSION;
        my $statement := self.IMPL-UNWRAP-LIST($semilist.statements)[0];
        return $expr if nqp::isconcrete($statement.condition-modifier)
            || nqp::isconcrete($statement.loop-modifier);
        my $list := $statement.expression;
        return $expr unless nqp::istype($list, RakuAST::ApplyListInfix)
            && nqp::istype($list.infix, RakuAST::Infix)
            && $list.infix.operator eq ','
            && nqp::elems(nqp::getattr($list, RakuAST::ApplyListInfix, '$!adverbs')) == 0
            && self.IMPL-OPERATOR-IS-CORE($resolver, $list.infix);
        my @indexes := self.IMPL-UNWRAP-LIST($list.operands);
        for @indexes {
            return $expr unless nqp::istype($_, RakuAST::IntLiteral);
        }

        return $expr if self.IMPL-IN-SOFT-SCOPE($resolver);
        my $pc := $resolver.resolve-lexical('&postcircumfix:<[ ]>');
        return $expr unless nqp::isconcrete($pc)
            && nqp::istype($pc, RakuAST::Declaration::External::Setting);
        my $comma := $resolver.resolve-lexical('&infix:<,>');
        return $expr unless nqp::isconcrete($comma)
            && nqp::istype($comma, RakuAST::Declaration::External::Setting);

        my @calls;
        for @indexes {
            nqp::push(@calls, RakuAST::ApplyPostfix.new(
                :operand($operand),
                :postfix(RakuAST::Call::Method.new(
                    :name(RakuAST::Name.from-identifier('AT-POS')),
                    :args(RakuAST::ArgList.new($_))))));
        }
        my $comma-op := RakuAST::Infix.new(',');
        $comma-op.set-resolution($comma);
        my $unrolled := RakuAST::ApplyListInfix.new(
            :infix($comma-op), :operands(@calls));
        $unrolled.set-origin($expr.origin) if nqp::isconcrete($expr.origin);
        $unrolled
    }

    # Inline a dot-assignment to an assignment of the method call's result,
    # dropping the dispatcher. The call arrives as a `dispatch:<.=>` callmethod
    # whose first child is the target and whose second is the method name. The
    # method name is kept as that second child, so clearing the op name leaves a
    # plain method call whose result is stored back. A target that is not a plain
    # variable is bound to a temporary so it is evaluated once.
    method IMPL-INLINE-DOT-ASSIGN(Mu $call) {
        my $target := $call[0];
        $call.name('');
        if nqp::istype($target, QAST::Var) {
            QAST::Op.new(:op<p6store>, $target, $call)
        }
        else {
            $target := $call.shift;
            my str $name := QAST::Node.unique('dot_assign');
            $call.unshift(QAST::Var.new(:name($name), :scope<local>));
            QAST::Stmt.new(
                QAST::Op.new(:op<bind>,
                    QAST::Var.new(:name($name), :scope<local>, :decl<var>), $target),
                QAST::Op.new(:op<p6store>,
                    QAST::Var.new(:name($name), :scope<local>), $call))
        }
    }

    # True when the `soft` pragma is in effect in the enclosing scope. It keeps
    # routines wrappable, so the lowerings stand down.
    method IMPL-IN-SOFT-SCOPE(RakuAST::Resolver $resolver) {
        nqp::istrue($resolver.find-scope-property(-> $scope { $scope.soft }))
    }

    # True when the operator resolves to the CORE routine itself. An operator
    # bound to a lexical variable has no compile-time value and throws, which
    # declines the lowering. A user `multi` or `sub` that shadows or extends the
    # operator produces a distinct routine object whose file may still read
    # SETTING::, so the file alone is not enough. The name is resolved again in
    # the scope of the node being offered: a lexical declaration is visible to
    # that walk wherever it sits in its block, so a user operator declared after
    # a use of the name still turns the lowering off, where the resolution
    # stored on the node (made when the declaration had not been parsed yet)
    # would still claim the CORE routine. When the walk reaches no declaration
    # at all (the operator is being defined as CORE itself compiles), the file
    # vouches.
    method IMPL-OPERATOR-IS-CORE(RakuAST::Resolver $resolver, Mu $operator) {
        CATCH {
            return False;
        }
        my $routine := $operator.resolution.compile-time-value;
        return False
          unless nqp::can($routine, 'file') && $routine.file.starts-with('SETTING::');
        my str $category := nqp::istype($operator, RakuAST::Postfix) ?? '&postfix'
                         !! nqp::istype($operator, RakuAST::Prefix)  ?? '&prefix'
                         !! '&infix';
        my $current := $resolver.resolve-lexical(
          $category ~ $resolver.IMPL-CANONICALIZE-PAIR($operator.operator));
        nqp::isconcrete($current)
          ?? nqp::eqaddr($routine, nqp::decont($current.compile-time-value))
          !! True
    }

    # A ternary with a constant condition becomes the branch the condition
    # selects. The branches are expressions, so the value is preserved, and the
    # unselected branch is one the running program would not have evaluated. The
    # condition is removed as well, so it too must be droppable.
    method IMPL-COLLAPSE-TERNARY(RakuAST::Resolver $resolver, Mu $expr) {
        return $expr unless nqp::istype($expr, RakuAST::Ternary);
        my int $truth := self.IMPL-CONSTANT-TRUTH($resolver, $expr.condition);
        return $expr if $truth < 0;
        my $keep := $truth ?? $expr.then !! $expr.else;
        my $drop := $truth ?? $expr.else !! $expr.then;
        self.IMPL-DROPPABLE($expr.condition) && self.IMPL-DROPPABLE($drop)
          ?? $keep !! $expr
    }

    # A boolean short-circuit (&& and ||, or their loose forms `and` and `or`)
    # with a constant left operand becomes the side the operator yields: an
    # `and` gives the right side when the left is true and the left otherwise,
    # an `or` the mirror. The dropped side is one the running program would not
    # have evaluated, so its code is removed too. The left's constant truth
    # stands in for the runtime test only when the left is droppable.
    method IMPL-COLLAPSE-SHORT-CIRCUIT(RakuAST::Resolver $resolver, Mu $expr) {
        return $expr unless nqp::istype($expr, RakuAST::ApplyInfix);
        my $infix := $expr.infix;
        return $expr
            unless nqp::istype($infix, RakuAST::Infix)
            && $infix.is-resolved && $infix.short-circuit;
        my str $op := $infix.operator;
        my int $is-and := $op eq '&&' || $op eq 'and';
        return $expr unless $is-and || $op eq '||' || $op eq 'or';

        my $left  := $expr.left;
        my $right := $expr.right;
        return $expr unless nqp::isconcrete($left) && nqp::isconcrete($right);

        my int $truth := self.IMPL-CONSTANT-TRUTH($resolver, $left);
        return $expr if $truth < 0;

        my $keep := $is-and
            ?? ($truth ?? $right !! $left)
            !! ($truth ?? $left  !! $right);
        my $drop := $keep =:= $left ?? $right !! $left;
        self.IMPL-DROPPABLE($left) && self.IMPL-DROPPABLE($drop)
          ?? $keep !! $expr
    }

    # Raku truth value of a node, or -1 when it cannot be determined safely.
    # Folding only ever evaluates pure operators on foldable operands, while
    # truthiness has to consider any constant, so this is deliberately narrow:
    # the value must be a concrete Cool or Bool, whose .Bool is pure and
    # well-defined. Type objects (not concrete) are declined, since a type used
    # here is not the instance the running program would test. Resolving the
    # guard types also declines during early bootstrap, before they are
    # available.
    method IMPL-CONSTANT-TRUTH(RakuAST::Resolver $resolver, Mu $expr) {
        return -1 unless $expr.has-compile-time-value;
        my $value := $expr.maybe-compile-time-value;
        return -1 unless nqp::isconcrete($value);

        my $Cool := self.IMPL-OPTIMIZE-SETTING-TYPE($resolver, 'Cool');
        my $Bool := self.IMPL-OPTIMIZE-SETTING-TYPE($resolver, 'Bool');
        return -1 if nqp::isnull($Cool) || nqp::isnull($Bool);
        return -1 unless nqp::istype($value, $Cool) || nqp::istype($value, $Bool);

        # A constant whose .Bool itself throws keeps that throw at runtime,
        # where the program put it, so the collapse declines.
        CATCH {
            return -1;
        }
        nqp::istrue($value.Bool) ?? 1 !! 0
    }

    # Whether an expression may serve as an operand for compile-time
    # evaluation: a literal, an enumeration value such as True, a quoted
    # string whose value is known at compile time, which is how a plain
    # string literal parses, or a reference to a constant declaration. A
    # stash reference (a name with a trailing ::) is left out: it claims
    # the resolved package as its value where evaluating it produces that
    # package's stash.
    method IMPL-FOLDABLE-OPERAND(Mu $operand) {
        return 0 unless nqp::isconcrete($operand);
        return 1 if nqp::istype($operand, RakuAST::Literal);
        if nqp::istype($operand, RakuAST::QuotedString)
            || nqp::istype($operand, RakuAST::Term::Enum) {
            return $operand.has-compile-time-value ?? 1 !! 0;
        }
        if nqp::istype($operand, RakuAST::Term::Name) {
            return $operand.is-resolved
                && self.IMPL-CONSTANT-RESOLUTION($operand.resolution)
                && !$operand.name.is-package-lookup ?? 1 !! 0;
        }
        if nqp::istype($operand, RakuAST::Var::Lexical) {
            return $operand.is-resolved
                && self.IMPL-CONSTANT-RESOLUTION($operand.resolution) ?? 1 !! 0;
        }
        0
    }

    # Whether a resolution is a genuinely constant declaration, whose
    # compile-time value is the value evaluating a reference to it produces.
    # The compile-time-value protocol alone cannot vouch for that: a plain
    # variable declaration claims a value too, giving its container default.
    # A containerized value is no constant either: an our-scoped variable
    # imports as its Scalar container, whose value a later assignment can
    # replace.
    method IMPL-CONSTANT-RESOLUTION(Mu $decl) {
        (nqp::istype($decl, RakuAST::VarDeclaration::Constant)
          || nqp::istype($decl, RakuAST::VarDeclaration::Implicit::EnumValue)
          || nqp::istype($decl, RakuAST::Declaration::External::Constant)
          || nqp::istype($decl, RakuAST::Declaration::ResolvedConstant))
          && !nqp::iscont($decl.compile-time-value)
            ?? 1 !! 0
    }

    # Whether a branch can be removed from the tree. The running program would
    # not evaluate a dropped branch, so its runtime code is safe to remove, but
    # a declaration in it has a lexical effect that outlives the branch and must
    # be kept, so a branch containing one is not dropped. The declaration test
    # comes first because a node can be both a declaration and a scope, the way
    # a named sub installs itself in the surrounding scope while its body is a
    # scope of its own. A node that is only a lexical scope confines anything
    # declared inside it, so there is no need to look further down.
    method IMPL-DROPPABLE(Mu $node) {
        return 1 unless nqp::isconcrete($node);
        return 0 if nqp::istype($node, RakuAST::Declaration);
        return 1 if nqp::istype($node, RakuAST::LexicalScope);
        my int $droppable := 1;
        $node.visit-children(-> $child {
            $droppable := 0 unless self.IMPL-DROPPABLE($child);
        });
        $droppable
    }

    # Constant folding. Given a child expression, if it is a pure operator
    # applied to constant operands, evaluate it now and return a
    # RakuAST::Literal holding the result. Otherwise the expression is
    # returned unchanged. Operands must satisfy IMPL-FOLDABLE-OPERAND, so a
    # value only known at runtime never feeds an evaluation here.
    # The optimize walk is post-order, so a nested operator that has already
    # folded is itself a literal here, and nested constant arithmetic folds
    # up. Evaluation is guarded: a throw keeps the original runtime behaviour,
    # one-shot or failure-like results are not folded, and folding is declined
    # before the guard types are available (early bootstrap).
    method IMPL-FOLD-CONSTANT(RakuAST::Resolver $resolver, Mu $expr) {
        return $expr unless nqp::isconcrete($expr);

        # Grouping parentheses around a single constant are transparent, so a
        # parenthesized literal folds into the expression that holds it.
        my $unparen := self.IMPL-FOLD-PARENS($expr);
        return $unparen unless $unparen =:= $expr;

        my int $foldable := 0;
        if nqp::istype($expr, RakuAST::ApplyInfix) {
            my $infix := $expr.infix;
            # A chaining comparison (a < b < c) means (a < b) && (b < c), so it
            # reuses the middle operand and is not the binary operation its
            # nesting suggests. Folding the inner comparison to a literal would
            # drop that operand and change the result, so chaining infixes are
            # left for runtime.
            $foldable := nqp::istype($infix, RakuAST::Infix)
                && $infix.is-resolved
                && self.IMPL-PURE-ROUTINE($infix)
                && !$infix.properties.chain
                && !nqp::isconcrete($expr.args.arg-at-pos(2))
                && self.IMPL-FOLDABLE-OPERAND($expr.left)
                && self.IMPL-FOLDABLE-OPERAND($expr.right);
        }
        elsif nqp::istype($expr, RakuAST::ApplyPrefix) {
            my $prefix := $expr.prefix;
            # An adverb on the operator (zpre 4 :x(5)) is a named argument the
            # interpreter does not pass on, so an operator carrying one is left
            # for runtime. The infix case is covered by the arg-at-pos check
            # above, which sees the adverb as a further argument.
            $foldable := nqp::istype($prefix, RakuAST::Prefix)
                && $prefix.is-resolved
                && self.IMPL-PURE-ROUTINE($prefix)
                && nqp::elems($prefix.colonpairs) == 0
                && self.IMPL-FOLDABLE-OPERAND($expr.operand);
        }
        elsif nqp::istype($expr, RakuAST::ApplyListInfix) {
            my $infix := $expr.infix;
            # The comma is skipped without evaluating: it is marked pure, but
            # its list result would only be declined by the Iterable guard
            # below, after building a list for every literal list in the
            # program.
            if nqp::istype($infix, RakuAST::Infix)
              && $infix.operator ne ','
              && $infix.is-resolved
              && self.IMPL-PURE-ROUTINE($infix)
              && !$infix.properties.chain
              && nqp::elems(nqp::getattr($expr, RakuAST::ApplyListInfix, '$!adverbs')) == 0 {
                my @operands := self.IMPL-UNWRAP-LIST($expr.operands);
                if nqp::elems(@operands) {
                    $foldable := 1;
                    for @operands {
                        $foldable := 0 unless self.IMPL-FOLDABLE-OPERAND($_);
                    }
                }
            }
        }
        return $expr unless $foldable;

        # Evaluation and the value checks rely on setting types, so during
        # early bootstrap, before Failure resolves, folding declines.
        return $expr
            if nqp::isnull(self.IMPL-OPTIMIZE-SETTING-TYPE($resolver, 'Failure'));

        my @result := self.IMPL-CONSTANT-FOLD-EVALUATE($expr);
        return $expr unless @result[0];
        my $value := @result[1];
        return $expr unless self.IMPL-FOLDABLE-VALUE($resolver, $value);

        my $literal := RakuAST::Literal.from-value($value);
        $literal.set-origin($expr.origin) if nqp::isconcrete($expr.origin);
        $literal
    }

    # Interpret the expression at compile time, returning a (success, value)
    # pair. A throw means folding is declined and the original runtime behaviour
    # is kept. The handler is a block here rather than nqp::handle, which does
    # not lower cleanly in this source.
    method IMPL-CONSTANT-FOLD-EVALUATE(Mu $expr) {
        CATCH {
            return nqp::list(0, nqp::null);
        }
        nqp::list(1, $expr.IMPL-INTERPRET(RakuAST::IMPL::InterpContext.new))
    }

    # Whether a resolved operator's routine carries the `is pure` trait, the
    # declaration that calling it with the same arguments always gives the same
    # result and has no side effects. The trait mixes in an is-pure method, so
    # its presence is the signal, the same one the legacy optimizer uses. Only
    # such a routine may be evaluated at compile time. The operator node's own
    # is-pure is a name table that claims purity for any unknown operator, which
    # is fine for the sink warnings it serves but must not license running a
    # user-defined routine during compilation.
    method IMPL-PURE-ROUTINE(Mu $operator) {
        # Asking a resolution without a compile-time value, the way an
        # operator bound to a lexical variable resolves, throws. Declining
        # keeps such operators at runtime.
        CATCH {
            return 0;
        }
        my $routine := $operator.resolution.compile-time-value;
        nqp::can($routine, 'is-pure') ?? 1 !! 0
    }

    # Whether an operator is pure. A pure operator has no effect, so the
    # sink-context warning flags discarding its result as useless. A resolved
    # simple operator answers from its routine's `is pure` trait, the basis
    # the legacy frontend uses, so a user-defined operator with side effects
    # is not flagged. A meta operator or one not yet resolved keeps the
    # operator node's own classification.
    method IMPL-SUNK-OPERATOR-PURE(Mu $operator) {
        (nqp::istype($operator, RakuAST::Infix)
          || nqp::istype($operator, RakuAST::Prefix))
          && $operator.is-resolved
            ?? self.IMPL-PURE-ROUTINE($operator)
            !! $operator.is-pure
    }

    # Whether a computed value is reasonable to embed in the compilation
    # unit. Each restriction carries its own explanation, and a new one
    # belongs here with the same treatment.
    method IMPL-FOLDABLE-VALUE(RakuAST::Resolver $resolver, Mu $value) {
        CATCH {
            return 0;
        }

        # A Failure stands for an error the runtime raises where the value
        # is used, so it stays a runtime result. It is marked handled before
        # declining, since an unused Failure warns when it is destroyed.
        my $Failure := self.IMPL-OPTIMIZE-SETTING-TYPE($resolver, 'Failure');
        if !nqp::isnull($Failure) && nqp::istype($value, $Failure) {
            $value.Bool;
            return 0;
        }

        # An Iterable result is potentially lazy or one-shot, so consuming
        # it at compile time would not preserve the program.
        my $Iterable := self.IMPL-OPTIMIZE-SETTING-TYPE($resolver, 'Iterable');
        return 0 if !nqp::isnull($Iterable) && nqp::istype($value, $Iterable);

        # An oversized string stays a runtime computation: the runtime can
        # represent an enormous repetition cheaply, while a folded constant
        # is serialized flattened with the unit. This serves the same
        # purpose as the QAST optimizer's refusal to fold the x operator on
        # a large count, but measures the result rather than that one
        # operator's arguments, so any route to an oversized string
        # declines. The probes stay cheap at any size: the grapheme count
        # does not flatten a repetition strand, and the codepoint count,
        # which catches a repetition of bare combiners that is few graphemes
        # but arbitrarily many codepoints, only runs on strings the grapheme
        # limit already passed. 1024 is the threshold the QAST optimizer's
        # own check uses, described there as just a heuristic rather than a
        # measured boundary, and a larger value may well be fine.
        if nqp::istype($value, Str) {
            my str $str := nqp::unbox_s($value);
            return 0 if nqp::chars($str) > 1024 || nqp::codes($str) > 1024;
        }

        1
    }

    # Resolve a setting type by name for use as an optimization guard, or return
    # null when it is not available. A null result is how a compile-time rewrite
    # detects early bootstrap, where the type it would test against does not yet
    # exist, and declines rather than acting on incomplete information.
    method IMPL-OPTIMIZE-SETTING-TYPE(RakuAST::Resolver $resolver, str $name) {
        my $decl := $resolver.resolve-lexical($name);
        nqp::isconcrete($decl) && nqp::can($decl, 'compile-time-value')
            ?? $decl.compile-time-value
            !! nqp::null
    }

    # If the expression is grouping parentheses around a single constant with
    # no statement modifier, return that constant. Otherwise return the
    # expression unchanged. A multi-element or comma list does not qualify, so
    # list parentheses are left alone. The optimize walk is post-order, so a
    # parenthesized constant expression has already folded by the time this
    # runs.
    method IMPL-FOLD-PARENS(Mu $expr) {
        return $expr unless nqp::istype($expr, RakuAST::Circumfix::Parentheses);
        # Some constructs build grouping parentheses around a bare expression
        # rather than a semilist, such as a regex assertion's argument, so the
        # payload's shape is checked rather than assumed.
        my $semilist := $expr.semilist;
        return $expr unless nqp::istype($semilist, RakuAST::SemiList)
            && $semilist.IMPL-IS-SINGLE-EXPRESSION;
        my $statement := self.IMPL-UNWRAP-LIST($semilist.statements)[0];
        return $expr
            if nqp::isconcrete($statement.condition-modifier)
            || nqp::isconcrete($statement.loop-modifier);
        my $inner := $statement.expression;
        self.IMPL-FOLDABLE-OPERAND($inner) ?? $inner !! $expr
    }
}

# Anything with a known compile time value does RakuAST::CompileTimeValue.
class RakuAST::CompileTimeValue
  is RakuAST::Node
{
    method compile-time-value() {
        nqp::die('compile-time-value not implemented for ' ~ self.HOW.name(self))
    }

    method has-compile-time-value() {
        True
    }

    method maybe-compile-time-value() {
        self.compile-time-value
    }
}

class RakuAST::MayCreateBlock {
    method creates-block {
        False
    }
}
