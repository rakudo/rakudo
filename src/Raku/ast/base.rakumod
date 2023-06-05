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

        # Ensure parse time was performed already before visiting children, when it is a
        # lexical scope that we are entering.
        my int $is-scope := nqp::istype(self, RakuAST::LexicalScope);
        my int $is-parse-time := nqp::istype(self, RakuAST::ParseTime);
        if $is-scope && $is-parse-time {
            self.ensure-parse-performed($resolver, $context);
            $is-parse-time := 0;
        }

        # TODO Move all resolve-with into parse time or begin time
        if nqp::istype(self, RakuAST::Lookup) && !self.is-resolved {
            self.resolve-with($resolver);
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
    method IMPL-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context, Bool $resolve-only) {
        # Apply implicit block semantics.
        if nqp::istype(self, RakuAST::ImplicitBlockSemanticsProvider) {
            self.apply-implicit-block-semantics();
        }

        # Visit children and do their CHECK time.
        my int $is-scope := nqp::istype(self, RakuAST::LexicalScope);
        my int $is-package := nqp::istype(self, RakuAST::Package);
        $resolver.push-scope(self) if $is-scope;
        $resolver.push-package(self) if $is-package;
        self.visit-children(-> $child { $child.IMPL-CHECK($resolver, $context, $resolve-only) });
        $resolver.pop-scope() if $is-scope;
        $resolver.pop-package() if $is-package;

        # Do resolution.
        # TODO eliminate in favor of it happening at explicit parse/begin/check times
        if nqp::istype(self, RakuAST::Lookup) && !self.is-resolved {
            self.resolve-with($resolver);
            if !$resolve-only && !self.is-resolved && self.needs-resolution {
                $resolver.add-node-unresolved-after-check-time(self);
            }
        }

        # Unless in resolve-only mode, do other check-time activities.
        # TODO eliminate resolve-only, since that's just check time.
        unless $resolve-only {
            if nqp::istype(self, RakuAST::SinkBoundary) && !self.sink-calculated {
                self.calculate-sink();
            }
            if nqp::istype(self, RakuAST::CheckTime) {
                self.clear-check-time-problems();
                self.PERFORM-CHECK($resolver, $context);
                if self.has-check-time-problems {
                    $resolver.add-node-with-check-time-problems(self);
                }
            }
        }

        Nil
    }

    # Recursively walks the tree finding nodes of the specified type that are
    # beneath this one. A node that matches the stopper type will be returned
    # if it satisfies the specified type, but its children shall not be
    # visited. The search is strict - that is to say, it starts at the children
    # of the current node, but doesn't consider the current one.
    method find-nodes(Mu $type, Code :$condition, Mu :$stopper) {
        # Walk the tree searching for matching nodes.
        my int $have-stopper := !nqp::eqaddr($stopper, Mu);
        my @visit-queue := [self];
        my @result;
        my $collector := sub collector($node) {
            if nqp::istype($node, $type) {
                unless $condition && !$condition($node) {
                    nqp::push(@result, $node);
                }
            }
            unless $have-stopper && nqp::istype($node, $stopper) {
                nqp::push(@visit-queue, $node);
            }
        }
        while @visit-queue {
            nqp::shift(@visit-queue).visit-children($collector);
        }
        self.IMPL-WRAP-LIST(@result)
    }

    # Recursively walks the tree finding nodes of the specified type that are
    # beneath this one. A node that matches the stopper type will *not* be returned
    # even if it satisfies the specified type and its children shall not be
    # visited. The search is strict - that is to say, it starts at the children
    # of the current node, but doesn't consider the current one.
    # Note: this is more expensive than find-nodes due to calling $stopper on
    #       each node, instead of doing a simple type check.
    method find-nodes-exclusive(Mu $type, Code :$condition, Code :$stopper) {
        # Walk the tree searching for matching nodes.;
        my @visit-queue := [self];
        my @result;
        my $collector := sub collector($node) {
            if nqp::isconcrete($stopper) {
                my $do-stop := $stopper($node);
                if nqp::istype($node, $type) && !$do-stop {
                    unless $condition && !$condition($node) {
                        nqp::push(@result, $node);
                    }
                }
                unless $do-stop {
                    nqp::push(@visit-queue, $node)
                }
            } else {
                if nqp::istype($node, $type) {
                    unless $condition && !$condition($node) {
                        nqp::push(@result, $node);
                    }
                }
                nqp::push(@visit-queue, $node);
            }
        }
        while @visit-queue {
            nqp::shift(@visit-queue).visit-children($collector);
        }
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
            my $reified := nqp::getattr($list, List, '$!reified');
            nqp::isconcrete($reified)
                ?? $reified
                !! $list.FLATTENABLE_LIST
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
            if $_.HOW.name($_) eq 'Str' {  # XXX better way to detect HLL Str?
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
}

# Anything with a known compile time value does RakuAST::CompileTimeValue.
class RakuAST::CompileTimeValue
  is RakuAST::Node
{
    method compile-time-value() {
        nqp::die('compile-time-value not implemented for ' ~ self.HOW.name(self))
    }
}
