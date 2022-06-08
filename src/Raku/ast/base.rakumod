# The base of all RakuAST nodes.
class RakuAST::Node {
    # What type does evaluating this node produce, if known?
    method type() { Mu }

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

    # Resolves all nodes beneath this one, recursively, using the specified
    # resolver.
    method resolve-all(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.IMPL-CHECK($resolver, $context, True);
    }

    # Perform CHECK-time activities on this node.
    method IMPL-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context, Bool $resolve-only) {
        # Perform resolutions.
        if nqp::istype(self, RakuAST::Lookup) && !self.is-resolved {
            self.resolve-with($resolver);
            if !$resolve-only && !self.is-resolved && self.needs-resolution {
                $resolver.add-node-unresolved-after-check-time(self);
            }
        }
        if nqp::istype(self, RakuAST::ImplicitLookups) {
            self.resolve-implicit-lookups-with($resolver);
        }
        if nqp::istype(self, RakuAST::Attaching) {
            self.attach($resolver);
        }
        if nqp::istype(self, RakuAST::ImplicitBlockSemanticsProvider) {
            self.apply-implicit-block-semantics();
        }

        # Apply any pre-children BEGIN-time effects that were not yet
        # performed (and figure out if we have to do the later).
        my int $needs-begin-after;
        if nqp::istype(self, RakuAST::BeginTime) {
            if self.is-begin-performed-before-children() {
                self.ensure-begin-performed($resolver, $context);
            }
            else {
                $needs-begin-after := 1;
            }
        }

        # Visit children.
        my int $is-scope := nqp::istype(self, RakuAST::LexicalScope);
        my int $is-package := nqp::istype(self, RakuAST::Package);
        $resolver.push-scope(self) if $is-scope;
        $resolver.push-package(self) if $is-package;
        self.visit-children(-> $child { $child.IMPL-CHECK($resolver, $context, $resolve-only) });
        $resolver.pop-scope() if $is-scope;
        $resolver.pop-package() if $is-package;

        # Perform any after-children BEGIN-time effects.
        if $needs-begin-after {
            self.ensure-begin-performed($resolver, $context);
        }

        # Unless in resolve-only mode, do other check-time activities.
        unless $resolve-only {
            if nqp::istype(self, RakuAST::SinkBoundary) && !self.sink-calculated {
                self.calculate-sink();
            }
            if nqp::istype(self, RakuAST::CheckTime) {
                self.clear-check-time-problems();
                self.PERFORM-CHECK($resolver);
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
        else {
            my $reified := nqp::getattr($list, List, '$!reified');
            nqp::isconcrete($reified)
                ?? $reified
                !! $list.FLATTENABLE_LIST
        }
    }

    method dump(int $indent?) {
        my str $prefix := nqp::x(' ', $indent);
        my $name := nqp::substr(self.HOW.name(self), nqp::chars('RakuAST::'));

        my @markers;
        @markers.push('⚓') if nqp::istype(self, RakuAST::Sinkable) && self.sunk;
        @markers.push('▪') if nqp::istype(self, RakuAST::BlockStatementSensitive) && self.is-block-statement;
        my $markers := @markers ?? ' ' ~ nqp::join('', @markers) !! '';

        my $dump := "$prefix$name$markers\n";
        if nqp::istype(self, RakuAST::Expression) {
            self.visit-thunks(-> $thunk {
                $dump := "$dump$prefix  🧠 " ~ $thunk.thunk-kind ~ "\n";
                $thunk.visit-children(-> $child {
                    $dump := $dump ~ $child.dump($indent + 4);
                });
            });
        }
        self.visit-children(-> $child {
            $dump := $dump ~ $child.dump($indent + 2);
        });
        $dump
    }

    # Hook into the Raku RakuAST::Deparse class (by default) or any other
    # class that has been put into the hllsym hash for 'Raku'
    method DEPARSE(*%_) {
        my $deparser := nqp::gethllsym('Raku','DEPARSE');
        nqp::isnull($deparser)
          ?? nqp::die("No deparser class found")
          !! $deparser.new(|%_).deparse(self)
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
}

# Anything with a known compile time value does RakuAST::CompileTimeValue.
class RakuAST::CompileTimeValue is RakuAST::Node {
    method compile-time-value() {
        nqp::die('compile-time-value not implemented for ' ~ self.HOW.name(self))
    }
}
