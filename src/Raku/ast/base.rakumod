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

    # Resolves all nodes beneath this one, recursively, using the specified
    # resolver.
    method resolve-all(RakuAST::Resolver $resolver) {
        self.IMPL-CHECK($resolver, True);
    }

    # Perform CHECK-time activities on this node.
    method IMPL-CHECK(RakuAST::Resolver $resolver, Bool $resolve-only) {
        # Perform resolutions.
        if nqp::istype(self, RakuAST::Lookup) && !self.is-resolved {
            self.resolve-with($resolver);
        }
        if nqp::istype(self, RakuAST::ImplicitLookups) {
            self.resolve-implicit-lookups-with($resolver);
        }
        if nqp::istype(self, RakuAST::Attaching) {
            self.attach($resolver);
        }

        # Unless in resolve-only mode, do other check-time activities.
        unless $resolve-only {
            if nqp::istype(self, RakuAST::SinkBoundary) && !self.sink-calculated {
                self.calculate-sink();
            }
        }

        # Visit children.
        my int $is-scope := nqp::istype(self, RakuAST::LexicalScope);
        $resolver.push-scope(self) if $is-scope;
        self.visit-children(-> $child { $child.IMPL-CHECK($resolver, $resolve-only) });
        $resolver.pop-scope() if $is-scope;

        Nil
    }

    # Recursively walks the tree finding nodes of the specified type that are
    # beneath this one. A node that matches the stopper type will be returned
    # if it satisfies the specified type, but it's children shall not be
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
        self.visit-children(-> $child {
            $dump := $dump ~ $child.dump($indent + 2);
        });
        $dump
    }
}

# Anything with a known compile time value does RakuAST::CompileTimeValue.
class RakuAST::CompileTimeValue is RakuAST::Node {
    method compile-time-value() {
        nqp::die('compile-time-value not implemented for ' ~ self.HOW.name(self))
    }
}
