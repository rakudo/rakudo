use NQPP6Regex;

# This powers the optimization pass. It takes place after we've done all
# of the stuff in the grammar and actions, which means CHECK time is over.
# Thus we're allowed to assume that lexpads are immutable, declarations are
# over and done with, multi candidate lists won't change and so forth.
class Perl6::Optimizer {
    # Tracks the nested blocks we're in; it's the lexical chain, essentially.
    has @!block_stack;
    
    # How deep a chain we're in, for chaining operators.
    has $!chain_depth;
    
    # Unique ID for topic ($_) preservation registers.
    has $!pres_topic_counter;
    
    # Things that should cause compilation to fail; keys are errors, value is
    # array of line numbers.
    has %!deadly;
    
    # Things that should be warned about; keys are warnings, value is an array
    # of line numbers.
    has %!worrying;
    
    # Entry point for the optimization process.
    method optimize($past, *%adverbs) {
        # Initialize.
        @!block_stack := [$past];
        $!chain_depth := 0;
        $!pres_topic_counter := 0;
        %!deadly := nqp::hash();
        %!worrying := nqp::hash();
        
        # We'll start walking over UNIT (we wouldn't find it by going
        # over OUTER since we don't walk loadinits).
        my $unit := $past<UNIT>;
        unless $unit.isa(PAST::Block) {
            pir::die("Optimizer could not find UNIT");
        }
        self.visit_block($unit);
        
        # Die if we failed check in any way; otherwise, print any warnings.
        if +%!deadly {
            my @fails;
            for %!deadly {
                @fails.push($_.key ~ " (line" ~ (+$_.value == 1 ?? ' ' !! 's ') ~
                    pir::join(', ', $_.value) ~ ")");
            }
            pir::die("CHECK FAILED:\n" ~ pir::join("\n", @fails))
        }
        if +%!worrying {
            pir::printerr__vs("WARNINGS:\n");
            my @fails;
            for %!worrying {
                pir::printerr__vs($_.key ~ " (line" ~ (+$_.value == 1 ?? ' ' !! 's ') ~
                    pir::join(', ', $_.value) ~ ")\n");
            }
        }
        
        $past
    }
    
    # Called when we encounter a block in the tree.
    method visit_block($block) {
        # Push block onto block stack.
        @!block_stack.push($block);
        
        # Visit children.
        self.visit_children($block);
        
        # Pop block from block stack.
        @!block_stack.pop();
        
        # If the block is immediate, we may be able to inline it.
        my $outer := @!block_stack[+@!block_stack - 1];
        if $block.blocktype eq 'immediate' && !$outer.handlers() {
            # Scan symbols for any non-interesting ones.
            # XXX Shouldn't have to care about $/ and $! here...
            my @sigsyms;
            for $block.symtable() {
                my $name := $_.key;
                if $name ne '$/' && $name ne '$!' && $name ne '$_' && $name ne 'call_sig' {
                    @sigsyms.push($name);
                }
            }
            
            # If we have no interesting ones, then we can inline the
            # statements.
            # XXX We can also check for lack of colliding symbols and
            # do something in that case. However, it's non-trivial as
            # the static lexpad entries will need twiddling with.
            if +@sigsyms == 0 {
                return self.inline_immediate_block($block, $outer);
            }
        }
        
        $block
    }
    
    # Called when we encounter a PAST::Op in the tree. Produces either
    # the op itself or some replacement opcode to put in the tree.
    method visit_op($op) {
        # A chain with exactly two children can become the op itself.
        my $pasttype := $op.pasttype;
        if $pasttype eq 'chain' {
            $!chain_depth := $!chain_depth + 1;
            $pasttype := 'call' if $!chain_depth == 1 &&
                !($op[0].isa(PAST::Op) && $op[0].pasttype eq 'chain') &&
                !($op[1].isa(PAST::Op) && $op[1].pasttype eq 'chain');
        }
        
        # Calls are especially interesting as we may wish to do some
        # kind of inlining.
        if ($pasttype eq 'call' || $pasttype eq '') && $op.name ne '' {
            # See if we can find the thing we're going to call.
            my $obj;
            my $found;
            try {
                $obj := self.find_lexical($op.name);
                $found := 1;
            }
            if $found {
                # If it's an onlystar proto, we have a couple of options.
                # The first is that we may be able to work out what to
                # call at compile time. Failing that, we can at least inline
                # the proto.
                my $dispatcher;
                try { if $obj.is_dispatcher { $dispatcher := 1 } }
                if $dispatcher {
                    # Visit the children.
                    self.visit_children($op);
                    
                    # Try to do compile-time multi-dispatch.
                    my @types;
                    my @flags;
                    my $possible := 1;
                    for @($op) {
                        # Can't cope with flattening or named.
                        if $_.flat || $_.named ne '' {
                            $possible := 0;
                            last;
                        }
                        
                        # See if we know the node's type.
                        if $_<boxable_native> {
                            @types.push(nqp::null());
                            @flags.push($_<boxable_native>);
                        }
                        elsif pir::can__IPs($_, 'type') && !pir::isnull__IP($_.type) {
                            my $type := $_.type();
                            if pir::isa($type, 'Undef') {
                                $possible := 0;
                            }
                            elsif $type.HOW.archetypes.generic {
                                $possible := 0;
                            }
                            else {
                                my $prim := pir::repr_get_primitive_type_spec__IP($type);
                                @types.push($type);
                                @flags.push($prim);
                            }
                        }
                        else {
                            $possible := 0;
                            last;
                        }
                    }
                    if $possible {
                        my @ct_result := pir::perl6_multi_dispatch_ct__PPPP($obj, @types, @flags);
                        if @ct_result[0] == 1 {
                            my $chosen := @ct_result[1];
                            if $op.pasttype eq 'chain' { $!chain_depth := $!chain_depth - 1 }
                            return pir::can($chosen, 'inline_info') && $chosen.inline_info ne ''
                                ?? self.inline_call($op, $chosen)
                                !! self.call_ct_chosen_multi($op, $obj, $chosen);
                        }
                        elsif @ct_result[0] == -1 {
                            my @arg_names;
                            my $i := 0;
                            while $i < +@types {
                                @arg_names.push(
                                    @flags[$i] == 1 ?? 'int' !!
                                    @flags[$i] == 2 ?? 'num' !!
                                    @flags[$i] == 3 ?? 'str' !!
                                    @types[$i].HOW.name(@types[$i]));
                                $i := $i + 1;
                            }
                            self.add_deadly($op, "Dispatch to '" ~ $obj.name ~
                                "' could never work with the arguments of types (" ~
                                pir::join(', ', @arg_names) ~
                                ')');
                        }
                    }
                    
                    # Otherwise, inline the proto.
                    if $op.pasttype eq 'chain' { $!chain_depth := $!chain_depth - 1 }
                    return self.inline_proto($op, $obj);
                }
            }
            else {
                # We really should find routines; failure to do so is a CHECK
                # time error. Check that it's not just compile-time unknown,
                # however (shows up in e.g. sub foo(&x) { x() }).
                unless self.is_lexical_declared($op.name) {
                    self.add_deadly($op, "Undefined routine '" ~ $op.name ~ "' called");
                }
            }
        }
        
        # If we end up here, just visit children and leave op as is.
        self.visit_children($op);
        if $op.pasttype eq 'chain' {
            $!chain_depth := $!chain_depth - 1;
        }
        $op
    }
    
    # Visits all of a nodes children, and dispatches appropriately.
    method visit_children($node) {
        my $i := 0;
        while $i < +@($node) {
            my $visit := $node[$i];
            unless pir::isa($visit, 'String') || pir::isa($visit, 'Integer') {
                if $visit.isa(PAST::Op) {
                    $node[$i] := self.visit_op($visit)
                }
                elsif $visit.isa(PAST::Block) {
                    $node[$i] := self.visit_block($visit);
                }
                elsif $visit.isa(PAST::Stmts) {
                    self.visit_children($visit);
                }
                elsif $visit.isa(PAST::Stmt) {
                    self.visit_children($visit);
                }
            }
            $i := $i + 1;
        }
    }
    
    # Locates a lexical symbol and returns its compile time value. Dies if
    # it does not exist.
    method find_lexical($name) {
        my $i := +@!block_stack;
        while $i > 0 {
            $i := $i - 1;
            my $block := @!block_stack[$i];
            my %sym := $block.symbol($name);
            if +%sym {
                if pir::exists(%sym, 'value') {
                    return %sym<value>;
                }
                else {
                    pir::die("Optimizer: No lexical compile time value for $name");
                }
            }
        }
        pir::die("Optimizer: No lexical $name found");
    }
    
    # Checks if a given lexical is declared, though it needn't have a compile
    # time known value.
    method is_lexical_declared($name) {
        my $i := +@!block_stack;
        while $i > 0 {
            $i := $i - 1;
            my $block := @!block_stack[$i];
            my %sym := $block.symbol($name);
            if +%sym {
                return 1;
            }
        }
        0
    }
    
    # Inlines an immediate block.
    method inline_immediate_block($block, $outer) {
        # Extract interesting parts of block.
        my $decls := $block.shift;
        my $stmts := $block.shift;
        
        # Turn block into an "optimized out" stub (deserialization
        # or fixup will still want it to be there).
        $block.blocktype('declaration');
        $block[0] := PAST::Op.new( :pirop('die vs'),
            'INTERNAL ERROR: Execution of block eliminated by optimizer');                
        $outer[0].push($block);
        
        # Copy over interesting stuff in declaration section.
        for @($decls) {
            if $_.isa(PAST::Op) && $_.pirop eq 'bind_signature vP' {
                # Don't copy this binder call.
            }
            elsif $_.isa(PAST::Var) && ($_.name eq '$/' || $_.name eq '$!' ||
                    $_.name eq '$_' || $_.name eq 'call_sig') {
                # Don't copy this variable node.
            }
            else {
                $outer[0].push($_);
            }
        }
        
        # Hand back the statements, but be sure to preserve $_
        # around them.
        $!pres_topic_counter := $!pres_topic_counter + 1;
        $outer[0].push(PAST::Var.new( :scope('register'),
            :name("pres_topic_$!pres_topic_counter"), :isdecl(1) ));
        return PAST::Stmts.new(
            :signature('1PPP'),
            PAST::Op.new( :pasttype('bind_6model'),
                PAST::Var.new( :name("pres_topic_$!pres_topic_counter"), :scope('register') ),
                PAST::Var.new( :name('$_'), :scope('lexical') )
            ),
            $stmts,
            PAST::Op.new( :pasttype('bind_6model'),
                PAST::Var.new( :name('$_'), :scope('lexical') ),
                PAST::Var.new( :name("pres_topic_$!pres_topic_counter"), :scope('register') )
            )
        );
    }
    
    # Inlines a proto.
    method inline_proto($call, $proto) {
        $call.unshift(PAST::Op.new(
            :pirop('perl6_multi_dispatch_thunk PP'),
            PAST::Var.new( :name($call.name), :scope('lexical_6model') )));
        $call.name(nqp::null());
        $call.pasttype('call');
        $call
    }
    
    # Inlines a call to a sub.
    method inline_call($call, $code_obj) {
        my $inline := $code_obj.inline_info();
        my @tokens := pir::split(' ', $inline);
        my @stack := [PAST::Stmt.new()];
        while +@tokens {
            my $cur_tok := @tokens.shift;
            if $cur_tok eq ')' {
                my $popped := @stack.pop();
                @stack[+@stack - 1].push($popped);
            }
            elsif $cur_tok eq 'ARG' {
                @stack[+@stack - 1].push($call[+@tokens.shift()]);
            }
            elsif $cur_tok eq 'PIROP' {
                @stack.push(PAST::Op.new( :pirop(@tokens.shift()) ));
                unless @tokens.shift() eq '(' {
                    pir::die("INTERNAL ERROR: Inline corrupt; expected ')'");
                }
            }
            elsif $cur_tok eq 'WANT' {
                @stack.push(PAST::Want.new());
                unless @tokens.shift() eq '(' {
                    pir::die("INTERNAL ERROR: Inline corrupt; expected ')'");
                }
            }
            elsif $cur_tok eq 'WANTSPEC' {
                @stack[+@stack - 1].push(~@tokens.shift());
            }
            else {
                pir::die("INTERNAL ERROR: Unexpected inline token: " ~ $cur_tok);
                return $call;
            }
        }
        if +@stack != 1 {
            pir::die("INTERNAL ERROR: Non-empty inline stack")
        }
        #say("# inlined a call to " ~ $call.name);
        @stack[0]
    }
    
    # If we decide a dispatch at compile time, this emits the direct call.
    method call_ct_chosen_multi($call, $proto, $chosen) {
        my @cands := $proto.dispatchees();
        my $idx := 0;
        for @cands {
            if $_ =:= $chosen {
                $call.unshift(PAST::Op.new(
                    :pirop('perl6_multi_dispatch_cand_thunk PPi'),
                    PAST::Var.new( :name($call.name), :scope('lexical_6model') ),
                    $idx));
                $call.name(nqp::null());
                $call.pasttype('call');
                #say("# Compile-time resolved a call to " ~ $proto.name);
                last;
            }
            $idx := $idx + 1;
        }
        $call
    }
    
    # Adds an entry to the list of things that would cause a check fail.
    method add_deadly($past_node, $message) {
        my $line := PAST::Compiler.lineof($past_node<source>, $past_node<pos>);
        unless %!deadly{$message} {
            %!deadly{$message} := [];
        }
        %!deadly{$message}.push($line);
    }
}
