# This file contains classes related to static optimization of Raku programs
# using RakuAST. It performs optimizations after the initial compilation phases,
# assuming lexpads are immutable, declarations are complete, and multi candidates are fixed.

use QAST;
#use RakuAST;

# A null QAST node, inserted when we want to eliminate something.
my $NULL := QAST::Op.new( :op<null> );

# Represents the current set of blocks we're in and thus the symbols they
# make available, and allows for queries over them.
class RakuAST::Optimizer::Symbols {
    # The nested blocks we're in; it's the lexical chain, essentially.
    has @!block_stack;
    
    # Type information storage - stack of type tables, one per lexical scope
    has @!type_scopes;
    
    # Constant value storage - stack of constant value tables, one per lexical scope
    has @!constant_scopes;

    # Some interesting scopes.
    has $!GLOBALish;
    has $!UNIT;
    has @!CORES;

    # Cached setting lookups.
    has %!SETTING_CACHE;

    # Some interesting symbols.
    has $!Mu;
    has $!Junction;
    has $!Any;
    has $!Block;
    has $!Code;
    has $!Routine;
    has $!Method;
    has $!Signature;
    has $!Pair;
    has $!Regex;
    has $!Nil;
    has $!Failure;
    has $!False;
    has $!True;
    has $!Seq;

    # Top routine, for faking it when optimizing post-inline.
    has $!top_routine;

    # Constructs a new instance of the symbols handling class.
    method new($compunit) {
        my $obj := nqp::create(self);
        $obj.BUILD($compunit);
        $obj
    }
    
    method BUILD($compunit) {
        @!block_stack     := [$compunit[0]];
        @!type_scopes     := [{}];  # Initialize with a type scope for the compunit
        @!constant_scopes := [{}];  # Initialize with a constant scope for the compunit
        @!CORES           := [];
        $!GLOBALish       := $compunit.ann('GLOBALish');
        $!UNIT            := $compunit.ann('UNIT');
        %!SETTING_CACHE   := {};
        
        unless nqp::istype($!UNIT, QAST::Block) {
            nqp::die("Optimizer could not find UNIT");
        }
        
        nqp::push(@!block_stack, $!UNIT);
        
        # Initialize common symbol cache
        $!Mu       := self.find_in_setting('Mu');
        $!Junction := self.find_in_setting('Junction');
        $!Any      := self.find_in_setting('Any');
        $!Block    := self.find_in_setting('Block');
        $!Code     := self.find_in_setting('Code');
        $!Routine  := self.find_in_setting('Routine');
        $!Method   := self.find_in_setting('Method');
        $!Signature := self.find_in_setting('Signature');
        $!Pair     := self.find_in_setting('Pair');
        $!Regex    := self.find_in_setting('Regex');
        $!Nil      := self.find_in_setting('Nil');
        $!Failure  := self.find_in_setting('Failure');
        $!False    := self.find_in_setting('False');
        $!True     := self.find_in_setting('True');
        $!Seq      := self.find_in_setting('Seq');
        
        nqp::pop(@!block_stack);
    }

    # Block handling methods
    method push_block($block) {
        nqp::push(@!block_stack, $block);
        nqp::push(@!type_scopes, {});     # Create new type scope for the block
        nqp::push(@!constant_scopes, {}); # Create new constant scope for the block
    }
    
    method pop_block() {
        nqp::pop(@!constant_scopes) if @!constant_scopes.elems() > 0;
        nqp::pop(@!type_scopes) if @!type_scopes.elems() > 0;
        nqp::pop(@!block_stack)
    }
    
    method top_block() {
        @!block_stack[+@!block_stack - 1]
    }
    
    # Symbol lookup methods
    method find_in_setting($symbol) {
        unless %!SETTING_CACHE{$symbol} {
            my $compunit := @!block_stack[0];
            my $W := $compunit.ann('W');
            %!SETTING_CACHE{$symbol} := $W.?find($symbol) || nqp::null();
        }
        %!SETTING_CACHE{$symbol}
    }
    
    method find_symbol($path) {
        my $compunit := @!block_stack[0];
        my $W := $compunit.ann('W');
        $W.?find(|$path) || nqp::null();
    }
    
    method find_lexical($name) {
        for @!block_stack.reverse -> $block {
            if $block.symtable(){$name} {
                return $block.symtable(){$name}{value};
            }
        }
        nqp::null();
    }
    
    method is_from_core($name) {
        $name ~~ /^ '\&infix:' | '\&prefix:' | '\&postfix:' | '\&circumfix:' | '\&circumfix:' | '\&postcircumfix:' /
    }
    
    # Accessors for common symbols
    method Mu() { $!Mu }
    method Junction() { $!Junction }
    method Any() { $!Any }
    method Block() { $!Block }
    method Code() { $!Code }
    method Routine() { $!Routine }
    method Method() { $!Method }
    method Signature() { $!Signature }
    method Pair() { $!Pair }
    method Regex() { $!Regex }
    method Nil() { $!Nil }
    method Failure() { $!Failure }
    method False() { $!False }
    method True() { $!True }
    method Seq() { $!Seq }
    method GLOBALish() { $!GLOBALish }
    method UNIT() { $!UNIT }
    
    # Type inference methods
    method set_var_type($name, $type) {
        # Store variable type in the current lexical scope
        if @!type_scopes.elems() > 0 {
            @!type_scopes[*-1]{$name} := $type;
        }
    }
    
    method get_var_type($name) {
        # Look up variable type in the lexical chain (current scope first)
        for @!type_scopes.reverse() -> %scope {
            if %scope{$name}:exists {
                return %scope{$name};
            }
        }
        # If not found in any scope, return null
        nqp::null()
    }
    
    method has_var_type($name) {
        # Check if a variable has a known type in any scope
        for @!type_scopes.reverse() -> %scope {
            if %scope{$name}:exists {
                return True;
            }
        }
        False
    }
    
    # Constant value methods
    method set_var_constant_value($name, $value) {
        # Store variable constant value in the current lexical scope
        if @!constant_scopes.elems() > 0 {
            @!constant_scopes[*-1]{$name} := $value;
        }
    }
    
    method get_var_constant_value($name) {
        # Look up variable constant value in the lexical chain (current scope first)
        for @!constant_scopes.reverse() -> %scope {
            if %scope{$name}:exists {
                return %scope{$name};
            }
        }
        # If not found in any scope, return null
        nqp::null()
    }
    
    method has_var_constant_value($name) {
        # Check if a variable has a known constant value in any scope
        for @!constant_scopes.reverse() -> %scope {
            if %scope{$name}:exists {
                return True;
            }
        }
        False
    }
}

# Tracks variables and their usage within a block
class RakuAST::Optimizer::BlockVarOptimizer {
    # Variable declarations.
    has %!decls;

    # Variable usages.
    has %!usages_inner;
    has %!usages_flat;
    
    # Outer lexical lookups
    has @!getlexouter_usages;
    
    # Autoslurpy handling
    has @!autoslurpy_setups;
    has @!autoslurpy_binds;
    
    # Take dispatcher
    has $!takedispatcher;
    
    # Flags
    has int $!poisoned = 0;
    has int $!topic_poisoned = 0;
    has int $!uses_bindsig = 0;
    has int $!uses_p6return = 0;
    has int $!in_handle_handler = 0;
    has %!used_in_handle_handler;
    
    # Constructor
    method new(:$debug = False) {
        my $obj := nqp::create(self);
        $obj.%!decls := {};
        $obj.%!usages_inner := {};
        $obj.%!usages_flat := {};
        $obj.@!getlexouter_usages := [];
        $obj.@!autoslurpy_setups := [];
        $obj.@!autoslurpy_binds := [];
        $obj.%!used_in_handle_handler := {};
        $obj
    }
    
    # Registration methods
    method register_decl($node, $name) {
        %!decls{$name} := $node;
    }
    
    method register_usage_inner($node, $name) {
        %!usages_inner{$name} := [] unless %!usages_inner{$name};
        nqp::push(%!usages_inner{$name}, $node);
    }
    
    method register_usage_flat($node, $name) {
        %!usages_flat{$name} := [] unless %!usages_flat{$name};
        nqp::push(%!usages_flat{$name}, $node);
    }
    
    method register_getlexouter_usage($node) {
        nqp::push(@!getlexouter_usages, $node);
    }
    
    method register_autoslurpy_setup($node) {
        nqp::push(@!autoslurpy_setups, $node);
    }
    
    method register_autoslurpy_bind($node) {
        nqp::push(@!autoslurpy_binds, $node);
    }
    
    method register_takedispatcher($node) {
        $!takedispatcher := $node;
    }
    
    # Flag methods
    method poison_lowering() { $!poisoned := 1; }
    method poison_topic_lowering() { $!topic_poisoned := 1; }
    method uses_bindsig() { $!uses_bindsig := 1; }
    method uses_p6return() { $!uses_p6return := 1; }
    method entering_handle_handler() { $!in_handle_handler++; }
    method leaving_handle_handler() { $!in_handle_handler--; }
    method mark_used_in_handle_handler($name) {
        %!used_in_handle_handler{$name} := 1;
    }
    
    # Accessor methods
    method get_decls() { %!decls }
    method get_usages_flat() { %!usages_flat }
    method get_usages_inner() { %!usages_inner }
    method get_getlexouter_usages() { @!getlexouter_usages }
    method get_takedispatcher() { $!takedispatcher }
    method is_poisoned() { $!poisoned }
    method is_topic_poisoned() { $!topic_poisoned }
    method is_in_handle_handler() { $!in_handle_handler > 0 }
    
    # Inlinability check
    method is_inlinable() {
        !($!poisoned || $!uses_p6return)
    }
    
    # Get escaping handler variables
    method get_escaping_handler_vars() {
        my @esc;
        for %!used_in_handle_handler {
            my $name := $_.key;
            unless %!decls{$name} {
                @esc.push($name);
            }
        }
        return @esc;
    }
    
    # Track local caches created for outer references
    has %!local_caches;
    has int $!in_hot_loop = 0;
    
    # Method to get variable usage count
    method get_usage_count($name) {
        my $count := 0;
        if %!usages_flat{$name} {
            $count := +%!usages_flat{$name};
        }
        return $count;
    }
    
    # Check if a local cache exists for a variable
    method has_local_cache($name) {
        nqp::existskey(%!local_caches, $name);
    }
    
    # Mark a variable as having a local cache
    method mark_local_cache($name) {
        %!local_caches{$name} := 1;
    }
    
    # Check if we're in a hot loop
    method is_in_hot_loop() {
        $!in_hot_loop > 0;
    }
    
    # Set hot loop status
    method set_hot_loop($value) {
        $!in_hot_loop := $value ?? $!in_hot_loop + 1 !! $!in_hot_loop - 1;
    }
    
    # Method to prepend a statement to the current block
    method prepend_to_current_block($block, $statement) {
        my $body := $block[1];
        if $body && nqp::istype($body, QAST::Stmts) {
            my @new_children;
            @new_children.push($statement);
            for $body.list() -> $child {
                @new_children.push($child);
            }
            $body.set_children(@new_children);
        }
    }
    
    # Optimization methods
    method delete_unused_magicals($block, $can_lower_topic) {
        # Remove unused magical variables like $_ (topic) and $/ (match variable)
        my $body := $block[1];
        return unless $body && nqp::istype($body, QAST::Stmts);
        
        my %magical_vars := (
            '$_' => True,
            '$/' => True,
            '$!' => True,
            '$?' => True,
            '$!' => True,
        );
        
        # Check if we can remove the topic variable
        if $can_lower_topic && !$!topic_poisoned {
            for %!decls.kv -> $name, $decl {
                if %magical_vars{$name} && !%!usages_flat{$name} {
                    # Remove the declaration from the block
                    my @new_children;
                    for $body.list() -> $child {
                        if $child !=== $decl {
                            @new_children.push($child);
                        }
                    }
                    $body.set_children(@new_children);
                }
            }
        }
    }
    
    method delete_unused_autoslurpy() {
        # Remove autoslurpy setups that are not used
        if @!autoslurpy_setups && @!autoslurpy_setups.elems > 0 {
            for @!autoslurpy_setups -> $setup {
                my $arg_name := $setup.name;
                if !%!usages_flat{$arg_name} || +%!usages_flat{$arg_name} == 0 {
                    # Replace with null op to remove it
                    $setup.op := 'null';
                    $setup.set_children([]);
                }
            }
        }
        
        # Also clean up unused autoslurpy binds
        if @!autoslurpy_binds && @!autoslurpy_binds.elems > 0 {
            for @!autoslurpy_binds -> $bind {
                my $var_name := $bind[0].name;
                if !%!usages_flat{$var_name} || +%!usages_flat{$var_name} == 0 {
                    # Replace with null op to remove it
                    $bind.op := 'null';
                    $bind.set_children([]);
                }
            }
        }
    }
    
    method simplify_takedispatcher() {
        # Simplify or remove takedispatcher if not needed
        if $!takedispatcher {
            # Check if any variables are used in a way that would require the takedispatcher
            my $needs_takedispatcher := False;
            for %!usages_flat.kv -> $name, $usages {
                if +$usages > 0 {
                    $needs_takedispatcher := True;
                    last;
                }
            }
            
            # If no variables need the takedispatcher, remove it
            unless $needs_takedispatcher {
                $!takedispatcher.op := 'null';
                $!takedispatcher.set_children([]);
            }
        }
    }
    
    method lexical_vars_to_locals($block, $lowered_away_lexical, $can_lower_topic) {
        # Lower lexical variables to local variables where possible
        return if $!poisoned;
        
        my $body := $block[1];
        return unless $body && nqp::istype($body, QAST::Stmts);
        
        # Process variable declarations
        for %!decls.kv -> $name, $decl {
            # Skip magical variables or if topic lowering is not allowed
            next if $name eq '$_' && (!$can_lower_topic || $!topic_poisoned);
            
            # Skip if variable is used in outer scopes or in handle handlers
            next if %!used_in_handle_handler{$name};
            
            # Check if the variable is only used within its own block
            my $can_lower := True;
            
            # Check for outer usages
            for @!getlexouter_usages -> $usage {
                if $usage.name eq $name {
                    $can_lower := False;
                    last;
                }
            }
            
            if $can_lower {
                # Lower the variable from lexical to local
                if nqp::istype($decl, QAST::Var) {
                    $decl.scope := 'local';
                    $decl.annotate('lowered_from_lexical', 1);
                }
                
                # Also update all usages
                if %!usages_inner{$name} {
                    for %!usages_inner{$name} -> $usage {
                        if nqp::istype($usage, QAST::Var) {
                            $usage.scope := 'local';
                        }
                    }
                }
            }
        }
    }
}

# Stack for managing block variable optimizers
class RakuAST::Optimizer::BlockVarStack {
    has $!debug;
    has @!block_var_stack;
    has $!top;
    # If set then any request to the actual BlockVarOptimizer on the stack top would be ignored
    has $!dry-run;

    method new(:$debug = False, :$dry-run = False) {
        my $obj := nqp::create(self);
        $obj.$!debug := $debug;
        $obj.@!block_var_stack := [];
        $obj.$!dry-run := $dry-run ?? 1 !! 0;
        $obj.$!top := nqp::null();
        $obj
    }

    method push() {
        $!top := RakuAST::Optimizer::BlockVarOptimizer.new(:$!debug);
        @!block_var_stack.push($!top);
        $!top
    }

    method pop() {
        my $old-top := @!block_var_stack.pop();
        my $count := +@!block_var_stack;
        $!top := $count ?? @!block_var_stack[+@!block_var_stack - 1] !! nqp::null();
        $old-top
    }

    method top() { $!top }

    method enter-dry-run() {
        $!dry-run := ($!dry-run // 0) + 1;
    }

    method exit-dry-run() {
        nqp::die("Exiting block var stack dry run mode without a previous enter") unless $!dry-run;
        --$!dry-run
    }

    method in-dry-run() { nqp::istrue($!dry-run) }

    method do($method, *@pos, *%named) {
        return nqp::null() if $!dry-run;
        $!top.?"$method"(|@pos, |%named) || nqp::null();
    }

    method elems() { +@!block_var_stack }

    method poison-var-lowering() {
        for @!block_var_stack {
            $_.poison_lowering();
        }
    }
}

# Main optimizer class that drives the optimization process
class RakuAST::Optimizer {
    # Symbols tracking object.
    has $!symbols;

    # Stack of block variable optimizers.
    has $!block_var_stack;

    # Track problems we encounter.
    has $!problems;

    # Optimizer configuration.
    has %!adverbs;

    # The optimization level.
    has $!level;

    # How deep a chain we're in, for chaining operators.
    has int $!chain_depth;

    # Are we in void context?
    has int $!void_context;

    # Are we in a declaration?
    has int $!in_declaration;

    # one shared QAST tree we'll put into every block we've eliminated
    has $!eliminated_block_contents;

    # Are we allowed to lower the topic ($_) to a local?
    has $!can_lower_topic;

    # Are we in debug mode?
    has $!debug;

    # Entry point for the optimization process.
    method optimize($past, *%adverbs) {
        # Initialize.
        $!symbols                 := RakuAST::Optimizer::Symbols.new($past);
        $!block_var_stack         := RakuAST::Optimizer::BlockVarStack.new(:$!debug, :!dry-run);
        $!problems                := RakuAST::Optimizer::Problems.new($!symbols);
        $!chain_depth             := 0;
        $!in_declaration          := 0;
        $!void_context            := 0;
        $!can_lower_topic         := $past.ann('CAN_LOWER_TOPIC');
        $!debug                   := nqp::getenvhash<RAKUDO_OPTIMIZER_DEBUG>;
        my $*DYNAMICALLY_COMPILED := 0;
        my $*OPTIMIZER-SYMBOLS    := $!symbols;
        my $*W                    := $past.ann('W');

        # Work out optimization level.
        $!level := nqp::existskey(%adverbs, 'optimize') ??
            +%adverbs<optimize> !! 2;
        %!adverbs := %adverbs;

        note("method optimize before\n" ~ $past.dump) if $!debug;

        $!eliminated_block_contents := QAST::Op.new( :op('die_s'),
            QAST::SVal.new( :value('INTERNAL ERROR: Execution of block eliminated by optimizer') ) );

        # Walk and optimize the program.
        self.visit_block($!symbols.UNIT);

        # Report any discovered problems.
        $!problems.report();

        note("method optimize after\n" ~ $past.dump) if $!debug;

        $past
    }

    # Called when we encounter a block in the tree.
    method visit_block($block) {
        # Push block onto block stack and create vars tracking.
        $!symbols.push_block($block);
        $!block_var_stack.push();

        # we don't want any "blah in sink context" warnings emitted here
        if $block.ann: 'WANTMEPLEASE' {
            my $body := $block[1];
            if $body && nqp::istype($body, QAST::Stmts) && nqp::elems($body) > 0 {
                my $last_stmt := $body[nqp::elems($body) - 1];
                $last_stmt.annotate: 'sink-quietly', 1;
            }
        }

        # Visit children.
        if $block.ann('DYNAMICALLY_COMPILED') {
            my $*DYNAMICALLY_COMPILED := 1;
            self.visit_children($block, :resultchild(nqp::elems($block) - 1),
                :void_default, :block_structure);
        }
        else {
            self.visit_children($block, :resultchild(nqp::elems($block) - 1),
                :void_default, :block_structure);
        }

        # Pop block from block stack and get computed block var info.
        $!symbols.pop_block();
        my $vars_info := $!block_var_stack.pop();

        # If block var stack is in dry run mode no work on the data it provides is possible
        return $block if $!block_var_stack.in-dry-run;

        # If this block is UNIT and we're in interactive mode, poison lexical
        # to local lowering, or else we may lose symbols we need to remember.
        if $block =:= $!symbols.UNIT && %!adverbs<interactive> {
            $vars_info.poison_lowering();
        }

        # We might be able to delete some of the magical variables when they
        # are trivially unused, and also simplify takedispatcher.
        if $!level >= 1 {
            $vars_info.delete_unused_magicals($block, $!can_lower_topic);
            $vars_info.delete_unused_autoslurpy();
            $vars_info.simplify_takedispatcher();
        }

        # Do any possible lexical => local lowering.
        if $!level >= 2 {
            $vars_info.lexical_vars_to_locals($block, $!symbols.LoweredAwayLexical, $!can_lower_topic);
        }

        # Return the potentially modified block
        $block
    }

    # Visit children of a node, with special handling for result child.
    method visit_children($node, :$resultchild = -1, :$void_default = False, :$block_structure = False) {
        my int $i := 0;
        my $result := nqp::null();
        my @new_children;
        
        for $node.list() -> $child {
            my int $is_result := $i == $resultchild;
            my int $old_void  := $!void_context;
            $!void_context := $void_default || !$is_result;
            my $visited := self.visit($child, :$block_structure);
            $!void_context := $old_void;
            
            # Don't add null nodes to the children list
            unless nqp::istype($visited, QAST::Op) && $visited.op eq 'null' {
                @new_children.push($visited);
            }
            
            if $is_result {
                $result := $visited;
            }
            $i++;
        }
        
        # Replace the children with optimized versions
        $node.set_children(@new_children);
        $result
    }

    # Main visitor method that dispatches to specific node type handlers.
    method visit($node, :$block_structure = False) {
        # If node is null, just return it
        return $node unless $node;
        
        # Dispatch based on node type
        if nqp::istype($node, QAST::Block) {
            return self.visit_block($node);
        }
        elsif nqp::istype($node, QAST::Op) {
            return self.visit_op($node, :$block_structure);
        }
        elsif nqp::istype($node, QAST::Stmts) {
            return self.visit_stmts($node, :$block_structure);
        }
        elsif nqp::istype($node, QAST::Var) {
            return self.visit_var($node);
        }
        elsif nqp::istype($node, QAST::Want) {
            return self.visit_want($node, :$block_structure);
        }
        elsif nqp::istype($node, QAST::Val) || nqp::istype($node, QAST::IVal) ||
              nqp::istype($node, QAST::NVal) || nqp::istype($node, QAST::SVal) ||
              nqp::istype($node, QAST::BVal) {
            # Constant values - perform any value-specific optimizations
            return self.visit_constant($node);
        }
        elsif nqp::istype($node, QAST::Call) {
            # Function/method calls - optimize calls
            return self.visit_call($node, :$block_structure);
        }
        elsif nqp::istype($node, QAST::Bind) {
            # Variable bindings - optimize bindings
            return self.visit_bind($node, :$block_structure);
        }
        
        # Default case: visit children
        self.visit_children($node, :block_structure($block_structure));
        
        # Apply dead code elimination for complex nodes at higher optimization levels
        # Apply to statement lists and blocks which are common sources of dead code
        if ($!level >= 2 && (nqp::istype($node, QAST::Stmts) || nqp::istype($node, QAST::Block))) {
            return self.dead_code_elimination($node);
        }
        
        $node
    }
    
    # Visit constant value nodes
    method visit_constant($const) {
        # Only apply optimizations at higher levels
        if $!level >= 2 {
            # String interning and optimization
            if nqp::istype($const, QAST::SVal) {
                my $value := $const.value;
                
                # Empty string optimization
                if $value eq '' {
                    # Use a shared empty string constant
                    state $empty_str_constant;
                    if !$empty_str_constant {
                        $empty_str_constant := QAST::SVal.new(:value(''));
                    }
                    return $empty_str_constant;
                }
                
                # Short string interning (common strings)
                state %interned_strings;
                if $value.chars < 10 { # Only intern short strings
                    unless %interned_strings{$value} {
                        %interned_strings{$value} := $const;
                    }
                    return %interned_strings{$value}; # Return the interned version
                }
            }
            
            # Integer optimization
            elsif nqp::istype($const, QAST::IVal) {
                my $value := $const.value;
                
                # Common integer constants optimization
                state %interned_ints;
                if $value >= -10 && $value <= 100 { # Common range of integers
                    unless %interned_ints{$value} {
                        %interned_ints{$value} := $const;
                    }
                    return %interned_ints{$value};
                }
            }
            
            # Number optimization
            elsif nqp::istype($const, QAST::NVal) {
                my $value := $const.value;
                
                # Common floating point values
                state %interned_nums;
                if $value === 0e0 || $value === 1e0 || $value === -1e0 || 
                   $value === 0.5e0 || $value === 2e0 {
                    unless %interned_nums{$value} {
                        %interned_nums{$value} := $const;
                    }
                    return %interned_nums{$value};
                }
            }
            
            # Boolean optimization
            elsif nqp::istype($const, QAST::BVal) {
                state $true_constant;
                state $false_constant;
                
                if $const.value {
                    unless $true_constant {
                        $true_constant := QAST::BVal.new(:value(1));
                    }
                    return $true_constant;
                } else {
                    unless $false_constant {
                        $false_constant := QAST::BVal.new(:value(0));
                    }
                    return $false_constant;
                }
            }
        }
        
        # Return the original constant if no optimization applies
        $const
    }
    
    # Visit call nodes
    method visit_call($call, :$block_structure = False) {
        # Visit children first
        self.visit_children($call, :resultchild(0), :block_structure($block_structure));
        
        # Apply call-specific optimizations
        if $!level >= 2 {
            # Inline small functions when appropriate
            if self.can_inline_call($call) {
                return self.inline_call($call);
            }
        }
        
        $call
    }
    
    # Visit bind nodes
    method visit_bind($bind, :$block_structure = False) {
        # Visit children
        self.visit_children($bind, :block_structure($block_structure));
        
        # Apply bind-specific optimizations
        if $!level >= 2 {
            # Optimize bindings when possible
            my $optimized := self.optimize_binding($bind);
            return $optimized if $optimized;
        }
        
        $bind
    }
    
    # Check if a call can be inlined with enhanced decision logic
    method can_inline_call($call) {
        # Only consider inlining at higher optimization levels
        return False unless $!optimization_level >= 2;
        
        # Only inline calls with known targets
        my $target := $call.target;
        return False unless nqp::istype($target, QAST::Var) || nqp::istype($target, QAST::WVal);
        
        # Get the routine from the target
        my $routine := $target.compile-time-value;
        return False unless $routine && nqp::can($routine, 'body');
        
        # Get the body of the routine
        my $body := $routine.body;
        return False unless $body && nqp::istype($body, QAST::Block);
        
        # Don't inline native routines or routines marked as non-inlinable
        if $routine.is-native || $routine.is-hidden-from-backend || $routine.does-not-invoke || $routine.is-inlinable === False {
            return False;
        }
        
        # Special case: always inline routines explicitly marked as inlinable
        if $routine.is-inlinable === True {
            return True;
        }
        
        # Check for special inline cases first
        if self.is_special_inline_case($routine, $call) {
            return True;
        }
        
        # Analyze routine complexity with enhanced metrics
        my %complexity := self.analyze_routine_complexity($routine, $body);
        
        # If we exceeded any of the complexity thresholds, don't inline
        if %complexity<instruction_count> > 50 || %complexity<branch_count> > 5 || %complexity<call_count> > 3 {
            return False;
        }
        
        # Check for recursion - don't inline recursive functions
        if %complexity<has_recursion> {
            return False;
        }
        
        # Check for side effects (more important at lower optimization levels)
        if $!optimization_level < 4 && %complexity<has_significant_side_effects> {
            # Be more cautious with routines that have side effects
            if %complexity<instruction_count> > 15 {
                return False;
            }
        }
        
        # Very small routines can always be inlined
        if %complexity<instruction_count> <= 5 && %complexity<branch_count> == 0 {
            return True;
        }
        
        # For moderately sized routines, use a heuristic based on instruction count, branch count,
        # call count, and estimated performance benefit
        if %complexity<instruction_count> <= 30 && %complexity<branch_count> <= 2 && %complexity<call_count> <= 1 {
            # Estimate performance benefit based on routine characteristics
            if self.estimate_inline_benefit(%complexity) > 1.2 {
                return True;
            }
        }
        
        # Otherwise, don't inline
        False
    }
    
    # Analyze routine complexity with enhanced metrics
    method analyze_routine_complexity($routine, $body) {
        my int $instruction_count := 0;
        my int $branch_count := 0;
        my int $call_count := 0;
        my int $memory_usage := 0;
        my int $has_recursion := 0;
        my int $has_significant_side_effects := 0;
        my $routine_name := $routine.name // '';
        
        $body.walk(-> $node {
            if nqp::istype($node, QAST::Op) {
                $instruction_count++;
                
                # Count branches and calls which make inlining less beneficial
                my $op := $node.op;
                if $op eq 'if' || $op eq 'unless' || $op eq 'for' || $op eq 'while' || 
                   $op eq 'loop' || $op eq 'given' || $op eq 'when' {
                    $branch_count++;
                    $memory_usage += 10; # Estimate stack usage for control flow
                } elsif $op eq 'call' || $op eq 'callmethod' || $op eq 'callvirt' {
                    $call_count++;
                    $memory_usage += 20; # Estimate stack usage for calls
                } elsif $op eq 'die' || $op eq 'fail' || $op eq 'warn' || $op eq 'note' {
                    $has_significant_side_effects := 1;
                } elsif $op eq 'assign' || $op eq 'bind' {
                    # Track assignments to non-local variables as side effects
                    if $node[0] && nqp::istype($node[0], QAST::Var) && 
                       ($node[0].scope eq 'outer' || $node[0].scope eq 'package') {
                        $has_significant_side_effects := 1;
                    }
                }
            }
            elsif nqp::istype($node, QAST::Var) && $node.scope eq 'our' || $node.scope eq 'package' {
                # Global variable access
                $memory_usage += 5;
            }
            
            # Check for recursion (enhanced detection)
            if $routine_name && nqp::istype($node, QAST::Call) && $node.name eq $routine_name {
                $has_recursion := 1;
                return 0; # Stop walking once we find a self-call
            }
            
            # Early termination if clearly too complex
            return 1 if $instruction_count > 50 || # Too many instructions
                       $branch_count > 5 ||       # Too many branches
                       $call_count > 3;           # Too many nested calls
            
            1
        });
        
        return %(instruction_count => $instruction_count,
                 branch_count => $branch_count,
                 call_count => $call_count,
                 memory_usage => $memory_usage,
                 has_recursion => $has_recursion,
                 has_significant_side_effects => $has_significant_side_effects);
    }
    
    # Check for special cases where inlining is always beneficial
    method is_special_inline_case($routine, $call) {
        # Special case 1: Accessor/mutator methods
        my $name := $routine.name // '';
        if $name ~~ /^ (get_ | set_ | is_ | has_) / {
            # Check if it's a simple accessor/mutator
            my $body := $routine.body;
            if $body && nqp::istype($body, QAST::Block) && $body[0] && nqp::istype($body[0], QAST::Stmts) {
                my $stmts := $body[0];
                # Very simple getters/setters are good candidates
                if $stmts.elems <= 3 {
                    return True;
                }
            }
        }
        
        # Special case 2: Known performance-critical small utility functions
        my @perf_critical_utils := < min max abs floor ceil sqrt >;
        if $name && @perf_critical_utils.grep({ $_ eq $name }) {
            return True;
        }
        
        # Special case 3: Called with constant arguments (good for constant folding after inline)
        my $all_const_args := 1;
        if $call.args {
            for $call.args.list() -> $arg {
                unless nqp::istype($arg, QAST::IVal) || nqp::istype($arg, QAST::NVal) || 
                       nqp::istype($arg, QAST::SVal) || nqp::istype($arg, QAST::BVal) {
                    $all_const_args := 0;
                    last;
                }
            }
        }
        if $all_const_args {
            return True;
        }
        
        False
    }
    
    # Estimate the performance benefit of inlining
    method estimate_inline_benefit(%complexity) {
        # Base benefit (always some benefit to inlining)
        my $benefit := 1.0;
        
        # Lower complexity means higher benefit
        $benefit += 0.5 / (%complexity<instruction_count> + 1);
        
        # Fewer branches means higher benefit
        $benefit += 0.3 / (%complexity<branch_count> + 1);
        
        # Fewer calls means higher benefit
        $benefit += 0.2 / (%complexity<call_count> + 1);
        
        # Penalize routines with side effects
        if %complexity<has_significant_side_effects> {
            $benefit *= 0.9;
        }
        
        # Penalize routines with high memory usage
        if %complexity<memory_usage> > 100 {
            $benefit *= 0.95;
        }
        
        $benefit
    }
    
    # Inline a function call
    method inline_call($call) {
        # Use the can_inline_call method to determine if we should inline
        return $call unless self.can_inline_call($call);
        
        # Get the target and routine (we know they're valid from can_inline_call)
        my $target := $call.target;
        my $routine := $target.compile-time-value;
        my $body := $routine.body;
        
        # Create parameter map for substitution
        my %param_map;
        my @params := $routine.params;
        my @args := $call.args;
        
        # Map positional parameters
        my int $i := 0;
        while $i < @params && $i < @args {
            my $param := @params[$i];
            my $arg := @args[$i];
            if $param.name {
                # Handle default values if argument is not provided
                if $arg {
                    %param_map{$param.name} := $arg.clone;
                } elsif $param.has_default {
                    %param_map{$param.name} := $param.default.clone;
                }
            }
            $i++;
        }
        
        # Map named parameters
        for @args -> $arg {
            next unless nqp::istype($arg, QAST::Named);
            my $name := $arg.name;
            if $name {
                for @params -> $param {
                    if $param.name && $param.name eq $name {
                        %param_map{$param.name} := $arg.value.clone;
                        last;
                    }
                }
            }
        }
        
        # Clone the body for inlining
        my $inlined_body := $body.clone;
        
        # Replace parameters with arguments
        $inlined_body.walk(-> $node {
            if nqp::istype($node, QAST::Var) && $node.scope eq 'lexical' && $node.name && %param_map{$node.name} {
                return %param_map{$node.name}.clone;
            }
            1
        });
        
        # Handle return statements
        $inlined_body.walk(-> $node is rw {
            if nqp::istype($node, QAST::Op) && $node.op eq 'return' {
                # Replace return with just the return value
                if $node.args {
                    $node := $node.args[0];
                } else {
                    $node := QAST::Val.new(:value(Nil));
                }
            }
            1
        });
        
        # Perform post-inlining optimizations
        $inlined_body := self.optimize_inlined_body($inlined_body);
        
        # Return the inlined body
        $inlined_body
    }
    
    # Optimize an inlined function body
    method optimize_inlined_body($body) {
        # If optimization level is high enough, perform additional optimizations
        return $body unless $!optimization_level >= 3;
        
        # Perform constant folding on the inlined body
        $body.walk(-> $node is rw {
            if nqp::istype($node, QAST::Op) {
                my $folded := self.constant_fold($node, $node.op);
                $node := $folded if $folded;
            }
            1
        });
        
        # Perform dead code elimination
        if nqp::istype($body, QAST::Block) || nqp::istype($body, QAST::Stmts) {
            my @optimized_stmts;
            for $body.list() -> $stmt {
                # Skip null operations or statements that have no effect
                unless nqp::istype($stmt, QAST::Op) && $stmt.op eq 'null' {
                    @optimized_stmts.push($stmt);
                }
            }
            
            # Replace the body's statements if we removed any
            if @optimized_stmts.elems < $body.list().elems {
                my $new_body := nqp::istype($body, QAST::Block) 
                    ?? QAST::Block.new() 
                    !! QAST::Stmts.new();
                
                # Copy attributes from original body
                for nqp::getattr($body, QAST::Node, '$!attrs').hash() -> $key, $value {
                    nqp::setattr($new_body, QAST::Node, '$!attrs')<{$key}> := $value;
                }
                
                # Add optimized statements
                for @optimized_stmts -> $stmt {
                    $new_body.push($stmt);
                }
                
                return $new_body;
            }
        }
        
        # Return the optimized body
        $body
    }
    
    # Optimize variable bindings
    method optimize_binding($bind) {
        # Only perform binding optimizations at appropriate optimization levels
        return nqp::null() unless $!optimization_level >= 1;
        
        # Check if this is a binding operation
        return nqp::null() unless $bind.op eq 'bind';
        
        my $target := $bind.args[0];
        my $value := $bind.args[1];
        
        # Basic sanity check
        return nqp::null() unless $target && $value;
        
        # Optimization 1: Direct constant binding
        if nqp::istype($target, QAST::Var) && $target.scope eq 'lexical' && $target.name {
            # If binding a constant to a lexical variable, check if we can optimize
            if nqp::istype($value, QAST::Val) || nqp::istype($value, QAST::IVal) || 
               nqp::istype($value, QAST::NVal) || nqp::istype($value, QAST::SVal) || 
               nqp::istype($value, QAST::BVal) {
                
                # For read-only variables, we can optimize more aggressively
                if $target.readonly || $target.is-constant {
                    # Record the constant value for potential constant propagation
                    self.add_constant_value($target.name, $value);
                    
                    # Return a simplified binding operation
                    return QAST::Op.new(
                        :op('bind'),
                        $target,
                        $value
                    );
                } else {
                    # Even for mutable variables, we can track the initial constant value
                    self.add_constant_value($target.name, $value);
                }
            }
            
            # Optimization 5: Detect and optimize repeated bindings to the same variable
            # If this variable was previously bound and we can prove the value is the same
            if $!symbols.has_var($target.name) {
                my $prev_value := $!symbols.get_var_value($target.name);
                if $prev_value && self.are_values_identical($prev_value, $value) {
                    # Repeated binding to the same value can be eliminated
                    return QAST::Op.new(:op('null'));
                }
            }
        }
        
        # Optimization 2: Avoid unnecessary container creation
        if nqp::istype($target, QAST::Var) {
            # For scalar variables
            if $target.sigil eq '$' {
                # Check if the value is a simple scalar value that doesn't need a container
                if !nqp::istype($value, QAST::Op) || 
                   ($value.op eq 'call' || $value.op eq 'callmethod') && !$value.needs-container {
                    # Mark the binding as not needing a container
                    $bind.needs-container := 0;
                    return $bind;
                }
            }
            # For array and hash variables, avoid unnecessary container wrapping
            elsif $target.sigil eq '@' || $target.sigil eq '%' {
                # If binding directly to a list/hash constructor, we can optimize container creation
                if nqp::istype($value, QAST::Op) && ($value.op eq 'list' || $value.op eq 'hash') {
                    $bind.needs-container := 0;
                    return $bind;
                }
            }
        }
        
        # Optimization 3: Binding to self
        if nqp::istype($target, QAST::Var) && $target.name eq 'self' {
            # Self bindings are special cases
            if nqp::istype($value, QAST::Var) && $value.name eq 'self' {
                # Binding self to self is redundant and can be removed
                return QAST::Op.new(:op('null'));
            }
            
            # For other self bindings, mark for special handling
            $bind.is-self-binding := 1;
            
            # In some cases, we might be able to avoid container creation for self
            if !$value.needs-container {
                $bind.needs-container := 0;
            }
            
            return $bind;
        }
        
        # Optimization 4: Check if binding to the same value
        if self.are_values_identical($target, $value) {
            # Binding a value to itself is unnecessary
            return QAST::Op.new(:op('null'));
        }
        
        # Optimization 5: Binding to constants
        if nqp::istype($target, QAST::Var) && $target.scope eq 'lexical' && 
           (nqp::istype($value, QAST::Val) || nqp::istype($value, QAST::IVal) ||
            nqp::istype($value, QAST::NVal) || nqp::istype($value, QAST::SVal) ||
            nqp::istype($value, QAST::BVal)) {
            # Store constant value information for future optimizations
            if nqp::istype($value, QAST::IVal) {
                $!symbols.set_var_constant_value($target.name, $value.value);
            }
            elsif nqp::istype($value, QAST::NVal) {
                $!symbols.set_var_constant_value($target.name, $value.value);
            }
            elsif nqp::istype($value, QAST::SVal) {
                $!symbols.set_var_constant_value($target.name, $value.value);
            }
            elsif nqp::istype($value, QAST::BVal) {
                $!symbols.set_var_constant_value($target.name, $value.value);
            }
            
            # Mark binding as constant
            $bind.is-constant-binding := 1;
        }
        
        # Optimization 6: Type compatibility optimization
        # If we know the type of both target and value, we can optimize container handling
        if $!level >= 2 && nqp::istype($target, QAST::Var) {
            my $target_type := $!symbols.get_var_type($target.name);
            my $value_type := self.infer_expr_type($value);
            
            if $target_type && $value_type && $target_type === $value_type {
                # If types match exactly, we can optimize container handling
                $bind.type-checked := 1;
                return $bind;
            }
        }
        
        # Optimization 7: Bindings that can be completely removed
        if nqp::istype($target, QAST::Var) && $target.name {
            # If this is a lexical variable that is never used after this binding
            if $target.scope eq 'lexical' && !self.is_variable_used_after($target.name, $bind) {
                return QAST::Op.new(:op('null'));
            }
        }
        
        # No optimizations applicable
        return nqp::null()
    }
    
    # Check if a variable is used after a certain node in the current scope
    method is_variable_used_after($var_name, $node) {
        # This is a simplified implementation that checks if the variable is used
        # in subsequent siblings of the node
        
        # Find the parent of the node
        my $parent := $node.parent;
        return 1 unless $parent && nqp::can($parent, 'list');
        
        # Find the index of the current node
        my @siblings := $parent.list();
        my int $node_index := -1;
        
        for @siblings.kv -> $i, $sibling {
            if $sibling =:= $node {
                $node_index := $i;
                last;
            }
        }
        
        return 1 if $node_index < 0; # Node not found, assume it's used
        
        # Check subsequent siblings for usage of the variable
        for @siblings[$node_index + 1..*] -> $sibling {
            if self.contains_variable($sibling, $var_name) {
                return 1; # Variable is used after the node
            }
        }
        
        # Variable is not used after the node
        0
    }
    
    # Check if a node contains a reference to a specific variable
    method contains_variable($node, $var_name) {
        # Base case: node is a variable with the given name
        if nqp::istype($node, QAST::Var) && $node.name eq $var_name {
            return 1;
        }
        
        # Check children recursively
        if $node && nqp::can($node, 'list') {
            for $node.list() -> $child {
                if self.contains_variable($child, $var_name) {
                    return 1;
                }
            }
        }
        
        # No reference found
        0
    }
    
    # Helper method to check if two values are identical
    method are_values_identical($val1, $val2) {
        # Check if they are the same node
        return 1 if $val1 =:= $val2;
        
        # Check if both are constant nodes with the same value
        if (nqp::istype($val1, QAST::Val) && nqp::istype($val2, QAST::Val)) ||
           (nqp::istype($val1, QAST::IVal) && nqp::istype($val2, QAST::IVal)) ||
           (nqp::istype($val1, QAST::NVal) && nqp::istype($val2, QAST::NVal)) ||
           (nqp::istype($val1, QAST::SVal) && nqp::istype($val2, QAST::SVal)) ||
           (nqp::istype($val1, QAST::BVal) && nqp::istype($val2, QAST::BVal)) {
            return $val1.value === $val2.value;
        }
        
        # Check if both are the same variable
        if nqp::istype($val1, QAST::Var) && nqp::istype($val2, QAST::Var) {
            return $val1.name eq $val2.name && $val1.scope eq $val2.scope;
        }
        
        # Default to not identical
        0
    }

    # Visit a QAST::Op node
    method visit_op($op, :$block_structure = False) {
        my str $optype := $op.op;
        my str $opname := $op.name;
        
        # Visit children
        self.visit_op_children($op);
        
        # Perform optimizations based on op type
        my $optimized := self.optimize_op($op, $optype, $opname, :$block_structure);
        
        $optimized || $op
    }

    # Visit children of an op node
    method visit_op_children($op) {
        my int $orig_void := $!void_context;
        $!void_context    := $op.op eq 'callmethod' && $op.name eq 'sink';
        self.visit_children($op);
        $!void_context := $orig_void;
    }

    # Optimize a QAST::Op node with enhanced optimization techniques
    method optimize_op($op, $optype, $opname, :$block_structure = False) {
        # Apply type inference first to enable other optimizations
        if $!level >= 2 {
            self.infer_types($op);
        }
        
        # Apply constant propagation for higher optimization levels
        if $!level >= 2 {
            $op := self.constant_propagation($op);
        }
        
        # Apply enhanced constant folding with short-circuit evaluation
        my $folded := self.constant_fold($op, $optype);
        return $folded if $folded;
        
        # Apply operator-specific optimizations
        my $op_optimized := self.optimize_specific_op($op, $optype, $opname);
        return $op_optimized if $op_optimized;
        
        # Apply method call optimizations
        my $call_optimized := self.optimize_method_call($op, $optype, $opname);
        return $call_optimized if $call_optimized;
        
        # Apply dead code elimination with our new implementation
        if $!level >= 2 && ($optype eq 'if' || $optype eq 'unless' || $optype eq 'for' || $optype eq 'while' || $optype eq 'until') {
            my $dead_code := self.dead_code_elimination($op);
            return $dead_code if !nqp::eqaddr($dead_code, $op);
        }
        
        # Apply loop optimizations
        my $loop_optimized := self.optimize_loops($op);
        return $loop_optimized if $loop_optimized;
        
        # Apply type-based optimizations
        my $type_optimized := self.type_based_optimizations($op);
        return $type_optimized if $type_optimized;
        
        # Apply memory optimizations
        my $mem_optimized := self.optimize_memory_usage($op);
        return $mem_optimized if $mem_optimized;
        
        # New: Apply algebraic simplifications (level 3+)
        if $!level >= 3 {
            my $algebraic := self.apply_algebraic_simplifications($op, $optype);
            return $algebraic if $algebraic;
        }
        
        # New: Apply strength reduction (level 4+)
        if $!level >= 4 {
            my $strength_reduced := self.apply_strength_reduction($op, $optype);
            return $strength_reduced if $strength_reduced;
        }
        
        nqp::null() # No optimization applied
    }
    
    # Apply algebraic simplifications
    method apply_algebraic_simplifications($op, $optype) {
        # Examples of algebraic simplifications:
        # x + 0 = x
        # x * 1 = x
        # x * 0 = 0
        # x - 0 = x
        # x / 1 = x
        
        # Addition with zero
        if ($optype eq 'add_i' || $optype eq 'add_n') {
            if nqp::istype($op[1], QAST::IVal) && $op[1].value == 0 || 
               nqp::istype($op[1], QAST::NVal) && $op[1].value == 0.0 {
                return $op[0];
            }
            elsif nqp::istype($op[0], QAST::IVal) && $op[0].value == 0 || 
                  nqp::istype($op[0], QAST::NVal) && $op[0].value == 0.0 {
                return $op[1];
            }
        }
        
        # Multiplication with one
        elsif ($optype eq 'mul_i' || $optype eq 'mul_n') {
            if nqp::istype($op[1], QAST::IVal) && $op[1].value == 1 || 
               nqp::istype($op[1], QAST::NVal) && $op[1].value == 1.0 {
                return $op[0];
            }
            elsif nqp::istype($op[0], QAST::IVal) && $op[0].value == 1 || 
                  nqp::istype($op[0], QAST::NVal) && $op[0].value == 1.0 {
                return $op[1];
            }
            # Multiplication with zero
            elsif (nqp::istype($op[1], QAST::IVal) && $op[1].value == 0 || 
                   nqp::istype($op[1], QAST::NVal) && $op[1].value == 0.0) {
                return QAST::IVal.new(:value(0));
            }
            elsif (nqp::istype($op[0], QAST::IVal) && $op[0].value == 0 || 
                   nqp::istype($op[0], QAST::NVal) && $op[0].value == 0.0) {
                return QAST::IVal.new(:value(0));
            }
        }
        
        # Subtraction with zero
        elsif ($optype eq 'sub_i' || $optype eq 'sub_n') {
            if nqp::istype($op[1], QAST::IVal) && $op[1].value == 0 || 
               nqp::istype($op[1], QAST::NVal) && $op[1].value == 0.0 {
                return $op[0];
            }
        }
        
        # Division by one
        elsif ($optype eq 'div_i' || $optype eq 'div_n') {
            if nqp::istype($op[1], QAST::IVal) && $op[1].value == 1 || 
               nqp::istype($op[1], QAST::NVal) && $op[1].value == 1.0 {
                return $op[0];
            }
        }
        
        nqp::null()
    }
    
    # Apply strength reduction
    method apply_strength_reduction($op, $optype) {
        # Examples of strength reduction:
        # x * 2 = x + x (or shift left by 1)
        # x * 4 = x << 2
        # x / 2 = x >> 1 (for integers)
        
        # Multiplication by powers of 2 for integers
        if $optype eq 'mul_i' && nqp::istype($op[1], QAST::IVal) {
            my $val := $op[1].value;
            # Check if value is a power of 2
            if $val > 0 && ($val & ($val - 1)) == 0 {
                # Calculate the exponent
                my $shift := 0;
                my $temp := $val;
                while $temp > 1 {
                    $temp := $temp / 2;
                    $shift := $shift + 1;
                }
                # Replace with shift operation
                my $result := QAST::Op.new(:op<shl_i>, $op[0], QAST::IVal.new(:value($shift)));
                self.log_optimization('strength_reduction', $op, $result);
                return $result;
            }
        }
        # Division by powers of 2 for integers (unsigned division)
        elsif $optype eq 'div_i' && nqp::istype($op[1], QAST::IVal) && $op[1].value > 0 {
            my $val := $op[1].value;
            # Check if value is a power of 2
            if ($val & ($val - 1)) == 0 {
                # Calculate the exponent
                my $shift := 0;
                my $temp := $val;
                while $temp > 1 {
                    $temp := $temp / 2;
                    $shift := $shift + 1;
                }
                # Replace with shift operation (simplified, real implementation would handle signed division)
                my $result := QAST::Op.new(:op<shr_i>, $op[0], QAST::IVal.new(:value($shift)));
                self.log_optimization('strength_reduction', $op, $result);
                return $result;
            }
        }
        
        nqp::null()
    }
    
    # Optimize specific operators with identity and absorption properties
    method optimize_specific_op($op, $optype, $opname) {
        # Record optimization start time (for detailed logging)
        my $start_time := nqp::time_n();
        my $optimized := nqp::null();
        
        # 1. Integer operation optimization
        if $optype eq 'add_i' {
            $optimized := self.optimize_int_add($op);
        }
        elsif $optype eq 'sub_i' {
            $optimized := self.optimize_int_sub($op);
        }
        elsif $optype eq 'mul_i' {
            $optimized := self.optimize_int_mul($op);
        }
        elsif $optype eq 'div_i' {
            $optimized := self.optimize_int_div($op);
        }
        elsif $optype eq 'mod_i' {
            $optimized := self.optimize_int_mod($op);
        }
        # 2. Float operation optimization
        elsif $optype eq 'add_n' {
            $optimized := self.optimize_float_add($op);
        }
        elsif $optype eq 'sub_n' {
            $optimized := self.optimize_float_sub($op);
        }
        elsif $optype eq 'mul_n' {
            $optimized := self.optimize_float_mul($op);
        }
        elsif $optype eq 'div_n' {
            $optimized := self.optimize_float_div($op);
        }
        # 3. String operation optimization
        elsif $optype eq 'concat_s' {
            $optimized := self.optimize_str_concat($op);
        }
        # 4. Logical operation optimization
        elsif $optype eq 'and_b' {
            $optimized := self.optimize_logical_and($op);
        }
        elsif $optype eq 'or_b' {
            $optimized := self.optimize_logical_or($op);
        }
        elsif $optype eq 'not_b' {
            $optimized := self.optimize_logical_not($op);
        }
        # 5. Comparison operation optimization
        elsif $optype eq 'eq_i' || $optype eq 'eq_n' || $optype eq 'eq_s' {
            $optimized := self.optimize_equality($op, $optype);
        }
        elsif $optype eq 'ne_i' || $optype eq 'ne_n' || $optype eq 'ne_s' {
            $optimized := self.optimize_inequality($op, $optype);
        }
        # 6. Added: Greater than/Less than comparison optimization
        elsif $optype eq 'gt_i' || $optype eq 'gt_n' || $optype eq 'lt_i' || $optype eq 'lt_n' {
            $optimized := self.optimize_comparison($op, $optype);
        }
        # 7. Added: Bitwise operation optimization
        elsif $optype eq 'bit_and_i' || $optype eq 'bit_or_i' || $optype eq 'bit_xor_i' || $optype eq 'bit_not_i' {
            $optimized := self.optimize_bitwise($op, $optype);
        }
        # 8. Added: Compound expression optimization
        elsif $!level >= 3 {
            $optimized := self.optimize_compound_expression($op, $optype);
        }
        
        # Log optimization
        if $optimized {
            my $duration := nqp::time_n() - $start_time;
            self.log_optimization(
                :description("Operator optimization: $optype"),
                :original-type($optype),
                :optimized-type($optimized.WHAT.name),
                :original-size(self.estimate_node_size($op)),
                :optimized-size(self.estimate_node_size($optimized)),
                :additional-info("Operator: $optype, Name: $opname"),
                :duration($duration)
            );
        }
        
        $optimized
    }
    
    # Integer addition optimization
    method optimize_int_add($op) {
        # Addition identity: x + 0 = x
        if nqp::istype($op[1], QAST::IVal) && $op[1].value == 0 {
            return $op[0];
        }
        elsif nqp::istype($op[0], QAST::IVal) && $op[0].value == 0 {
            return $op[1];
        }
        # Added: Constant folding
        elsif nqp::istype($op[0], QAST::IVal) && nqp::istype($op[1], QAST::IVal) {
            return QAST::IVal.new(:value($op[0].value + $op[1].value));
        }
        nqp::null()
    }
    
    # Integer subtraction optimization
    method optimize_int_sub($op) {
        # Subtraction optimization: x - 0 = x, x - x = 0
        if nqp::istype($op[1], QAST::IVal) && $op[1].value == 0 {
            return $op[0];
        }
        elsif nqp::istype($op[0], QAST::IVal) && nqp::istype($op[1], QAST::IVal) && 
              $op[0].value == $op[1].value {
            return QAST::IVal.new(:value(0));
        }
        # Added: Constant folding
        elsif nqp::istype($op[0], QAST::IVal) && nqp::istype($op[1], QAST::IVal) {
            return QAST::IVal.new(:value($op[0].value - $op[1].value));
        }
        nqp::null()
    }
    
    # Integer multiplication optimization
    method optimize_int_mul($op) {
        # Multiplication identity: x * 1 = x
        if nqp::istype($op[1], QAST::IVal) && $op[1].value == 1 {
            return $op[0];
        }
        elsif nqp::istype($op[0], QAST::IVal) && $op[0].value == 1 {
            return $op[1];
        }
        # Multiplication by zero: x * 0 = 0
        elsif (nqp::istype($op[0], QAST::IVal) && $op[0].value == 0 ||
              nqp::istype($op[1], QAST::IVal) && $op[1].value == 0) {
            return QAST::IVal.new(:value(0));
        }
        # Added: Constant folding
        elsif nqp::istype($op[0], QAST::IVal) && nqp::istype($op[1], QAST::IVal) {
            return QAST::IVal.new(:value($op[0].value * $op[1].value));
        }
        # Added: Multiplication associativity optimization
        elsif $!level >= 2 && nqp::istype($op[0], QAST::Op) && $op[0].op eq 'mul_i' && nqp::istype($op[0][1], QAST::IVal) {
            # (a * const) * b => a * (const * b) when b is also a constant
            if nqp::istype($op[1], QAST::IVal) {
                my $new_const := QAST::IVal.new(:value($op[0][1].value * $op[1].value));
                return QAST::Op.new(:op('mul_i'), $op[0][0], $new_const);
            }
        }
        nqp::null()
    }
    
    # Integer division optimization
    method optimize_int_div($op) {
        # Division optimization: x / 1 = x
        if nqp::istype($op[1], QAST::IVal) && $op[1].value == 1 {
            return $op[0];
        }
        elsif nqp::istype($op[0], QAST::IVal) && nqp::istype($op[1], QAST::IVal) && 
              $op[0].value == $op[1].value && $op[1].value != 0 {
            return QAST::IVal.new(:value(1));
        }
        # Added: Constant folding
        elsif nqp::istype($op[0], QAST::IVal) && nqp::istype($op[1], QAST::IVal) && $op[1].value != 0 {
            return QAST::IVal.new(:value($op[0].value div $op[1].value));
        }
        nqp::null()
    }
    
    # Integer modulus optimization
    method optimize_int_mod($op) {
        if nqp::istype($op[1], QAST::IVal) && $op[1].value == 1 {
            return QAST::IVal.new(:value(0));
        }
        elsif nqp::istype($op[0], QAST::IVal) && nqp::istype($op[1], QAST::IVal) && 
              $op[0].value == $op[1].value && $op[1].value != 0 {
            return QAST::IVal.new(:value(0));
        }
        elsif nqp::istype($op[0], QAST::IVal) && $op[0].value == 0 && 
              nqp::istype($op[1], QAST::IVal) && $op[1].value != 0 {
            return QAST::IVal.new(:value(0));
        }
        # Added: Constant folding
        elsif nqp::istype($op[0], QAST::IVal) && nqp::istype($op[1], QAST::IVal) && $op[1].value != 0 {
            return QAST::IVal.new(:value($op[0].value % $op[1].value));
        }
        nqp::null()
    }
    
    # Float addition optimization
    method optimize_float_add($op) {
        if nqp::istype($op[1], QAST::NVal) && $op[1].value == 0.0 {
            return $op[0];
        }
        elsif nqp::istype($op[0], QAST::NVal) && $op[0].value == 0.0 {
            return $op[1];
        }
        # Added: Constant folding
        elsif nqp::istype($op[0], QAST::NVal) && nqp::istype($op[1], QAST::NVal) {
            return QAST::NVal.new(:value($op[0].value + $op[1].value));
        }
        nqp::null()
    }
    
    # Float subtraction optimization
    method optimize_float_sub($op) {
        if nqp::istype($op[1], QAST::NVal) && $op[1].value == 0.0 {
            return $op[0];
        }
        elsif nqp::istype($op[0], QAST::NVal) && nqp::istype($op[1], QAST::NVal) && 
              $op[0].value == $op[1].value {
            return QAST::NVal.new(:value(0.0));
        }
        # Added: Constant folding
        elsif nqp::istype($op[0], QAST::NVal) && nqp::istype($op[1], QAST::NVal) {
            return QAST::NVal.new(:value($op[0].value - $op[1].value));
        }
        nqp::null()
    }
    
    # Float multiplication optimization
    method optimize_float_mul($op) {
        if nqp::istype($op[1], QAST::NVal) && $op[1].value == 1.0 {
            return $op[0];
        }
        elsif nqp::istype($op[0], QAST::NVal) && $op[0].value == 1.0 {
            return $op[1];
        }
        # Multiplication by zero: x * 0 = 0
        elsif (nqp::istype($op[0], QAST::NVal) && $op[0].value == 0.0 ||
              nqp::istype($op[1], QAST::NVal) && $op[1].value == 0.0) {
            return QAST::NVal.new(:value(0.0));
        }
        # Added: Constant folding
        elsif nqp::istype($op[0], QAST::NVal) && nqp::istype($op[1], QAST::NVal) {
            return QAST::NVal.new(:value($op[0].value * $op[1].value));
        }
        nqp::null()
    }
    
    # Float division optimization
    method optimize_float_div($op) {
        if nqp::istype($op[1], QAST::NVal) && $op[1].value == 1.0 {
            return $op[0];
        }
        elsif nqp::istype($op[0], QAST::NVal) && nqp::istype($op[1], QAST::NVal) && 
              $op[0].value == $op[1].value && $op[1].value != 0.0 {
            return QAST::NVal.new(:value(1.0));
        }
        # Added: Constant folding
        elsif nqp::istype($op[0], QAST::NVal) && nqp::istype($op[1], QAST::NVal) && $op[1].value != 0.0 {
            return QAST::NVal.new(:value($op[0].value / $op[1].value));
        }
        nqp::null()
    }
    
    # String concatenation optimization
    method optimize_str_concat($op) {
        # Concatenate with empty string: x ~ "" = x, "" ~ x = x
        if nqp::istype($op[1], QAST::SVal) && $op[1].value eq "" {
            return $op[0];
        }
        elsif nqp::istype($op[0], QAST::SVal) && $op[0].value eq "" {
            return $op[1];
        }
        # Added: Constant folding
        elsif nqp::istype($op[0], QAST::SVal) && nqp::istype($op[1], QAST::SVal) {
            return QAST::SVal.new(:value($op[0].value ~ $op[1].value));
        }
        # Added: String concatenation chain optimization
        elsif $!level >= 2 && nqp::istype($op[0], QAST::Op) && $op[0].op eq 'concat_s' && nqp::istype($op[0][1], QAST::SVal) {
            # (a ~ const) ~ b => a ~ (const ~ b) when b is also a constant
            if nqp::istype($op[1], QAST::SVal) {
                my $new_const := QAST::SVal.new(:value($op[0][1].value ~ $op[1].value));
                return QAST::Op.new(:op('concat_s'), $op[0][0], $new_const);
            }
        }
        nqp::null()
    }
    
    # Logical AND optimization
    method optimize_logical_and($op) {
        # Logical AND with true: x && true = x
        if nqp::istype($op[1], QAST::BVal) && $op[1].value == 1 {
            return $op[0];
        }
        elsif nqp::istype($op[0], QAST::BVal) && $op[0].value == 1 {
            return $op[1];
        }
        # Logical AND with false: x && false = false
        elsif (nqp::istype($op[0], QAST::BVal) && $op[0].value == 0 ||
              nqp::istype($op[1], QAST::BVal) && $op[1].value == 0) {
            return QAST::BVal.new(:value(0));
        }
        # Added: Constant folding
        elsif nqp::istype($op[0], QAST::BVal) && nqp::istype($op[1], QAST::BVal) {
            return QAST::BVal.new(:value($op[0].value && $op[1].value));
        }
        # Added: Short-circuit logic optimization
        elsif $!level >= 3 && nqp::istype($op[0], QAST::Op) && $op[0].op eq 'and_b' && nqp::istype($op[0][1], QAST::BVal) && $op[0][1].value == 0 {
            # ((a && false) && b) => false
            return QAST::BVal.new(:value(0));
        }
        nqp::null()
    }
    
    # Logical OR optimization
    method optimize_logical_or($op) {
        # Logical OR with false: x || false = x
        if nqp::istype($op[1], QAST::BVal) && $op[1].value == 0 {
            return $op[0];
        }
        elsif nqp::istype($op[0], QAST::BVal) && $op[0].value == 0 {
            return $op[1];
        }
        # Logical OR with true: x || true = true
        elsif (nqp::istype($op[0], QAST::BVal) && $op[0].value == 1 ||
              nqp::istype($op[1], QAST::BVal) && $op[1].value == 1) {
            return QAST::BVal.new(:value(1));
        }
        # Added: Constant folding
        elsif nqp::istype($op[0], QAST::BVal) && nqp::istype($op[1], QAST::BVal) {
            return QAST::BVal.new(:value($op[0].value || $op[1].value));
        }
        # Added: Short-circuit logic optimization
        elsif $!level >= 3 && nqp::istype($op[0], QAST::Op) && $op[0].op eq 'or_b' && nqp::istype($op[0][1], QAST::BVal) && $op[0][1].value == 1 {
            # ((a || true) || b) => true
            return QAST::BVal.new(:value(1));
        }
        nqp::null()
    }
    
    # Logical NOT optimization
    method optimize_logical_not($op) {
        # Double negation: !!x = x
        if nqp::istype($op[0], QAST::Op) && $op[0].op eq 'not_b' {
            return $op[0][0];
        }
        # Negate a constant
        elsif nqp::istype($op[0], QAST::BVal) {
            return QAST::BVal.new(:value(!$op[0].value));
        }
        # Added: Negation optimization for comparison operations
        elsif $!level >= 3 && nqp::istype($op[0], QAST::Op) {
            given $op[0].op {
                when 'eq_i' | 'eq_n' | 'eq_s' {
                    # !(a == b) => a != b
                    my $new_op := $op[0].op.subst('eq', 'ne');
                    return QAST::Op.new(:op($new_op), |$op[0].list);
                }
                when 'ne_i' | 'ne_n' | 'ne_s' {
                    # !(a != b) => a == b
                    my $new_op := $op[0].op.subst('ne', 'eq');
                    return QAST::Op.new(:op($new_op), |$op[0].list);
                }
                when 'gt_i' | 'gt_n' {
                    # !(a > b) => a <= b
                    return QAST::Op.new(:op('le_' ~ $op[0].op.substr(*-1)), |$op[0].list);
                }
                when 'lt_i' | 'lt_n' {
                    # !(a < b) => a >= b
                    return QAST::Op.new(:op('ge_' ~ $op[0].op.substr(*-1)), |$op[0].list);
                }
            }
        }
        nqp::null()
    }
    
    # Equality comparison optimization
    method optimize_equality($op, $optype) {
        # x == x is always true
        if $op[0] =:= $op[1] {
            return QAST::BVal.new(:value(1));
        }
        # Added: Constant comparison
        elsif (nqp::istype($op[0], QAST::IVal) && nqp::istype($op[1], QAST::IVal)) ||
              (nqp::istype($op[0], QAST::NVal) && nqp::istype($op[1], QAST::NVal)) ||
              (nqp::istype($op[0], QAST::SVal) && nqp::istype($op[1], QAST::SVal)) {
            my $result = 0;
            if $optype ~~ /^eq_i$/ {
                $result = $op[0].value == $op[1].value ?? 1 !! 0;
            }
            elsif $optype ~~ /^eq_n$/ {
                $result = $op[0].value == $op[1].value ?? 1 !! 0;
            }
            elsif $optype ~~ /^eq_s$/ {
                $result = $op[0].value eq $op[1].value ?? 1 !! 0;
            }
            return QAST::BVal.new(:value($result));
        }
        nqp::null()
    }
    
    # Inequality comparison optimization
    method optimize_inequality($op, $optype) {
        # x != x is always false
        if $op[0] =:= $op[1] {
            return QAST::BVal.new(:value(0));
        }
        # Added: Constant comparison
        elsif (nqp::istype($op[0], QAST::IVal) && nqp::istype($op[1], QAST::IVal)) ||
              (nqp::istype($op[0], QAST::NVal) && nqp::istype($op[1], QAST::NVal)) ||
              (nqp::istype($op[0], QAST::SVal) && nqp::istype($op[1], QAST::SVal)) {
            my $result = 0;
            if $optype ~~ /^ne_i$/ {
                $result = $op[0].value != $op[1].value ?? 1 !! 0;
            }
            elsif $optype ~~ /^ne_n$/ {
                $result = $op[0].value != $op[1].value ?? 1 !! 0;
            }
            elsif $optype ~~ /^ne_s$/ {
                $result = $op[0].value ne $op[1].value ?? 1 !! 0;
            }
            return QAST::BVal.new(:value($result));
        }
        nqp::null()
    }
    
    # Added: Greater than/Less than comparison optimization
    method optimize_comparison($op, $optype) {
        # Constant comparison
        if (nqp::istype($op[0], QAST::IVal) && nqp::istype($op[1], QAST::IVal)) ||
           (nqp::istype($op[0], QAST::NVal) && nqp::istype($op[1], QAST::NVal)) {
            my $result = 0;
            if $optype ~~ /^gt_i$/ {
                $result = $op[0].value > $op[1].value ?? 1 !! 0;
            }
            elsif $optype ~~ /^gt_n$/ {
                $result = $op[0].value > $op[1].value ?? 1 !! 0;
            }
            elsif $optype ~~ /^lt_i$/ {
                $result = $op[0].value < $op[1].value ?? 1 !! 0;
            }
            elsif $optype ~~ /^lt_n$/ {
                $result = $op[0].value < $op[1].value ?? 1 !! 0;
            }
            return QAST::BVal.new(:value($result));
        }
        nqp::null()
    }
    
    # Added: Bitwise operation optimization
    method optimize_bitwise($op, $optype) {
        # Bitwise AND optimization: x & 0 = 0, x & -1 = x (assuming -1 is all 1s)
        if $optype eq 'bit_and_i' {
            if nqp::istype($op[1], QAST::IVal) {
                if $op[1].value == 0 {
                    return QAST::IVal.new(:value(0));
                }
                elsif $op[1].value == -1 {
                    return $op[0];
                }
            }
            elsif nqp::istype($op[0], QAST::IVal) {
                if $op[0].value == 0 {
                    return QAST::IVal.new(:value(0));
                }
                elsif $op[0].value == -1 {
                    return $op[1];
                }
            }
            # Constant folding
            elsif nqp::istype($op[0], QAST::IVal) && nqp::istype($op[1], QAST::IVal) {
                return QAST::IVal.new(:value($op[0].value +& $op[1].value));
            }
        }
        # Bitwise OR optimization: x | 0 = x, x | -1 = -1
        elsif $optype eq 'bit_or_i' {
            if nqp::istype($op[1], QAST::IVal) {
                if $op[1].value == 0 {
                    return $op[0];
                }
                elsif $op[1].value == -1 {
                    return QAST::IVal.new(:value(-1));
                }
            }
            elsif nqp::istype($op[0], QAST::IVal) {
                if $op[0].value == 0 {
                    return $op[1];
                }
                elsif $op[0].value == -1 {
                    return QAST::IVal.new(:value(-1));
                }
            }
            # Constant folding
            elsif nqp::istype($op[0], QAST::IVal) && nqp::istype($op[1], QAST::IVal) {
                return QAST::IVal.new(:value($op[0].value +| $op[1].value));
            }
        }
        # Bitwise XOR optimization: x ^ 0 = x, x ^ x = 0
        elsif $optype eq 'bit_xor_i' {
            if nqp::istype($op[1], QAST::IVal) {
                if $op[1].value == 0 {
                    return $op[0];
                }
                elsif $op[0] =:= $op[1] {
                    return QAST::IVal.new(:value(0));
                }
            }
            elsif nqp::istype($op[0], QAST::IVal) {
                if $op[0].value == 0 {
                    return $op[1];
                }
            }
            # Constant folding
            elsif nqp::istype($op[0], QAST::IVal) && nqp::istype($op[1], QAST::IVal) {
                return QAST::IVal.new(:value($op[0].value +^ $op[1].value));
            }
        }
        # Bitwise NOT optimization: ~~x = x
        elsif $optype eq 'bit_not_i' {
            if nqp::istype($op[0], QAST::Op) && $op[0].op eq 'bit_not_i' {
                return $op[0][0];
            }
            # Constant folding
            elsif nqp::istype($op[0], QAST::IVal) {
                return QAST::IVal.new(:value(+^$op[0].value));
            }
        }
        nqp::null()
    }
    
    # Added: Compound expression optimization
    method optimize_compound_expression($op, $optype) {
        # Only enabled at high optimization levels
        if $!level < 3 {
            return nqp::null();
        }
        
        # (a + b) + c => a + (b + c) when b and c are constants
        if $optype eq 'add_i' && nqp::istype($op[0], QAST::Op) && $op[0].op eq 'add_i' && 
           nqp::istype($op[0][1], QAST::IVal) && nqp::istype($op[1], QAST::IVal) {
            my $new_const := QAST::IVal.new(:value($op[0][1].value + $op[1].value));
            return QAST::Op.new(:op('add_i'), $op[0][0], $new_const);
        }
        
        # Similarly, associativity optimization for multiplication
        elsif $optype eq 'mul_i' && nqp::istype($op[0], QAST::Op) && $op[0].op eq 'mul_i' && 
              nqp::istype($op[0][1], QAST::IVal) && nqp::istype($op[1], QAST::IVal) {
            my $new_const := QAST::IVal.new(:value($op[0][1].value * $op[1].value));
            return QAST::Op.new(:op('mul_i'), $op[0][0], $new_const);
        }
        
        # Distributive law optimization: a * (b + c) => a*b + a*c when a is a constant
        elsif $optype eq 'mul_i' && nqp::istype($op[1], QAST::Op) && $op[1].op eq 'add_i' && 
              nqp::istype($op[0], QAST::IVal) {
            my $a := $op[0];
            my $b := $op[1][0];
            my $c := $op[1][1];
            
            my $ab := QAST::Op.new(:op('mul_i'), $a, $b);
            my $ac := QAST::Op.new(:op('mul_i'), $a, $c);
            
            return QAST::Op.new(:op('add_i'), $ab, $ac);
        }
        
        nqp::null()
    }
    
    # Optimize method calls
    method optimize_method_call($op, $optype, $opname) {
        # Record optimization start time
        my $start_time := nqp::time_n();
        my $optimized := nqp::null();
        
        # Only perform method call optimization at optimization level >= 2
        if $!level < 2 {
            return nqp::null();
        }
        
        # Optimize common method calls
        if $optype eq 'callmethod' {
            my $method_name := $op.name;
            
            # 根据方法类型分发到不同的优化子方法
            $optimized := self.optimize_type_conversion_methods($op, $method_name);
            unless $optimized {
                $optimized := self.optimize_string_methods($op, $method_name);
            }
            unless $optimized {
                $optimized := self.optimize_array_methods($op, $method_name);
            }
            unless $optimized {
                $optimized := self.optimize_hash_methods($op, $method_name);
            }
            unless $optimized {
                $optimized := self.optimize_numeric_methods($op, $method_name);
            }
            unless $optimized {
                $optimized := self.optimize_boolean_methods($op, $method_name);
            }
            unless $optimized {
                $optimized := self.optimize_chain_method_calls($op, $method_name);
            }
            unless $optimized {
                $optimized := self.optimize_container_methods($op, $method_name);
            }
            unless $optimized && $!level < 4 {
                # Advanced optimizations only enabled at high optimization levels
                $optimized := self.optimize_advanced_method_calls($op, $method_name);
            }
        }
        
        # Log optimization
        if $optimized {
            my $duration := nqp::time_n() - $start_time;
            self.log_optimization(
                :description("Method call optimization: $op.name"),
                :original-type($optype),
                :optimized-type($optimized.WHAT.name),
                :original-size(self.estimate_node_size($op)),
                :optimized-size(self.estimate_node_size($optimized)),
                :additional-info("Method: $op.name, Args: $op.elems - 1"),
                :duration($duration)
            );
        }
        
        $optimized
    }
    
    # Type conversion method optimization
    method optimize_type_conversion_methods($op, $method_name) {
        # Optimize .Str method calls
        if $method_name eq 'Str' {
            if nqp::istype($op[0], QAST::SVal) {
                return $op[0]; # String to string conversion is redundant
            }
            # Str method call on empty containers
            elsif nqp::istype($op[0], QAST::Op) {
                if $op[0].op eq 'emptyarray' {
                    return QAST::SVal.new(:value('[]'));
                }
                elsif $op[0].op eq 'emptyhash' {
                    return QAST::SVal.new(:value('{}'));
                }
                # String representation of constant arrays
                elsif $!level >= 3 && ($op[0].op eq 'array' || $op[0].op eq 'constarray') {
                    my @str_values;
                    for $op[0].list -> $elem {
                        if nqp::istype($elem, QAST::SVal) {
                            @str_values.push("'" ~ $elem.value ~ "'");
                        }
                        elsif nqp::istype($elem, QAST::IVal) {
                            @str_values.push($elem.value.Str);
                        }
                        elsif nqp::istype($elem, QAST::NVal) {
                            @str_values.push($elem.value.Str);
                        }
                        elsif nqp::istype($elem, QAST::BVal) {
                            @str_values.push($elem.value ?? 'True' !! 'False');
                        }
                        else {
                            last; # Cannot fully optimize, abandon
                        }
                    }
                    if @str_values.elems == $op[0].elems {
                        return QAST::SVal.new(:value('["' ~ @str_values.join('", "') ~ '"]'));
                    }
                }
            }
        }
        
        # Optimize .Int on integer literals
        elsif $method_name eq 'Int' {
            if nqp::istype($op[0], QAST::IVal) {
                return $op[0]; # Integer to integer conversion is redundant
            }
            # String constant to integer conversion
            elsif $!level >= 3 && nqp::istype($op[0], QAST::SVal) {
                my $int_val := $op[0].value.Int;
                if $int_val.defined {
                    return QAST::IVal.new(:value($int_val));
                }
            }
            # Float constant to integer conversion
            elsif $!level >= 2 && nqp::istype($op[0], QAST::NVal) {
                return QAST::IVal.new(:value($op[0].value.Int));
            }
        }
        
        # Optimize .Num on number literals
        elsif $method_name eq 'Num' {
            if nqp::istype($op[0], QAST::NVal) {
                return $op[0]; # Floating point to floating point conversion is redundant
            }
            # Integer constant to floating point conversion
            elsif $!level >= 2 && nqp::istype($op[0], QAST::IVal) {
                return QAST::NVal.new(:value($op[0].value.Num));
            }
            # String constant to floating point conversion
            elsif $!level >= 3 && nqp::istype($op[0], QAST::SVal) {
                my $num_val := $op[0].value.Num;
                if $num_val.defined {
                    return QAST::NVal.new(:value($num_val));
                }
            }
        }
        
        # Optimize .Bool method calls
        elsif $method_name eq 'Bool' || $method_name eq 'so' {
            if nqp::istype($op[0], QAST::BVal) {
                return $op[0];
            }
            elsif nqp::istype($op[0], QAST::IVal) {
                return QAST::BVal.new(:value($op[0].value != 0 ?? 1 !! 0));
            }
            elsif nqp::istype($op[0], QAST::SVal) {
                return QAST::BVal.new(:value($op[0].value.chars != 0 ?? 1 !! 0));
            }
            elsif nqp::istype($op[0], QAST::NVal) {
                return QAST::BVal.new(:value($op[0].value != 0 ?? 1 !! 0));
            }
            # Boolean value of empty container
            elsif nqp::istype($op[0], QAST::Op) && 
                  ($op[0].op eq 'emptyarray' || $op[0].op eq 'emptyhash') {
                return QAST::BVal.new(:value(0));
            }
            # Boolean value of constant array
            elsif $!level >= 3 && nqp::istype($op[0], QAST::Op) && 
                  ($op[0].op eq 'array' || $op[0].op eq 'constarray') {
                return QAST::BVal.new(:value($op[0].elems > 0 ?? 1 !! 0));
            }
        }
        
        nqp::null()
    }
    
    # String method optimization
    method optimize_string_methods($op, $method_name) {
        # String length optimization
        if $method_name eq 'chars' || $method_name eq 'elems' || $method_name eq 'length' {
            if nqp::istype($op[0], QAST::SVal) {
                return QAST::IVal.new(:value($op[0].value.chars));
            }
            # Length of empty container
            elsif nqp::istype($op[0], QAST::Op) && 
                  ($op[0].op eq 'emptyarray' || $op[0].op eq 'emptyhash') {
                return QAST::IVal.new(:value(0));
            }
        }
        
        # String case conversion optimization
        elsif $method_name eq 'uc' && nqp::istype($op[0], QAST::SVal) {
            return QAST::SVal.new(:value($op[0].value.uc));
        }
        elsif $method_name eq 'lc' && nqp::istype($op[0], QAST::SVal) {
            return QAST::SVal.new(:value($op[0].value.lc));
        }
        # Added: First letter case conversion optimization
        elsif $method_name eq 'tc' && nqp::istype($op[0], QAST::SVal) {
            return QAST::SVal.new(:value($op[0].value.tc));
        }
        elsif $method_name eq 'fc' && nqp::istype($op[0], QAST::SVal) {
            return QAST::SVal.new(:value($op[0].value.fc));
        }
        
        # String trimming optimization
        elsif ($method_name eq 'trim' || $method_name eq 'triml' || $method_name eq 'trimr') && 
              nqp::istype($op[0], QAST::SVal) {
            my $result;
            if $method_name eq 'trim' {
                $result := $op[0].value.trim;
            }
            elsif $method_name eq 'triml' {
                $result := $op[0].value.triml;
            }
            elsif $method_name eq 'trimr' {
                $result := $op[0].value.trimr;
            }
            return QAST::SVal.new(:value($result));
        }
        
        # String substring optimization
        elsif $method_name eq 'substr' && $op.elems >= 3 && nqp::istype($op[0], QAST::SVal) {
            # Check if parameters are constants
            if nqp::istype($op[2], QAST::IVal) {
                my $start := $op[2].value;
                my $length = nqp::elems($op) >= 4 && nqp::istype($op[3], QAST::IVal) 
                            ?? $op[3].value !! Inf;
                
                # Try to compute substring at compile time
                my $result := $op[0].value.substr($start, $length);
                return QAST::SVal.new(:value($result));
            }
        }
        
        # String replacement optimization
        elsif $!level >= 3 && $method_name eq 'subst' && $op.elems >= 4 && 
              nqp::istype($op[0], QAST::SVal) && nqp::istype($op[2], QAST::SVal) && 
              nqp::istype($op[3], QAST::SVal) {
            # Perform compile-time replacement when all parameters are string constants
            my $result := $op[0].value.subst($op[2].value, $op[3].value);
            return QAST::SVal.new(:value($result));
        }
        
        # String search method optimization
        elsif $method_name eq 'contains' && $op.elems >= 3 && nqp::istype($op[0], QAST::SVal) {
            if nqp::istype($op[2], QAST::SVal) {
                my $result := $op[0].value.contains($op[2].value);
                return QAST::BVal.new(:value($result ?? 1 !! 0));
            }
        }
        
        elsif $method_name eq 'starts-with' && $op.elems >= 3 && nqp::istype($op[0], QAST::SVal) {
            if nqp::istype($op[2], QAST::SVal) {
                my $result := $op[0].value.starts-with($op[2].value);
                return QAST::BVal.new(:value($result ?? 1 !! 0));
            }
        }
        
        elsif $method_name eq 'ends-with' && $op.elems >= 3 && nqp::istype($op[0], QAST::SVal) {
            if nqp::istype($op[2], QAST::SVal) {
                my $result := $op[0].value.ends-with($op[2].value);
                return QAST::BVal.new(:value($result ?? 1 !! 0));
            }
        }
        
        # Added: String repetition optimization
        elsif $method_name eq 'x' && $op.elems >= 3 && nqp::istype($op[0], QAST::SVal) {
            if nqp::istype($op[2], QAST::IVal) && $op[2].value >= 0 {
                my $result := $op[0].value x $op[2].value;
                return QAST::SVal.new(:value($result));
            }
        }
        
        # Added: String comparison optimization
        elsif ($method_name eq 'eq' || $method_name eq 'ne' || $method_name eq 'gt' || 
               $method_name eq 'ge' || $method_name eq 'lt' || $method_name eq 'le') && 
              $op.elems >= 3 && nqp::istype($op[0], QAST::SVal) && nqp::istype($op[2], QAST::SVal) {
            my $result := 0;
            given $method_name {
                when 'eq' { $result = $op[0].value eq $op[2].value ?? 1 !! 0; }
                when 'ne' { $result = $op[0].value ne $op[2].value ?? 1 !! 0; }
                when 'gt' { $result = $op[0].value gt $op[2].value ?? 1 !! 0; }
                when 'ge' { $result = $op[0].value ge $op[2].value ?? 1 !! 0; }
                when 'lt' { $result = $op[0].value lt $op[2].value ?? 1 !! 0; }
                when 'le' { $result = $op[0].value le $op[2].value ?? 1 !! 0; }
            }
            return QAST::BVal.new(:value($result));
        }
        
        nqp::null()
    }
    
    # Array method optimization
    method optimize_array_methods($op, $method_name) {
        # Array size optimization
        if $method_name eq 'elems' || $method_name eq 'size' || $method_name eq 'length' {
            # Number of elements in empty array
            if nqp::istype($op[0], QAST::Op) && $op[0].op eq 'emptyarray' {
                return QAST::IVal.new(:value(0));
            }
            # Number of elements in constant array
            elsif nqp::istype($op[0], QAST::Op) && 
                  ($op[0].op eq 'array' || $op[0].op eq 'constarray' || $op[0].op eq 'optimized_list') {
                return QAST::IVal.new(:value($op[0].elems));
            }
        }
        
        # Array index optimization
        elsif $method_name eq 'AT-POS' && $op.elems >= 3 && 
              $op[2] && nqp::istype($op[2], QAST::IVal) {
            my $index := $op[2].value;
            
            # Get element at specified index from constant array
            if nqp::istype($op[0], QAST::Op) && 
               ($op[0].op eq 'array' || $op[0].op eq 'constarray' || $op[0].op eq 'optimized_list') {
                my $elems := $op[0].elems;
                # Handle negative index
                my $real_index := $index < 0 ?? $elems + $index !! $index;
                
                # Check if index is valid
                if $real_index >= 0 && $real_index < $elems {
                    return $op[0][$real_index];
                }
            }
        }
        
        # Array first/last element optimization
        elsif $method_name eq 'first' && $op.elems >= 2 {
            # first method on empty array
            if nqp::istype($op[0], QAST::Op) && $op[0].op eq 'emptyarray' {
                return QAST::Op.new(:op('null'));
            }
            # first method on constant array (without parameters)
            elsif $op.elems == 2 && 
                  nqp::istype($op[0], QAST::Op) && 
                  ($op[0].op eq 'array' || $op[0].op eq 'constarray' || $op[0].op eq 'optimized_list') {
                if $op[0].elems > 0 {
                    return $op[0][0];
                } else {
                    return QAST::Op.new(:op('null'));
                }
            }
        }
        
        elsif $method_name eq 'last' && $op.elems >= 2 {
            # last method on empty array
            if nqp::istype($op[0], QAST::Op) && $op[0].op eq 'emptyarray' {
                return QAST::Op.new(:op('null'));
            }
            # last method on constant array (without parameters)
            elsif $op.elems == 2 && 
                  nqp::istype($op[0], QAST::Op) && 
                  ($op[0].op eq 'array' || $op[0].op eq 'constarray' || $op[0].op eq 'optimized_list') {
                my $elems := $op[0].elems;
                if $elems > 0 {
                    return $op[0][$elems - 1];
                } else {
                    return QAST::Op.new(:op('null'));
                }
            }
        }
        
        # Added: Array concatenation optimization
        elsif $!level >= 3 && $method_name eq 'append' && $op.elems >= 3 && 
              nqp::istype($op[0], QAST::Op) && $op[0].op eq 'emptyarray' && 
              nqp::istype($op[2], QAST::Op) && 
              ($op[2].op eq 'array' || $op[2].op eq 'constarray') {
            # Empty array append another array can directly return the other array
            return $op[2];
        }
        
        # Added: Array contains optimization
        elsif $method_name eq 'contains' && $op.elems >= 3 && 
              nqp::istype($op[0], QAST::Op) && 
              ($op[0].op eq 'array' || $op[0].op eq 'constarray') && 
              (nqp::istype($op[2], QAST::SVal) || nqp::istype($op[2], QAST::IVal) || 
               nqp::istype($op[2], QAST::NVal) || nqp::istype($op[2], QAST::BVal)) {
            my $search_val := $op[2].value;
            my $result := 0;
            
            for $op[0].list -> $elem {
                if nqp::istype($elem, $op[2].WHAT) && $elem.value == $search_val {
                    $result := 1;
                    last;
                }
            }
            
            return QAST::BVal.new(:value($result));
        }
        
        nqp::null()
    }
    
    # Hash method optimization
    method optimize_hash_methods($op, $method_name) {
        # Hash size optimization
        if $method_name eq 'elems' || $method_name eq 'size' || $method_name eq 'keys' || $method_name eq 'values' {
            # Number of elements in empty hash
            if nqp::istype($op[0], QAST::Op) && $op[0].op eq 'emptyhash' {
                if $method_name eq 'keys' || $method_name eq 'values' {
                    return QAST::Op.new(:op('emptyarray'));
                }
                return QAST::IVal.new(:value(0));
            }
        }
        
        # Hash access optimization
        elsif $method_name eq 'AT-KEY' && $op.elems >= 3 && 
              $op[2] && nqp::istype($op[2], QAST::SVal) {
            # Look up key from constant hash
            if $!level >= 3 && nqp::istype($op[0], QAST::Op) && $op[0].op eq 'hash' {
                # Note: This requires knowing the specific structure of the hash to optimize, simplified version
                # In actual implementation, more complex hash constant recognition may be needed
            }
        }
        
        # Hash key existence check optimization
        elsif $method_name eq 'exists' && $op.elems >= 3 && 
              $op[2] && nqp::istype($op[2], QAST::SVal) {
            # Empty hash never has keys
            if nqp::istype($op[0], QAST::Op) && $op[0].op eq 'emptyhash' {
                return QAST::BVal.new(:value(0));
            }
        }
        
        nqp::null()
    }
    
    # Numeric method optimization
    method optimize_numeric_methods($op, $method_name) {
        # Absolute value optimization
        if $method_name eq 'abs' {
            if nqp::istype($op[0], QAST::IVal) {
                return QAST::IVal.new(:value(abs($op[0].value)));
            }
            elsif nqp::istype($op[0], QAST::NVal) {
                return QAST::NVal.new(:value(abs($op[0].value)));
            }
        }
        
        # Square root optimization
        elsif $!level >= 3 && $method_name eq 'sqrt' && nqp::istype($op[0], QAST::NVal) {
            return QAST::NVal.new(:value(sqrt($op[0].value)));
        }
        
        # Numeric comparison optimization
        elsif ($method_name eq '==' || $method_name eq '!=' || $method_name eq '>' || 
               $method_name eq '>=' || $method_name eq '<' || $method_name eq '<=') && 
              $op.elems >= 3 && 
              ((nqp::istype($op[0], QAST::IVal) && nqp::istype($op[2], QAST::IVal)) ||
               (nqp::istype($op[0], QAST::NVal) && nqp::istype($op[2], QAST::NVal))) {
            my $result := 0;
            given $method_name {
                when '==' { $result = $op[0].value == $op[2].value ?? 1 !! 0; }
                when '!=' { $result = $op[0].value != $op[2].value ?? 1 !! 0; }
                when '>'  { $result = $op[0].value >  $op[2].value ?? 1 !! 0; }
                when '>=' { $result = $op[0].value >= $op[2].value ?? 1 !! 0; }
                when '<'  { $result = $op[0].value <  $op[2].value ?? 1 !! 0; }
                when '<=' { $result = $op[0].value <= $op[2].value ?? 1 !! 0; }
            }
            return QAST::BVal.new(:value($result));
        }
        
        nqp::null()
    }
    
    # Boolean method optimization
    method optimize_boolean_methods($op, $method_name) {
        # Boolean negation optimization
        if $method_name eq 'not' || $method_name eq '!so' {
            if nqp::istype($op[0], QAST::BVal) {
                return QAST::BVal.new(:value(!$op[0].value));
            }
            # Negate numeric values
            elsif nqp::istype($op[0], QAST::IVal) {
                return QAST::BVal.new(:value($op[0].value == 0 ?? 1 !! 0));
            }
            elsif nqp::istype($op[0], QAST::NVal) {
                return QAST::BVal.new(:value($op[0].value == 0 ?? 1 !! 0));
            }
            # Negate string values
            elsif nqp::istype($op[0], QAST::SVal) {
                return QAST::BVal.new(:value($op[0].value.chars == 0 ?? 1 !! 0));
            }
        }
        
        # Boolean short-circuit optimization
        elsif ($method_name eq '&&' || $method_name eq 'and') && $op.elems >= 3 {
            if nqp::istype($op[2], QAST::BVal) {
                if $op[2].value == 0 {
                    # x && false => false
                    return QAST::BVal.new(:value(0));
                }
                else {
                    # x && true => x
                    return $op[0];
                }
            }
        }
        
        elsif ($method_name eq '||' || $method_name eq 'or') && $op.elems >= 3 {
            if nqp::istype($op[2], QAST::BVal) {
                if $op[2].value == 1 {
                    # x || true => true
                    return QAST::BVal.new(:value(1));
                }
                else {
                    # x || false => x
                    return $op[0];
                }
            }
        }
        
        nqp::null()
    }
    
    # Chained method call optimization
    method optimize_chain_method_calls($op, $method_name) {
        # Check if it's a chained method call
        if nqp::istype($op[0], QAST::Op) && $op[0].op eq 'callmethod' {
            my $inner_op := $op[0];
            my $inner_method := $inner_op.name;
            
            # .Str.chars optimization
            if $method_name eq 'chars' && $inner_method eq 'Str' {
                if nqp::istype($inner_op[0], QAST::SVal) {
                    return QAST::IVal.new(:value($inner_op[0].value.chars));
                }
            }
            
            # .uc.chars optimization
            elsif $method_name eq 'chars' && ($inner_method eq 'uc' || $inner_method eq 'lc' || $inner_method eq 'tc') {
                if nqp::istype($inner_op[0], QAST::SVal) {
                    # Case conversion doesn't change string length
                    return QAST::IVal.new(:value($inner_op[0].value.chars));
                }
            }
            
            # .substr.uc optimization
            elsif $method_name eq 'uc' && $inner_method eq 'substr' && 
                   nqp::istype($inner_op[0], QAST::SVal) && $inner_op.elems >= 3 && 
                   nqp::istype($inner_op[2], QAST::IVal) {
                my $start := $inner_op[2].value;
                my $length = nqp::elems($inner_op) >= 4 && nqp::istype($inner_op[3], QAST::IVal) 
                            ?? $inner_op[3].value !! Inf;
                
                my $substr_val := $inner_op[0].value.substr($start, $length);
                return QAST::SVal.new(:value($substr_val.uc));
            }
            
            # .chars.so optimization
            elsif $method_name eq 'so' && ($inner_method eq 'chars' || $inner_method eq 'elems') {
                if nqp::istype($inner_op[0], QAST::SVal) {
                    return QAST::BVal.new(:value($inner_op[0].value.chars > 0 ?? 1 !! 0));
                }
                elsif nqp::istype($inner_op[0], QAST::Op) && $inner_op[0].op eq 'emptyarray' {
                    return QAST::BVal.new(:value(0));
                }
            }
            
            # Triple chain optimization: e.g. .Str.uc.chars
            elsif $!level >= 3 && nqp::istype($inner_op[0], QAST::Op) && $inner_op[0].op eq 'callmethod' {
                my $double_inner_op := $inner_op[0];
                my $double_inner_method := $double_inner_op.name;
                
                if $method_name eq 'chars' && $inner_method eq 'uc' && $double_inner_method eq 'Str' && 
                   nqp::istype($double_inner_op[0], QAST::SVal) {
                    return QAST::IVal.new(:value($double_inner_op[0].value.chars));
                }
            }
        }
        
        nqp::null()
    }
    
    # Container method optimization
    method optimize_container_methods($op, $method_name) {
        # Definition check optimization
        if $method_name eq 'defined' {
            # Constants are always defined
            if nqp::istype($op[0], QAST::IVal) || 
               nqp::istype($op[0], QAST::NVal) || 
               nqp::istype($op[0], QAST::SVal) || 
               nqp::istype($op[0], QAST::BVal) {
                return QAST::BVal.new(:value(1));
            }
            # Empty containers are also defined
            elsif nqp::istype($op[0], QAST::Op) && 
                  ($op[0].op eq 'emptyarray' || $op[0].op eq 'emptyhash') {
                return QAST::BVal.new(:value(1));
            }
        }
        
        # Empty check optimization
        elsif $method_name eq 'is-empty' || $method_name eq '!so' {
            # Empty container check
            if nqp::istype($op[0], QAST::Op) && 
               ($op[0].op eq 'emptyarray' || $op[0].op eq 'emptyhash') {
                return QAST::BVal.new(:value(1));
            }
            # Constant empty string check
            elsif nqp::istype($op[0], QAST::SVal) && $op[0].value eq "" {
                return QAST::BVal.new(:value(1));
            }
            # Constant zero check
            elsif nqp::istype($op[0], QAST::IVal) && $op[0].value == 0 {
                return QAST::BVal.new(:value(1));
            }
            elsif nqp::istype($op[0], QAST::NVal) && $op[0].value == 0.0 {
                return QAST::BVal.new(:value(1));
            }
        }
        
        nqp::null()
    }
    
    # Advanced method call optimization (only enabled at high optimization levels)
    method optimize_advanced_method_calls($op, $method_name) {
        # Enable different levels of method call optimization based on optimization level
        if $!level < 3 {
            return nqp::null();
        }
        
        # Basic method call optimization (level 3+)
        my $optimized := self.optimize_basic_method_calls($op, $method_name);
        return $optimized if $optimized;
        
        # Advanced method call optimization (level 4+)
        if $!level >= 4 {
            $optimized := self.optimize_high_level_method_calls($op, $method_name);
            return $optimized if $optimized;
        }
        
        nqp::null()
    }
    
    # Basic method call optimization (level 3+)
    method optimize_basic_method_calls($op, $method_name) {
        # Method inlining optimization: replace simple method calls with equivalent operations
        
        # String method optimization
        if $method_name eq 'uc' || $method_name eq 'lc' || $method_name eq 'ucfirst' || $method_name eq 'lcfirst' {
            # Case conversion optimization for constant strings
            if $op.elems == 2 && nqp::istype($op[0], QAST::SVal) {
                my $value := $op[0].value;
                my $result := $value;
                
                if $method_name eq 'uc' {
                    $result := $value.uc;
                } elsif $method_name eq 'lc' {
                    $result := $value.lc;
                } elsif $method_name eq 'ucfirst' {
                    $result := $value.ucfirst;
                } elsif $method_name eq 'lcfirst' {
                    $result := $value.lcfirst;
                }
                
                return QAST::SVal.new(:value($result));
            }
        }
        
        # Numeric method optimization
        elsif $method_name eq 'abs' {
            # Absolute value optimization for constant numbers
            if $op.elems == 2 {
                if nqp::istype($op[0], QAST::IVal) {
                    my $value := $op[0].value;
                    return QAST::IVal.new(:value($value < 0 ?? -$value !! $value));
                } elsif nqp::istype($op[0], QAST::NVal) {
                    my $value := $op[0].value;
                    return QAST::NVal.new(:value($value < 0 ?? -$value !! $value));
                }
            }
        }
        
        # Null check optimization
        elsif $method_name eq 'defined' || $method_name eq 'so' || $method_name eq 'not' {
            # Logical check optimization for constant values
            if $op.elems == 2 {
                if nqp::istype($op[0], QAST::Val) && $op[0].value =:= nqp::null() {
                    return QAST::BVal.new(:value($method_name eq 'not')); # defined/null is false, not is true
                } elsif nqp::istype($op[0], QAST::BVal) {
                    my $val := $op[0].value;
                    if $method_name eq 'defined' || $method_name eq 'so' {
                        return QAST::BVal.new(:value($val));
                    } elsif $method_name eq 'not' {
                        return QAST::BVal.new(:value(!$val));
                    }
                }
            }
        }
        
        nqp::null()
    }
    
    # Advanced method call optimization (level 4+)
    method optimize_high_level_method_calls($op, $method_name) {
        # Array method optimization
        # max/min on constant arrays
        if ($method_name eq 'max' || $method_name eq 'min') && $op.elems == 2 && 
           nqp::istype($op[0], QAST::Op) && 
           ($op[0].op eq 'array' || $op[0].op eq 'constarray') && $op[0].elems > 0 {
            my @values;
            my $all_numeric := 1;
            my $has_ints := 0;
            my $has_nums := 0;
            
            # Collect all numeric values and check types
            for $op[0].list -> $elem {
                if nqp::istype($elem, QAST::IVal) {
                    @values.push($elem.value);
                    $has_ints := 1;
                } elsif nqp::istype($elem, QAST::NVal) {
                    @values.push($elem.value);
                    $has_nums := 1;
                } else {
                    $all_numeric := 0;
                    last;
                }
            }
            
            if $all_numeric && @values.elems > 0 {
                my $result;
                if $method_name eq 'max' {
                    $result := [max] @values;
                } else {
                    $result := [min] @values;
                }
                
                # Return appropriate constant based on type mixture
                if $has_nums {
                    return QAST::NVal.new(:value($result));
                } else {
                    return QAST::IVal.new(:value($result.Int));
                }
            }
        }
        
        # String sorting optimization (for constant string arrays)
        elsif $method_name eq 'sort' && $op.elems == 2 && 
              nqp::istype($op[0], QAST::Op) && 
              ($op[0].op eq 'array' || $op[0].op eq 'constarray') {
            my @str_values;
            my $all_strings := 1;
            
            # Collect all strings
            for $op[0].list -> $elem {
                if nqp::istype($elem, QAST::SVal) {
                    @str_values.push($elem.value);
                } else {
                    $all_strings := 0;
                    last;
                }
            }
            
            if $all_strings {
                my @sorted := @str_values.sort;
                my @sorted_nodes := @sorted.map({ QAST::SVal.new(:value($_)) });
                return QAST::Op.new(:op('constarray'), |@sorted_nodes);
            }
        }
        
        # Array length optimization
        elsif $method_name eq 'elems' || $method_name eq 'size' || $method_name eq 'length' {
            if $op.elems == 2 && nqp::istype($op[0], QAST::Op) && 
               ($op[0].op eq 'array' || $op[0].op eq 'constarray') {
                my $count := $op[0].elems;
                return QAST::IVal.new(:value($count));
            }
        }
        
        # String concatenation optimization
        elsif $method_name eq '~' && $op.elems == 3 {
            # Constant string concatenation optimization
            if nqp::istype($op[0], QAST::SVal) && nqp::istype($op[1], QAST::SVal) {
                my $result := $op[0].value ~ $op[1].value;
                return QAST::SVal.new(:value($result));
            }
        }
        
        # Chain method call optimization detection
        # Detect chain calls like obj.method1.method2 and attempt to optimize the entire chain
        if $op.elems == 2 && nqp::istype($op[0], QAST::Op) && $op[0].op eq 'callmethod' {
            my $optimized_chain := self.optimize_method_chain($op);
            return $optimized_chain if $optimized_chain;
        }
        
        # Method result caching optimization - add cache markers for pure function method calls
        if self.is_pure_function($method_name) && self.can_cache_method_result($op) {
            # Add cache markers for pure function method calls
            # 1. Check if method call parameters are constants or deterministic expressions
            my $can_cache := True;
            for $op.list -> $arg {
                unless self.is_deterministic_expression($arg) {
                    $can_cache := False;
                    last;
                }
            }
            
            # 2. If all parameters are deterministic, add cache marker
            if $can_cache {
                # Create a new method call node with cache marker
                my $cached_op := QAST::Op.new(
                    :op('callmethod'),
                    :name($method_name),
                    :cacheable(1),  # 添加缓存标记
                    |$op.list
                );
                
                # Copy other attributes from the original node
                for nqp::getattr_s($op) -> $attr {
                    unless $attr eq 'op' || $attr eq 'name' || $attr eq 'list' {
                        nqp::setattr($cached_op, $attr, nqp::getattr($op, $attr));
                    }
                }
                
                # Log the optimization
                self.log_optimization("Method call caching for pure function '$method_name'");
                return $cached_op;
            }
            
            # 3. For partially deterministic cases, consider inline caching (only for small fixed-parameter methods)
            elsif $self.optimization_level >= 5 && $op.elems <= 4 {
                # Add inline caching hint for method calls with partially deterministic parameters
                $op.cache_inline := 1;
                self.log_optimization("Potential inline caching for '$method_name'");
            }
        }
        
        nqp::null()
    }
    
    # Detect if a method is a pure function (no side effects)
    method is_pure_function($method_name) {
        # List of common pure function methods
        my @pure_methods := <
            abs sqrt sin cos tan log exp max min
            uc lc ucfirst lcfirst substr chars
            elems size length reverse sort
            Int Num Str Bool
        >;
        
        nqp::exists(@pure_methods, $method_name);
    }
    
    # Check if an expression is deterministic (same input always produces same output)
    method is_deterministic_expression($node) {
        # Constant values are always deterministic
        if nqp::istype($node, QAST::Val) {
            return True;
        }
        
        # Deterministic operators
        elsif nqp::istype($node, QAST::Op) {
            my $op := $node.op;
            
            # Arithmetic, logical, and comparison operations are typically deterministic
            if $op eq 'add' || $op eq 'sub' || $op eq 'mul' || $op eq 'div' ||
               $op eq 'pow' || $op eq 'mod' || $op eq 'and' || $op eq 'or' ||
               $op eq 'xor' || $op eq 'eq' || $op eq 'ne' || $op eq 'lt' ||
               $op eq 'gt' || $op eq 'le' || $op eq 'ge' || $op eq 'concat' ||
               $op eq 'repeat' {
                # 检查所有操作数是否都是确定性的
                for $node.list -> $arg {
                    unless self.is_deterministic_expression($arg) {
                        return False;
                    }
                }
                return True;
            }
            
            # Method calls are only deterministic if they are pure functions
            elsif $op eq 'callmethod' {
                my $method_name := $node.name;
                if self.is_pure_function($method_name) {
                    # 检查方法调用的接收者和参数是否是确定性的
                    for $node.list -> $arg {
                        unless self.is_deterministic_expression($arg) {
                            return False;
                        }
                    }
                    return True;
                }
            }
        }
        
        # Variable references are only deterministic if they are read-only/constant
        elsif nqp::istype($node, QAST::Var) {
            my $sigil := $node.sigil;
            my $name := $node.name;
            
            # Check if variable is marked as read-only or constant
            if $node.is_readonly || $node.is_constant || $sigil eq '$/' {
                return True;
            }
            
            # Check variable binding information if available
            if $!type_env && $!type_env{$name} && $!type_env{$name}<readonly> {
                return True;
            }
        }
        
        # Not deterministic by default
        False
    }
    
    # Check if a method call is suitable for result caching
    method can_cache_method_result($op) {
        # Basic conditions: method call node with only a receiver (no extra arguments or few arguments)
        if $op.op eq 'callmethod' && $op.elems <= 5 {
            # 1. Check if it's a repeated method call (can be detected via symbol table)
            # 2. Estimate method call complexity and potential caching benefits
            my $estimated_size := self.estimate_node_size($op);
            
            # For small to medium-sized method calls, caching benefits typically outweigh overhead
            if $estimated_size > 10 && $estimated_size < 1000 {
                return True;
            }
        }
        
        False
    }
    
    # Optimize chained method calls
    method optimize_method_chain($chain) {
        # This is a simplified implementation of chain call optimization
        # In a real implementation, more complex analysis and transformation would be needed
        
        # Only optimize simple two-method chains
        my $inner_op := $chain[0];
        if $inner_op && $inner_op.op eq 'callmethod' && $chain.op eq 'callmethod' {
            my $outer_method := $chain.name;
            my $inner_method := $inner_op.name;
            
            # Optimize common string chain operations
            if $outer_method eq 'lc' && $inner_method eq 'uc' || 
               $outer_method eq 'uc' && $inner_method eq 'lc' {
                # Mutually canceling operations, simplify to original object
                return $inner_op[0];
            }
            elsif $outer_method eq 'uc' && $inner_method eq 'uc' || 
                  $outer_method eq 'lc' && $inner_method eq 'lc' ||
                  $outer_method eq 'ucfirst' && $inner_method eq 'ucfirst' ||
                  $outer_method eq 'lcfirst' && $inner_method eq 'lcfirst' {
                # Repeating the same operation, simplify to a single operation
                return $inner_op;
            }
        }
        
        nqp::null()
    }
    
    # Enhanced dead code elimination
    method eliminate_dead_code($node, $optype) {
        # Record optimization start time
        my $start_time := nqp::time_n();
        my $optimized := nqp::null();
        
        # Apply basic dead code elimination for all optimization levels >= 2
        if $!level >= 2 {
            # Apply basic dead code elimination techniques
            $optimized := self.apply_basic_dead_code_elimination($node, $optype);
        }
        
        # Apply advanced dead code elimination for higher optimization levels
        if !$optimized && $!level >= 3 {
            $optimized := self.apply_advanced_dead_code_elimination($node, $optype);
        }
        
        # Apply aggressive dead code elimination for maximum optimization
        if !$optimized && $!level >= 4 {
            $optimized := self.apply_aggressive_dead_code_elimination($node, $optype);
        }
        
        # Record optimization log if changes were made
        if $optimized {
            my $duration := nqp::time_n() - $start_time;
            self.log_optimization(
                :description("Dead code elimination ($optype)"),
                :original-type($optype),
                :optimized-type($optimized.WHAT.name),
                :original-size(self.estimate_node_size($node)),
                :optimized-size(self.estimate_node_size($optimized)),
                :additional-info("Level: $!level"),
                :duration($duration)
            );
        }
        
        $optimized
    }
    
    # Apply basic dead code elimination techniques
    method apply_basic_dead_code_elimination($node, $optype) {
        # Constant condition branch elimination
        if $optype eq 'if' || $optype eq 'unless' {
            if nqp::istype($node[0], QAST::BVal) {
                my $condition_value := $node[0].value;
                # For unless, invert the condition logic
                if $optype eq 'unless' {
                    $condition_value := !$condition_value;
                }
                
                if $condition_value == 1 {
                    # Condition is always true, keep only the true branch
                    return $node[1];
                }
                elsif $condition_value == 0 && nqp::elems($node) > 2 {
                    # Condition is always false, keep only the false branch
                    return $node[2];
                }
                elsif $condition_value == 0 {
                    # Condition is always false, no else branch
                    return QAST::Op.new(:op<null>);
                }
            }
            # Also handle numeric constants in conditions
            elsif nqp::istype($node[0], QAST::IVal) {
                my $condition_value := $node[0].value != 0;
                if $optype eq 'unless' {
                    $condition_value := !$condition_value;
                }
                
                if $condition_value {
                    return $node[1];
                }
                elsif nqp::elems($node) > 2 {
                    return $node[2];
                } else {
                    return QAST::Op.new(:op<null>);
                }
            }
            # Handle string constants in conditions
            elsif nqp::istype($node[0], QAST::SVal) {
                my $condition_value := $node[0].value.chars != 0;
                if $optype eq 'unless' {
                    $condition_value := !$condition_value;
                }
                
                if $condition_value {
                    return $node[1];
                }
                elsif nqp::elems($node) > 2 {
                    return $node[2];
                } else {
                    return QAST::Op.new(:op<null>);
                }
            }
        }
        
        # Dead code after return/last/next/redo/exit/die
        elsif $optype eq 'stmts' {
            my int $i := 0;
            my int $last_idx := nqp::elems($node) - 1;
            my int $dead_start := -1;
            
            for $node.list() -> $stmt {
                # Check for terminating statements
                if self.is_termination_statement($stmt) {
                    $dead_start := $i + 1;
                    last;
                }
                $i++;
            }
            
            # If we found dead code, remove it
            if $dead_start > 0 && $dead_start <= $last_idx {
                my @new_stmts;
                for 0..$dead_start-1 -> $j {
                    @new_stmts.push($node[$j]);
                }
                
                # Create a new stmts node with only live code
                my $new_node := QAST::Stmts.new();
                $new_node.set_children(@new_stmts);
                return $new_node;
            }
        }
        
        # Remove empty statements blocks
        elsif $optype eq 'stmts' && nqp::elems($node) == 0 {
            return QAST::Op.new(:op<null>);
        }
        
        # Remove redundant conditionals (if true { ... })
        elsif $optype eq 'if' && nqp::istype($node[0], QAST::Op) && $node[0].op eq 'true' {
            return $node[1];
        }
        elsif $optype eq 'if' && nqp::istype($node[0], QAST::Op) && $node[0].op eq 'false' && nqp::elems($node) > 2 {
            return $node[2];
        }
        
        nqp::null()
    }
    
    # Apply advanced dead code elimination techniques
    method apply_advanced_dead_code_elimination($node, $optype) {
        # Create a working copy of the node
        my $optimized := self.clone_node($node);
        my %reachable;
        my %modified;
        
        # Step 1: Perform reachability analysis
        $optimized := self.mark_reachable_code($optimized, %reachable);
        
        # Step 2: Remove unreachable code
        my $new_node := self.remove_unreachable_code($optimized, %reachable);
        
        # Step 3: Eliminate redundant assignments
        $new_node := self.eliminate_redundant_assignments($new_node);
        
        # Step 4: Remove invariant code from loops
        $new_node := self.move_invariant_code_out_of_loops($new_node);
        
        # Check if any optimizations were applied
        if !nqp::eqaddr($new_node, $node) {
            return $new_node;
        }
        
        nqp::null()
    }
    
    # Apply aggressive dead code elimination techniques
    method apply_aggressive_dead_code_elimination($node, $optype) {
        # Only for maximum optimization level
        if $!level < 4 {
            return nqp::null();
        }
        
        my $optimized := nqp::null();
        
        # Remove unused variables and assignments
        $optimized := self.eliminate_unused_variables($node);
        if $optimized {
            return $optimized;
        }
        
        # Eliminate common subexpressions across larger scopes
        $optimized := self.perform_global_cse($node);
        if $optimized {
            return $optimized;
        }
        
        # Remove code with no observable effects
        $optimized := self.remove_effectless_code($node);
        if $optimized {
            return $optimized;
        }
        
        nqp::null()
    }
    
    # Check if a statement is a termination statement
    method is_termination_statement($stmt) {
        return nqp::istype($stmt, QAST::Op) && (
            $stmt.op eq 'return'  || $stmt.op eq 'last'   || 
            $stmt.op eq 'next'   || $stmt.op eq 'redo'   || 
            $stmt.op eq 'exit'   || $stmt.op eq 'die'    ||
            $stmt.op eq 'take'   || $stmt.op eq 'yield'  ||
            ($stmt.op eq 'call'  && $stmt[0] && $stmt[0].name eq 'exit') ||
            ($stmt.op eq 'call'  && $stmt[0] && $stmt[0].name eq 'die')
        );
    }
    
    # Mark reachable code (enhanced)
    method mark_reachable_code($node, %reachable is rw) {
        # Skip null nodes
        if !$node {
            return $node;
        }
        
        # Mark this node as reachable
        %reachable{nqp::addr($node)} := True;
        
        # Process based on node type
        given $node {
            when QAST::Stmts {
                # Process each statement in sequence
                for $node.list() -> $stmt {
                    self.mark_reachable_code($stmt, %reachable);
                    
                    # If this is a termination statement, stop processing
                    if self.is_termination_statement($stmt) {
                        last;
                    }
                }
            }
            when QAST::Op {
                # For conditional branches, use smart reachability analysis
                if $node.op eq 'if' || $node.op eq 'unless' {
                    # Condition is always reachable
                    if $node[0] {
                        self.mark_reachable_code($node[0], %reachable);
                    }
                    
                    # Check for constant conditions to optimize branch reachability
                    my $const_condition := self.evaluate_constant_condition($node[0]);
                    if $const_condition.defined {
                        # For unless, invert the condition
                        my $condition_value := $node.op eq 'unless' ? !$const_condition.value : $const_condition.value;
                        
                        # Only mark the reachable branch
                        if $condition_value && $node[1] {
                            self.mark_reachable_code($node[1], %reachable);
                        }
                        elsif !$condition_value && nqp::elems($node.list()) > 2 && $node[2] {
                            self.mark_reachable_code($node[2], %reachable);
                        }
                    } else {
                        # Both branches are potentially reachable
                        if $node[1] {
                            self.mark_reachable_code($node[1], %reachable);
                        }
                        if nqp::elems($node.list()) > 2 && $node[2] {
                            self.mark_reachable_code($node[2], %reachable);
                        }
                    }
                }
                # For loops, mark the body as reachable (at least once)
                elsif $node.op eq 'loop' || $node.op eq 'while' || $node.op eq 'for' || $node.op eq 'repeat' {
                    # Condition is always reachable
                    if $node[0] && ($node.op eq 'while' || $node.op eq 'until') {
                        self.mark_reachable_code($node[0], %reachable);
                    }
                    
                    # Body is always reachable
                    my $body_idx := $node.op eq 'loop' || $node.op eq 'repeat' ? 0 : 1;
                    if nqp::elems($node.list()) > $body_idx && $node[$body_idx] {
                        self.mark_reachable_code($node[$body_idx], %reachable);
                    }
                }
                # For try-catch blocks
                elsif $node.op eq 'try' || $node.op eq 'catch' {
                    # Both try and catch blocks are potentially reachable
                    for $node.list() -> $child {
                        if $child {
                            self.mark_reachable_code($child, %reachable);
                        }
                    }
                }
            }
        }
        
        # Recursively process all children for nodes not specially handled
        if nqp::can($node, 'list') {
            for $node.list() -> $child {
                if $child && !self.is_child_already_processed($node, $child, %reachable) {
                    self.mark_reachable_code($child, %reachable);
                }
            }
        }
        
        return $node;
    }
    
    # Check if a child node has already been processed
    method is_child_already_processed($parent, $child, %reachable) {
        # Check if the child was already marked as reachable through special handling
        if %reachable{nqp::addr($child)}:exists {
            return True;
        }
        return False;
    }
    
    # Try to evaluate a condition as a constant
    method evaluate_constant_condition($node) {
        # Check for boolean constants
        if nqp::istype($node, QAST::BVal) {
            return $node;
        }
        # Check for numeric constants
        elsif nqp::istype($node, QAST::IVal) {
            return QAST::BVal.new(:value($node.value != 0 ?? 1 !! 0));
        }
        # Check for string constants
        elsif nqp::istype($node, QAST::SVal) {
            return QAST::BVal.new(:value($node.value.chars != 0 ?? 1 !! 0));
        }
        # Check for true/false operations
        elsif nqp::istype($node, QAST::Op) {
            if $node.op eq 'true' {
                return QAST::BVal.new(:value(1));
            }
            elsif $node.op eq 'false' {
                return QAST::BVal.new(:value(0));
            }
        }
        # Couldn't determine a constant value
        return nqp::null();
    }
    
    # Remove unreachable code (enhanced)
    method remove_unreachable_code($node, %reachable) {
        # Skip null nodes
        if !$node {
            return $node;
        }
        
        # If this node is not reachable, remove it
        if !%reachable{nqp::addr($node)}:exists {
            return QAST::Op.new(:op<null>);
        }
        
        # Process based on node type
        given $node {
            when QAST::Stmts {
                my $new_stmts := QAST::Stmts.new();
                my $modified := False;
                
                for $node.list() -> $stmt {
                    my $optimized_stmt := self.remove_unreachable_code($stmt, %reachable);
                    if $optimized_stmt {
                        if !nqp::istype($optimized_stmt, QAST::Op) || $optimized_stmt.op ne 'null' {
                            $new_stmts.push($optimized_stmt);
                        }
                        if !nqp::eqaddr($optimized_stmt, $stmt) {
                            $modified := True;
                        }
                    } else {
                        $modified := True;
                    }
                }
                
                # If we removed all statements, return null
                if nqp::elems($new_stmts) == 0 {
                    return QAST::Op.new(:op<null>);
                }
                
                if $modified {
                    return $new_stmts;
                }
            }
            when QAST::Op {
                # For control flow structures, process children specially
                if $node.op eq 'if' || $node.op eq 'unless' || 
                   $node.op eq 'while' || $node.op eq 'for' || 
                   $node.op eq 'loop' || $node.op eq 'try' || $node.op eq 'catch' {
                    my @new_children;
                    my $modified := False;
                    
                    for $node.list() -> $child {
                        if $child {
                            my $optimized_child := self.remove_unreachable_code($child, %reachable);
                            if $optimized_child {
                                push @new_children, $optimized_child;
                                if !nqp::eqaddr($optimized_child, $child) {
                                    $modified := True;
                                }
                            } else {
                                push @new_children, QAST::Op.new(:op<null>);
                                $modified := True;
                            }
                        } else {
                            push @new_children, nqp::null();
                        }
                    }
                    
                    if $modified {
                        my $new_node := QAST::Op.new(:op($node.op));
                        $new_node.set_children(@new_children);
                        # Copy attributes from original node
                        $new_node.name := $node.name if nqp::can($node, 'name');
                        $new_node.block := $node.block if nqp::can($node, 'block');
                        return $new_node;
                    }
                }
            }
        }
        
        # Recursively process children
        if nqp::can($node, 'list') {
            my @children := $node.list();
            my $modified := False;
            
            for @children.kv -> $i, $child {
                if $child {
                    my $optimized_child := self.remove_unreachable_code($child, %reachable);
                    if !nqp::eqaddr($optimized_child, $child) {
                        if $optimized_child {
                            $node[$i] := $optimized_child;
                        } else {
                            # Remove the child if it's unreachable
                            $node[$i] := QAST::Op.new(:op<null>);
                        }
                        $modified := True;
                    }
                }
            }
            
            if $modified {
                return $node;
            }
        }
        
        return $node;
    }
    
    # Log optimization details
    method log_optimization(:$description, :$original-type, :$optimized-type, :$original-size, :$optimized-size, :$additional-info = '', :$duration) {
        # Only log optimizations if logging is enabled
        if %*ENV<RAKU_OPTIMIZER_LOGGING> || $!debug {
            # In a production environment, this would write to a proper log
            # For now, we'll just provide a placeholder
        }
    }
    
    # Estimate the size of a node for optimization metrics
    method estimate_node_size($node) {
        # Simple heuristic to estimate node size
        my int $size := 1;
        
        if nqp::can($node, 'list') {
            for $node.list() -> $child {
                if $child {
                    $size := $size + self.estimate_node_size($child);
                }
            }
        }
        
        $size
    }
    
    # Eliminate redundant assignments
    method eliminate_redundant_assignments($node) {
        # Skip null nodes
        if !$node {
            return $node;
        }
        
        # Only process statement blocks
        if nqp::istype($node, QAST::Stmts) {
            my @new_stmts;
            my %live_assignments; # Track the last assignment to each variable
            my $modified := False;
            
            # Process statements in reverse to identify redundant assignments
            my @reversed_stmts := $node.list().reverse;
            for @reversed_stmts -> $stmt {
                # Check if this is an assignment to a variable
                if self.is_simple_assignment($stmt) {
                    my $var_name := self.get_assigned_variable_name($stmt);
                    my $assigned_value := $stmt[2]; # Assuming format is (target, value)
                    
                    if $var_name {
                        # Check if this variable is read before being reassigned
                        if %live_assignments{$var_name}:exists {
                            # Check if this is a self-assignment (x = x)
                            if self.is_self_assignment($stmt) {
                                # Remove self-assignment
                                $modified := True;
                                next;
                            }
                            # Check if the assigned value is the same as the last assignment
                            elsif self.are_assignments_equivalent($assigned_value, %live_assignments{$var_name}) {
                                # Remove redundant assignment
                                $modified := True;
                                next;
                            }
                        }
                        # Update the last assignment for this variable
                        %live_assignments{$var_name} := $assigned_value;
                    }
                }
                # Check if this statement uses any variables
                self.track_variable_usage($stmt, %live_assignments);
                
                # Keep this statement
                @new_stmts.push($stmt);
            }
            
            # Reverse back to original order
            if $modified {
                my $new_stmts := QAST::Stmts.new();
                $new_stmts.set_children(@new_stmts.reverse);
                return $new_stmts;
            }
        }
        
        # Recursively process children
        if nqp::can($node, 'list') {
            for $node.list().kv -> $i, $child {
                if $child {
                    my $optimized_child := self.eliminate_redundant_assignments($child);
                    if !nqp::eqaddr($optimized_child, $child) {
                        $node[$i] := $optimized_child;
                    }
                }
            }
        }
        
        return $node;
    }
    
    # Check if a statement is a simple assignment
    method is_simple_assignment($stmt) {
        return nqp::istype($stmt, QAST::Op) && 
               ($stmt.op eq 'bind' || $stmt.op eq 'assign' || $stmt.op eq 'bindlex');
    }
    
    # Get the name of the variable being assigned to
    method get_assigned_variable_name($stmt) {
        if nqp::istype($stmt, QAST::Op) && nqp::istype($stmt[1], QAST::Var) {
            return $stmt[1].name;
        }
        return nqp::null();
    }
    
    # Check if an assignment is a self-assignment (x = x)
    method is_self_assignment($stmt) {
        if nqp::istype($stmt, QAST::Op) && nqp::istype($stmt[1], QAST::Var) && nqp::istype($stmt[2], QAST::Var) {
            return $stmt[1].name eq $stmt[2].name && 
                   $stmt[1].scope eq $stmt[2].scope && 
                   $stmt[1].type eq $stmt[2].type;
        }
        return False;
    }
    
    # Check if two assignment values are equivalent
    method are_assignments_equivalent($val1, $val2) {
        # Check if they are the same object
        if nqp::eqaddr($val1, $val2) {
            return True;
        }
        
        # Check for equivalent constants
        if nqp::istype($val1, QAST::IVal) && nqp::istype($val2, QAST::IVal) {
            return $val1.value == $val2.value;
        }
        elsif nqp::istype($val1, QAST::NVal) && nqp::istype($val2, QAST::NVal) {
            return $val1.value == $val2.value;
        }
        elsif nqp::istype($val1, QAST::SVal) && nqp::istype($val2, QAST::SVal) {
            return $val1.value eq $val2.value;
        }
        elsif nqp::istype($val1, QAST::BVal) && nqp::istype($val2, QAST::BVal) {
            return $val1.value == $val2.value;
        }
        
        return False;
    }
    
    # Track variable usage to determine if assignments are live
    method track_variable_usage($stmt, %live_assignments is rw) {
        # Implementation would track which variables are used in this statement
        # For now, we'll use a simplified approach
        
        # If this is a variable access, mark all other variables as possibly needed
        if nqp::istype($stmt, QAST::Var) {
            # In a real implementation, we would be more precise
        }
        elsif nqp::istype($stmt, QAST::Op) {
            # Process children of operations
            for $stmt.list() -> $child {
                if $child {
                    self.track_variable_usage($child, %live_assignments);
                }
            }
        }
    }
    
    # Move invariant code out of loops (enhanced)
    method move_invariant_code_out_of_loops($node) {
        # Skip null nodes
        if !$node {
            return $node;
        }
        
        # Only process for optimization level >= 3
        if $!level < 3 {
            return $node;
        }
        
        # Check for loop structures
        if nqp::istype($node, QAST::Op) && ($node.op eq 'while' || $node.op eq 'for' || $node.op eq 'loop' || $node.op eq 'repeat') {
            my $loop_body_idx := $node.op eq 'loop' || $node.op eq 'repeat' ? 0 : 1;
            if nqp::elems($node.list()) > $loop_body_idx && nqp::istype($node[$loop_body_idx], QAST::Stmts) {
                my $loop_body := $node[$loop_body_idx];
                my @invariants;
                my @remaining_stmts;
                
                # Analyze statements in the loop body
                for $loop_body.list() -> $stmt {
                    if self.is_loop_invariant($stmt, $loop_body, $node) {
                        @invariants.push($stmt);
                    } else {
                        @remaining_stmts.push($stmt);
                    }
                }
                
                # If we found invariant code, move it out of the loop
                if +@invariants > 0 {
                    my $new_loop_body := QAST::Stmts.new();
                    $new_loop_body.set_children(@remaining_stmts);
                    
                    # Create a new stmts block with invariants before the loop
                    my $new_block := QAST::Stmts.new();
                    $new_block.push($_) for @invariants;
                    
                    # Create a copy of the loop with the modified body
                    my $new_loop := QAST::Op.new(:op($node.op));
                    $new_loop.set_children($node.list());
                    $new_loop[$loop_body_idx] := $new_loop_body;
                    
                    # Add the loop to the new block
                    $new_block.push($new_loop);
                    
                    return $new_block;
                }
            }
        }
        
        # Recursively process children
        if nqp::can($node, 'list') {
            my $modified := False;
            
            for $node.list().kv -> $i, $child {
                if $child {
                    my $optimized_child := self.move_invariant_code_out_of_loops($child);
                    if !nqp::eqaddr($optimized_child, $child) {
                        $node[$i] := $optimized_child;
                        $modified := True;
                    }
                }
            }
            
            if $modified {
                return $node;
            }
        }
        
        return $node;
    }
    
    # Check if a statement is loop-invariant
    method is_loop_invariant($stmt, $loop_body, $loop_node) {
        # Skip null statements
        if !$stmt {
            return False;
        }
        
        # Simple assignments to variables not modified in the loop body
        if self.is_simple_assignment($stmt) {
            my $target := $stmt[1];
            my $value := $stmt[2];
            
            # Check if target is a variable
            if nqp::istype($target, QAST::Var) {
                my $var_name := $target.name;
                
                # Check if this variable is modified elsewhere in the loop body
                if !self.is_variable_modified_in_block($var_name, $loop_body, $stmt) {
                    # Check if the value expression is itself invariant
                    return self.is_expression_invariant($value, $loop_body, $var_name);
                }
            }
        }
        
        # Constant expressions are always invariant
        if self.is_constant_expression($stmt) {
            return True;
        }
        
        return False;
    }
    
    # Check if a variable is modified in a block (other than a specific statement)
    method is_variable_modified_in_block($var_name, $block, $except_stmt = nqp::null()) {
        if nqp::istype($block, QAST::Stmts) {
            for $block.list() -> $stmt {
                # Skip the excepted statement
                if nqp::eqaddr($stmt, $except_stmt) {
                    next;
                }
                
                # Check for assignments
                if self.is_simple_assignment($stmt) {
                    my $target := $stmt[1];
                    if nqp::istype($target, QAST::Var) && $target.name eq $var_name {
                        return True;
                    }
                }
                
                # Recursively check nested blocks
                if nqp::istype($stmt, QAST::Stmts) {
                    if self.is_variable_modified_in_block($var_name, $stmt) {
                        return True;
                    }
                }
            }
        }
        
        return False;
    }
    
    # Check if an expression is invariant (doesn't depend on loop variables)
    method is_expression_invariant($expr, $loop_body, $own_var_name = nqp::null()) {
        # Constant values are invariant
        if self.is_constant_expression($expr) {
            return True;
        }
        
        # Variables are invariant if they're not modified in the loop body
        if nqp::istype($expr, QAST::Var) {
            my $var_name := $expr.name;
            # Don't count our own variable name
            if $own_var_name && $var_name eq $own_var_name {
                return True;
            }
            return !self.is_variable_modified_in_block($var_name, $loop_body);
        }
        
        # For operations, check if all operands are invariant
        if nqp::istype($expr, QAST::Op) {
            # Skip potentially side-effecting operations
            my %side_effect_ops := ('assign' => 1, 'bind' => 1, 'bindlex' => 1, 'call' => 1);
            if %side_effect_ops{$expr.op}:exists {
                return False;
            }
            
            # Check all operands
            for $expr.list() -> $operand {
                if $operand && !self.is_expression_invariant($operand, $loop_body, $own_var_name) {
                    return False;
                }
            }
            
            return True;
        }
        
        return False;
    }
    
    # Check if an expression is a constant expression
    method is_constant_expression($expr) {
        return nqp::istype($expr, QAST::IVal) || 
               nqp::istype($expr, QAST::NVal) || 
               nqp::istype($expr, QAST::SVal) || 
               nqp::istype($expr, QAST::BVal);
    }
    
    # Eliminate unused variables
    method eliminate_unused_variables($node) {
        # Skip null nodes
        if !$node {
            return nqp::null();
        }
        
        # Only for highest optimization level
        if $!level < 4 {
            return nqp::null();
        }
        
        # Perform variable usage analysis
        my %var_uses;
        my %var_defs;
        self.analyze_variable_usage($node, %var_uses, %var_defs);
        
        # Create a modified copy of the node
        my $optimized := self.remove_unused_variable_definitions($node, %var_uses, %var_defs);
        
        # Check if any changes were made
        if !nqp::eqaddr($optimized, $node) {
            return $optimized;
        }
        
        return nqp::null();
    }
    
    # Analyze variable usage in a node
    method analyze_variable_usage($node, %var_uses is rw, %var_defs is rw) {
        # Skip null nodes
        if !$node {
            return;
        }
        
        # Process variable references
        if nqp::istype($node, QAST::Var) {
            my $var_name := $node.name;
            my $var_key := "{$node.scope}:{$var_name}";
            %var_uses{$var_key} := 1;
        }
        # Process variable definitions (assignments)
        elsif nqp::istype($node, QAST::Op) && ($node.op eq 'assign' || $node.op eq 'bind' || $node.op eq 'bindlex') {
            my $target := $node[1];
            if nqp::istype($target, QAST::Var) {
                my $var_name := $target.name;
                my $var_key := "{$target.scope}:{$var_name}";
                %var_defs{$var_key} := 1;
            }
        }
        
        # Recursively process children
        if nqp::can($node, 'list') {
            for $node.list() -> $child {
                if $child {
                    self.analyze_variable_usage($child, %var_uses, %var_defs);
                }
            }
        }
    }
    
    # Remove variable definitions that are never used
    method remove_unused_variable_definitions($node, %var_uses, %var_defs) {
        # Skip null nodes
        if !$node {
            return $node;
        }
        
        # Process statement blocks
        if nqp::istype($node, QAST::Stmts) {
            my $new_stmts := QAST::Stmts.new();
            my $modified := False;
            
            for $node.list() -> $stmt {
                # Check if this is a variable definition that's never used
                if nqp::istype($stmt, QAST::Op) && ($stmt.op eq 'assign' || $stmt.op eq 'bind' || $stmt.op eq 'bindlex') {
                    my $target := $stmt[1];
                    if nqp::istype($target, QAST::Var) {
                        my $var_name := $target.name;
                        my $var_key := "{$target.scope}:{$var_name}";
                        
                        # Check if this variable is never used
                        if %var_defs{$var_key}:exists && !%var_uses{$var_key}:exists {
                            # Skip this assignment
                            $modified := True;
                            next;
                        }
                    }
                }
                
                # Recursively process the statement
                my $optimized_stmt := self.remove_unused_variable_definitions($stmt, %var_uses, %var_defs);
                if !nqp::eqaddr($optimized_stmt, $stmt) {
                    $modified := True;
                }
                
                $new_stmts.push($optimized_stmt);
            }
            
            if $modified {
                return $new_stmts;
            }
        }
        
        # Recursively process children
        if nqp::can($node, 'list') {
            my $modified := False;
            
            for $node.list().kv -> $i, $child {
                if $child {
                    my $optimized_child := self.remove_unused_variable_definitions($child, %var_uses, %var_defs);
                    if !nqp::eqaddr($optimized_child, $child) {
                        $node[$i] := $optimized_child;
                        $modified := True;
                    }
                }
            }
            
            if $modified {
                return $node;
            }
        }
        
        return $node;
    }
    
    # Perform global common subexpression elimination
    method perform_global_cse($node) {
        # Only for highest optimization level
        if $!level < 4 {
            return nqp::null();
        }
        
        # Create a working environment for CSE
        my %expr_hash;
        
        # Apply CSE transformation
        my $optimized := self.apply_common_subexpression_elimination($node, %expr_hash);
        
        # Check if any changes were made
        if !nqp::eqaddr($optimized, $node) {
            return $optimized;
        }
        
        return nqp::null();
    }
    
    # Apply common subexpression elimination
    method apply_common_subexpression_elimination($node, %expr_hash is rw) {
        # Skip null nodes
        if !$node {
            return $node;
        }
        
        # Process operations for CSE
        if nqp::istype($node, QAST::Op) {
            # Skip operations with side effects
            my %side_effect_ops := ('assign' => 1, 'bind' => 1, 'bindlex' => 1, 'call' => 1, 
                                   'return' => 1, 'last' => 1, 'next' => 1, 'redo' => 1, 
                                   'exit' => 1, 'die' => 1);
            
            if !%side_effect_ops{$node.op}:exists {
                # Generate a hash key for this expression
                my $expr_key := self.generate_expression_hash_key($node);
                
                # If we've seen this expression before, return the previously computed value
                if %expr_hash{$expr_key}:exists {
                    return %expr_hash{$expr_key};
                }
                
                # Process children first (bottom-up approach)
                my $modified := False;
                for $node.list().kv -> $i, $child {
                    if $child {
                        my $optimized_child := self.apply_common_subexpression_elimination($child, %expr_hash);
                        if !nqp::eqaddr($optimized_child, $child) {
                            $node[$i] := $optimized_child;
                            $modified := True;
                        }
                    }
                }
                
                # Store this expression in the hash
                %expr_hash{$expr_key} := $node;
                
                if $modified {
                    return $node;
                }
            }
            else {
                # For side-effecting operations, still process children but don't track the expression
                my $modified := False;
                for $node.list().kv -> $i, $child {
                    if $child {
                        my $optimized_child := self.apply_common_subexpression_elimination($child, %expr_hash);
                        if !nqp::eqaddr($optimized_child, $child) {
                            $node[$i] := $optimized_child;
                            $modified := True;
                        }
                    }
                }
                
                if $modified {
                    return $node;
                }
            }
        }
        
        # Process statement blocks with fresh expression hashes for each scope
        elsif nqp::istype($node, QAST::Stmts) {
            my $new_stmts := QAST::Stmts.new();
            my $modified := False;
            
            for $node.list() -> $stmt {
                # Process each statement with the current expression hash
                my $optimized_stmt := self.apply_common_subexpression_elimination($stmt, %expr_hash);
                if !nqp::eqaddr($optimized_stmt, $stmt) {
                    $modified := True;
                }
                $new_stmts.push($optimized_stmt);
            }
            
            if $modified {
                return $new_stmts;
            }
        }
        
        # Recursively process children for other node types
        if nqp::can($node, 'list') {
            my $modified := False;
            
            for $node.list().kv -> $i, $child {
                if $child {
                    my $optimized_child := self.apply_common_subexpression_elimination($child, %expr_hash);
                    if !nqp::eqaddr($optimized_child, $child) {
                        $node[$i] := $optimized_child;
                        $modified := True;
                    }
                }
            }
            
            if $modified {
                return $node;
            }
        }
        
        return $node;
    }
    
    # Generate a hash key for an expression
    method generate_expression_hash_key($expr) {
        # For constants, use their value as the key
        if nqp::istype($expr, QAST::IVal) {
            return "IVal:{$expr.value}";
        }
        elsif nqp::istype($expr, QAST::NVal) {
            return "NVal:{$expr.value}";
        }
        elsif nqp::istype($expr, QAST::SVal) {
            return "SVal:{$expr.value}";
        }
        elsif nqp::istype($expr, QAST::BVal) {
            return "BVal:{$expr.value}";
        }
        elsif nqp::istype($expr, QAST::Var) {
            return "Var:{$expr.scope}:{$expr.name}";
        }
        elsif nqp::istype($expr, QAST::Op) {
            # For operations, combine the op name with the keys of its operands
            my $key := "Op:{$expr.op}";
            for $expr.list() -> $child {
                if $child {
                    $key := $key ~ ":" ~ self.generate_expression_hash_key($child);
                }
            }
            return $key;
        }
        
        # Default fallback
        return "Unknown:" ~ nqp::addr($expr);
    }
    
    # Remove code with no observable effects
    method remove_effectless_code($node) {
        # Check for operations that have no side effects and whose results are not used
        if nqp::istype($node, QAST::Op) && $!void_context {
            # Examples of operations with no side effects
            my %effectless_ops := ('add' => 1, 'sub' => 1, 'mul' => 1, 'div' => 1, 
                                  'eq' => 1, 'ne' => 1, 'lt' => 1, 'gt' => 1, 
                                  'le' => 1, 'ge' => 1, 'and' => 1, 'or' => 1, 
                                  'abs' => 1, 'sqrt' => 1, 'log' => 1);
            
            if %effectless_ops{$node.op}:exists {
                return QAST::Op.new(:op<null>);
            }
        }
        
        # Also check for redundant sequence of statements that cancel each other out
        if nqp::istype($node, QAST::Stmts) && nqp::elems($node) >= 2 {
            my @stmts := $node.list();
            my int $last_idx := nqp::elems(@stmts) - 1;
            
            # Check for pairs of assignments that cancel each other
            for 0..$last_idx-1 -> $i {
                my $stmt1 := @stmts[$i];
                my $stmt2 := @stmts[$i+1];
                
                if self.are_canceling_assignments($stmt1, $stmt2) {
                    # Create a new block without these two statements
                    my $new_stmts := QAST::Stmts.new();
                    for 0..$last_idx -> $j {
                        if $j != $i && $j != $i+1 {
                            $new_stmts.push(@stmts[$j]);
                        }
                    }
                    return $new_stmts;
                }
            }
        }
        
        return nqp::null();
    }
    
    # Check if two assignments cancel each other out
    method are_canceling_assignments($stmt1, $stmt2) {
        # Check if both are assignments
        if self.is_simple_assignment($stmt1) && self.is_simple_assignment($stmt2) {
            my $target1 := $stmt1[1];
            my $target2 := $stmt2[1];
            my $value1 := $stmt1[2];
            my $value2 := $stmt2[2];
            
            # Check if they assign to the same variable
            if nqp::istype($target1, QAST::Var) && nqp::istype($target2, QAST::Var) &&
               $target1.name eq $target2.name && $target1.scope eq $target2.scope {
                # Check if the second assignment uses the variable assigned by the first
                # This is a simplified check - in a real optimizer this would be more complex
                return self.does_expression_use_variable($value2, $target1.name, $target1.scope);
            }
        }
        
        return False;
    }
    
    # Check if an expression uses a specific variable
    method does_expression_use_variable($expr, $var_name, $var_scope) {
        # Skip null expressions
        if !$expr {
            return False;
        }
        
        # Check if this is the variable we're looking for
        if nqp::istype($expr, QAST::Var) && $expr.name eq $var_name && $expr.scope eq $var_scope {
            return True;
        }
        
        # Recursively check children
        if nqp::can($expr, 'list') {
            for $expr.list() -> $child {
                if $child && self.does_expression_use_variable($child, $var_name, $var_scope) {
                    return True;
                }
            }
        }
        
        return False;
    }
    
    # Check if a statement is a simple assignment
    method is_simple_assignment($stmt) {
        return nqp::istype($stmt, QAST::Op) && ($stmt.op eq 'assign' || $stmt.op eq 'bind' || $stmt.op eq 'bindlex') && 
               nqp::elems($stmt) == 3;
    }
    
    # Unroll small fixed-iteration loops
    method unroll_loops($node) {
        # Skip null nodes
        if !$node {
            return nqp::null();
        }
        
        # Only for higher optimization levels
        if $!level < 3 {
            return nqp::null();
        }
        
        # Process for loops with constant bounds
        if nqp::istype($node, QAST::Op) && $node.op eq 'for' && nqp::elems($node) == 3 {
            my $iterable := $node[1];
            my $loop_body := $node[2];
            
            # Check if iterable is a range with constant bounds
            if nqp::istype($iterable, QAST::Op) && $iterable.op eq 'range' && nqp::elems($iterable) == 3 {
                my $start := $iterable[1];
                my $end := $iterable[2];
                my $step := $iterable[3];
                
                # Check if start, end, and step are constants
                if nqp::istype($start, QAST::IVal) && nqp::istype($end, QAST::IVal) {
                    my $start_val := $start.value;
                    my $end_val := $end.value;
                    my $step_val := 1;
                    
                    # Handle step if it's a constant
                    if nqp::istype($step, QAST::IVal) {
                        $step_val := $step.value;
                    }
                    
                    # Only unroll small loops to avoid code bloat
                    my $iter_count := self.calculate_iteration_count($start_val, $end_val, $step_val);
                    if $iter_count > 0 && $iter_count <= 8 {
                        # Get the loop variable (assuming it's a QAST::Var node)
                        if nqp::istype($loop_body, QAST::Stmts) && nqp::elems($loop_body) >= 1 {
                            my $first_stmt := $loop_body[0];
                            if nqp::istype($first_stmt, QAST::Op) && ($first_stmt.op eq 'bind' || $first_stmt.op eq 'bindlex') {
                                my $loop_var := $first_stmt[1];
                                
                                if nqp::istype($loop_var, QAST::Var) {
                                    # Create the unrolled loop body
                                    my $unrolled_body := QAST::Stmts.new();
                                    
                                    # Unroll the loop by creating copies of the body
                                    for $start_val, $start_val + $step_val ... $end_val -> $i {
                                        my $iteration := self.replace_variable_in_node(
                                            $loop_body, $loop_var.name, $loop_var.scope, QAST::IVal.new(:value($i)));
                                        
                                        # Skip the binding statement in each iteration except the first
                                        if nqp::istype($iteration, QAST::Stmts) && nqp::elems($iteration) > 1 {
                                            for 1..nqp::elems($iteration)-1 -> $j {
                                                $unrolled_body.push($iteration[$j]);
                                            }
                                        }
                                    }
                                    
                                    # Add a null operation at the end to maintain void context
                                    $unrolled_body.push(QAST::Op.new(:op<null>));
                                    
                                    return $unrolled_body;
                                }
                            }
                        }
                    }
                }
            }
        }
        
        return nqp::null();
    }
    
    # Calculate the number of iterations for a range
    method calculate_iteration_count($start, $end, $step) {
        # Handle different cases based on step direction
        if $step > 0 && $start <= $end {
            return (($end - $start) / $step).Int + 1;
        }
        elsif $step < 0 && $start >= $end {
            return (($start - $end) / (-$step)).Int + 1;
        }
        
        return 0;
    }
    
    # Replace occurrences of a variable with a value in a node
    method replace_variable_in_node($node, $var_name, $var_scope, $replacement) {
        # Skip null nodes
        if !$node {
            return $node;
        }
        
        # Clone the node to avoid modifying the original
        my $cloned := self.clone_node($node);
        
        # Replace variable references
        if nqp::istype($cloned, QAST::Var) && $cloned.name eq $var_name && $cloned.scope eq $var_scope {
            return $replacement;
        }
        
        # Recursively process children
        if nqp::can($cloned, 'list') {
            for $cloned.list().kv -> $i, $child {
                if $child {
                    $cloned[$i] := self.replace_variable_in_node($child, $var_name, $var_scope, $replacement);
                }
            }
        }
        
        return $cloned;
    }
    
    # Merge adjacent statement blocks
    method merge_adjacent_blocks($node) {
        # Skip null nodes
        if !$node {
            return nqp::null();
        }
        
        # Process statement blocks
        if nqp::istype($node, QAST::Stmts) {
            my $new_stmts := QAST::Stmts.new();
            my $modified := False;
            
            for $node.list() -> $stmt {
                # If the statement is itself a statement block, merge its contents
                if nqp::istype($stmt, QAST::Stmts) {
                    for $stmt.list() -> $inner_stmt {
                        $new_stmts.push($inner_stmt);
                    }
                    $modified := True;
                }
                else {
                    $new_stmts.push($stmt);
                }
            }
            
            # If we modified something, return the merged block
            if $modified {
                return $new_stmts;
            }
        }
        
        return nqp::null();
    }
    
    # Optimize branch predictions
    method optimize_branch_predictions($node) {
        # Skip null nodes
        if !$node {
            return nqp::null();
        }
        
        # Only for higher optimization levels
        if $!level < 3 {
            return nqp::null();
        }
        
        # Process if statements
        if nqp::istype($node, QAST::Op) && $node.op eq 'if' && nqp::elems($node) >= 3 {
            my $condition := $node[1];
            my $then_branch := $node[2];
            my $else_branch := nqp::elems($node) >= 4 ?? $node[3] !! nqp::null();
            
            # Check if the condition is a constant (known at compile-time)
            if self.is_constant_condition($condition) {
                # Determine if the condition is always true or false
                my $always_true := self.evaluate_constant_condition($condition);
                
                # Replace with the appropriate branch
                if $always_true {
                    return $then_branch;
                }
                elsif $else_branch {
                    return $else_branch;
                }
                else {
                    # If condition is always false and there's no else branch, replace with null
                    return QAST::Op.new(:op<null>);
                }
            }
        }
        
        return nqp::null();
    }
    
    # Check if a condition is constant
    method is_constant_condition($condition) {
        # Check for boolean constants
        if nqp::istype($condition, QAST::BVal) {
            return True;
        }
        
        # Check for operations that always evaluate to the same boolean value
        if nqp::istype($condition, QAST::Op) {
            # Check if all operands are constants
            for $condition.list() -> $operand {
                if !self.is_constant_condition($operand) {
                    return False;
                }
            }
            
            # Return true for operations with all constant operands
            return True;
        }
        
        return False;
    }
    
    # Evaluate a constant condition
    method evaluate_constant_condition($condition) {
        # Handle boolean values directly
        if nqp::istype($condition, QAST::BVal) {
            return $condition.value;
        }
        
        # Handle simple boolean operations on constants
        if nqp::istype($condition, QAST::Op) {
            my $op := $condition.op;
            
            # Handle comparison operations
            if $op eq 'eq' || $op eq 'ne' || $op eq 'lt' || $op eq 'gt' || $op eq 'le' || $op eq 'ge' {
                my $left := $condition[1];
                my $right := $condition[2];
                
                # String comparisons
                if nqp::istype($left, QAST::SVal) && nqp::istype($right, QAST::SVal) {
                    given $op {
                        when 'eq' { return $left.value eq $right.value; }
                        when 'ne' { return $left.value ne $right.value; }
                        when 'lt' { return $left.value lt $right.value; }
                        when 'gt' { return $left.value gt $right.value; }
                        when 'le' { return $left.value le $right.value; }
                        when 'ge' { return $left.value ge $right.value; }
                    }
                }
                # Integer comparisons
                elsif nqp::istype($left, QAST::IVal) && nqp::istype($right, QAST::IVal) {
                    given $op {
                        when 'eq' { return $left.value == $right.value; }
                        when 'ne' { return $left.value != $right.value; }
                        when 'lt' { return $left.value < $right.value; }
                        when 'gt' { return $left.value > $right.value; }
                        when 'le' { return $left.value <= $right.value; }
                        when 'ge' { return $left.value >= $right.value; }
                    }
                }
                # Numeric comparisons
                elsif nqp::istype($left, QAST::NVal) && nqp::istype($right, QAST::NVal) {
                    given $op {
                        when 'eq' { return $left.value == $right.value; }
                        when 'ne' { return $left.value != $right.value; }
                        when 'lt' { return $left.value < $right.value; }
                        when 'gt' { return $left.value > $right.value; }
                        when 'le' { return $left.value <= $right.value; }
                        when 'ge' { return $left.value >= $right.value; }
                    }
                }
            }
            # Handle logical operations
            elsif $op eq 'and' || $op eq 'or' {
                my $left := self.evaluate_constant_condition($condition[1]);
                my $right := self.evaluate_constant_condition($condition[2]);
                
                if $op eq 'and' {
                    return $left && $right;
                }
                else {
                    return $left || $right;
                }
            }
            # Handle negation
            elsif $op eq 'not' {
                return !self.evaluate_constant_condition($condition[1]);
            }
        }
        
        # Default to false if we can't evaluate
        return False;
    }
    
    # Clone a node for safe modification
    method clone_node($node) {
        if !$node {
            return nqp::null();
        }
        
        # Create a new node of the same type
        my $new_node;
        given $node {
            when QAST::Op {
                $new_node := QAST::Op.new(:op($node.op));
                $new_node.name := $node.name if nqp::can($node, 'name');
                $new_node.block := $node.block if nqp::can($node, 'block');
                # Copy children
                for $node.list() -> $child {
                    $new_node.push(self.clone_node($child));
                }
            }
            when QAST::Stmts {
                $new_node := QAST::Stmts.new();
                # Copy children
                for $node.list() -> $child {
                    $new_node.push(self.clone_node($child));
                }
            }
            default {
                # For other node types, just return the original
                # In a real implementation, we would clone these as well
                $new_node := $node;
            }
        }
        
        return $new_node;
    }

    # Visit a QAST::Stmts node
    method visit_stmts($stmts, :$block_structure = False) {
        # Visit all statements
        my @new_stmts;
        my int $i := 0;
        my int $last := nqp::elems($stmts) - 1;
        my int $dead_start := -1;
        
        for $stmts.list() -> $stmt {
            # If we've already found dead code, skip processing
            if $dead_start >= 0 {
                next;
            }
            
            my int $old_void := $!void_context;
            $!void_context := $i != $last;
            my $visited := self.visit($stmt, :$block_structure);
            $!void_context := $old_void;
            
            # Skip null statements (optimized away)
            unless nqp::istype($visited, QAST::Op) && $visited.op eq 'null' {
                # Apply branch prediction optimization at level 3 or higher
                if $!level >= 3 && nqp::istype($visited, QAST::Op) && $visited.op eq 'if' {
                    my $optimized := self.optimize_branch_predictions($visited);
                    if $optimized && !nqp::eqaddr($optimized, $visited) {
                        $visited := $optimized;
                    }
                }
                # Apply loop unrolling at level 3 or higher
                elsif $!level >= 3 && nqp::istype($visited, QAST::Op) && $visited.op eq 'for' {
                    my $optimized := self.unroll_loops($visited);
                    if $optimized && !nqp::eqaddr($optimized, $visited) {
                        $visited := $optimized;
                    }
                }
                
                @new_stmts.push($visited);
                
                # Check for terminating statements that make subsequent code dead
                if $!level >= 2 && nqp::istype($visited, QAST::Op) && 
                   ($visited.op eq 'return' || $visited.op eq 'last' || 
                    $visited.op eq 'next' || $visited.op eq 'redo') {
                    $dead_start := $i;
                }
            }
            $i++;
        }
        
        # Apply common subexpression elimination if optimization level is high enough
        if $!level >= 3 && +@new_stmts > 1 {
            @new_stmts := self.eliminate_common_subexpressions(@new_stmts);
        }
        
        # Apply statement block merging at level 1 or higher
        # Create a new statement block with the processed statements
        my $optimized_stmts := QAST::Stmts.new();
        for @new_stmts -> $stmt {
            $optimized_stmts.push($stmt);
        }
        
        # Try to merge adjacent blocks
        my $merged := self.merge_adjacent_blocks($optimized_stmts);
        if $merged && !nqp::eqaddr($merged, $optimized_stmts) {
            $stmts.set_children($merged.list());
        } else {
            $stmts.set_children(@new_stmts);
        }
        
        $stmts
    }
    
    # Eliminate common subexpressions within a statement list
    method eliminate_common_subexpressions(@stmts) {
        # Only apply at optimization level 3 or higher
        if $!level < 3 {
            return @stmts;
        }
        
        my %expr_to_var;     # Map from expression hash to variable node
        my %var_counter;     # Counter for generating unique variable names
        my @new_stmts;       # New statement list with optimized expressions
        my %modified_vars;   # Set of variables modified so far
        
        # Process each statement
        for @stmts -> $stmt {
            # Process the statement to eliminate common subexpressions
            my ($processed_stmt, $new_vars) := self.process_statement($stmt, %expr_to_var, %var_counter, %modified_vars);
            
            # Add the processed statement to the new list
            @new_stmts.push($processed_stmt);
            
            # Update the modified variables set
            if $new_vars {
                for @$new_vars -> $var {
                    %modified_vars{$var} := True;
                    
                    # Remove any expressions that depend on this variable
                    self.remove_dependent_expressions(%expr_to_var, $var);
                }
            }
        }
        
        @new_stmts
    }
    
    # Process a single statement for common subexpression elimination
    method process_statement($stmt, %expr_to_var, %var_counter, %modified_vars) {
        my @modified_vars;
        
        # Process based on statement type
        if nqp::istype($stmt, QAST::Op) {
            # Check if this is a variable modification operation
            if self.is_variable_modification($stmt) {
                my $var_name := self.get_modified_variable_name($stmt);
                if $var_name {
                    @modified_vars.push($var_name);
                }
            }
            
            # Process the operation node
            $stmt := self.process_op_node($stmt, %expr_to_var, %var_counter, %modified_vars);
        }
        elsif nqp::istype($stmt, QAST::Bind) {
            # This is a binding operation, which modifies the left-hand side
            if nqp::istype($stmt[0], QAST::Var) {
                @modified_vars.push($stmt[0].name);
            }
            
            # Process the right-hand side expression
            $stmt[1] := self.process_expression($stmt[1], %expr_to_var, %var_counter, %modified_vars);
        }
        elsif nqp::istype($stmt, QAST::Stmts) {
            # Recursively process nested statements
            my @processed_children;
            for $stmt.list() -> $child {
                my ($processed_child, $child_modified_vars) := 
                    self.process_statement($child, %expr_to_var, %var_counter, %modified_vars);
                @processed_children.push($processed_child);
                if $child_modified_vars {
                    @modified_vars.append(@$child_modified_vars);
                }
            }
            $stmt.set_children(@processed_children);
        }
        
        return ($stmt, @modified_vars);
    }
    
    # Process an operation node for common subexpression elimination
    method process_op_node($op, %expr_to_var, %var_counter, %modified_vars) {
        my $op_type := $op.op;
        
        # Don't process control flow operations or variable modifications
        if self.is_control_flow_op($op_type) || self.is_variable_modification($op) {
            # Just process the children
            for 0 ..^ nqp::elems($op) -> $i {
                if $op[$i] {
                    $op[$i] := self.process_expression($op[$i], %expr_to_var, %var_counter, %modified_vars);
                }
            }
            return $op;
        }
        
        # Process children first (bottom-up approach)
        for 0 ..^ nqp::elems($op) -> $i {
            if $op[$i] {
                $op[$i] := self.process_expression($op[$i], %expr_to_var, %var_counter, %modified_vars);
            }
        }
        
        # Now process this node itself
        return self.process_expression($op, %expr_to_var, %var_counter, %modified_vars);
    }
    
    # Process an expression node for common subexpression elimination
    method process_expression($expr, %expr_to_var, %var_counter, %modified_vars) {
        # Skip processing for simple values and variables
        if self.is_simple_value($expr) || nqp::istype($expr, QAST::Var) {
            return $expr;
        }
        
        # Skip complex operations that aren't worth optimizing
        if !self.is_optimizable_expression($expr) {
            return $expr;
        }
        
        # Check if any variables in this expression have been modified
        if self.has_modified_variables($expr, %modified_vars) {
            return $expr;
        }
        
        # Generate a hash key for this expression
        my $hash_key := self.generate_expression_hash($expr);
        
        # If this expression has been seen before, replace it with a variable reference
        if %expr_to_var{$hash_key} {
            return %expr_to_var{$hash_key}.clone();
        }
        
        # Otherwise, create a new variable to store this expression result
        # Only do this for non-trivial expressions that are likely to be repeated
        if self.is_complex_expression($expr) {
            # Generate a unique variable name
            %var_counter<temp> := (%var_counter<temp> // 0) + 1;
            my $var_name := "_temp_\{%var_counter<temp>\}";
            
            # Create a new lexical variable
            my $var_node := QAST::Var.new(:name($var_name), :scope('lexical'));
            
            # Create a binding statement to compute and store the expression
            my $bind_stmt := QAST::Bind.new();
            $bind_stmt.push(QAST::Var.new(:name($var_name), :scope('lexical'), :decl<var>));
            $bind_stmt.push($expr.clone());
            
            # Insert the binding statement before the current statement
            # This will be handled by the caller
            
            # Store the variable reference for future use
            %expr_to_var{$hash_key} := $var_node;
            
            # Return the variable reference
            return $var_node;
        }
        
        return $expr;
    }
    
    # Generate a hash key for an expression node
    method generate_expression_hash($expr) {
        my $hash := 'type:' ~ $expr.WHAT.^name;
        
        # For Op nodes, include the operation type
        if nqp::istype($expr, QAST::Op) {
            $hash ~= ';op:' ~ $expr.op;
            
            # Add hash of children
            for 0 ..^ nqp::elems($expr) -> $i {
                if $expr[$i] {
                    $hash ~= ';child' ~ $i ~ ':' ~ self.generate_expression_hash($expr[$i]);
                }
            }
        }
        # For Var nodes, include the name and scope
        elsif nqp::istype($expr, QAST::Var) {
            $hash ~= ';name:' ~ $expr.name ~ ';scope:' ~ $expr.scope;
        }
        # For value nodes, include the value
        elsif nqp::istype($expr, QAST::IVal) || nqp::istype($expr, QAST::NVal) || 
              nqp::istype($expr, QAST::SVal) || nqp::istype($expr, QAST::BVal) {
            $hash ~= ';value:' ~ $expr.value;
        }
        
        $hash
    }
    
    # Check if an expression contains any variables that have been modified
    method has_modified_variables($expr, %modified_vars) {
        # Check variables in the expression
        if nqp::istype($expr, QAST::Var) && $expr.scope eq 'lexical' {
            return %modified_vars{$expr.name}:exists;
        }
        
        # Check children recursively
        if $expr.can('list') {
            for $expr.list() -> $child {
                if $child && self.has_modified_variables($child, %modified_vars) {
                    return True;
                }
            }
        }
        
        False
    }
    
    # Remove expressions that depend on a modified variable with enhanced dependency analysis
    method remove_dependent_expressions(%expr_to_var, $var_name) {
        # Create a new hash to store only non-dependent expressions
        my %new_expr_to_var;

        # Check if we have dependency tracking data
        if $!expr_dependencies {
            # Enhanced dependency tracking with transitive dependency support
            for %expr_to_var.kv -> $hash_key, $var_node {
                # Skip expressions that directly or indirectly depend on the modified variable
                unless self.has_dependency($hash_key, $var_name) {
                    %new_expr_to_var{$hash_key} := $var_node;
                }
            }
        }
        else {
            # Improved heuristic approach with better pattern matching
            for %expr_to_var.kv -> $hash_key, $var_node {
                # Check for variable name with various patterns and scoping contexts
                my $var_pattern := /;name:$var_name;scope:/;
                my $simple_var_pattern := /;$var_name;/;
                
                # Enhanced dereferencing patterns for more complex variable accesses
                my $deref_pattern := /\.\$var_name|\$var_name\[|\$var_name\.|\$var_name\(|postcircumfix:<\[\]>.*\$var_name|postcircumfix:<\(\)>.*\$var_name/;
                
                # Check for method calls on the variable
                my $method_pattern := /callmethod.*\$var_name|\$var_name.*callmethod/;
                
                # Skip expressions that match any of these dependency patterns
                unless $hash_key ~~ $var_pattern || $hash_key ~~ $simple_var_pattern || 
                       $hash_key ~~ $deref_pattern || $hash_key ~~ $method_pattern {
                    %new_expr_to_var{$hash_key} := $var_node;
                }
            }
        }
        
        # Replace the old map with the filtered one
        %expr_to_var := %new_expr_to_var;
        
        # Log this optimization if enabled
        if $!level >= 4 {
            self.log_optimization('dependency_removal', nqp::hash(), nqp::hash(), "Removed expressions dependent on $var_name");
        }
    }
    
    # Check if an expression has a direct or transitive dependency on a variable
    method has_dependency($expr_key, $var_name) {
        # Check direct dependency
        if $!expr_dependencies{$expr_key} && $!expr_dependencies{$expr_key}{$var_name} {
            return True;
        }
        
        # Check transitive dependencies
        if $!expr_dependencies{$expr_key} {
            for $!expr_dependencies{$expr_key}.keys -> $dep_var {
                # Check if this dependency itself depends on the target variable
                for %!expr_to_var.keys -> $other_key {
                    if $!expr_dependencies{$other_key} && $!expr_dependencies{$other_key}{$dep_var} {
                        if self.has_dependency($other_key, $var_name) {
                            return True;
                        }
                    }
                }
            }
        }
        
        False;
    }
    
    # Track variable dependencies for an expression
    method track_expression_dependencies($expr, $hash_key) {
        # Initialize dependency map if not exists
        unless $!expr_dependencies {
            $!expr_dependencies := nqp::hash();
        }
        
        # Create a new set for this expression's dependencies
        my %dependencies;
        
        # Extract variable references from the expression
        # This is a simplified approach - in a real implementation, we would traverse the AST
        if nqp::istype($expr, QAST::Var) {
            %dependencies{$expr.name} := True;
        }
        elsif nqp::istype($expr, QAST::Op) {
            # Process each child node
            for $expr.list() -> $child {
                if nqp::istype($child, QAST::Var) {
                    %dependencies{$child.name} := True;
                }
                elsif nqp::istype($child, QAST::Op) || nqp::istype($child, QAST::Stmts) {
                    # Recursively track dependencies for nested expressions
                    my %child_deps = self.track_expression_dependencies($child, "");
                    for %child_deps.kv -> $dep_name, $value {
                        %dependencies{$dep_name} := True;
                    }
                }
            }
        }
        
        # Store the dependencies for this expression
        if nqp::elems(%dependencies) > 0 && $hash_key {
            $!expr_dependencies{$hash_key} := %dependencies;
        }
        
        %dependencies;
    }
    
    # Check if an operation modifies a variable
    method is_variable_modification($op) {
        if !nqp::istype($op, QAST::Op) {
            return False;
        }
        
        my $op_type := $op.op;
        
        # Assignment operations
        return $op_type eq 'assign' || $op_type eq 'addassign' || 
               $op_type eq 'subassign' || $op_type eq 'mulassign' || 
               $op_type eq 'divassign' || $op_type eq 'modassign' ||
               $op_type eq 'xassign' || $op_type eq 'xxassign' ||
               $op_type eq 'powassign' || $op_type eq 'concatassign' ||
               $op_type eq 'push' || $op_type eq 'append';
    }
    
    # Get the name of the variable modified by an operation
    method get_modified_variable_name($op) {
        if !nqp::istype($op, QAST::Op) || !$op[0] || !nqp::istype($op[0], QAST::Var) {
            return nqp::null();
        }
        
        return $op[0].name if $op[0].scope eq 'lexical';
        return nqp::null();
    }
    
    # Check if an operation is a control flow operation
    method is_control_flow_op($op_type) {
        my @control_flow_ops := <if unless while loop for forkvals foreach given when take last next redo return leave>;
        nqp::exists(@control_flow_ops, $op_type);
    }
    
    # Check if a node is a simple value (constant)
    method is_simple_value($node) {
        return nqp::istype($node, QAST::IVal) || nqp::istype($node, QAST::NVal) ||
               nqp::istype($node, QAST::SVal) || nqp::istype($node, QAST::BVal) ||
               nqp::istype($node, QAST::Val);
    }
    
    # Enhanced check for optimizable expressions with additional criteria
    method is_optimizable_expression($expr) {
        # Skip if not an operation node
        if !nqp::istype($expr, QAST::Op) {
            return False;
        }
        
        my $op_type := $expr.op;
        
        # Skip control flow operations
        if self.is_control_flow_op($op_type) {
            return False;
        }
        
        # Skip variable modification operations
        if self.is_variable_modification($expr) {
            return False;
        }
        
        # Skip operations with side effects
        if self.has_side_effects($expr) {
            return False;
        }
        
        # Ensure we have enough children for meaningful optimization
        if nqp::elems($expr) < 1 {
            return False;
        }
        
        # Check if the expression is deterministic
        if !self.is_deterministic($expr) {
            return False;
        }
        
        # For certain operations, we require at least two children
        my @multi_arg_ops := <add sub mul div mod pow infix:<+> infix:<-> infix:<*> infix:</> infix:<*> infix:<**>>;
        if @multi_arg_ops.grep({ $_ eq $op_type }) && nqp::elems($expr) < 2 {
            return False;
        }
        
        # Check for optimization potential based on complexity
        return self.has_optimization_potential($expr);
    }
    
    # Check if an expression has side effects
    method has_side_effects($expr) {
        if !nqp::istype($expr, QAST::Op) {
            return False;
        }
        
        # Known operations with side effects
        my @side_effect_ops := <bind bindattr bindkey bindpos assign addassign subassign mulassign divassign modassign xassign xxassign powassign concatassign push append pop shift unshift splice delete deletekey deletepos>;
        
        # Check for direct side effects
        if @side_effect_ops.grep({ $_ eq $expr.op }) {
            return True;
        }
        
        # Check for method calls that might have side effects
        if $expr.op eq 'callmethod' {
            # Common methods that are known to be pure
            my @pure_methods := <uc lc tc fc chars ords flip trim triml trimr starts-with ends-with contains index rindex substr chomp chompchomp>;
            my $method_name := $expr.name // '';
            
            # If it's not in our list of known pure methods, assume it has side effects
            if !@pure_methods.grep({ $_ eq $method_name }) {
                return True;
            }
        }
        
        # Recursively check children for side effects
        for $expr.list() -> $child {
            if self.has_side_effects($child) {
                return True;
            }
        }
        
        False;
    }
    
    # Check if an expression is deterministic (same inputs always produce same outputs)
    method is_deterministic($expr) {
        if !nqp::istype($expr, QAST::Op) {
            return True;  # Assume simple values are deterministic
        }
        
        # Known non-deterministic operations
        my @non_deterministic := <rand roll pick now time>;
        if @non_deterministic.grep({ $_ eq $expr.op }) {
            return False;
        }
        
        # Method calls might be non-deterministic
        if $expr.op eq 'callmethod' {
            # Known deterministic methods
            my @deterministic_methods := <uc lc tc fc chars ords flip trim triml trimr starts-with ends-with contains index rindex substr chomp chompchomp>;
            my $method_name := $expr.name // '';
            
            # If not in our known deterministic list, assume non-deterministic for safety
            if !@deterministic_methods.grep({ $_ eq $method_name }) {
                return False;
            }
        }
        
        # Recursively check children
        for $expr.list() -> $child {
            if !self.is_deterministic($child) {
                return False;
            }
        }
        
        True;
    }
    
    # Check if an expression has sufficient complexity to benefit from optimization
    method has_optimization_potential($expr) {
        # Count operations to determine complexity
        my $op_count := 0;
        
        self.visit_node($expr, sub ($node, $) {
            if nqp::istype($node, QAST::Op) && !self.is_control_flow_op($node.op) {
                $op_count++;
            }
            1;  # Continue walking
        });
        
        # Expressions with at least one operation or multiple constant values are worth optimizing
        $op_count >= 1;
    }
    
    # Check if an expression is complex enough to warrant optimization
    method is_complex_expression($expr) {
        if !nqp::istype($expr, QAST::Op) {
            return False;
        }
        
        # Count the number of operations in the expression tree
        my int $complexity := self.calculate_expression_complexity($expr);
        
        # Only optimize expressions with sufficient complexity
        return $complexity >= 2;
    }
    
    # Calculate the complexity of an expression
    method calculate_expression_complexity($expr) {
        if self.is_simple_value($expr) || nqp::istype($expr, QAST::Var) {
            return 1;
        }
        
        my int $complexity := 1;
        
        if $expr.can('list') {
            for $expr.list() -> $child {
                if $child {
                    $complexity := $complexity + self.calculate_expression_complexity($child);
                }
            }
        }
        
        $complexity
    }

    # Visit a QAST::Var node with enhanced type optimizations
    method visit_var($var) {
        my str $name := $var.name;
        my str $scope := $var.scope;
        
        # Register variable usage
        if $scope eq 'lexical' || $scope eq 'lexicalref' {
            $!block_var_stack.do('register_usage_inner', $var, $name);
            $!block_var_stack.do('register_usage_flat', $var, $name);
            if $!block_var_stack.do('is_in_handle_handler') {
                $!block_var_stack.do('mark_used_in_handle_handler', $name);
            }
            
            # Enhanced optimizations for higher optimization levels
            if $!level >= 2 && $scope eq 'lexical' {
                # 1. Advanced constant propagation with type awareness
                my $const_value := self.get_constant_value($var);
                if $const_value {
                    # Log the optimization for profiling
                    self.log_optimization('var_to_constant', $var, $const_value);
                    return $const_value;
                }
                
                # 2. Type-based optimizations (level 3+)
                if $!level >= 3 {
                    my $type := self.infer_variable_type($var);
                    if $type {
                        # Apply type-specific optimizations
                        my $optimized := self.apply_type_optimization($var, $type);
                        return $optimized if $optimized;
                    }
                }
            }
        }
        elsif $scope eq 'outer' || $scope eq 'outerref' {
            $!block_var_stack.do('register_getlexouter_usage', $var);
            
            # Check if outer references can be optimized (level 3+)
            if $!level >= 3 {
                my $optimized := self.optimize_outer_reference($var);
                return $optimized if $optimized;
            }
        }
        
        # Dead variable elimination (level 4+)
        if $!level >= 4 && self.is_dead_variable($var) {
            # Replace with null operation for dead variables
            return QAST::Op.new(:op<null>);
        }
        
        $var
    }
    
    # Infer the type of a variable
    method infer_variable_type($var) {
        # Use the symbols system to get type information
        return $!symbols.get_type_for($var.name) if $!symbols.can('get_type_for');
        return nqp::null();
    }
    
    # Apply type-specific optimizations
    method apply_type_optimization($var, $type) {
        # Implement type-specific optimizations based on the variable's type
        return nqp::null() unless $type;
        
        # Check if we're in a high enough optimization level
        return nqp::null() unless $!level >= 3;
        
        # Safely apply type annotations regardless of variable scope
        my $type_hint := nqp::null();
        
        # Integer-specific optimizations
        if nqp::existskey($type.WHAT.HOW, 'PRIM_TYPE') && nqp::objprimspec($type) == nqp::const::BIND_VAL_INT ||
           $type.DEFINITE && $type.WHAT.^name eq 'Int' ||
           nqp::istype($type, Str) && $type ~~ /^int(\d+)?$/ {
            $type_hint := 'int';
            
            # For integer variables, add type hints to help JIT optimizer
            $var.annotate('type_hint', $type_hint);
            $var.annotate('optimize_for', 'integer');
            
            # For integer operations, add additional optimizations
            if nqp::istype($var, QAST::Op) {
                # Add operation-specific optimizations
                given $var.op {
                    when 'add', 'sub', 'mul' {
                        $var.annotate('optimized_arithmetic', 1);
                    }
                    when 'div' {
                        $var.annotate('optimized_integer_division', 1);
                    }
                    when 'bit_and', 'bit_or', 'bit_xor' {
                        $var.annotate('optimized_bitwise', 1);
                    }
                }
            }
        }
        # Float/Num optimizations
        elsif nqp::existskey($type.WHAT.HOW, 'PRIM_TYPE') && nqp::objprimspec($type) == nqp::const::BIND_VAL_NUM ||
              $type.DEFINITE && $type.WHAT.^name eq 'Num' ||
              nqp::istype($type, Str) && $type ~~ /^(num|float)(\d+)?$/ {
            $type_hint := 'num';
            $var.annotate('type_hint', $type_hint);
            $var.annotate('optimize_for', 'number');
            
            if nqp::istype($var, QAST::Op) {
                given $var.op {
                    when 'add', 'sub', 'mul', 'div' {
                        $var.annotate('optimized_float_arithmetic', 1);
                    }
                }
            }
        }
        # Str optimizations
        elsif $type.DEFINITE && $type.WHAT.^name eq 'Str' ||
              nqp::istype($type, Str) && $type ~~ /^str(ing)?$/ {
            $type_hint := 'str';
            $var.annotate('type_hint', $type_hint);
            $var.annotate('optimize_for', 'string');
            
            if nqp::istype($var, QAST::Op) {
                given $var.op {
                    when 'concat', 'substr', 'chr' {
                        $var.annotate('optimized_string_op', 1);
                    }
                }
            }
        }
        # Bool optimizations
        elsif $type.DEFINITE && $type.WHAT.^name eq 'Bool' ||
              nqp::istype($type, Str) && $type ~~ /^bool(ean)?$/ {
            $type_hint := 'bool';
            $var.annotate('type_hint', $type_hint);
            $var.annotate('optimize_for', 'boolean');
            
            if nqp::istype($var, QAST::Op) {
                given $var.op {
                    when 'if', 'and', 'or', 'not' {
                        $var.annotate('optimized_boolean_op', 1);
                    }
                }
            }
        }
        # Handle known types from the symbols system
        else {
            # Safely check symbols without relying on specific methods
            try {
                if $!symbols && $!symbols.DEFINITE {
                    # Check for common types through string comparison as fallback
                    my $type_name := $type.DEFINITE ?? $type.WHAT.^name !! Str($type);
                    
                    # Handle container types
                    if $type_name eq 'Array' || $type_name eq 'List' || $type_name eq 'Positional' {
                        $var.annotate('optimize_for', 'collection');
                        $var.annotate('container_type', $type_name);
                    }
                    elsif $type_name eq 'Hash' || $type_name eq 'Map' || $type_name eq 'Associative' {
                        $var.annotate('optimize_for', 'associative');
                        $var.annotate('container_type', $type_name);
                    }
                    # Add type-specific method call optimizations
                    if nqp::istype($var, QAST::Call) && $var.name {
                        $var.annotate('method_call_optimized', 1);
                        $var.annotate('target_type', $type_name);
                    }
                }
            }
        }
        
        # Add general type optimization annotation
        $var.annotate('type_optimized', 1);
        
        return nqp::null(); # Return null to indicate no replacement
    }
    
    # Optimize outer references
    method optimize_outer_reference($var) {
        # Only optimize at higher optimization levels
        return nqp::null() unless $!level >= 2;
        
        # Only optimize lexical outer references
        return nqp::null() unless $var.scope eq 'lexical';
        
        # Get the variable name
        my $name := $var.name;
        return nqp::null() unless $name;
        
        # Get the current block from symbols
        my $current_block := nqp::null();
        if $!symbols && $!symbols.responds-to('current_block') {
            $current_block := $!symbols.current_block();
        }
        return nqp::null() unless $current_block; # Can't optimize without a block
        
        # Safely get usage count
        my $usage_count := 0;
        my $has_cache := False;
        my $in_hot_loop := False;
        
        if $!block_var_stack && $!block_var_stack.top {
            my $top := $!block_var_stack.top;
            if $top.can('get_usage_count') {
                $usage_count := $top.get_usage_count($name);
            }
            if $top.can('has_local_cache') {
                $has_cache := $top.has_local_cache($name);
            }
            if $top.can('is_in_hot_loop') {
                $in_hot_loop := $top.is_in_hot_loop();
            }
        }
        
        # Determine if we should cache this outer reference
        my $should_cache := 
            !$has_cache && 
            ($usage_count >= 2 || $in_hot_loop || $!level >= 3);
        
        # Early return if no caching needed
        unless $should_cache {
            # For level 3 optimization, still add an optimization hint
            if $!level >= 3 {
                $var.annotate('optimized_outer_ref', 1);
            }
            return nqp::null();
        }
        
        # Create a unique local variable name
        my $local_name := "_cached_outer_" ~ $name.substr(1);
        
        # Create a binding op to cache the outer reference
        my $cache_op := QAST::Op.new(
            :op('bind'),
            QAST::Var.new(:name($local_name), :scope('local'), :decl('var')),
            QAST::Var.new(:name($name), :scope('lexical'))
        );
        
        # Safely add the cache op to the current block
        if $!block_var_stack && $!block_var_stack.top && $!block_var_stack.top.can('prepend_to_current_block') {
            $!block_var_stack.top.prepend_to_current_block($current_block, $cache_op);
            $!block_var_stack.top.mark_local_cache($name);
            
            # Return a reference to the local cache variable
            return QAST::Var.new(:name($local_name), :scope('local'));
        }
        
        return nqp::null(); # No optimization applicable
    }
    
    # Check if a variable is dead (not used after definition)
    method is_dead_variable($var) {
        # Use block var stack to check if the variable is truly dead
        return False unless $var.name;
        return $!block_var_stack.do('is_variable_dead', $var.name) if $!block_var_stack.can('is_variable_dead');
        return False;
    }

    # Visit a QAST::Want node
    method visit_want($want, :$block_structure = False) {
        # Visit the children (condition and branches)
        self.visit($want[0], :$block_structure);
        self.visit($want[2], :$block_structure);
        
        # Optimize want nodes with constant conditions
        if $!level >= 2 && nqp::istype($want[0], QAST::BVal) {
            if $want[0].value == 1 {
                # Condition is always true, replace with the true branch
                return $want[1];
            }
            else {
                # Condition is always false, replace with the false branch
                return $want[2] || QAST::Op.new(:op<null>);
            }
        }
        
        $want
    }
    
    # Inline small routines for better performance
    method inline_small_routines($routine) {
        # Only inline at optimization level 3 or higher
        if $!level < 3 {
            return $routine;
        }
        
        # Check if the routine is eligible for inlining
        if self.is_routine_inlinable($routine) {
            # Get the body of the routine
            my $body := $routine[1];
            if $body && nqp::istype($body, QAST::Stmts) {
                # Create a copy of the body statements
                my @inlined_stmts;
                
                # Process each statement in the body
                for $body.list() -> $stmt {
                    # Skip return statements and adjust them as needed
                    if nqp::istype($stmt, QAST::Op) && $stmt.op eq 'return' {
                        # For simple returns, just keep the value
                        if $stmt.elems == 1 {
                            @inlined_stmts.push($stmt[0]);
                        }
                    }
                    else {
                        @inlined_stmts.push($stmt.clone());
                    }
                }
                
                # Create a new statement list for the inlined code
                my $inlined_body := QAST::Stmts.new();
                for @inlined_stmts -> $stmt {
                    $inlined_body.push($stmt);
                }
                
                # Log the optimization
                self.log_optimization('routine_inlining', $routine, $inlined_body);
                
                return $inlined_body;
            }
        }
        
        # If not inlined, return the original routine
        $routine
    }
    
    # Check if a routine is eligible for inlining
    method is_routine_inlinable($routine) {
        # Simple heuristics for inlining eligibility:
        # - Small number of statements
        # - No complex control flow
        # - No recursion
        # - Not too many parameters
        
        # Check if this is a routine with a body
        unless $routine && nqp::istype($routine, QAST::Op) && $routine.op eq 'sub' || $routine.op eq 'method' {
            return False;
        }
        
        # Get the body of the routine
        my $body := $routine[1];
        unless $body && nqp::istype($body, QAST::Stmts) {
            return False;
        }
        
        # Count statements (more accurately)
        my int $stmt_count := 0;
        my int $complexity := 0;
        my int $param_count := 0;
        my $has_complex_control := False;
        my $has_recursion := False;
        my $has_exceptions := False;
        my $routine_name := $routine.name // '';
        
        # Count parameters if signature is available
        if $routine[0] && nqp::istype($routine[0], QAST::WVal) && $routine[0].value {
            # Simple parameter count estimation
            my $sig_str := nqp::unbox_s($routine[0].value);
            # This is a simplified approach - in a real implementation we'd parse the signature
            if $sig_str ~~ /<[\(]>\s*(<[\w:]>+\s*(?:,<\s*<[\w:]>+\s*)*)/ {
                $param_count := +$0.split(',');
            }
        }
        
        # Don't inline routines with too many parameters
        if $param_count > 5 {
            return False;
        }
        
        # Analyze the body
        $self.analyze_routine_body($body, $routine_name, $stmt_count, $complexity, $has_complex_control, $has_recursion, $has_exceptions);
        
        # Don't inline routines with too many statements
        if $stmt_count > 15 {
            return False;
        }
        
        # Don't inline routines with recursion
        if $has_recursion {
            return False;
        }
        
        # Be cautious with complex control flow
        if $has_complex_control && $stmt_count > 5 {
            return False;
        }
        
        # Be cautious with exceptions
        if $has_exceptions && $stmt_count > 10 {
            return False;
        }
        
        # Consider overall complexity
        if $complexity > 25 {
            return False;
        }
        
        return True;
    }
    
    # Helper method to analyze a routine body
    method analyze_routine_body($body, $routine_name, $stmt_count is rw, $complexity is rw, 
                              $has_complex_control is rw, $has_recursion is rw, $has_exceptions is rw) {
        for $body.list() -> $stmt {
            $stmt_count := $stmt_count + 1;
            $complexity := $complexity + 1;
            
            if nqp::istype($stmt, QAST::Op) {
                my $op_type := $stmt.op;
                
                # Check for complex control flow
                if $op_type eq 'loop' || $op_type eq 'while' || $op_type eq 'for' || 
                   $op_type eq 'if' || $op_type eq 'unless' || $op_type eq 'given' || 
                   $op_type eq 'when' || $op_type eq 'try' {
                    $has_complex_control := True;
                    $complexity := $complexity + 2;
                    
                    # Analyze nested statements
                    for 1 ..^ nqp::elems($stmt) -> $i {
                        if $stmt[$i] && nqp::istype($stmt[$i], QAST::Stmts) {
                            self.analyze_routine_body($stmt[$i], $routine_name, 
                                                    $stmt_count, $complexity, 
                                                    $has_complex_control, $has_recursion, $has_exceptions);
                        }
                    }
                }
                
                # Check for exceptions
                elsif $op_type eq 'die' || $op_type eq 'fail' || $op_type eq 'CATCH' {
                    $has_exceptions := True;
                }
                
                # Check for recursion (simplified - looks for calls with the same name)
                elsif $op_type eq 'call' && $stmt.name && $stmt.name eq $routine_name {
                    $has_recursion := True;
                }
                
                # Increase complexity for method calls
                elsif $op_type eq 'callmethod' {
                    $complexity := $complexity + 1;
                }
            }
            elsif nqp::istype($stmt, QAST::Stmts) {
                # Analyze nested statement blocks
                self.analyze_routine_body($stmt, $routine_name, 
                                        $stmt_count, $complexity, 
                                        $has_complex_control, $has_recursion, $has_exceptions);
            }
        }
    }
        False
    }
    
    # Optimize loops for better performance
    method optimize_loops($op) {
        # Only apply loop optimizations at higher optimization levels
        if $!level < 2 {
            return nqp::null();
        }
        
        my $optype := $op.op;
        
        # Optimize for loops
        if $optype eq 'for' {
            # Advanced loop invariant code motion (level 3+)
            if $!level >= 3 {
                my $optimized := self.move_invariant_code_out_of_loops($op);
                return $optimized if $optimized;
            }
            else {
                # Basic loop invariant code motion
                my $optimized := self.move_loop_invariants($op);
                return $optimized if $optimized;
            }
            
            # Loop unrolling for small fixed iterations
            $optimized := self.unroll_small_loops($op);
            return $optimized if $optimized;
        }
        
        # Optimize while loops
        elsif $optype eq 'while' {
            # Advanced loop invariant code motion for while loops (level 3+)
            if $!level >= 3 {
                my $optimized := self.move_invariant_code_out_of_loops($op);
                return $optimized if $optimized;
            }
            
            # Hoist invariant conditions
            my $optimized := self.optimize_while_condition($op);
            return $optimized if $optimized;
        }
        # Optimize loop loops
        elsif $optype eq 'loop' {
            # Advanced loop invariant code motion for loop loops (level 3+)
            if $!level >= 3 {
                my $optimized := self.move_invariant_code_out_of_loops($op);
                return $optimized if $optimized;
            }
            
            # Try loop unrolling for counter-based loop loops
            my $optimized := self.unroll_small_loops($op);
            return $optimized if $optimized;
        }
        
        nqp::null()
    }
    
    # Move loop-invariant code outside the loop
    method move_loop_invariants($loop) {
        # Only perform loop invariant optimization at higher optimization levels
        return nqp::null() unless $!optimization_level >= 2;
        
        # Only process recognized loop types
        return nqp::null() unless $loop.op eq 'while' || $loop.op eq 'for' || $loop.op eq 'loop';
        
        # Only process loops with a block body
        if $loop[1] && nqp::istype($loop[1], QAST::Block) {
            my $body := $loop[1][0];  # Get the body statements
            
            # Skip if no body or not a statements node
            if $body && nqp::istype($body, QAST::Stmts) {
                # Find loop variables - improved detection for different loop types
                my @loop_vars := self.find_loop_variables($loop);
                
                # Also find variables modified inside the loop body
                my @modified_vars := self.find_modified_variables($body);
                
                # Add modified variables to loop variables
                @loop_vars := @loop_vars.unique.push(@modified_vars);
                
                # Process each statement in the body and identify invariants
                my %invariant_exprs;
                my @new_body;
                
                # First pass: find all invariant expressions
                for $body.list() -> $stmt {
                    # Check if this statement is invariant (doesn't depend on loop variables)
                    if self.is_invariant($stmt, @loop_vars) && self.can_be_hoisted($stmt) {
                        # Check for complex expressions that are worth hoisting (cost-benefit analysis)
                        if self.is_worth_hoisting($stmt) {
                            # Generate a temporary variable name
                            my $temp_name := "_invariant_".nqp::concat(nqp::to_s(nqp::elems(%invariant_exprs)));
                            
                            # Create a binding for the invariant expression
                            my $bind := QAST::Bind.new(
                                QAST::Var.new(:name($temp_name), :scope<lexical>),
                                $stmt
                            );
                            
                            # Store the binding and the variable reference
                            %invariant_exprs{$stmt} := $bind;
                            
                            # Replace the statement with a reference to the temporary variable
                            @new_body.push(QAST::Var.new(:name($temp_name), :scope<lexical>));
                        } else {
                            # Keep the statement as is if it's not worth hoisting
                            @new_body.push($stmt);
                        }
                    } else {
                        # Keep the statement as is
                        @new_body.push($stmt);
                    }
                }
                
                # If we found invariants to hoist
                if nqp::elems(%invariant_exprs) > 0 {
                    # Create a new statements block with the hoisted code and modified body
                    my $new_stmts := QAST::Stmts.new();
                    
                    # Add the hoisted bindings
                    for %invariant_exprs.values() -> $bind {
                        $new_stmts.push($bind);
                    }
                    
                    # Create a new loop with the modified body
                    my $new_loop_body := QAST::Block.new();
                    $new_loop_body.push(QAST::Stmts.new(@new_body));
                    
                    # Preserve the block's properties
                    $new_loop_body.arity := $loop[1].arity if $loop[1].arity.DEFINITE;
                    $new_loop_body.signature := $loop[1].signature if $loop[1].signature.DEFINITE;
                    $new_loop_body.labels := $loop[1].labels if $loop[1].labels.DEFINITE;
                    
                    # Create a new loop op with the original parameters but new body
                    my $new_loop := QAST::Op.new(:op($loop.op));
                    for $loop.list() -> $i { $new_loop.push($i) }
                    $new_loop[1] := $new_loop_body;  # Replace the body
                    
                    # Preserve loop properties
                    $new_loop.labels := $loop.labels if $loop.labels.DEFINITE;
                    
                    # Add the new loop to the statements
                    $new_stmts.push($new_loop);
                    
                    # Log the optimization if needed
                    self.log_optimization("Loop invariant hoisting", $loop, $new_stmts) if $!verbose;
                    
                    return $new_stmts;
                }
            }
        }
        
        nqp::null()
    }
    
    # Find variables bound in a loop
    method find_loop_variables($loop) {
        my @loop_vars;
        
        # Check the loop body for parameter bindings
        if $loop[1] && nqp::istype($loop[1], QAST::Block) {
            my $signature := $loop[1].signature;
            if $signature && nqp::istype($signature, QAST::Op) && $signature.op eq 'signature' {
                # Extract parameters from the signature
                for $signature.list() -> $param {
                    if $param && nqp::istype($param, QAST::Var) {
                        @loop_vars.push($param.name);
                    }
                }
            }
        }
        
        @loop_vars
    }
    
    # Check if an expression is invariant (doesn't depend on loop variables)
    method is_invariant($node, @loop_vars) {
        # Base cases
        if nqp::istype($node, QAST::Var) {
            return !@loop_vars.any($node.name);
        }
        elsif nqp::istype($node, QAST::Val) || 
              nqp::istype($node, QAST::IVal) || 
              nqp::istype($node, QAST::NVal) || 
              nqp::istype($node, QAST::SVal) || 
              nqp::istype($node, QAST::BVal) {
            return True;  # Constants are always invariant
        }
        
        # For other nodes, check all children
        for $node.list() -> $child {
            if $child && !self.is_invariant($child, @loop_vars) {
                return False;
            }
        }
        
        True
    }
    
    # Check if a statement can be safely hoisted
    method can_be_hoisted($stmt) {
        # Only expressions and simple operations can be hoisted
        # Avoid hoisting assignments, calls with side effects, etc.
        if nqp::istype($stmt, QAST::Op) {
            # Check for operations that have side effects
            my str $op := $stmt.op;
            # Skip operations that modify state
            return 0 if $op eq 'bind' || $op eq 'assoc' || $op eq 'attrbind' || 
                        $op eq 'call' || $op eq 'callmethod' || $op eq 'assign' ||
                        $op eq 'push' || $op eq 'pop' || $op eq 'shift' ||
                        $op eq 'unshift' || $op eq 'splice' || $op eq 'append';
            return 1;
        }
        # Allow simple values and variables
        return nqp::istype($stmt, QAST::Var) || 
               nqp::istype($stmt, QAST::Val) ||
               nqp::istype($stmt, QAST::IVal) ||
               nqp::istype($stmt, QAST::NVal) ||
               nqp::istype($stmt, QAST::SVal) ||
               nqp::istype($stmt, QAST::BVal);
    }
    
    # Find variables that are modified inside a code block
    method find_modified_variables($node) {
        my @modified;
        
        # Check if this node modifies a variable
        if nqp::istype($node, QAST::Op) {
            my str $op := $node.op;
            if $op eq 'bind' || $op eq 'assoc' || $op eq 'attrbind' || $op eq 'assign' {
                my $target := $node[0];
                if nqp::istype($target, QAST::Var) && $target.name {
                    @modified.push($target.name);
                }
            }
        }
        
        # Check all children
        for $node.list() -> $child {
            if $child {
                @modified := @modified.push(self.find_modified_variables($child));
            }
        }
        
        # Return unique variable names
        @modified.unique
    }
    
    # Determine if an expression is worth hoisting based on its complexity
    method is_worth_hoisting($stmt) {
        # Count the number of operations in the expression
        my int $op_count := 0;
        
        $stmt.walk(-> $node {
            $op_count++ if nqp::istype($node, QAST::Op);
            1 # Continue walking
        });
        
        # Only hoist expressions with sufficient complexity to justify the cost
        # of creating a temporary variable and binding
        $op_count >= 2;
    }
    
    # Enhanced optimization logging with detailed information and performance metrics
    method log_optimization($desc, $original, $optimized, $extra_info? is copy) {
        # Check if optimization logging is enabled based on level and flags
        if $!level >= 4 || $!enable_logging {
            # Comprehensive logging implementation
            my $log_entry := nqp::hash();
            
            # Basic information
            $log_entry<description> := $desc;
            $log_entry<timestamp> := nqp::time_n();  # High-resolution timestamp
            $log_entry<level> := $!level;
            
            # Node type information
            if $original {
                $log_entry<original_type> := $original.WHAT.^name;
                
                # Operation-specific details
                if nqp::istype($original, QAST::Op) {
                    $log_entry<original_op> := $original.op;
                    $log_entry<original_args> := nqp::elems($original);
                }
                
                # Size estimate for the original node
                $log_entry<original_size> := self.estimate_node_size($original);
            }
            
            if $optimized {
                $log_entry<optimized_type> := $optimized.WHAT.^name;
                
                # Operation-specific details
                if nqp::istype($optimized, QAST::Op) {
                    $log_entry<optimized_op> := $optimized.op;
                    $log_entry<optimized_args> := nqp::elems($optimized);
                }
                
                # Size estimate for the optimized node
                $log_entry<optimized_size> := self.estimate_node_size($optimized);
                
                # Potential performance impact
                if $log_entry<original_size> && $log_entry<optimized_size> {
                    my $size_diff := $log_entry<original_size> - $log_entry<optimized_size>;
                    $log_entry<size_reduction> := $size_diff;
                    $log_entry<size_reduction_percent> := ($size_diff / $log_entry<original_size>) * 100 if $log_entry<original_size> > 0;
                }
            }
            
            # Store extra information if provided
            if $extra_info {
                $log_entry<extra_info> := $extra_info;
            }
            
            # Increment optimization counters
            if !$!optimization_stats {
                $!optimization_stats := nqp::hash();
            }
            
            $!optimization_stats{$desc} := ($!optimization_stats{$desc} // 0) + 1;
            $!optimization_stats<total> := ($!optimization_stats<total> // 0) + 1;
            
            # Store the log entry
            if $!optimization_logs {
                $!optimization_logs.push($log_entry);
            } else {
                # Initialize the log buffer if it doesn't exist
                $!optimization_logs := nqp::list($log_entry);
            }
            
            # Log to standard output if verbose mode is enabled
            if $!verbose {
                self.print_optimization_log($log_entry);
            }
        }
        
        # Return success
        True
    }
    
    # Estimate the size/complexity of a node
    method estimate_node_size($node) {
        my $size := 0;
        
        self.visit_node($node, sub ($child, $) {
            $size++;
            1;  # Continue walking
        });
        
        $size;
    }
    
    # Print a human-readable optimization log entry with rich context
    method print_optimization_log($entry) {
        # Get basic information from the log entry
        my $desc := $entry<description> // 'unknown';
        my $orig_type := $entry<original_type> // 'unknown';
        my $opt_type := $entry<optimized_type> // 'unknown';
        my $timestamp := $entry<timestamp> || nqp::time_n();
        
        # Format timestamp (ISO 8601-like format)
        my $formatted_time := self.format_timestamp($timestamp);
        
        # Build the base log message
        my $message := "[$formatted_time] [OPTIMIZE] $desc";
        
        # Add type transformation information
        $message := "$message\n  Transformation: $orig_type → $opt_type";
        
        # Add performance metrics if available
        if $entry<size_reduction> || $entry<size_reduction_percent> {
            my $reduction := $entry<size_reduction> || 0;
            my $percent := $entry<size_reduction_percent> || 0;
            # Fix the formatting issue with the percent value
            $message := "$message\n  Performance: Size reduction: $reduction (-{sprintf('%.2f', $percent)}%)";
        }
        
        # Add duration information if available
        if $entry<duration_ms> {
            $message := "$message\n  Duration: {$entry<duration_ms>} ms";
        }
        
        # Add extra information if provided
        if $entry<extra_info> {
            $message := "$message\n  Details: {$entry<extra_info>}";
        }
        
        # Add source location if available
        if $entry<source_location> {
            $message := "$message\n  Location: {$entry<source_location>}";
        }
        
        # In a real implementation, this would use a proper logging framework
        # with support for different log levels and destinations
        nqp::say($message);
    }
    
    # Helper method to format timestamps for log entries
    method format_timestamp($timestamp) {
        # Get current time components
        my $now := nqp::time_n();
        my $days := nqp::int($now / 86400000000000);
        my $rest := $now % 86400000000000;
        my $hours := nqp::int($rest / 3600000000000);
        $rest := $rest % 3600000000000;
        my $minutes := nqp::int($rest / 60000000000);
        $rest := $rest % 60000000000;
        my $seconds := nqp::int($rest / 1000000000);
        my $milliseconds := nqp::int(($rest % 1000000000) / 1000000);
        
        # Format as ISO 8601-like timestamp
        return sprintf('%02d:%02d:%02d.%03d', $hours, $minutes, $seconds, $milliseconds);
    }
    
    # Unroll small loops with fixed iteration counts
    method unroll_small_loops($loop) {
        # Only perform loop unrolling at optimization level 2 or higher
        if $!level < 2 {
            return nqp::null();
        }
        
        # Check loop type
        if $loop.op eq 'for' || $loop.op eq 'loop' || $loop.op eq 'while' {
            # Handle for loops with constant iterators
            if $loop.op eq 'for' {
                my $unrolled := self.unroll_for_loop($loop);
                if $unrolled {
                    self.log_optimization('loop_unroll', $loop);
                    return $unrolled;
                }
            }
            
            # Handle loop and while with simple counter patterns
            if ($loop.op eq 'loop' || $loop.op eq 'while') && $!level >= 3 {
                my $unrolled := self.detect_and_unroll_counter_loop($loop);
                if $unrolled {
                    self.log_optimization('loop_unroll', $loop);
                    return $unrolled;
                }
            }
        }
        
        nqp::null()
    }
    
    # Unroll for loops with constant ranges or small iterables
    method unroll_for_loop($loop) {
        my $iter := $loop[0];
        
        # Case 1: Range iterator
        if $iter && nqp::istype($iter, QAST::Op) && $iter.op eq 'range' {
            my $start := $iter[0];
            my $end := $iter[1];
            my $excl := $iter[2];  # Check for exclusive range
            
            # Handle integer ranges
            if $start && nqp::istype($start, QAST::IVal) && 
               $end && nqp::istype($end, QAST::IVal) {
                
                my int $start_val := $start.value;
                my int $end_val := $end.value;
                my int $iter_count := $excl && nqp::istype($excl, QAST::BVal) && $excl.value
                    ?? $end_val - $start_val
                    !! $end_val - $start_val + 1;
                
                # Only unroll small loops to avoid code bloat
                my int $max_unroll := $!level >= 3 ?? 8 !! 5;
                if $iter_count > 0 && $iter_count <= $max_unroll {
                    return self.perform_loop_unrolling($loop, $start_val, $end_val, $excl ?? False !! True);
                }
            }
        }
        
        # Case 2: Array literal iterator
        elsif $iter && nqp::istype($iter, QAST::Op) && $iter.op eq 'list' {
            my @elements := $iter.list();
            
            # Only unroll if all elements are constants and count is small
            my $max_unroll := $!level >= 3 ?? 8 !! 5;
            if nqp::elems(@elements) > 0 && nqp::elems(@elements) <= $max_unroll {
                if [&&] @elements.map({ $_ && (nqp::istype($_, QAST::IVal) || nqp::istype($_, QAST::NVal) || nqp::istype($_, QAST::SVal) || nqp::istype($_, QAST::BVal)) }) {
                    return self.perform_array_unrolling($loop, @elements);
                }
            }
        }
        
        nqp::null()
    }
    
    # Perform the actual loop unrolling for range-based loops
    method perform_loop_unrolling($loop, int $start_val, int $end_val, bool $inclusive) {
        # Get the loop body and loop variable
        my $loop_body := $loop[1];
        if !$loop_body || !nqp::istype($loop_body, QAST::Block) {
            return nqp::null();
        }
        
        my $body_stmts := $loop_body[0];
        if !$body_stmts || !nqp::istype($body_stmts, QAST::Stmts) {
            return nqp::null();
        }
        
        # Find the loop variable
        my @loop_vars := self.find_loop_variables($loop);
        if nqp::elems(@loop_vars) != 1 {
            return nqp::null();
        }
        
        my $loop_var := @loop_vars[0];
        
        # Create a new statements block for the unrolled loop
        my $unrolled_stmts := QAST::Stmts.new();
        
        # For each iteration, create a copy of the body with the loop variable replaced
        my int $step := $start_val <= $end_val ? 1 : -1;
        my int $current := $start_val;
        my int $end := $inclusive ? $end_val : $end_val - $step;
        
        while ($step > 0 && $current <= $end) || ($step < 0 && $current >= $end) {
            # Bind the loop variable to the current value
            $unrolled_stmts.push(
                QAST::Bind.new(
                    QAST::Var.new(:name($loop_var), :scope<lexical>),
                    QAST::IVal.new(:value($current))
                )
            );
            
            # Add a copy of the body statements
            for $body_stmts.list() -> $stmt {
                # Skip last statements if they are control flow that would break the loop
                if $stmt && !self.is_break_statement($stmt) {
                    # Clone the statement to avoid modifying the original
                    my $cloned_stmt := self.clone_node($stmt);
                    $unrolled_stmts.push($cloned_stmt);
                }
            }
            
            $current += $step;
        }
        
        return $unrolled_stmts;
    }
    
    # Unroll loops with array literal iterators
    method perform_array_unrolling($loop, @elements) {
        # Get the loop body and loop variable
        my $loop_body := $loop[1];
        if !$loop_body || !nqp::istype($loop_body, QAST::Block) {
            return nqp::null();
        }
        
        my $body_stmts := $loop_body[0];
        if !$body_stmts || !nqp::istype($body_stmts, QAST::Stmts) {
            return nqp::null();
        }
        
        # Find the loop variable
        my @loop_vars := self.find_loop_variables($loop);
        if nqp::elems(@loop_vars) != 1 {
            return nqp::null();
        }
        
        my $loop_var := @loop_vars[0];
        
        # Create a new statements block for the unrolled loop
        my $unrolled_stmts := QAST::Stmts.new();
        
        # For each element, create a copy of the body with the loop variable replaced
        for @elements -> $elem {
            # Bind the loop variable to the current element
            $unrolled_stmts.push(
                QAST::Bind.new(
                    QAST::Var.new(:name($loop_var), :scope<lexical>),
                    self.clone_node($elem)
                )
            );
            
            # Add a copy of the body statements
            for $body_stmts.list() -> $stmt {
                if $stmt && !self.is_break_statement($stmt) {
                    # Clone the statement to avoid modifying the original
                    my $cloned_stmt := self.clone_node($stmt);
                    $unrolled_stmts.push($cloned_stmt);
                }
            }
        }
        
        return $unrolled_stmts;
    }
    
    # Detect and unroll counter-based while/loop loops
    method detect_and_unroll_counter_loop($loop) {
        # Only perform this optimization at level 2 or higher
        if $!level < 2 || !$loop || ($loop.op ne 'while' && $loop.op ne 'loop') {
            return nqp::null();
        }
        
        # Use analyze_loop_pattern to get insights about the loop type
        my $loop_pattern := self.analyze_loop_pattern($loop);
        
        # Apply specific optimizations based on loop pattern
        # For example, array iteration loops are often good candidates for unrolling
        if $loop_pattern eq 'array_iteration_loop' && $!level >= 3 {
            # For array iteration loops at higher optimization levels,
            # we can be more aggressive with unrolling thresholds
            # (We'll handle this later in the method)
        }
        
        # Get the condition and body - for 'loop', condition may be inside the loop
        my $condition := $loop.op eq 'while' ?? $loop[0] !! nqp::null();
        my $body := $loop[1];
        
        if !$body || !nqp::istype($body, QAST::Block) {
            return nqp::null();
        }
        
        my $body_stmts := $body[0];
        if !$body_stmts || !nqp::istype($body_stmts, QAST::Stmts) {
            return nqp::null();
        }
        
        # Advanced analysis: Look for initialization statement before the loop (for while loops)
        my $init_stmt := self.find_initialization_statement($loop);
        
        # Step 1: Analyze the condition to find counter variable and bounds
        my ($counter_var, $start_val, $end_val, $step_val, $comparison_op, $break_condition);
        
        # For while loops, analyze the loop condition
        if $loop.op eq 'while' && $condition && nqp::istype($condition, QAST::Op) {
            # Check for comparison operators: <, >, <=, >=, !=, ==
            my $op := $condition.op;
            if $op eq '<' || $op eq '>' || $op eq '<=' || $op eq '>=' {
                # Try both operand orders (var compared to const or const compared to var)
                my $left := $condition[0];
                my $right := $condition[1];
                
                # Case 1: Variable on left, constant on right
                if nqp::istype($left, QAST::Var) && self.is_constant_node($right) {
                    $counter_var := $left.name;
                    $end_val := self.evaluate_constant($right);
                    $comparison_op := $op;
                }
                # Case 2: Constant on left, variable on right
                elsif self.is_constant_node($left) && nqp::istype($right, QAST::Var) {
                    $counter_var := $right.name;
                    $end_val := self.evaluate_constant($left);
                    # Reverse comparison operator
                    $comparison_op := $op eq '<' ?? '>' !! $op eq '>' ?? '<' !! $op eq '<=' ?? '>=' !! '<=';
                }
            }
            # Handle equality comparisons for specific cases (e.g., i != 10)
            elsif $op eq '!=' && $condition.elems >= 2 {
                my $left := $condition[0];
                my $right := $condition[1];
                
                if nqp::istype($left, QAST::Var) && self.is_constant_node($right) {
                    $counter_var := $left.name;
                    $end_val := self.evaluate_constant($right);
                    # For !=, we need additional context to determine direction
                    # Make an educated guess based on common patterns
                    $comparison_op := '<';  # Default to ascending comparison
                }
                elsif self.is_constant_node($left) && nqp::istype($right, QAST::Var) {
                    $counter_var := $right.name;
                    $end_val := self.evaluate_constant($left);
                    $comparison_op := '<';
                }
            }
        }
        # For loop loops, look for a break with a condition at the end
        elsif $loop.op eq 'loop' {
            # Check if the last statement is a break with a condition
            my @stmts := $body_stmts.list();
            if @stmts && @stmts.elems > 0 {
                my $last_stmt := @stmts[@stmts.elems - 1];
                if nqp::istype($last_stmt, QAST::Op) && $last_stmt.op eq 'last' && $last_stmt[0] {
                    $break_condition := $last_stmt[0];
                    
                    # Analyze the break condition with more complex analysis
                    if nqp::istype($break_condition, QAST::Op) {
                        my $op := $break_condition.op;
                        # Handle basic comparison operators
                        if $op eq '<' || $op eq '>' || $op eq '<=' || $op eq '>=' || $op eq '==' || $op eq '!=' {
                            my $left := $break_condition[0];
                            my $right := $break_condition[1];
                            
                            if nqp::istype($left, QAST::Var) && self.is_constant_node($right) {
                                $counter_var := $left.name;
                                $end_val := self.evaluate_constant($right);
                                # Convert the condition for iteration counting
                                if $op eq '<' || $op eq '<=' || $op eq '>' || $op eq '>=' {
                                    $comparison_op := $op eq '<' ?? '>=' !! $op eq '>' ?? '<=' !! 
                                                    $op eq '<=' ?? '>' !! '<';
                                } else {
                                    # For equality/inequality, make an educated guess
                                    $comparison_op := '<';
                                }
                            }
                            elsif self.is_constant_node($left) && nqp::istype($right, QAST::Var) {
                                $counter_var := $right.name;
                                $end_val := self.evaluate_constant($left);
                                $comparison_op := $op eq '<' ?? '<=' !! $op eq '>' ?? '>=' !! 
                                                $op eq '<=' ?? '<' !! '>';
                            }
                        }
                        # Handle compound conditions (e.g., i >= 0 && i < 10)
                        elsif $op eq '&&' || $op eq '||' {
                            # Try to find comparison with a constant in compound conditions
                            for 0, 1 -> $i {
                                if $break_condition[$i] && nqp::istype($break_condition[$i], QAST::Op) {
                                    my $sub_op := $break_condition[$i].op;
                                    if $sub_op eq '<' || $sub_op eq '>' || $sub_op eq '<=' || $sub_op eq '>=' {
                                        my $sub_left := $break_condition[$i][0];
                                        my $sub_right := $break_condition[$i][1];
                                        
                                        if nqp::istype($sub_left, QAST::Var) && self.is_constant_node($sub_right) {
                                            $counter_var := $sub_left.name;
                                            $end_val := self.evaluate_constant($sub_right);
                                            $comparison_op := $sub_op eq '<' ?? '>=' !! $sub_op eq '>' ?? '<=' !! 
                                                            $sub_op eq '<=' ?? '>' !! '<';
                                            last;
                                        }
                                        elsif self.is_constant_node($sub_left) && nqp::istype($sub_right, QAST::Var) {
                                            $counter_var := $sub_right.name;
                                            $end_val := self.evaluate_constant($sub_left);
                                            $comparison_op := $sub_op eq '<' ?? '<=' !! $sub_op eq '>' ?? '>=' !! 
                                                            $sub_op eq '<=' ?? '<' !! '>';
                                            last;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        
        # If we couldn't identify a counter variable and bounds, give up
        if !$counter_var.defined {
            return nqp::null();
        }
        
        # Step 2: Try to find the counter initialization
        $start_val := 0;
        $step_val := 1;
        
        # Check for initialization statement before the loop (if found)
        if $init_stmt && $counter_var.defined {
            if nqp::istype($init_stmt, QAST::Bind) && $init_stmt[0] && nqp::istype($init_stmt[0], QAST::Var) {
                if $init_stmt[0].name eq $counter_var && $init_stmt[1] && self.is_constant_node($init_stmt[1]) {
                    $start_val := self.evaluate_constant($init_stmt[1]);
                }
            }
        }
        
        # Check if the first statement is a binding to the counter variable
        my @stmts := $body_stmts.list();
        if @stmts && @stmts.elems > 0 {
            my $first_stmt := @stmts[0];
            if nqp::istype($first_stmt, QAST::Bind) && $first_stmt[0] && nqp::istype($first_stmt[0], QAST::Var) {
                if $first_stmt[0].name eq $counter_var && $first_stmt[1] && self.is_constant_node($first_stmt[1]) {
                    $start_val := self.evaluate_constant($first_stmt[1]);
                }
            }
        }
        
        # Step 3: Analyze the loop body for counter increment/decrement
        my $has_counter_update := False;
        my @non_update_stmts;
        
        for @stmts -> $stmt {
            next unless $stmt;
            
            # Skip the break condition for 'loop' loops
            if $loop.op eq 'loop' && $stmt === $break_condition {
                next;
            }
            
            # Skip the initialization statement if we identified it
            if nqp::istype($stmt, QAST::Bind) && $stmt[0] && nqp::istype($stmt[0], QAST::Var) {
                if $stmt[0].name eq $counter_var && $stmt[1] && self.is_constant_node($stmt[1]) {
                    next;
                }
            }
            
            # Check if this is a counter update statement
            if self.is_counter_update($stmt, $counter_var) {
                $has_counter_update := True;
                # Determine the step value from the update operation
                if nqp::istype($stmt, QAST::Op) {
                    my $op := $stmt.op;
                    if $op eq '++' || $op eq '+=' {
                        # For += with explicit value, try to extract it
                        if $op eq '+=' && $stmt[1] && self.is_constant_node($stmt[1]) {
                            $step_val := self.evaluate_constant($stmt[1]);
                        } else {
                            $step_val := 1;
                        }
                    } elsif $op eq '--' || $op eq '-=' {
                        # For -= with explicit value, try to extract it
                        if $op eq '-=' && $stmt[1] && self.is_constant_node($stmt[1]) {
                            $step_val := -self.evaluate_constant($stmt[1]);
                        } else {
                            $step_val := -1;
                        }
                    }
                    # Handle assignment with counter expression (e.g., i = i + 2)
                    elsif $op eq '=' && $stmt[1] && nqp::istype($stmt[1], QAST::Op) {
                        my $rhs_op := $stmt[1].op;
                        if $rhs_op eq '+' || $rhs_op eq '-' {
                            my $left := $stmt[1][0];
                            my $right := $stmt[1][1];
                            
                            # Case 1: var = var + const
                            if nqp::istype($left, QAST::Var) && $left.name eq $counter_var && 
                               self.is_constant_node($right) {
                                my $const_val := self.evaluate_constant($right);
                                $step_val := $rhs_op eq '+' ?? $const_val !! -$const_val;
                            }
                            # Case 2: var = const + var
                            elsif self.is_constant_node($left) && 
                                  nqp::istype($right, QAST::Var) && $right.name eq $counter_var {
                                my $const_val := self.evaluate_constant($left);
                                $step_val := $rhs_op eq '+' ?? $const_val !! -$const_val;
                            }
                        }
                    }
                }
            } else {
                # Skip break statements, collect others for unrolling
                if !self.is_break_statement($stmt) {
                    @non_update_stmts.push($stmt);
                }
            }
        }
        
        # If there's no counter update in the body, it's not a valid counter loop
        if !$has_counter_update {
            return nqp::null();
        }
        
        # Step 4: Determine the number of iterations with additional validation
        my $iter_count := 0;
        if $step_val != 0 && $comparison_op.defined {
            $iter_count := self.calculate_iteration_count($start_val, $end_val, $step_val, $comparison_op);
            
            # Don't unroll if too many iterations (avoid code bloat)
            # Adjust thresholds based on loop pattern and optimization level
            my $max_iterations;
            if $loop_pattern eq 'array_iteration_loop' && $!level >= 3 {
                # Be more aggressive with array iteration loops at higher optimization levels
                $max_iterations := 16;
            } elsif $loop_pattern eq 'accumulation_loop' {
                # Accumulation loops can benefit from more unrolling
                $max_iterations := $!level >= 3 ?? 12 !! 8;
            } else {
                # Default thresholds
                $max_iterations := $!level >= 3 ?? 8 !! 5;
            }
            
            if $iter_count <= 0 || $iter_count > $max_iterations {
                return nqp::null();
            }
        } else {
            return nqp::null();
        }
        
        # Step 5: Perform the unrolling with safety checks
        my $unrolled_stmts := QAST::Stmts.new();
        
        my int $current := $start_val;
        my int $actual_iterations := 0;
        
        for 0..^$iter_count -> $i {
            # Safety check: Verify we're still within bounds before generating the iteration
            my $should_execute := False;
            if $step_val > 0 {
                if $comparison_op eq '<' { $should_execute := $current < $end_val }
                elsif $comparison_op eq '<=' { $should_execute := $current <= $end_val }
                elsif $comparison_op eq '>' { $should_execute := $current > $end_val }
                elsif $comparison_op eq '>=' { $should_execute := $current >= $end_val }
            }
            else {
                if $comparison_op eq '<' { $should_execute := $current < $end_val }
                elsif $comparison_op eq '<=' { $should_execute := $current <= $end_val }
                elsif $comparison_op eq '>' { $should_execute := $current > $end_val }
                elsif $comparison_op eq '>=' { $should_execute := $current >= $end_val }
            }
            
            if $should_execute {
                # Bind the loop variable to the current value
                $unrolled_stmts.push(
                    QAST::Bind.new(
                        QAST::Var.new(:name($counter_var), :scope<lexical>),
                        QAST::IVal.new(:value($current))
                    )
                );
                
                # Add copies of the body statements (excluding counter update and control flow)
                for @non_update_stmts -> $stmt {
                    if $stmt {
                        my $cloned_stmt := self.clone_node($stmt);
                        $unrolled_stmts.push($cloned_stmt);
                    }
                }
                
                $actual_iterations++;
            }
            
            $current += $step_val;
        }
        
        # If no iterations were actually generated, return null
        if $actual_iterations == 0 {
            return nqp::null();
        }
        
        # Add final binding to the counter variable to match original loop's behavior
        $unrolled_stmts.push(
            QAST::Bind.new(
                QAST::Var.new(:name($counter_var), :scope<lexical>),
                QAST::IVal.new(:value($current))
            )
        );
        
        self.log_optimization('counter_loop_unroll', $loop);
        return $unrolled_stmts;
    }
    
    # Check if a node is a constant (for counter loop detection)
    method is_constant_node($node) {
        return nqp::istype($node, QAST::IVal) || 
               nqp::istype($node, QAST::NVal) || 
               nqp::istype($node, QAST::SVal) || 
               nqp::istype($node, QAST::BVal);
    }
    
    # Evaluate a constant node to its value
    method evaluate_constant($node) {
        if nqp::istype($node, QAST::IVal) {
            return $node.value;
        }
        elsif nqp::istype($node, QAST::NVal) {
            return nqp::p6box_i($node.value);
        }
        0
    }
    
    # Check if a statement updates a counter variable
    method is_counter_update($stmt, $var_name) {
        if nqp::istype($stmt, QAST::Op) {
            my $op := $stmt.op;
            # Check for post/pre increment/decrement
            if ($op eq '++' || $op eq '--') && $stmt[0] && nqp::istype($stmt[0], QAST::Var) && $stmt[0].name eq $var_name {
                return True;
            }
            # Check for +=/-=
            elsif ($op eq '+=' || $op eq '-=') && $stmt[0] && nqp::istype($stmt[0], QAST::Var) && $stmt[0].name eq $var_name {
                return True;
            }
            # Check for assignment with counter expression (e.g., i = i + 1)
            elsif $op eq '=' && $stmt[0] && nqp::istype($stmt[0], QAST::Var) && $stmt[0].name eq $var_name {
                if $stmt[1] && nqp::istype($stmt[1], QAST::Op) && $stmt[1].elems >= 2 {
                    my $rhs_op := $stmt[1].op;
                    if $rhs_op eq '+' || $rhs_op eq '-' {
                        my $left := $stmt[1][0];
                        my $right := $stmt[1][1];
                        
                        # Case 1: var = var + const
                        if nqp::istype($left, QAST::Var) && $left.name eq $var_name && self.is_constant_node($right) {
                            return True;
                        }
                        # Case 2: var = const + var
                        elsif self.is_constant_node($left) && nqp::istype($right, QAST::Var) && $right.name eq $var_name {
                            return True;
                        }
                    }
                }
            }
        }
        False
    }
    
    # Find initialization statement before a loop (enhanced analysis)
    method find_initialization_statement($loop) {
        # Check if we can access the parent block and statements
        my $parent := nqp::getattr($loop, QAST::Node, '$parent');
        if !$parent || !nqp::istype($parent, QAST::Stmts) {
            return nqp::null();
        }
        
        # Get the list of statements in the parent block
        my @stmts := $parent.list();
        
        # Find the index of the loop in the statements list
        my $loop_idx := -1;
        for @stmts.kv -> $i, $stmt {
            if $stmt === $loop {
                $loop_idx := $i;
                last;
            }
        }
        
        # If loop not found or it's the first statement, return null
        if $loop_idx <= 0 {
            return nqp::null();
        }
        
        # Look at the statement immediately before the loop
        my $prev_stmt := @stmts[$loop_idx - 1];
        
        # Check if it's a binding (assignment) to a variable
        if nqp::istype($prev_stmt, QAST::Bind) && $prev_stmt[0] && nqp::istype($prev_stmt[0], QAST::Var) {
            # Verify right-hand side is a constant
            if $prev_stmt[1] && self.is_constant_node($prev_stmt[1]) {
                return $prev_stmt;
            }
        }
        
        # Also check for assignment via QAST::Op '='
        if nqp::istype($prev_stmt, QAST::Op) && $prev_stmt.op eq '=' && $prev_stmt[0] && nqp::istype($prev_stmt[0], QAST::Var) {
            if $prev_stmt[1] && self.is_constant_node($prev_stmt[1]) {
                # Convert to QAST::Bind for consistent handling
                return QAST::Bind.new($prev_stmt[0], $prev_stmt[1]);
            }
        }
        
        nqp::null()
    }
    
    # Enhanced analysis for detecting loop types and patterns
    method analyze_loop_pattern($loop) {
        # Basic loop type classification
        my $loop_type;
        if $loop.op eq 'while' {
            $loop_type := 'while_loop';
        }
        elsif $loop.op eq 'loop' {
            $loop_type := 'loop_with_break';
        } else {
            return 'unknown_loop';
        }
        
        # Analyze the loop body to identify specific patterns
        my $body := $loop[1];
        if $body && nqp::istype($body, QAST::Block) {
            my $body_stmts := $body[0];
            if $body_stmts && nqp::istype($body_stmts, QAST::Stmts) {
                my @stmts := $body_stmts.list();
                
                # Check for array iteration pattern
                my $has_array_access := False;
                my $array_var := nqp::null();
                my $counter_var := nqp::null();
                
                for @stmts -> $stmt {
                    next unless $stmt;
                    
                    # Check for array access pattern: array[counter]
                    if nqp::istype($stmt, QAST::Op) && $stmt.op eq '[]' && $stmt.elems >= 2 {
                        my $array := $stmt[0];
                        my $index := $stmt[1];
                        
                        if nqp::istype($array, QAST::Var) && nqp::istype($index, QAST::Var) {
                            $has_array_access := True;
                            $array_var := $array.name;
                            $counter_var := $index.name;
                            last;
                        }
                    }
                }
                
                # If we detected an array access with a variable index, and there's a counter update
                # this is likely an array iteration loop
                if $has_array_access && $counter_var.defined {
                    for @stmts -> $stmt {
                        if self.is_counter_update($stmt, $counter_var) {
                            return 'array_iteration_loop';
                        }
                    }
                }
                
                # Check for accumulation pattern (sum, product)
                my $has_accumulation := False;
                for @stmts -> $stmt {
                    next unless $stmt;
                    
                    if nqp::istype($stmt, QAST::Op) && ($stmt.op eq '+=' || $stmt.op eq '*=') && $stmt.elems >= 2 {
                        if nqp::istype($stmt[0], QAST::Var) {
                            $has_accumulation := True;
                            # Check if the right-hand side is either a constant or involves another variable
                            if $stmt[1] && (self.is_constant_node($stmt[1]) || nqp::istype($stmt[1], QAST::Var)) {
                                return 'accumulation_loop';
                            }
                        }
                    }
                }
            }
        }
        
        # Return the basic loop type if no specific pattern detected
        $loop_type
    }
    
    # Calculate the number of iterations for a counter loop with more complex analysis
    method calculate_iteration_count($start, $end, $step, $op) {
        # Handle cases where step is zero (infinite loop)
        if $step == 0 {
            return 0; # Not a valid counter loop
        }
        
        # More comprehensive iteration count calculation
        if $step > 0 {
            if $op eq '<' {
                return $end > $start ?? $end - $start !! 0;
            }
            elsif $op eq '<=' {
                return $end >= $start ?? $end - $start + 1 !! 0;
            }
            # Handle inverted comparisons
            elsif $op eq '>' {
                # This would be a decreasing loop but with positive step
                return $end < $start ?? $start - $end !! 0;
            }
            elsif $op eq '>=' {
                return $end <= $start ?? $start - $end + 1 !! 0;
            }
        }
        elsif $step < 0 {
            if $op eq '>' {
                return $end < $start ?? $start - $end !! 0;
            }
            elsif $op eq '>=' {
                return $end <= $start ?? $start - $end + 1 !! 0;
            }
            # Handle inverted comparisons
            elsif $op eq '<' {
                return $end > $start ?? $end - $start !! 0;
            }
            elsif $op eq '<=' {
                return $end >= $start ?? $end - $start + 1 !! 0;
            }
        }
        
        0
    }
    
    # Check if a statement is a break/last/next statement that would exit the loop
    method is_break_statement($stmt) {
        if nqp::istype($stmt, QAST::Op) {
            my $op := $stmt.op;
            return $op eq 'last' || $op eq 'next' || $op eq 'redo' || $op eq 'return' || $op eq 'leave';
        }
        False
    }
    
    # Clone a QAST node
    method clone_node($node) {
        # Simple cloning implementation - in a real system this would be more sophisticated
        if nqp::istype($node, QAST::Op) {
            my $new_op := QAST::Op.new(:op($node.op));
            for $node.list() -> $child {
                if $child {
                    $new_op.push(self.clone_node($child));
                }
            }
            return $new_op;
        }
        elsif nqp::istype($node, QAST::Var) {
            return QAST::Var.new(:name($node.name), :scope($node.scope));
        }
        elsif nqp::istype($node, QAST::IVal) {
            return QAST::IVal.new(:value($node.value));
        }
        elsif nqp::istype($node, QAST::NVal) {
            return QAST::NVal.new(:value($node.value));
        }
        elsif nqp::istype($node, QAST::SVal) {
            return QAST::SVal.new(:value($node.value));
        }
        elsif nqp::istype($node, QAST::BVal) {
            return QAST::BVal.new(:value($node.value));
        }
        elsif nqp::istype($node, QAST::Stmts) {
            my $new_stmts := QAST::Stmts.new();
            for $node.list() -> $child {
                if $child {
                    $new_stmts.push(self.clone_node($child));
                }
            }
            return $new_stmts;
        }
        
        # Default: return the node as is (in a real implementation, would handle all node types)
        $node
    }
    
    # Optimize while loop conditions
    method optimize_while_condition($while) {
        # Only perform condition optimization at optimization level 2 or higher
        if $!level < 2 || !$while || $while.op ne 'while' {
            return nqp::null();
        }
        
        # Get the condition
        my $condition := $while[0];
        if !$condition {
            return nqp::null();
        }
        
        # Try various condition optimizations
        my $optimized := self.simplify_boolean_expression($condition);
        if $optimized {
            # Create a new while loop with the optimized condition
            my $new_while := QAST::Op.new(:op<while>);
            $new_while.push($optimized);
            
            # Copy the loop body
            if $while[1] {
                $new_while.push(self.clone_node($while[1]));
            }
            
            self.log_optimization('while_condition', $while);
            return $new_while;
        }
        
        # Try constant folding in the condition
        $optimized := self.optimize($condition);
        if $optimized && !nqp::eqaddr($optimized, $condition) {
            # Create a new while loop with the optimized condition
            my $new_while := QAST::Op.new(:op<while>);
            $new_while.push($optimized);
            
            # Copy the loop body
            if $while[1] {
                $new_while.push(self.clone_node($while[1]));
            }
            
            self.log_optimization('while_condition', $while);
            return $new_while;
        }
        
        # Check for infinite loop with false condition (can be removed)
        if $optimized && nqp::istype($optimized, QAST::BVal) && !$optimized.value {
            # Return empty statements instead of an infinite loop
            my $empty_stmts := QAST::Stmts.new();
            self.log_optimization('dead_while_loop', $while);
            return $empty_stmts;
        }
        
        nqp::null()
    }
    
    # Simplify boolean expressions for better performance
    method simplify_boolean_expression($expr) {
        # Handle logical AND operations
        if nqp::istype($expr, QAST::Op) && $expr.op eq '&&' {
            return self.simplify_logical_and($expr);
        }
        # Handle logical OR operations
        elsif nqp::istype($expr, QAST::Op) && $expr.op eq '||' {
            return self.simplify_logical_or($expr);
        }
        # Handle logical NOT operations
        elsif nqp::istype($expr, QAST::Op) && $expr.op eq '!' {
            return self.simplify_logical_not($expr);
        }
        # Handle comparison operations
        elsif nqp::istype($expr, QAST::Op) && self.is_comparison_op($expr.op) {
            return self.simplify_comparison($expr);
        }
        
        nqp::null()
    }
    
    # Simplify logical AND expressions
    method simplify_logical_and($expr) {
        my @args := $expr.list();
        
        # Check for constants
        for @args.kv -> $i, $arg {
            if nqp::istype($arg, QAST::BVal) {
                # If any argument is false, the whole expression is false
                if !$arg.value {
                    return QAST::BVal.new(:value(False));
                }
            }
        }
        
        # Remove redundant true values
        my @new_args := @args.grep({ !(nqp::istype($_, QAST::BVal) && $_.value) });
        if nqp::elems(@new_args) != nqp::elems(@args) {
            if nqp::elems(@new_args) == 0 {
                return QAST::BVal.new(:value(True));
            }
            elsif nqp::elems(@new_args) == 1 {
                return @new_args[0];
            }
            
            # Create a new AND with the simplified arguments
            my $new_and := QAST::Op.new(:op<&&>);
            for @new_args -> $arg {
                $new_and.push($arg);
            }
            return $new_and;
        }
        
        nqp::null()
    }
    
    # Simplify logical OR expressions
    method simplify_logical_or($expr) {
        my @args := $expr.list();
        
        # Check for constants
        for @args.kv -> $i, $arg {
            if nqp::istype($arg, QAST::BVal) {
                # If any argument is true, the whole expression is true
                if $arg.value {
                    return QAST::BVal.new(:value(True));
                }
            }
        }
        
        # Remove redundant false values
        my @new_args := @args.grep({ !(nqp::istype($_, QAST::BVal) && !$_.value) });
        if nqp::elems(@new_args) != nqp::elems(@args) {
            if nqp::elems(@new_args) == 0 {
                return QAST::BVal.new(:value(False));
            }
            elsif nqp::elems(@new_args) == 1 {
                return @new_args[0];
            }
            
            # Create a new OR with the simplified arguments
            my $new_or := QAST::Op.new(:op<||>);
            for @new_args -> $arg {
                $new_or.push($arg);
            }
            return $new_or;
        }
        
        nqp::null()
    }
    
    # Simplify logical NOT expressions
    method simplify_logical_not($expr) {
        my $arg := $expr[0];
        
        # Double negation: !!x -> x
        if nqp::istype($arg, QAST::Op) && $arg.op eq '!' {
            return $arg[0];
        }
        # Negation of boolean constant
        elsif nqp::istype($arg, QAST::BVal) {
            return QAST::BVal.new(:value(!$arg.value));
        }
        # Negation of comparison operators
        elsif nqp::istype($arg, QAST::Op) && self.is_comparison_op($arg.op) {
            # Transform !($a == $b) to $a != $b and similar
            my %negated_ops = (
                '==' => '!=',
                '!=' => '==',
                '<'  => '>=',
                '<=' => '>',
                '>'  => '<=',
                '>=' => '<'
            );
            
            if %negated_ops{$arg.op} {
                my $new_op := QAST::Op.new(:op(%negated_ops{$arg.op}));
                $new_op.push($arg[0]); # Left operand
                $new_op.push($arg[1]); # Right operand
                return $new_op;
            }
        }
        # Negation of logical AND: !(a && b) -> !a || !b
        elsif nqp::istype($arg, QAST::Op) && $arg.op eq '&&' {
            my $or_op := QAST::Op.new(:op<||>);
            for $arg.list() -> $sub_expr {
                my $not_op := QAST::Op.new(:op<!>);
                $not_op.push($sub_expr);
                $or_op.push($not_op);
            }
            return $or_op;
        }
        # Negation of logical OR: !(a || b) -> !a && !b
        elsif nqp::istype($arg, QAST::Op) && $arg.op eq '||' {
            my $and_op := QAST::Op.new(:op<&&>);
            for $arg.list() -> $sub_expr {
                my $not_op := QAST::Op.new(:op<!>);
                $not_op.push($sub_expr);
                $and_op.push($not_op);
            }
            return $and_op;
        }
        
        nqp::null()
    }
    
    # Simplify comparison operations
    method simplify_comparison($expr) {
        my $op := $expr.op;
        my $left := $expr[0];
        my $right := $expr[1];
        
        # If both sides are constants, evaluate the comparison
        if $left && $right && 
           (nqp::istype($left, QAST::IVal) || nqp::istype($left, QAST::NVal) || nqp::istype($left, QAST::SVal) || nqp::istype($left, QAST::BVal)) &&
           (nqp::istype($right, QAST::IVal) || nqp::istype($right, QAST::NVal) || nqp::istype($right, QAST::SVal) || nqp::istype($right, QAST::BVal)) {
            
            my $result := False;
            
            # Compare based on the type and operation
            if nqp::istype($left, QAST::IVal) && nqp::istype($right, QAST::IVal) {
                my int $lval := $left.value;
                my int $rval := $right.value;
                
                given $op {
                    when '==' { $result := $lval == $rval }
                    when '!=' { $result := $lval != $rval }
                    when '<'  { $result := $lval < $rval }
                    when '<=' { $result := $lval <= $rval }
                    when '>'  { $result := $lval > $rval }
                    when '>=' { $result := $lval >= $rval }
                }
            }
            # Handle floating point comparisons
            elsif nqp::istype($left, QAST::NVal) && nqp::istype($right, QAST::NVal) {
                my num $lval := $left.value;
                my num $rval := $right.value;
                
                given $op {
                    when '==' { $result := $lval == $rval }
                    when '!=' { $result := $lval != $rval }
                    when '<'  { $result := $lval < $rval }
                    when '<=' { $result := $lval <= $rval }
                    when '>'  { $result := $lval > $rval }
                    when '>=' { $result := $lval >= $rval }
                }
            }
            # Handle string comparisons
            elsif nqp::istype($left, QAST::SVal) && nqp::istype($right, QAST::SVal) {
                my str $lval := $left.value;
                my str $rval := $right.value;
                
                given $op {
                    when '==' | 'eq' { $result := $lval eq $rval }
                    when '!=' | 'ne' { $result := $lval ne $rval }
                    when '<'  | 'lt' { $result := $lval lt $rval }
                    when '<=' | 'le' { $result := $lval le $rval }
                    when '>'  | 'gt' { $result := $lval gt $rval }
                    when '>=' | 'ge' { $result := $lval ge $rval }
                }
            }
            # Handle boolean comparisons
            elsif nqp::istype($left, QAST::BVal) && nqp::istype($right, QAST::BVal) {
                my bool $lval := $left.value;
                my bool $rval := $right.value;
                
                given $op {
                    when '==' { $result := $lval == $rval }
                    when '!=' { $result := $lval != $rval }
                }
            }
            
            # Return the constant result
            return QAST::BVal.new(:value($result));
        }
        
        # x == x -> True (except for NaN, but we'll ignore that for simplicity)
        if $op eq '==' && self.are_values_identical($left, $right) {
            return QAST::BVal.new(:value(True));
        }
        # x != x -> False (except for NaN)
        elsif $op eq '!=' && self.are_values_identical($left, $right) {
            return QAST::BVal.new(:value(False));
        }
        # x < x -> False
        elsif $op eq '<' && self.are_values_identical($left, $right) {
            return QAST::BVal.new(:value(False));
        }
        # x > x -> False
        elsif $op eq '>' && self.are_values_identical($left, $right) {
            return QAST::BVal.new(:value(False));
        }
        # x <= x -> True
        elsif $op eq '<=' && self.are_values_identical($left, $right) {
            return QAST::BVal.new(:value(True));
        }
        # x >= x -> True
        elsif $op eq '>=' && self.are_values_identical($left, $right) {
            return QAST::BVal.new(:value(True));
        }
        
        # Symmetric operator transformations: x > y -> y < x
        my %symmetric_ops = (
            '>'  => '<',
            '<'  => '>',
            '>=' => '<=',
            '<=' => '>='
        );
        
        if %symmetric_ops{$op} && $right && $left {
            # Only apply if right side is a constant and left is not, or vice versa
            my $left_is_constant := nqp::istype($left, QAST::IVal) || nqp::istype($left, QAST::NVal) || 
                                   nqp::istype($left, QAST::SVal) || nqp::istype($left, QAST::BVal);
            my $right_is_constant := nqp::istype($right, QAST::IVal) || nqp::istype($right, QAST::NVal) || 
                                    nqp::istype($right, QAST::SVal) || nqp::istype($right, QAST::BVal);
            
            if $left_is_constant != $right_is_constant {
                my $new_op := QAST::Op.new(:op(%symmetric_ops{$op}));
                $new_op.push($right);
                $new_op.push($left);
                return $new_op;
            }
        }
        
        # 0 < x < 10 类型的范围比较优化可以在更高层次实现
        
        nqp::null()
    }
    
    # Check if an operator is a comparison operator
    method is_comparison_op($op) {
        my @comparison_ops := <== != < <= > >= eq ne lt le gt ge ~~ !~~>;
        nqp::exists(@comparison_ops, $op);
    }
    
    # Perform type inference to enable better optimizations
    method infer_types($node) {
        # Simple type inference implementation
        # This tracks variable types through the program flow
        
        # Only perform inference at higher optimization levels
        if $!level < 2 {
            return $node;
        }
        
        # Create a type environment to track variable types
        my %type_env;
        
        # Perform the actual type inference
        self.infer_types_helper($node, %type_env);
        
        # Store the inferred types for later use in optimizations
        $!type_environment := %type_env;
        
        return $node;
    }
    
    # Helper method for type inference that recursively processes nodes
    method infer_types_helper($node, %type_env is rw) {
        # Skip null nodes
        if !$node {
            return;
        }
        
        # Process based on node type
        if nqp::istype($node, QAST::Stmts) {
            # Process each statement in sequence
            for $node.list() -> $stmt {
                self.infer_types_helper($stmt, %type_env);
            }
        }
        elsif nqp::istype($node, QAST::Bind) {
            # Infer types for variable bindings
            self.infer_binding_type($node, %type_env);
        }
        elsif nqp::istype($node, QAST::Op) {
            # Infer types for operations
            self.infer_op_type($node, %type_env);
        }
        elsif nqp::istype($node, QAST::Block) {
            # Process block bodies
            if $node[0] {
                # Create a new type environment for the block
                my %block_env := %type_env.clone();
                self.infer_types_helper($node[0], %block_env);
            }
        }
        elsif nqp::istype($node, QAST::Var) {
            # Variables are handled through their uses in other expressions
        }
    }
    
    # Infer types for variable bindings
    method infer_binding_type($bind, %type_env is rw) {
        my $target := $bind[0];
        my $value := $bind[1];
        
        # Only handle lexical variable bindings
        if $target && nqp::istype($target, QAST::Var) && $target.scope eq 'lexical' {
            my $var_name := $target.name;
            my $var_type := self.determine_type($value, %type_env);
            
            if $var_type {
                %type_env{$var_name} := $var_type;
            }
        }
    }
    
    # Infer types for operations
    method infer_op_type($op, %type_env is rw) {
        # First infer types for all operands
        for $op.list() -> $operand {
            self.infer_types_helper($operand, %type_env);
        }
        
        # Handle specific operations that affect type information
        given $op.op {
            when 'assign' {
                # For assignments, infer the type of the target based on the value
                my $target := $op[0];
                my $value := $op[1];
                
                if $target && nqp::istype($target, QAST::Var) && $target.scope eq 'lexical' {
                    my $var_name := $target.name;
                    my $var_type := self.determine_type($value, %type_env);
                    
                    if $var_type {
                        %type_env{$var_name} := $var_type;
                    }
                }
            }
            when 'if' | 'unless' | 'while' | 'for' | 'loop' {
                # For control structures, process their bodies
                # The condition is already processed by the loop above
                for 1..nqp::elems($op.list()-1) -> $i {
                    if $op[$i] {
                        self.infer_types_helper($op[$i], %type_env);
                    }
                }
            }
        }
    }
    
    # Determine the type of an expression based on its structure and the current type environment
    method determine_type($expr, %type_env) {
        # Handle constant values
        if nqp::istype($expr, QAST::IVal) {
            return 'Int';
        }
        elsif nqp::istype($expr, QAST::NVal) {
            return 'Num';
        }
        elsif nqp::istype($expr, QAST::SVal) {
            return 'Str';
        }
        elsif nqp::istype($expr, QAST::BVal) {
            return 'Bool';
        }
        # Handle variables
        elsif nqp::istype($expr, QAST::Var) && $expr.scope eq 'lexical' {
            my $var_name := $expr.name;
            if %type_env{$var_name}:exists {
                return %type_env{$var_name};
            }
        }
        # Handle operations with known result types
        elsif nqp::istype($expr, QAST::Op) {
            given $expr.op {
                when 'add', 'sub', 'mul', 'div', 'mod' {
                    # For numeric operations, check if operands are numeric
                    my @operands := $expr.list();
                    if @operands.all({ self.is_numeric_type(self.determine_type($_, %type_env)) }) {
                        return 'Num';
                    }
                }
                when 'concat' {
                    # String concatenation
                    return 'Str';
                }
                when '&&', '||', '!' {
                    # Boolean operations
                    return 'Bool';
                }
                when '==', '!=', '<', '<=', '>', '>=' {
                    # Comparison operations
                    return 'Bool';
                }
            }
        }
        
        # Default to Any if we can't determine the type
        'Any'
    }
    
    # Check if a type is a numeric type
    method is_numeric_type($type) {
        return $type eq 'Int' || $type eq 'Num' || $type eq 'Rat' || 
               $type eq 'Complex' || $type eq 'num' || $type eq 'int' ||
               ($type && $type.DEFINITE && nqp::istype($type, Numeric));
    }
    
    # Use inferred types to optimize operations
    method optimize_using_types($node) {
        # Only use type information at higher optimization levels
        if $!level < 3 || !$!type_environment {
            return nqp::null();
        }
        
        # Create a working copy of the node
        my $optimized := self.clone_node($node);
        
        # Apply type-based optimizations
        $optimized := self.apply_type_optimizations($optimized);
        
        # If any optimizations were applied, return the optimized node
        if !nqp::eqaddr($optimized, $node) {
            self.log_optimization('type_based', $node);
            return $optimized;
        }
        
        nqp::null()
    }
    
    # Apply various type-based optimizations recursively
    method apply_type_optimizations($node) {
        # Skip null nodes
        if !$node {
            return $node;
        }
        
        # Process children first (bottom-up approach)
        if nqp::can($node, 'list') {
            my @children := $node.list();
            my int $children_optimized := 0;
            
            for @children.kv -> $i, $child {
                if $child {
                    my $optimized_child := self.apply_type_optimizations($child);
                    if !nqp::eqaddr($optimized_child, $child) {
                        $node[$i] := $optimized_child;
                        $children_optimized := 1;
                    }
                }
            }
            
            # If children were optimized, refresh the node's type information
            if $children_optimized {
                self.refresh_node_type($node);
            }
        }
        
        # Apply optimizations based on node type
        given $node {
            when QAST::Op {
                # Optimize operations based on operand types
                my $optimized := self.optimize_typed_operation($node);
                if $optimized {
                    # Copy annotations from original node to optimized node
                    self.copy_annotations($node, $optimized);
                    return $optimized;
                }
                
                # Apply constant folding after type optimization
                my $folded := self.fold_constants($node);
                if $folded && !nqp::eqaddr($folded, $node) {
                    return $folded;
                }
            }
            when QAST::Call {
                # Optimize method calls for known types
                my $optimized := self.optimize_typed_method_call($node);
                if $optimized {
                    # Copy annotations from original node to optimized node
                    self.copy_annotations($node, $optimized);
                    return $optimized;
                }
            }
            when QAST::Var {
                # Apply variable-specific optimizations
                my $optimized := self.optimize_variable($node);
                if $optimized {
                    return $optimized;
                }
            }
        }
        
        return $node;
    }
    
    # Helper method to refresh node type after children changes
    method refresh_node_type($node) {
        # Remove cached type information so it gets recalculated
        $node.unannotate('type_hint');
        # Force type recalculation
        my $new_type := self.get_node_type($node);
        if $new_type ne 'Any' {
            $node.annotate('type_hint', $new_type);
        }
    }
    
    # Helper method to copy annotations from one node to another
    method copy_annotations($source, $target) {
        if $source.can('annotations') && $target.can('annotations') {
            my %ann := $source.annotations();
            for %ann.kv -> $key, $value {
                $target.annotate($key, $value);
            }
        }
    }
    
    # Optimize variable references based on type information
    method optimize_variable($var) {
        # Apply type-specific variable optimizations
        my $type := self.get_node_type($var);
        return self.apply_type_optimization($var, $type);
    }
    
    # Simple constant folding for operations with known values
    method fold_constants($op) {
        # Only fold at higher optimization levels
        return nqp::null() unless $!level >= 3;
        
        # Check if all operands are constants
        my @args := $op.list();
        my int $all_constants := 1;
        
        for @args -> $arg {
            unless nqp::istype($arg, QAST::IVal) || nqp::istype($arg, QAST::NVal) || 
                   nqp::istype($arg, QAST::SVal) || nqp::istype($arg, QAST::BVal) {
                $all_constants := 0;
                last;
            }
        }
        
        # If not all operands are constants, skip folding
        unless $all_constants {
            return nqp::null();
        }
        
        # Try to fold simple binary operations
        if nqp::elems(@args) == 2 {
            my $op_name := $op.op;
            my $left := @args[0];
            my $right := @args[1];
            
            # Integer operations
            if nqp::istype($left, QAST::IVal) && nqp::istype($right, QAST::IVal) {
                my int $lval := $left.value;
                my int $rval := $right.value;
                
                given $op_name {
                    when 'add', 'add_i' {
                        return QAST::IVal.new(:value($lval + $rval));
                    }
                    when 'sub', 'sub_i' {
                        return QAST::IVal.new(:value($lval - $rval));
                    }
                    when 'mul', 'mul_i' {
                        return QAST::IVal.new(:value($lval * $rval));
                    }
                    when 'div', 'div_i' {
                        # Integer division in Raku produces Rat, but we'll use Int for folding
                        return QAST::IVal.new(:value($lval / $rval)) if $rval != 0;
                    }
                    when 'mod', 'mod_i' {
                        return QAST::IVal.new(:value($lval % $rval)) if $rval != 0;
                    }
                    when '==', 'eq_i' {
                        return QAST::BVal.new(:value($lval == $rval));
                    }
                    when '!=', 'ne_i' {
                        return QAST::BVal.new(:value($lval != $rval));
                    }
                    when '<', 'lt_i' {
                        return QAST::BVal.new(:value($lval < $rval));
                    }
                    when '<=', 'le_i' {
                        return QAST::BVal.new(:value($lval <= $rval));
                    }
                    when '>', 'gt_i' {
                        return QAST::BVal.new(:value($lval > $rval));
                    }
                    when '>=', 'ge_i' {
                        return QAST::BVal.new(:value($lval >= $rval));
                    }
                }
            }
        }
        
        nqp::null()
    
    # Optimize operations based on operand types
    method optimize_typed_operation($op) {
        return nqp::null() unless $op;
        
        my $op_name := $op.op;
        my @args := $op.list();
        my int $args_count := nqp::elems(@args);
        
        # Handle unary operations
        if $args_count == 1 {
            my $arg_type := self.get_node_type(@args[0]);
            
            given $op_name {
                when 'abs' {
                    if $arg_type eq 'Int' || $arg_type eq 'int' {
                        return QAST::Op.new(:op<abs_i>, @args[0]);
                    }
                    elsif self.is_numeric_type($arg_type) {
                        return QAST::Op.new(:op<abs_n>, @args[0]);
                    }
                }
                when 'sqrt' {
                    if self.is_numeric_type($arg_type) {
                        return QAST::Op.new(:op<sqrt_n>, @args[0]);
                    }
                }
                when 'not' {
                    if $arg_type eq 'Bool' || $arg_type eq 'bool' {
                        return QAST::Op.new(:op<not>, @args[0]);
                    }
                }
                when 'uc', 'lc', 'trim' {
                    if $arg_type eq 'Str' || $arg_type eq 'str' {
                        $op.annotate('string_operation', 1);
                    }
                }
            }
            
            # Optimize container operations
            if self.types_compatible($arg_type, 'Positional') || self.types_compatible($arg_type, 'Associative') {
                given $op_name {
                    when 'not' {
                        # !$array or !$hash can be optimized to $array.elems == 0
                        return QAST::Op.new(:op<==>, 
                            QAST::Call.new(:name<elems>, @args[0]),
                            QAST::IVal.new(:value(0)));
                    }
                    when 'bool' {
                        # $array.Bool or $hash.Bool can be optimized to $array.elems > 0
                        return QAST::Op.new(:op<>>, 
                            QAST::Call.new(:name<elems>, @args[0]),
                            QAST::IVal.new(:value(0)));
                    }
                }
            }
        }
        
        # Handle binary operations
        if $args_count == 2 {
            my $left := @args[0];
            my $right := @args[1];
            
            # Get operand types
            my $left_type := self.get_node_type($left);
            my $right_type := self.get_node_type($right);
            
            # Specialize numeric operations
            if self.is_numeric_type($left_type) && self.is_numeric_type($right_type) {
                my $specialized := self.specialize_numeric_operation($op, $left_type, $right_type);
                if $specialized {
                    return $specialized;
                }
            }
            
            # Specialize string operations
            if $left_type eq 'Str' || $left_type eq 'str' {
                if $right_type eq 'Str' || $right_type eq 'str' {
                    my $specialized := self.specialize_string_operation($op);
                    if $specialized {
                        return $specialized;
                    }
                }
                # String operations with integers (like substr, index)
                elsif $right_type eq 'Int' || $right_type eq 'int' {
                    given $op_name {
                        when 'substr' {
                            return QAST::Op.new(:op<substr_s>, $left, $right);
                        }
                        when 'index' {
                            return QAST::Op.new(:op<index_s>, $left, $right);
                        }
                    }
                }
            }
            
            # Container operations optimization
            if self.types_compatible($left_type, 'Positional') {
                if $right_type eq 'Int' || $right_type eq 'int' {
                    given $op_name {
                        when 'atkey', '[]' {
                            return QAST::Op.new(:op<at_pos>, $left, $right);
                        }
                        when 'existskey' {
                            return QAST::Op.new(:op<exists_pos>, $left, $right);
                        }
                    }
                }
            }
            elsif self.types_compatible($left_type, 'Associative') {
                if $right_type eq 'Str' || $right_type eq 'str' {
                    given $op_name {
                        when 'atkey', '[]' {
                            return QAST::Op.new(:op<at_key_s>, $left, $right);
                        }
                        when 'existskey' {
                            return QAST::Op.new(:op<exists_key_s>, $left, $right);
                        }
                    }
                }
            }
            
            # Eliminate redundant type checks for known types
            if $op_name eq 'isa' || $op_name eq '~~' || $op_name eq '!~~' {
                my $optimized := self.eliminate_redundant_type_check($op, $left_type);
                if $optimized {
                    return $optimized;
                }
            }
            
            # Equality optimizations
            if nqp::exists(['==', 'eq', 'eq_i', 'eq_n', 'eq_s'], $op_name) {
                # x == x optimization
                if nqp::eqaddr($left, $right) {
                    return QAST::BVal.new(:value(True));
                }
            }
            elsif nqp::exists(['!=', 'ne', 'ne_i', 'ne_n', 'ne_s'], $op_name) {
                # x != x optimization
                if nqp::eqaddr($left, $right) {
                    return QAST::BVal.new(:value(False));
                }
            }
        }
        
        nqp::null()
    }
    
    # Specialize numeric operations for better performance
    method specialize_numeric_operation($op, $left_type, $right_type) {
        return nqp::null() unless $op;
        
        my $op_name := $op.op;
        my $left := $op[0];
        my $right := $op[1];
        
        # Handle primitive type aliases
        $left_type := 'Int' if $left_type eq 'int';
        $right_type := 'Int' if $right_type eq 'int';
        $left_type := 'Num' if $left_type eq 'num';
        $right_type := 'Num' if $right_type eq 'num';
        
        # For integer-only operations, we can use faster integer ops
        if $left_type eq 'Int' && $right_type eq 'Int' {
            given $op_name {
                when 'add' { return QAST::Op.new(:op<add_i>, $left, $right) }
                when 'sub' { return QAST::Op.new(:op<sub_i>, $left, $right) }
                when 'mul' { return QAST::Op.new(:op<mul_i>, $left, $right) }
                when 'div' { return QAST::Op.new(:op<div_i>, $left, $right) }
                when 'mod' { return QAST::Op.new(:op<mod_i>, $left, $right) }
                when '=='  { return QAST::Op.new(:op<eq_i>, $left, $right) }
                when '!='  { return QAST::Op.new(:op<ne_i>, $left, $right) }
                when '<'   { return QAST::Op.new(:op<lt_i>, $left, $right) }
                when '<='  { return QAST::Op.new(:op<le_i>, $left, $right) }
                when '>'   { return QAST::Op.new(:op<gt_i>, $left, $right) }
                when '>='  { return QAST::Op.new(:op<ge_i>, $left, $right) }
                # Bitwise operations
                when 'bitwise-and' { return QAST::Op.new(:op<bit_and_i>, $left, $right) }
                when 'bitwise-or'  { return QAST::Op.new(:op<bit_or_i>, $left, $right) }
                when 'bitwise-xor' { return QAST::Op.new(:op<bit_xor_i>, $left, $right) }
                when 'shift-left'  { return QAST::Op.new(:op<shl_i>, $left, $right) }
                when 'shift-right' { return QAST::Op.new(:op<shr_i>, $left, $right) }
                # Power operation
                when '**'  { return QAST::Op.new(:op<pow_i>, $left, $right) }
            }
            
            # Constant optimizations for integer operations
            if nqp::istype($right, QAST::IVal) {
                my int $rval := $right.value;
                
                given $op_name {
                    when 'add' {
                        if $rval == 0 { return $left }  # x + 0 = x
                    }
                    when 'sub' {
                        if $rval == 0 { return $left }  # x - 0 = x
                    }
                    when 'mul' {
                        if $rval == 0 { return QAST::IVal.new(:value(0)) }  # x * 0 = 0
                        elsif $rval == 1 { return $left }  # x * 1 = x
                    }
                    when 'div' {
                        if $rval == 1 { return $left }  # x / 1 = x
                    }
                }
            }
        }
        
        # For floating-point operations
        elsif self.is_numeric_type($left_type) && self.is_numeric_type($right_type) {
            given $op_name {
                when 'add' { return QAST::Op.new(:op<add_n>, $left, $right) }
                when 'sub' { return QAST::Op.new(:op<sub_n>, $left, $right) }
                when 'mul' { return QAST::Op.new(:op<mul_n>, $left, $right) }
                when 'div' { return QAST::Op.new(:op<div_n>, $left, $right) }
                when 'mod' { return QAST::Op.new(:op<mod_n>, $left, $right) }
                when '=='  { return QAST::Op.new(:op<eq_n>, $left, $right) }
                when '!='  { return QAST::Op.new(:op<ne_n>, $left, $right) }
                when '<'   { return QAST::Op.new(:op<lt_n>, $left, $right) }
                when '<='  { return QAST::Op.new(:op<le_n>, $left, $right) }
                when '>'   { return QAST::Op.new(:op<gt_n>, $left, $right) }
                when '>='  { return QAST::Op.new(:op<ge_n>, $left, $right) }
                # Power operation
                when '**'  { return QAST::Op.new(:op<pow_n>, $left, $right) }
            }
            
            # Constant optimizations for float operations
            if nqp::istype($right, QAST::NVal) || nqp::istype($right, QAST::IVal) {
                my num $rval := nqp::istype($right, QAST::IVal) ?? $right.value !! $right.value;
                
                given $op_name {
                    when 'add' {
                        if $rval == 0e0 { return $left }  # x + 0.0 = x
                    }
                    when 'sub' {
                        if $rval == 0e0 { return $left }  # x - 0.0 = x
                    }
                    when 'mul' {
                        if $rval == 0e0 { return QAST::NVal.new(:value(0e0)) }  # x * 0.0 = 0.0
                        elsif $rval == 1e0 { return $left }  # x * 1.0 = x
                    }
                    when 'div' {
                        if $rval == 1e0 { return $left }  # x / 1.0 = x
                    }
                }
            }
        }
        
        nqp::null()
    }
    
    # Specialize string operations for better performance
    method specialize_string_operation($op) {
        my $op_name := $op.op;
        
        given $op_name {
            when 'eq'    { return QAST::Op.new(:op<eq_s>, $op[0], $op[1]) }
            when 'ne'    { return QAST::Op.new(:op<ne_s>, $op[0], $op[1]) }
            when 'lt'    { return QAST::Op.new(:op<lt_s>, $op[0], $op[1]) }
            when 'le'    { return QAST::Op.new(:op<le_s>, $op[0], $op[1]) }
            when 'gt'    { return QAST::Op.new(:op<gt_s>, $op[0], $op[1]) }
            when 'ge'    { return QAST::Op.new(:op<ge_s>, $op[0], $op[1]) }
            when 'concat' { return QAST::Op.new(:op<concat_s>, $op[0], $op[1]) }
        }
        
        nqp::null()
    }
    
    # Eliminate redundant type checks when we know the type
    method eliminate_redundant_type_check($op, $expr_type) {
        my $check_type := $op[1];
        
        # Handle type objects as string literals
        if nqp::istype($check_type, QAST::SVal) {
            my $target_type := $check_type.value;
            
            # If we know the exact type, we can eliminate the check
            if $expr_type eq $target_type || self.is_subtype($expr_type, $target_type) {
                return QAST::BVal.new(:value(True));
            }
            # If we know the type is definitely not the target type
            elsif !self.types_compatible($expr_type, $target_type) {
                return QAST::BVal.new(:value(False));
            }
        }
        
        nqp::null()
    }
    
    # Optimize method calls based on the invocant type
    method optimize_typed_method_call($call) {
        # Get the invocant (first argument)
        my $invocant := $call[0];
        if !$invocant {
            return nqp::null();
        }
        
        # Get the invocant type
        my $invocant_type := self.get_node_type($invocant);
        if $invocant_type eq 'Any' {
            return nqp::null();
        }
        
        # Get the method name
        my $meth_name := $call.name;
        if !$meth_name {
            return nqp::null();
        }
        
        # Try to inline common methods for known types
        my $inlined := self.inline_common_method($invocant_type, $meth_name, $call);
        if $inlined {
            return $inlined;
        }
        
        # Specialize method calls for known types
        my $specialized := self.specialize_method_call($invocant_type, $meth_name, $call);
        if $specialized {
            return $specialized;
        }
        
        nqp::null()
    }
    
    # Inline common methods for performance
    method inline_common_method($type, $meth_name, $call) {
        return nqp::null() unless $call;
        
        my @args := $call.list();
        my $invocant := @args[0];
        
        # Handle primitive type aliases
        $type := 'Int' if $type eq 'int';
        $type := 'Num' if $type eq 'num';
        $type := 'Str' if $type eq 'str';
        $type := 'Bool' if $type eq 'bool';
        
        # Check for common method inlining across types
        given $type {
            # Integer methods
            when 'Int' {
                given $meth_name {
                    when 'abs' {
                        # Inline absolute value for Int
                        return QAST::Op.new(:op<abs_i>, $invocant);
                    }
                    when 'sqrt' {
                        # Inline square root for Int
                        return QAST::Op.new(:op<sqrt_n>, $invocant);
                    }
                    when 'Str' {
                        # Inline string conversion
                        return QAST::Op.new(:op<str>, $invocant);
                    }
                    when 'Num' {
                        # Inline numeric conversion
                        return QAST::Op.new(:op<num>, $invocant);
                    }
                    when 'Bool' {
                        # Inline boolean conversion (non-zero is true)
                        return QAST::Op.new(:op<ne_i>, $invocant, QAST::IVal.new(:value(0)));
                    }
                    when 'succ' {
                        # Inline successor
                        return QAST::Op.new(:op<add_i>, $invocant, QAST::IVal.new(:value(1)));
                    }
                    when 'pred' {
                        # Inline predecessor
                        return QAST::Op.new(:op<sub_i>, $invocant, QAST::IVal.new(:value(1)));
                    }
                }
            }
            # Numeric methods
            when 'Num', 'Rat' {
                given $meth_name {
                    when 'abs' {
                        # Inline absolute value for numeric
                        return QAST::Op.new(:op<abs_n>, $invocant);
                    }
                    when 'sqrt' {
                        # Inline square root for numeric
                        return QAST::Op.new(:op<sqrt_n>, $invocant);
                    }
                    when 'Int' {
                        # Inline integer conversion
                        return QAST::Op.new(:op<int>, $invocant);
                    }
                    when 'Str' {
                        # Inline string conversion
                        return QAST::Op.new(:op<str>, $invocant);
                    }
                    when 'Bool' {
                        # Inline boolean conversion (non-zero is true)
                        return QAST::Op.new(:op<ne_n>, $invocant, QAST::NVal.new(:value(0e0)));
                    }
                }
            }
            # String methods
            when 'Str' {
                given $meth_name {
                    when 'chars' {
                        # Inline string length
                        return QAST::Op.new(:op<chars_s>, $invocant);
                    }
                    when 'uc' {
                        # Inline uppercase conversion
                        return QAST::Op.new(:op<uc_s>, $invocant);
                    }
                    when 'lc' {
                        # Inline lowercase conversion
                        return QAST::Op.new(:op<lc_s>, $invocant);
                    }
                    when 'trim' {
                        # Inline trim operation
                        return QAST::Op.new(:op<trim_s>, $invocant);
                    }
                    when 'Str' {
                        # Identity operation
                        return $invocant;
                    }
                    when 'Bool' {
                        # Non-empty string is true
                        return QAST::Op.new(:op<ne_i>, 
                            QAST::Op.new(:op<chars_s>, $invocant), 
                            QAST::IVal.new(:value(0)));
                    }
                    when 'substr' {
                        # Inline substring with one or two arguments
                        if nqp::elems(@args) == 2 {
                            return QAST::Op.new(:op<substr_s>, $invocant, @args[1]);
                        }
                        elsif nqp::elems(@args) == 3 {
                            return QAST::Op.new(:op<substr_s>, $invocant, @args[1], @args[2]);
                        }
                    }
                    when 'index' {
                        # Inline index method
                        if nqp::elems(@args) == 2 {
                            return QAST::Op.new(:op<index_s>, $invocant, @args[1]);
                        }
                    }
                    when 'contains' {
                        # Inline contains method
                        if nqp::elems(@args) == 2 {
                            return QAST::Op.new(:op<ne_i>, 
                                QAST::Op.new(:op<index_s>, $invocant, @args[1]),
                                QAST::Op.new(:op<int>, QAST::NVal.new(:value(-1e0))));
                        }
                    }
                }
            }
            # Boolean methods
            when 'Bool' {
                given $meth_name {
                    when 'not' {
                        # Inline logical not
                        return QAST::Op.new(:op<not>, $invocant);
                    }
                    when 'Bool' {
                        # Identity operation
                        return $invocant;
                    }
                    when 'Str' {
                        # Inline string conversion
                        return QAST::Op.new(:op<if>, $invocant,
                            QAST::SVal.new(:value<True>),
                            QAST::SVal.new(:value<False>));
                    }
                    when 'Int' {
                        # Inline integer conversion
                        return QAST::Op.new(:op<if>, $invocant,
                            QAST::IVal.new(:value(1)),
                            QAST::IVal.new(:value(0)));
                    }
                }
            }
            # Array and Positional methods
            when 'Array', 'List' {
                given $meth_name {
                    when 'elems' {
                        # Inline array length
                        return QAST::Op.new(:op<elems_a>, $invocant);
                    }
                    when 'push' {
                        # Inline array push for single element
                        if nqp::elems(@args) == 2 {
                            return QAST::Op.new(:op<push_a>, $invocant, @args[1]);
                        }
                    }
                    when 'pop' {
                        # Inline array pop
                        return QAST::Op.new(:op<pop_a>, $invocant);
                    }
                    when 'shift' {
                        # Inline array shift
                        return QAST::Op.new(:op<shift_a>, $invocant);
                    }
                    when 'unshift' {
                        # Inline array unshift
                        if nqp::elems(@args) == 2 {
                            return QAST::Op.new(:op<unshift_a>, $invocant, @args[1]);
                        }
                    }
                    when 'Bool' {
                        # Non-empty array is true
                        return QAST::Op.new(:op<ne_i>, 
                            QAST::Op.new(:op<elems_a>, $invocant), 
                            QAST::IVal.new(:value(0)));
                    }
                    when 'AT-POS' {
                        # Inline positional access
                        if nqp::elems(@args) == 2 {
                            return QAST::Op.new(:op<at_pos>, $invocant, @args[1]);
                        }
                    }
                    when 'EXISTS-POS' {
                        # Inline positional existence check
                        if nqp::elems(@args) == 2 {
                            return QAST::Op.new(:op<exists_pos>, $invocant, @args[1]);
                        }
                    }
                }
            }
            # Hash and Associative methods
            when 'Hash', 'Map' {
                given $meth_name {
                    when 'elems' {
                        # Inline hash length
                        return QAST::Op.new(:op<elems_h>, $invocant);
                    }
                    when 'Bool' {
                        # Non-empty hash is true
                        return QAST::Op.new(:op<ne_i>, 
                            QAST::Op.new(:op<elems_h>, $invocant), 
                            QAST::IVal.new(:value(0)));
                    }
                    when 'AT-KEY' {
                        # Inline key access
                        if nqp::elems(@args) == 2 {
                            return QAST::Op.new(:op<at_key_s>, $invocant, @args[1]);
                        }
                    }
                    when 'EXISTS-KEY' {
                        # Inline key existence check
                        if nqp::elems(@args) == 2 {
                            return QAST::Op.new(:op<exists_key_s>, $invocant, @args[1]);
                        }
                    }
                    when 'DELETE-KEY' {
                        # Inline key deletion
                        if nqp::elems(@args) == 2 {
                            return QAST::Op.new(:op<delete_key_s>, $invocant, @args[1]);
                        }
                    }
                }
            }
        }
        
        nqp::null()
    }
    
    # Specialize method calls for better dispatch performance
    method specialize_method_call($type, $meth_name, $call) {
        # Create a specialized method call with type information
        my $specialized := QAST::Call.new(:name($meth_name), :specialized_type($type));
        
        # Copy all arguments
        for $call.list() -> $arg {
            $specialized.push($arg);
        }
        
        return $specialized;
    }
    
    # Get the type of a node from the type environment
    method get_node_type($node) {
        return 'Any' unless $node;
        
        # First check for type annotations on the node itself
        if $node.annotate('type_hint') -> $hint {
            return $hint;
        }
        
        # Check lexical variables in type environment
        if nqp::istype($node, QAST::Var) {
            my $var_name := $node.name;
            if $node.scope eq 'lexical' && $!type_environment{$var_name}:exists {
                return $!type_environment{$var_name};
            }
            # Handle local variables
            elsif $node.scope eq 'local' && $!type_environment{$var_name}:exists {
                return $!type_environment{$var_name};
            }
            # Handle parameters
            elsif $node.scope eq 'param' && $!type_environment{$var_name}:exists {
                return $!type_environment{$var_name};
            }
        }
        # Handle constant values
        elsif nqp::istype($node, QAST::IVal) {
            return 'Int';
        }
        elsif nqp::istype($node, QAST::NVal) {
            return 'Num';
        }
        elsif nqp::istype($node, QAST::SVal) {
            return 'Str';
        }
        elsif nqp::istype($node, QAST::BVal) {
            return 'Bool';
        }
        # Handle operations with known type results
        elsif nqp::istype($node, QAST::Op) {
            my $op_name := $node.op;
            
            # Type inference for binary operations
            if nqp::elems($node.list()) == 2 {
                my $left_type := self.get_node_type($node[0]);
                my $right_type := self.get_node_type($node[1]);
                
                # Numeric operations on numeric types produce numeric results
                if self.is_numeric_type($left_type) && self.is_numeric_type($right_type) {
                    # Determine the result type based on operands
                    if $left_type eq 'Int' && $right_type eq 'Int' {
                        # Integer division produces Rat in Raku
                        if $op_name eq 'div' || $op_name eq 'div_i' {
                            return 'Rat';
                        }
                        return 'Int';
                    }
                    else {
                        return 'Num';
                    }
                }
                # String operations on strings produce strings
                elsif $left_type eq 'Str' && $right_type eq 'Str' {
                    return 'Str';
                }
                # Comparison operations produce Bool
                elsif nqp::exists(['==', '!=', '<', '<=', '>', '>=', 'eq', 'ne', 'lt', 'le', 'gt', 'ge', '~~', '!~~'], $op_name) {
                    return 'Bool';
                }
            }
            # Unary operations
            elsif nqp::elems($node.list()) == 1 {
                my $arg_type := self.get_node_type($node[0]);
                
                given $op_name {
                    when 'not' { return 'Bool' }
                    when 'abs', 'abs_i', 'abs_n' { return $arg_type }
                    when 'uc', 'lc', 'trim' { return 'Str' }
                    when 'sqrt', 'sqrt_n' { return 'Num' }
                }
            }
        }
        # Handle method calls with known return types
        elsif nqp::istype($node, QAST::Call) && $node.name {
            my $meth_name := $node.name;
            if $node[0] {
                my $invocant_type := self.get_node_type($node[0]);
                
                # Known return types for common methods
                my %return_types := (
                    'Int' => {
                        'abs' => 'Int',
                        'sqrt' => 'Num',
                        'Str' => 'Str',
                    },
                    'Str' => {
                        'chars' => 'Int',
                        'uc' => 'Str',
                        'lc' => 'Str',
                        'trim' => 'Str',
                        'substr' => 'Str',
                    },
                    'Array' => {
                        'elems' => 'Int',
                        'Str' => 'Str',
                    },
                    'Hash' => {
                        'elems' => 'Int',
                    },
                );
                
                if %return_types{$invocant_type}:exists && %return_types{$invocant_type}{$meth_name}:exists {
                    return %return_types{$invocant_type}{$meth_name};
                }
            }
        }
        
        'Any'
    }
    
    # Check if a type is a subtype of another
    method is_subtype($subtype, $supertype) {
        # Return false if either type is undefined
        return False unless $subtype && $supertype;
        
        # Identity check
        if $subtype eq $supertype {
            return True;
        }
        
        # More comprehensive subtype relationships
        my %subtypes := (
            'Int'     => ('Num', 'Cool', 'Any', 'Mu'),
            'Num'     => ('Real', 'Cool', 'Any', 'Mu'),
            'Rat'     => ('Real', 'Num', 'Cool', 'Any', 'Mu'),
            'Complex' => ('Cool', 'Any', 'Mu'),
            'Str'     => ('Cool', 'Any', 'Mu'),
            'Bool'    => ('Int', 'Num', 'Real', 'Cool', 'Any', 'Mu'),
            'Array'   => ('Positional', 'Cool', 'Any', 'Mu'),
            'List'    => ('Positional', 'Cool', 'Any', 'Mu'),
            'Hash'    => ('Associative', 'Cool', 'Any', 'Mu'),
            'Map'     => ('Associative', 'Cool', 'Any', 'Mu'),
        );
        
        # Check direct subtype relationships
        if %subtypes{$subtype}:exists {
            return nqp::exists(%subtypes{$subtype}, $supertype);
        }
        
        # Check primitive aliases
        if $subtype eq 'int' && $supertype eq 'Int' | 'Num' | 'Cool' | 'Any' | 'Mu' {
            return True;
        }
        if $subtype eq 'num' && $supertype eq 'Num' | 'Cool' | 'Any' | 'Mu' {
            return True;
        }
        if $subtype eq 'str' && $supertype eq 'Str' | 'Cool' | 'Any' | 'Mu' {
            return True;
        }
        if $subtype eq 'bool' && $supertype eq 'Bool' | 'Int' | 'Num' | 'Cool' | 'Any' | 'Mu' {
            return True;
        }
        
        False
    }
    
    # Check if two types are compatible
    method types_compatible($type1, $type2) {
        # If either type is undefined, consider them compatible
        return True unless $type1 && $type2;
        
        # If either type is Any or Mu, they are compatible with everything
        if $type1 eq 'Any' || $type2 eq 'Any' || $type1 eq 'Mu' || $type2 eq 'Mu' {
            return True;
        }
        
        # If one is a subtype of the other
        if self.is_subtype($type1, $type2) || self.is_subtype($type2, $type1) {
            return True;
        }
        
        # Check for numeric compatibility
        if self.is_numeric_type($type1) && self.is_numeric_type($type2) {
            return True;
        }
        
        # Check for string compatibility
        if ($type1 eq 'Str' || $type1 eq 'str') && ($type2 eq 'Str' || $type2 eq 'str') {
            return True;
        }
        
        # Check for boolean compatibility
        if ($type1 eq 'Bool' || $type1 eq 'bool') && ($type2 eq 'Bool' || $type2 eq 'bool') {
            return True;
        }
        
        # Check for container compatibility
        my %container_groups := (
            'Positional' => ('Array', 'List', 'Positional'),
            'Associative' => ('Hash', 'Map', 'Associative'),
        );
        
        for %container_groups.kv -> $group, @types {
            if nqp::exists(@types, $type1) && nqp::exists(@types, $type2) {
                return True;
            }
        }
        
        False
    
    # Constant propagation optimization
    method constant_propagation($node) {
        # Only apply at higher optimization levels
        if $!level < 2 {
            return $node;
        }
        
        # Create a working copy of the node
        my $optimized := self.clone_node($node);
        
        # Create a constant environment to track known constant values
        my %const_env;
        
        # Apply constant propagation
        $optimized := self.propagate_constants_helper($optimized, %const_env);
        
        # If any optimizations were applied, return the optimized node
        if !nqp::eqaddr($optimized, $node) {
            self.log_optimization('constant_propagation', $node);
            return $optimized;
        }
        
        return $node;
    }
    
    # Helper method for constant propagation
    method propagate_constants_helper($node, %const_env is rw) {
        # Skip null nodes
        if !$node {
            return $node;
        }
        
        # Process nodes based on their type
        given $node {
            when QAST::Stmts {
                # Process each statement in sequence
                my $new_stmts := QAST::Stmts.new();
                my $modified := False;
                
                for $node.list() -> $stmt {
                    my $optimized_stmt := self.propagate_constants_helper($stmt, %const_env);
                    if !nqp::eqaddr($optimized_stmt, $stmt) {
                        $modified := True;
                    }
                    if $optimized_stmt {
                        $new_stmts.push($optimized_stmt);
                    }
                }
                
                if $modified {
                    return $new_stmts;
                }
            }
            when QAST::Bind {
                # Check if we're binding a constant value to a lexical variable
                if $node.scope eq 'lexical' {
                    my $var_name := $node.name;
                    my $value := $node[0];
                    
                    # Check if the value is a constant
                    if self.is_constant_node($value) {
                        # Record the constant in the environment
                        %const_env{$var_name} := self.get_constant_value($value);
                        return $node;
                    }
                    # If we're binding to a variable that has a constant value, propagate it
                    elsif nqp::istype($value, QAST::Var) && $value.scope eq 'lexical' {
                        my $source_name := $value.name;
                        if %const_env{$source_name}:exists {
                            # Replace the binding with a constant
                            my $const_value := %const_env{$source_name};
                            my $new_value := self.create_constant_node($const_value);
                            $node[0] := $new_value;
                            %const_env{$var_name} := $const_value;
                            return $node;
                        }
                    }
                }
            }
            when QAST::Var {
                # If this is a lexical variable with a known constant value, replace it
                if $node.scope eq 'lexical' {
                    my $var_name := $node.name;
                    if %const_env{$var_name}:exists {
                        my $const_value := %const_env{$var_name};
                        return self.create_constant_node($const_value);
                    }
                }
            }
            when QAST::Op {
                # Try to evaluate operations with constant operands
                my $can_evaluate := True;
                my @operands := $node.list();
                my @values;
                
                for @operands -> $op {
                    if self.is_constant_node($op) {
                        @values.push(self.get_constant_value($op));
                    } else {
                        $can_evaluate := False;
                        last;
                    }
                }
                
                # If all operands are constants, evaluate the operation
                if $can_evaluate {
                    my $result := self.evaluate_operation($node.op, @values);
                    if $result !=== nqp::null() {
                        return self.create_constant_node($result);
                    }
                }
                
                # Otherwise, try to propagate constants to operands
                else {
                    my $modified := False;
                    for @operands.kv -> $i, $op {
                        my $optimized_op := self.propagate_constants_helper($op, %const_env);
                        if !nqp::eqaddr($optimized_op, $op) {
                            $node[$i] := $optimized_op;
                            $modified := True;
                        }
                    }
                    
                    if $modified {
                        return $node;
                    }
                }
            }
        }
        
        # For other node types, recursively process children
        if nqp::can($node, 'list') {
            my @children := $node.list();
            my $modified := False;
            
            for @children.kv -> $i, $child {
                if $child {
                    my $optimized_child := self.propagate_constants_helper($child, %const_env);
                    if !nqp::eqaddr($optimized_child, $child) {
                        $node[$i] := $optimized_child;
                        $modified := True;
                    }
                }
            }
            
            if $modified {
                return $node;
            }
        }
        
        return $node;
    }
    
    # Dead code elimination
    method eliminate_dead_code($node, $optype) {
        # Only apply at higher optimization levels
        if $!level < 3 {
            return nqp::null();
        }
        
        # Create a working copy of the node
        my $optimized := self.clone_node($node);
        
        # Apply dead code elimination
        my %reachable;
        $optimized := self.mark_reachable_code($optimized, %reachable);
        $optimized := self.remove_unreachable_code($optimized, %reachable);
        
        # Also eliminate redundant assignments
        $optimized := self.eliminate_redundant_assignments($optimized);
        
        # If any optimizations were applied, return the optimized node
        if !nqp::eqaddr($optimized, $node) {
            self.log_optimization('dead_code_elimination', $node);
            return $optimized;
        }
        
        nqp::null()
    }
    
    # Mark reachable code
    method mark_reachable_code($node, %reachable is rw) {
        # Skip null nodes
        if !$node {
            return $node;
        }
        
        # Mark this node as reachable
        %reachable{nqp::addr($node)} := True;
        
        # Process based on node type
        given $node {
            when QAST::Stmts {
                # Process each statement in sequence
                for $node.list() -> $stmt {
                    self.mark_reachable_code($stmt, %reachable);
                    
                    # If this is a return, next, last, or redo statement, stop processing
                    if self.is_termination_statement($stmt) {
                        last;
                    }
                }
            }
            when QAST::Op {
                # For conditional branches, mark both branches
                if $node.op eq 'if' || $node.op eq 'unless' {
                    # Condition is always reachable
                    if $node[0] {
                        self.mark_reachable_code($node[0], %reachable);
                    }
                    
                    # Then branch is always reachable
                    if $node[1] {
                        self.mark_reachable_code($node[1], %reachable);
                    }
                    
                    # Else branch is conditionally reachable
                    if nqp::elems($node.list()) > 2 && $node[2] {
                        self.mark_reachable_code($node[2], %reachable);
                    }
                }
                # For loops, mark the body as reachable (at least once)
                elsif $node.op eq 'loop' || $node.op eq 'while' || $node.op eq 'for' {
                    # Condition is always reachable
                    if $node[0] {
                        self.mark_reachable_code($node[0], %reachable);
                    }
                    
                    # Body is always reachable
                    if nqp::elems($node.list()) > 1 && $node[1] {
                        self.mark_reachable_code($node[1], %reachable);
                    }
                }
            }
        }
        
        # Recursively process all children
        if nqp::can($node, 'list') {
            for $node.list() -> $child {
                if $child {
                    self.mark_reachable_code($child, %reachable);
                }
            }
        }
        
        return $node;
    }
    
    # Remove unreachable code
    method remove_unreachable_code($node, %reachable) {
        # Skip null nodes
        if !$node {
            return $node;
        }
        
        # If this node is not reachable, remove it
        if !%reachable{nqp::addr($node)}:exists {
            return nqp::null();
        }
        
        # Process based on node type
        given $node {
            when QAST::Stmts {
                my $new_stmts := QAST::Stmts.new();
                my $modified := False;
                
                for $node.list() -> $stmt {
                    my $optimized_stmt := self.remove_unreachable_code($stmt, %reachable);
                    if nqp::eqaddr($optimized_stmt, $stmt) {
                        $new_stmts.push($stmt);
                    } elsif $optimized_stmt {
                        $new_stmts.push($optimized_stmt);
                        $modified := True;
                    } else {
                        $modified := True;
                    }
                }
                
                if $modified {
                    return $new_stmts;
                }
            }
        }
        
        # Recursively process children
        if nqp::can($node, 'list') {
            my @children := $node.list();
            my $modified := False;
            
            for @children.kv -> $i, $child {
                if $child {
                    my $optimized_child := self.remove_unreachable_code($child, %reachable);
                    if !nqp::eqaddr($optimized_child, $child) {
                        if $optimized_child {
                            $node[$i] := $optimized_child;
                        } else {
                            # Remove the child if it's unreachable
                            $node[$i] := nqp::null();
                        }
                        $modified := True;
                    }
                }
            }
            
            if $modified {
                return $node;
            }
        }
        
        return $node;
    }
    
    # Eliminate redundant assignments
    method eliminate_redundant_assignments($node) {
        # Skip null nodes
        if !$node {
            return $node;
        }
        
        # Process based on node type
        given $node {
            when QAST::Stmts {
                # Track the last assignment to each variable
                my %last_assignment;
                my $new_stmts := QAST::Stmts.new();
                my $modified := False;
                
                for $node.list() -> $stmt {
                    my $keep_stmt := True;
                    
                    # Check if this is an assignment
                    if nqp::istype($stmt, QAST::Bind) && $stmt.scope eq 'lexical' {
                        my $var_name := $stmt.name;
                        my $value := $stmt[0];
                        
                        # If the value is the same variable, it's redundant
                        if nqp::istype($value, QAST::Var) && $value.scope eq 'lexical' && $value.name eq $var_name {
                            $keep_stmt := False;
                            $modified := True;
                        }
                        # If we have a previous assignment to this variable that hasn't been used
                        elsif %last_assignment{$var_name}:exists {
                            # Check if the variable is used between assignments
                            my $used := self.is_variable_used_between($node, %last_assignment{$var_name}, $stmt, $var_name);
                            if !$used {
                                $keep_stmt := False;
                                $modified := True;
                            }
                        }
                        
                        # Update the last assignment
                        if $keep_stmt {
                            %last_assignment{$var_name} := $stmt;
                        }
                    }
                    
                    # Process this statement
                    my $optimized_stmt := self.eliminate_redundant_assignments($stmt);
                    
                    # Keep the statement if needed
                    if $keep_stmt && $optimized_stmt {
                        $new_stmts.push($optimized_stmt);
                    }
                }
                
                if $modified {
                    return $new_stmts;
                }
            }
        }
        
        # Recursively process children
        if nqp::can($node, 'list') {
            for $node.list().kv -> $i, $child {
                if $child {
                    my $optimized_child := self.eliminate_redundant_assignments($child);
                    if !nqp::eqaddr($optimized_child, $child) {
                        $node[$i] := $optimized_child;
                    }
                }
            }
        }
        
        return $node;
    }
    
    # Add missing constant_fold method that's called in optimize_op
    method constant_fold($op, $optype) {
        # First try to handle specific operations that benefit from direct optimization
        # 1. Handle boolean short-circuit operations
        if ($optype eq '&&' || $optype eq 'and') {
            # Short-circuit AND: if first operand is false, whole expression is false
            if self.is_constant_node($op[0]) {
                my $val := self.get_constant_value($op[0]);
                if !$val.DEFINITE || !$val.Bool {
                    my $result := QAST::BVal.new(:value(False));
                    self.log_optimization('short_circuit', $op, $result);
                    return $result;
                }
                # If first operand is true, result is second operand
                return $op[1]; # No need to clone, will be handled by visitor
            }
        }
        elsif ($optype eq '||' || $optype eq 'or') {
            # Short-circuit OR: if first operand is true, whole expression is true
            if self.is_constant_node($op[0]) {
                my $val := self.get_constant_value($op[0]);
                if $val.DEFINITE && $val.Bool {
                    my $result := QAST::BVal.new(:value(True));
                    self.log_optimization('short_circuit', $op, $result);
                    return $result;
                }
                # If first operand is false, result is second operand
                return $op[1]; # No need to clone, will be handled by visitor
            }
        }
        
        # 2. For conditional branches, optimize based on constant conditions
        if $optype eq 'if' || $optype eq 'unless' {
            my $cond := $op[0];
            if self.is_constant_node($cond) {
                my $cond_val := self.get_constant_value($cond);
                my $bool_cond := $cond_val.DEFINITE && $cond_val.Bool;
                
                # For 'if', if condition is always true, return the then branch
                if ($optype eq 'if' && $bool_cond) || ($optype eq 'unless' && !$bool_cond) {
                    my $result := nqp::elems($op.list()) >= 2 ?? $op[1] !! QAST::Stmts.new();
                    self.log_optimization('cond_constant_true', $op, $result);
                    return $result;
                }
                # For 'if', if condition is always false, return the else branch or empty
                elsif ($optype eq 'if' && !$bool_cond) || ($optype eq 'unless' && $bool_cond) {
                    my $result := nqp::elems($op.list()) >= 3 ?? $op[2] !! QAST::Stmts.new();
                    self.log_optimization('cond_constant_false', $op, $result);
                    return $result;
                }
            }
            return nqp::null(); # No optimization possible for this condition
        }
        
        # 3. General case: use our enhanced constant evaluation system
        if nqp::elems($op.list()) >= 1 {
            # Check if all operands are constant
            my $all_constant := True;
            my @values;
            
            for $op.list() -> $child {
                if self.is_constant_node($child) {
                    @values.push(self.get_constant_value($child));
                } else {
                    $all_constant := False;
                    last;
                }
            }
            
            # If all operands are constant, evaluate the operation
            if $all_constant {
                my $result := self.evaluate_operation($optype, @values);
                if $result !=== nqp::null() {
                    my $const_node := self.create_constant_node($result);
                    self.log_optimization('const_fold', $op, $const_node);
                    return $const_node;
                }
            }
        }
        
        # 4. Handle some special cases for string operations
        if $optype eq 'concat_s' && nqp::istype($op[0], QAST::SVal) && nqp::istype($op[1], QAST::SVal) {
            my $result := QAST::SVal.new(:value($op[0].value ~ $op[1].value));
            self.log_optimization('const_fold', $op, $result);
            return $result;
        }
        
        # 5. Handle boolean operations
        if $optype eq 'and_b' && nqp::istype($op[0], QAST::BVal) && nqp::istype($op[1], QAST::BVal) {
            my $result := QAST::BVal.new(:value($op[0].value && $op[1].value));
            self.log_optimization('const_fold', $op, $result);
            return $result;
        }
        elsif $optype eq 'or_b' && nqp::istype($op[0], QAST::BVal) && nqp::istype($op[1], QAST::BVal) {
            my $result := QAST::BVal.new(:value($op[0].value || $op[1].value));
            self.log_optimization('const_fold', $op, $result);
            return $result;
        }
        elsif $optype eq 'not_b' && nqp::istype($op[0], QAST::BVal) {
            my $result := QAST::BVal.new(:value(!$op[0].value));
            self.log_optimization('const_fold', $op, $result);
            return $result;
        }
        
        # No optimization possible
        nqp::null()
    }
    
    # Add missing type_based_optimizations method that's called in optimize_op
    method type_based_optimizations($node) {
        # This is an alias for optimize_using_types for compatibility
        return self.optimize_using_types($node);
    }
    
    # Helper methods for constant propagation and dead code elimination
    method is_constant_node($node) {
        # Basic constant node types
        return 1 if nqp::istype($node, QAST::IVal);
        return 1 if nqp::istype($node, QAST::NVal);
        return 1 if nqp::istype($node, QAST::SVal);
        return 1 if nqp::istype($node, QAST::BVal);
        
        # Check for annotated constant nodes
        return 1 if $node && $node.has_annotation('constant_value');
        
        # Check for constant references to lexicals with known values
        if nqp::istype($node, QAST::Var) && $node.scope eq 'lexical' {
            return $node.has_annotation('constant_value');
        }
        
        # Check for simple operations with all constant operands
        if nqp::istype($node, QAST::Op) {
            for $node.list() -> $op {
                if !self.is_constant_node($op) {
                    return 0;
                }
            }
            # All operands are constant, this could be evaluated
            return 1;
        }
        
        0
    }
    
    method get_constant_value($node) {
        # Handle basic constant types
        if nqp::istype($node, QAST::IVal) {
            return $node.value;
        } elsif nqp::istype($node, QAST::NVal) {
            return $node.value;
        } elsif nqp::istype($node, QAST::SVal) {
            return $node.value;
        } elsif nqp::istype($node, QAST::BVal) {
            return $node.value;
        }
        
        # Handle annotated constant nodes
        if $node && $node.has_annotation('constant_value') {
            return $node.annotation('constant_value');
        }
        
        # For operations with all constant operands, evaluate them
        if nqp::istype($node, QAST::Op) {
            my @values;
            for $node.list() -> $op {
                my $val := self.get_constant_value($op);
                if $val === nqp::null() {
                    return nqp::null();
                }
                @values.push($val);
            }
            return self.evaluate_operation($node.op, @values);
        }
        
        nqp::null()
    }
    
    method create_constant_node($value) {
        given $value {
            when Int     { return QAST::IVal.new(:value($value)) }
            when Num     { return QAST::NVal.new(:value($value)) }
            when Str     { return QAST::SVal.new(:value($value)) }
            when Bool    { return QAST::BVal.new(:value($value)) }
            when Any     { 
                # For more complex types, we can't create a simple constant node
                # But we can annotate a node to indicate it has a constant value
                my $node := QAST::Op.new(:op<identity>);
                $node.annotate('constant_value', $value);
                return $node;
            }
        }
        nqp::null()
    }
    
    method evaluate_operation($op, @values) {
        # Handle unary operations
        if nqp::elems(@values) == 1 {
            my $val := @values[0];
            given $op {
                when '!'          { return !$val }
                when 'not'        { return !$val }
                when 'abs'        { return abs($val) }
                when 'sqrt'       { return sqrt($val) }
                when 'log'        { return log($val) }
                when 'sin'        { return sin($val) }
                when 'cos'        { return cos($val) }
                when 'uc'         { return $val.uc if nqp::istype($val, Str) }
                when 'lc'         { return $val.lc if nqp::istype($val, Str) }
                when 'trim'       { return $val.trim if nqp::istype($val, Str) }
                when 'chars'      { return $val.chars if nqp::istype($val, Str) }
                when 'Bool'       { return $val.Bool }
                when 'Int'        { return $val.Int }
                when 'Num'        { return $val.Num }
                when 'Str'        { return $val.Str }
                when 'succ'       { return $val.succ }
                when 'pred'       { return $val.pred }
                when 'bitwise-neg' { return +^$val if nqp::istype($val, Int) }
            }
        }
        
        # Handle binary operations
        elsif nqp::elems(@values) == 2 {
            my $val1 := @values[0];
            my $val2 := @values[1];
            
            given $op {
                # Arithmetic operations
                when 'add'         { return $val1 + $val2 }
                when 'sub'         { return $val1 - $val2 }
                when 'mul'         { return $val1 * $val2 }
                when 'div'         { return $val1 / $val2 }
                when 'mod'         { return $val1 % $val2 }
                when '**'          { return $val1 ** $val2 }
                
                # Comparison operations
                when 'eq'          { return $val1 eq $val2 }
                when 'ne'          { return $val1 ne $val2 }
                when '=='          { return $val1 == $val2 }
                when '!='          { return $val1 != $val2 }
                when '<'           { return $val1 < $val2 }
                when '<='          { return $val1 <= $val2 }
                when '>'           { return $val1 > $val2 }
                when '>='          { return $val1 >= $val2 }
                
                # Logical operations
                when '&&'          { return $val1 && $val2 }
                when '||'          { return $val1 || $val2 }
                when '^^'          { return ($val1 && !$val2) || (!$val1 && $val2) }
                
                # String operations
                when 'concat'      { return $val1 ~ $val2 }
                when 'substr'      { return substr($val1, $val2) if nqp::istype($val1, Str) && nqp::istype($val2, Int) }
                when 'index'       { return index($val1, $val2) if nqp::istype($val1, Str) && nqp::istype($val2, Str) }
                
                # Bitwise operations
                when 'bitwise-and' { return $val1 +& $val2 if nqp::istype($val1, Int) && nqp::istype($val2, Int) }
                when 'bitwise-or'  { return $val1 +| $val2 if nqp::istype($val1, Int) && nqp::istype($val2, Int) }
                when 'bitwise-xor' { return $val1 +^ $val2 if nqp::istype($val1, Int) && nqp::istype($val2, Int) }
                when 'shift-left'  { return $val1 +< $val2 if nqp::istype($val1, Int) && nqp::istype($val2, Int) }
                when 'shift-right' { return $val1 +> $val2 if nqp::istype($val1, Int) && nqp::istype($val2, Int) }
                
                # Container operations for known sizes
                when 'elems'       { return $val1.elems if nqp::can($val1, 'elems') }
            }
        }
        
        nqp::null()
    }
    
    # Perform dead code elimination
    method dead_code_elimination($node) {
        # Only apply at higher optimization levels
        if $!level < 2 {
            return $node;
        }
        
        # Create a working copy of the node
        my $optimized := self.clone_node($node);
        
        # Apply dead code elimination
        $optimized := self.dce_helper($optimized);
        
        # If any optimizations were applied, return the optimized node
        if !nqp::eqaddr($optimized, $node) {
            self.log_optimization('dead_code_elimination', $node);
            return $optimized;
        }
        
        return $node;
    }
    
    # Helper method for dead code elimination
    method dce_helper($node) {
        # Skip null nodes
        if !$node {
            return $node;
        }
        
        # Process nodes based on their type
        given $node {
            when QAST::Stmts {
                # Process each statement and remove unreachable code
                my $new_stmts := QAST::Stmts.new();
                my $modified := False;
                my $reached_end := False;
                
                for $node.list() -> $stmt {
                    # Skip processing if we've already reached an unreachable point
                    if $reached_end {
                        $modified := True;
                        next;
                    }
                    
                    # Process the statement
                    my $optimized_stmt := self.dce_helper($stmt);
                    
                    # If the optimized statement is different, mark as modified
                    if !nqp::eqaddr($optimized_stmt, $stmt) {
                        $modified := True;
                    }
                    
                    # Add the statement if it's not null
                    if $optimized_stmt {
                        $new_stmts.push($optimized_stmt);
                        
                        # Check if this statement is terminating
                        if self.is_terminating_statement($optimized_stmt) {
                            $reached_end := True;
                        }
                    } else {
                        $modified := True;
                    }
                }
                
                if $modified {
                    return $new_stmts;
                }
            }
            
            when QAST::Op {
                # Handle conditional statements with constant conditions
                if $node.op eq 'if' || $node.op eq 'unless' {
                    my $condition := $node[0];
                    
                    # Check if the condition is a constant boolean
                    if nqp::istype($condition, QAST::BVal) {
                        my $cond_value := $condition.value;
                        
                        # For 'unless', invert the condition
                        if $node.op eq 'unless' {
                            $cond_value := !$cond_value;
                        }
                        
                        # If condition is always true, keep the then branch
                        if $cond_value {
                            if nqp::elems($node.list()) >= 2 {
                                return self.dce_helper($node[1]);
                            } else {
                                return QAST::Stmts.new();  # Empty statement list
                            }
                        }
                        # If condition is always false, keep the else branch (if exists)
                        else {
                            if nqp::elems($node.list()) >= 3 {
                                return self.dce_helper($node[2]);
                            } else {
                                return QAST::Stmts.new();  # Empty statement list
                            }
                        }
                    }
                }
                
                # Process children of the operation
                my $modified := False;
                for $node.list().kv -> $i, $child {
                    my $optimized_child := self.dce_helper($child);
                    if !nqp::eqaddr($optimized_child, $child) {
                        $node[$i] := $optimized_child;
                        $modified := True;
                    }
                }
                
                if $modified {
                    return $node;
                }
            }
            
            when QAST::Bind {
                # Remove redundant assignments to the same variable
                if $node.scope eq 'lexical' {
                    my $var_name := $node.name;
                    my $value := $node[0];
                    
                    # If assigning a variable to itself
                    if nqp::istype($value, QAST::Var) && $value.scope eq 'lexical' && $value.name eq $var_name {
                        return QAST::Stmts.new();  # Remove the assignment
                    }
                }
                
                # Process the value
                my $optimized_value := self.dce_helper($node[0]);
                if !nqp::eqaddr($optimized_value, $node[0]) {
                    $node[0] := $optimized_value;
                    return $node;
                }
            }
        }
        
        # For other node types, recursively process children
        if nqp::can($node, 'list') {
            my $modified := False;
            for $node.list().kv -> $i, $child {
                if $child {
                    my $optimized_child := self.dce_helper($child);
                    if !nqp::eqaddr($optimized_child, $child) {
                        $node[$i] := $optimized_child;
                        $modified := True;
                    }
                }
            }
            
            if $modified {
                return $node;
            }
        }
        
        # Return the original node if no changes were made
        return $node;
    }
    
    # Check if a statement is terminating (no code after it will be executed)
    method is_terminating_statement($node) {
        return 0 unless $node;
        
        # Check for annotated terminating statements
        if $node.has_annotation('is_terminating') {
            return 1;
        }
        
        # Check for explicit terminating operations
        if nqp::istype($node, QAST::Op) {
            my $op_name := $node.op;
            
            # Direct terminating operations
            if nqp::exists(['return', 'leave', 'last', 'next', 'redo', 'exit', 'die', 'fail'], $op_name) {
                return 1;
            }
            
            # Check for calls that are known to not return
            if ($op_name eq 'call' || $op_name eq 'invoke') && $node.has_annotation('never_returns') {
                return 1;
            }
            
            # Check for if/unless with all terminating branches
            if $op_name eq 'if' || $op_name eq 'unless' {
                my @branches;
                if nqp::elems($node.list()) >= 2 {
                    @branches.push($node[1]);  # Then branch
                }
                if nqp::elems($node.list()) >= 3 {
                    @branches.push($node[2]);  # Else branch
                }
                
                # All branches must be terminating
                for @branches -> $branch {
                    if !self.is_terminating_statement($branch) {
                        return 0;
                    }
                }
                
                return +@branches > 0;  # If there are branches and all are terminating
            }
        }
        
        # Check for blocks with terminating last statement
        if nqp::istype($node, QAST::Block) && $node.body {
            my @body_nodes := $node.body.list();
            if nqp::elems(@body_nodes) > 0 {
                return self.is_terminating_statement(@body_nodes[*-1]);
            }
        }
        
        0
    }
    
    method is_termination_statement($stmt) {
        if nqp::istype($stmt, QAST::Op) {
            my $op_name := $stmt.op;
            return $op_name eq 'return' || $op_name eq 'next' || $op_name eq 'last' || $op_name eq 'redo';
        }
        False
    }
    
    method is_variable_used_between($stmts, $start_stmt, $end_stmt, $var_name) {
        my $start_idx := nqp::index($stmts.list(), $start_stmt);
        my $end_idx := nqp::index($stmts.list(), $end_stmt);
        
        # If either statement is not found, return True to be safe
        if $start_idx < 0 || $end_idx < 0 || $start_idx >= $end_idx {
            return True;
        }
        
        # Check statements between start and end
        for $stmts.list() -> $i, $stmt {
            if $i > $start_idx && $i < $end_idx && $stmt {
                if self.does_node_use_variable($stmt, $var_name) {
                    return True;
                }
            }
        }
        
        False
    }
    
    method does_node_use_variable($node, $var_name) {
        # Skip null nodes
        if !$node {
            return False;
        }
        
        # Check if this is a variable reference
        if nqp::istype($node, QAST::Var) && $node.scope eq 'lexical' && $node.name eq $var_name {
            return True;
        }
        
        # Check children
        if nqp::can($node, 'list') {
            for $node.list() -> $child {
                if $child && self.does_node_use_variable($child, $var_name) {
                    return True;
                }
            }
        }
        
        False
    }
    
    # Check if two values are identical (used in comparison optimizations)
    method are_values_identical($node1, $node2) {
        # If they are the same object, they are identical
        if nqp::eqaddr($node1, $node2) {
            return True;
        }
        
        # If they are not the same type, they are not identical
        if nqp::istype($node1, QAST::Var) && nqp::istype($node2, QAST::Var) {
            # For variables, check if they have the same name and scope
            return $node1.name eq $node2.name && $node1.scope eq $node2.scope;
        }
        elsif nqp::istype($node1, QAST::IVal) && nqp::istype($node2, QAST::IVal) {
            # For integer values, check if they have the same value
            return $node1.value == $node2.value;
        }
        elsif nqp::istype($node1, QAST::NVal) && nqp::istype($node2, QAST::NVal) {
            # For numeric values, check if they have the same value
            return $node1.value == $node2.value;
        }
        elsif nqp::istype($node1, QAST::SVal) && nqp::istype($node2, QAST::SVal) {
            # For string values, check if they have the same value
            return $node1.value eq $node2.value;
        }
        elsif nqp::istype($node1, QAST::BVal) && nqp::istype($node2, QAST::BVal) {
            # For boolean values, check if they have the same value
            return $node1.value == $node2.value;
        }
        
        # For other node types, we can't determine identity easily
        False
    }
            # Process each statement in sequence
            for $node.list() -> $stmt {
                if $stmt { self.infer_types($stmt) }
            }
        }
        
        # Return the node with (potentially) annotated type information
        $node
    }
    
    # Infer the type of an expression
    method infer_expr_type($expr) {
        # Constant values have known types
        if nqp::istype($expr, QAST::IVal) {
            return 'Int';
        }
        elsif nqp::istype($expr, QAST::NVal) {
            return 'Num';
        }
        elsif nqp::istype($expr, QAST::SVal) {
            return 'Str';
        }
        elsif nqp::istype($expr, QAST::BVal) {
            return 'Bool';
        }
        # Variables - look up inferred type
        elsif nqp::istype($expr, QAST::Var) {
            # Use get_var_type to retrieve the variable's inferred type
            return $!symbols.get_var_type($expr.name);
        }
        # Operators - infer type from operation
        elsif nqp::istype($expr, QAST::Op) {
            my $op := $expr.op;
            
            # Arithmetic operations on integers likely produce integers
            if $op eq '+' || $op eq '-' || $op eq '*' || $op eq 'div' {
                # Check if all arguments are integers
                my $all_ints := True;
                for $expr.list() -> $arg {
                    my $type := self.infer_expr_type($arg);
                    if $type ne 'Int' {
                        $all_ints := False;
                        last;
                    }
                }
                if $all_ints {
                    return 'Int';
                }
                # Otherwise, likely Num
                return 'Num';
            }
            # String concatenation
            elsif $op eq '~' {
                return 'Str';
            }
            # Comparison operators
            elsif $op eq '==' || $op eq '!=' || $op eq '<' || $op eq '<=' || $op eq '>' || $op eq '>=' {
                return 'Bool';
            }
            # Method calls - infer from the method name and receiver type
            elsif $op eq 'callmethod' && nqp::elems($expr) >= 2 {
                my $receiver_type := self.infer_expr_type($expr[0]);
                my $method_name := $expr[1].value if $expr[1] && nqp::istype($expr[1], QAST::SVal);
                
                # Simple method return type inference
                if $receiver_type eq 'Str' {
                    if $method_name eq 'chars' || $method_name eq 'codes' || $method_name eq 'ords' {
                        return 'Int';
                    }
                    elsif $method_name eq 'uc' || $method_name eq 'lc' || $method_name eq 'tc' || $method_name eq 'flip' {
                        return 'Str';
                    }
                    elsif $method_name eq 'contains' || $method_name eq 'starts-with' || $method_name eq 'ends-with' {
                        return 'Bool';
                    }
                }
            }
        }
        
        # Default: unknown type
        nqp::null()
    }
    
    # Perform constant propagation optimization
    method constant_propagation($node) {
        # Process node based on type
        if nqp::istype($node, QAST::Var) {
            # Try to replace variable with its constant value if known
            my $const_value := self.get_constant_value($node);
            return $const_value if $const_value;
        }
        elsif nqp::istype($node, QAST::Op) {
            # Process children first (bottom-up approach)
            my @new_children;
            for $node.list() -> $child {
                if $child {
                    my $optimized_child := self.constant_propagation($child);
                    @new_children.push($optimized_child);
                }
                else {
                    @new_children.push(nqp::null());
                }
            }
            
            # Replace original children with optimized ones
            $node.set_children(@new_children);
            
            # Try to fold operations with constant children
            my $folded := self.fold_constant_operation($node);
            return $folded if $folded;
        }
        elsif nqp::istype($node, QAST::Stmts) {
            # Process each statement in sequence
            my @new_stmts;
            for $node.list() -> $stmt {
                if $stmt {
                    # Track constant bindings before processing next statement
                    if nqp::istype($stmt, QAST::Bind) {
                        self.track_constant_binding($stmt);
                    }
                    my $optimized_stmt := self.constant_propagation($stmt);
                    @new_stmts.push($optimized_stmt);
                }
            }
            
            # Replace original statements with optimized ones
            $node.set_children(@new_stmts);
        }
        elsif nqp::istype($node, QAST::Bind) {
            # Process both sides
            if $node[0] {
                $node[0] := self.constant_propagation($node[0]);
            }
            if $node[1] {
                $node[1] := self.constant_propagation($node[1]);
            }
            
            # Update constant tracking after propagation
            self.track_constant_binding($node);
        }
        elsif nqp::istype($node, QAST::Block) {
            # Process block body
            if $node.body {
                $node.body := self.constant_propagation($node.body);
            }
        }
        elsif nqp::istype($node, QAST::Call) {
            # Process call arguments
            my @new_args;
            for $node.list() -> $arg {
                if $arg {
                    my $optimized_arg := self.constant_propagation($arg);
                    @new_args.push($optimized_arg);
                }
                else {
                    @new_args.push(nqp::null());
                }
            }
            $node.set_children(@new_args);
        }
        
        # Return the (potentially) optimized node
        $node
    }
    
    # Try to fold an operation with constant children
    method fold_constant_operation($op) {
        # Only attempt folding for binary operations
        my $optype := $op.op();
        if $optype ~~ /^(infix:<[+]>|infix:<[-]>|infix:<[*]>|infix:<[/]>|infix:<[**]>|infix:<[&&]>|infix:<[||]>)$/ {
            my $left := $op[0];
            my $right := $op[1];
            
            # Both operands must be constants
            if $left && $right {
                if nqp::istype($left, QAST::IVal) && nqp::istype($right, QAST::IVal) {
                    my $lval := $left.value;
                    my $rval := $right.value;
                    my $result;
                    
                    # Perform integer operations
                    if $optype eq 'infix:<+>' {
                        $result := $lval + $rval;
                    }
                    elsif $optype eq 'infix:<->' {
                        $result := $lval - $rval;
                    }
                    elsif $optype eq 'infix:<*>' {
                        $result := $lval * $rval;
                    }
                    elsif $optype eq 'infix:</>' {
                        # Handle division carefully to avoid exceptions
                        if $rval != 0 {
                            # Raku integer division returns Int
                            $result := $lval div $rval;
                        }
                    }
                    elsif $optype eq 'infix:<**>' {
                        $result := $lval ** $rval;
                    }
                    
                    if $result.DEFINITE {
                        return QAST::IVal.new(:value($result));
                    }
                }
                elsif nqp::istype($left, QAST::BVal) && nqp::istype($right, QAST::BVal) {
                    my $lval := $left.value;
                    my $rval := $right.value;
                    my $result;
                    
                    # Perform boolean operations
                    if $optype eq 'infix:<&&>' {
                        $result := $lval && $rval;
                    }
                    elsif $optype eq 'infix:<||>' {
                        $result := $lval || $rval;
                    }
                    
                    if $result.DEFINITE {
                        return QAST::BVal.new(:value($result));
                    }
                }
            }
        }
        
        nqp::null() # No folding possible
     }
     
     # Get the constant value of a variable if available
    method get_constant_value($var) {
        # Only handle lexical variables
        if nqp::istype($var, QAST::Var) && $var.scope eq 'lexical' {
            my $name := $var.name;
            
            # Check if the variable has a known constant value
            my $const_value := $!symbols.get_var_constant_value($name);
            if $const_value {
                # Return the appropriate QAST node based on the value type
                if $const_value ~~ Int {
                    return QAST::IVal.new(:value($const_value));
                }
                elsif $const_value ~~ Num {
                    return QAST::NVal.new(:value($const_value));
                }
                elsif $const_value ~~ Str {
                    return QAST::SVal.new(:value($const_value));
                }
                elsif $const_value ~~ Bool {
                    return QAST::BVal.new(:value($const_value));
                }
            }
        }
        
        nqp::null() # No constant value available
    }
    
    # Track constant bindings for later propagation
    method track_constant_binding($bind) {
        my $lhs := $bind[0];
        my $rhs := $bind[1];
        
        # Check if we're binding a variable to a constant value
        if $lhs && nqp::istype($lhs, QAST::Var) && $lhs.scope eq 'lexical' && $rhs {
            my $name := $lhs.name;
            
            # Store type information for the variable
            my $type := self.infer_expr_type($rhs);
            if $type {
                $!symbols.set_var_type($name, $type);
            }
            
            # Check if RHS is a literal constant
            if nqp::istype($rhs, QAST::IVal) {
                $!symbols.set_var_constant_value($name, $rhs.value);
            }
            elsif nqp::istype($rhs, QAST::NVal) {
                $!symbols.set_var_constant_value($name, $rhs.value);
            }
            elsif nqp::istype($rhs, QAST::SVal) {
                $!symbols.set_var_constant_value($name, $rhs.value);
            }
            elsif nqp::istype($rhs, QAST::BVal) {
                $!symbols.set_var_constant_value($name, $rhs.value);
            }
            # Also handle variables that themselves have constant values
            elsif nqp::istype($rhs, QAST::Var) && $rhs.scope eq 'lexical' {
                my $rhs_name := $rhs.name;
                if $!symbols.has_var_constant_value($rhs_name) {
                    my $rhs_const_value := $!symbols.get_var_constant_value($rhs_name);
                    $!symbols.set_var_constant_value($name, $rhs_const_value);
                }
            }
        }
    }
    
    # Optimize based on inferred types
    method type_based_optimizations($node) {
        # Only apply type-based optimizations at higher levels
        if $!level < 2 {
            return nqp::null();
        }
        
        # Devirtualize method calls when possible
        if nqp::istype($node, QAST::Op) && $node.op eq 'callmethod' {
            return self.devirtualize_method_call($node);
        }
        
        # Remove unnecessary type checks
        if nqp::istype($node, QAST::Op) && $node.op eq 'issame' {
            return self.remove_unnecessary_type_checks($node);
        }
        
        nqp::null()
    }
    
    # Devirtualize method calls when the receiver type is known
    method devirtualize_method_call($call) {
        # Get the receiver and method name
        my $receiver := $call[0];
        my $method_name := $call[1].value if $call[1] && nqp::istype($call[1], QAST::SVal);
        
        # Infer the receiver type
        my $receiver_type := self.infer_expr_type($receiver);
        
        # For known types and common methods, we can potentially devirtualize
        if $receiver_type eq 'Str' && $method_name {
            # Inline simple string methods
            if $method_name eq 'chars' && $receiver && nqp::istype($receiver, QAST::SVal) {
                # For string literals, we can compute the length at compile time
                return QAST::IVal.new(:value($receiver.value.chars));
            }
            elsif $method_name eq 'uc' && $receiver && nqp::istype($receiver, QAST::SVal) {
                # For string literals, we can compute the uppercase version at compile time
                return QAST::SVal.new(:value($receiver.value.uc));
            }
        }
        
        nqp::null()
    }
    
    # Remove unnecessary type checks
    method remove_unnecessary_type_checks($check) {
        # Only process type checks where the type is already known
        if nqp::elems($check) >= 2 {
            my $val := $check[0];
            my $type := $check[1];
            
            # Get the inferred type of the value
            my $inferred_type := self.infer_expr_type($val);
            
            # If we know the type and it matches the check, replace with True
            if $inferred_type && $type && nqp::istype($type, QAST::SVal) {
                if $inferred_type eq $type.value {
                    return QAST::BVal.new(:value(True));
                }
                # If we know the types are different, replace with False
                elsif self.are_types_incompatible($inferred_type, $type.value) {
                    return QAST::BVal.new(:value(False));
                }
            }
        }
        
        nqp::null()
    }
    
    # Check if two types are incompatible
    method are_types_incompatible($type1, $type2) {
        # Simple type hierarchy checks
        my %incompatible = (
            'Int' => ('Str', 'Bool', 'Array', 'Hash'),
            'Str' => ('Int', 'Bool', 'Array', 'Hash'),
            'Bool' => ('Int', 'Str', 'Array', 'Hash'),
            'Array' => ('Int', 'Str', 'Bool', 'Hash'),
            'Hash' => ('Int', 'Str', 'Bool', 'Array'),
        );
        
        if %incompatible{$type1} {
            for %incompatible{$type1} -> $t {
                if $t eq $type2 {
                    return True;
                }
            }
        }
        
        False
    }
    
    # Optimize memory usage by reducing unnecessary allocations
    method optimize_memory_usage($node) {
        # Only apply memory optimizations at higher levels
        if $!level < 3 {
            return nqp::null();
        }
        
        # Optimize array and hash allocations
        if nqp::istype($node, QAST::Op) {
            # Replace array construction with more efficient forms when possible
            if $node.op eq 'list' || $node.op eq 'array' {
                return self.optimize_array_construction($node);
            }
            # Optimize hash construction
            elsif $node.op eq 'hash' || $node.op eq 'consthash' {
                return self.optimize_hash_construction($node);
            }
            # Optimize temporary array allocations
            elsif $node.op eq 'push' || $node.op eq 'append' || $node.op eq 'prepend' {
                return self.optimize_collection_operations($node);
            }
            # Optimize array slice operations
            elsif $node.op eq 'atpos' || $node.op eq 'slice' {
                return self.optimize_array_access($node);
            }
            # Optimize method chains that create temporary objects
            elsif $node.op eq 'callmethod' && nqp::istype($node[0], QAST::Op) && $node[0].op eq 'callmethod' {
                return self.optimize_method_chain($node);
            }
        }
        
        # Eliminate unnecessary scalar containers
        if nqp::istype($node, QAST::Bind) {
            return self.eliminate_unnecessary_containers($node);
        }
        
        # Optimize temporary result variables
        if nqp::istype($node, QAST::Stmts) {
            return self.optimize_temporary_variables($node);
        }
        
        nqp::null()
    }
    
    # Optimize hash construction
    method optimize_hash_construction($hash) {
        # Check if this is a hash constructor operation
        if nqp::istype($hash, QAST::Op) && ($hash.op eq 'hash' || $hash.op eq 'consthash') {
            my $elem_count := nqp::elems($hash);
            
            # Optimization 1: Empty hash creation
            if $elem_count == 0 {
                # Return a more efficient empty hash constructor
                return QAST::Op.new(:op<emptyhash>);
            }
            
            # Optimization 2: Small hash with constant elements
            if $elem_count <= 20 {
                # Check if all key-value pairs are constants
                my $all_constants := True;
                my @constant_pairs;
                my $has_duplicates := False;
                my %seen_keys;
                
                for 0, 2 ...^$elem_count -> $i {
                    my $key := $hash[$i];
                    my $value := $hash[$i+1];
                    
                    # Keys should be string literals for best optimization
                    if $key && nqp::istype($key, QAST::SVal) {
                        # Check for duplicate keys
                        if %seen_keys{$key.value} {
                            $has_duplicates := True;
                            last;
                        }
                        %seen_keys{$key.value} := True;
                        
                        # Values can be any constants
                        if $value && (nqp::istype($value, QAST::Val) ||
                                   nqp::istype($value, QAST::IVal) ||
                                   nqp::istype($value, QAST::NVal) ||
                                   nqp::istype($value, QAST::SVal) ||
                                   nqp::istype($value, QAST::BVal)) {
                            @constant_pairs.push($key);
                            @constant_pairs.push($value);
                        } else {
                            $all_constants := False;
                            last;
                        }
                    } else {
                        $all_constants := False;
                        last;
                    }
                }
                
                if $all_constants && !$has_duplicates {
                    # For small constant hashes, create a direct hash constructor
                    my $const_hash := QAST::Op.new(:op<consthash>);
                    for @constant_pairs -> $pair { $const_hash.push($pair) }
                    return $const_hash;
                }
            }
            
            # Optimization 3: Hash with duplicate key-value pairs (rare but possible)
            if $elem_count <= 20 {  # Same size limitation as optimization 2
                # Check if all key-value pairs are constants, allowing duplicates
                my $all_constants := True;
                my %constant_pairs;  # Use a hash to handle duplicates (last one wins)
                
                for 0, 2 ...^$elem_count -> $i {
                    my $key := $hash[$i];
                    my $value := $hash[$i+1];
                    
                    # Keys should be string literals
                    if $key && nqp::istype($key, QAST::SVal) {
                        # Values can be any constants
                        if $value && (nqp::istype($value, QAST::Val) ||
                                   nqp::istype($value, QAST::IVal) ||
                                   nqp::istype($value, QAST::NVal) ||
                                   nqp::istype($value, QAST::SVal) ||
                                   nqp::istype($value, QAST::BVal)) {
                            # Store the key-value pair (will overwrite duplicates)
                            %constant_pairs{$key.value} := [$key, $value];
                        } else {
                            $all_constants := False;
                            last;
                        }
                    } else {
                        $all_constants := False;
                        last;
                    }
                }
                
                if $all_constants {
                    # For constant hashes with possible duplicates, create a direct hash constructor
                    # with only the unique keys (last value for each key)
                    my $const_hash := QAST::Op.new(:op<consthash>);
                    for %constant_pairs.values -> $pair {
                        $const_hash.push($pair[0]);
                        $const_hash.push($pair[1]);
                    }
                    return $const_hash;
                }
            }
        }
        
        nqp::null()
    }
    
    # Optimize array construction
    method optimize_array_construction($array) {
        # Check if this is a list constructor operation
        if nqp::istype($array, QAST::Op) && ($array.op eq 'list' || $array.op eq 'array') {
            my $elem_count := nqp::elems($array);
            
            # Optimization 1: Empty array creation
            if $elem_count == 0 {
                # Return a more efficient empty array constructor
                return QAST::Op.new(:op<emptyarray>);
            }
            
            # Optimization 2: Small array with constant elements
            if $elem_count <= 10 {
                # Check if all elements are constants
                my $all_constants := True;
                my @constant_values;
                
                for $array.list() -> $elem {
                    if $elem && (nqp::istype($elem, QAST::Val) || 
                               nqp::istype($elem, QAST::IVal) ||
                               nqp::istype($elem, QAST::NVal) ||
                               nqp::istype($elem, QAST::SVal) ||
                               nqp::istype($elem, QAST::BVal)) {
                        # Store the constant value for possible optimization
                        @constant_values.push($elem);
                    } else {
                        $all_constants := False;
                        last;
                    }
                }
                
                if $all_constants {
                    # For small constant arrays, create a direct array constructor
                    # that can be optimized further by the backend
                    my $const_array := QAST::Op.new(:op<constarray>);
                    for @constant_values -> $elem { $const_array.push($elem) }
                    return $const_array;
                }
            }
            
            # Optimization 3: Array with repeated elements
            if $elem_count > 1 && $elem_count <= 50 {
                my $first_elem := $array[0];
                my $all_same := True;
                
                # Check if all elements are the same
                for 1..^$elem_count -> $i {
                    if $array[$i] !=== $first_elem {
                        $all_same := False;
                        last;
                    }
                }
                
                if $all_same {
                    # Replace with array-of-same-elements constructor
                    return QAST::Op.new(:op<array_of_same>, $first_elem, QAST::IVal.new(:value($elem_count)));
                }
            }
            
            # Optimization 4: Array with sequential integer values
            if $elem_count >= 3 && $elem_count <= 100 {
                my $is_sequential := True;
                my $start_val;
                my $step;
                
                # Check first two elements to determine start and step
                if $array[0] && nqp::istype($array[0], QAST::IVal) &&
                   $array[1] && nqp::istype($array[1], QAST::IVal) {
                    $start_val := $array[0].value;
                    $step := $array[1].value - $start_val;
                    
                    # Check remaining elements
                    for 2..^$elem_count -> $i {
                        if $array[$i] && nqp::istype($array[$i], QAST::IVal) {
                            my $expected := $start_val + $step * $i;
                            if $array[$i].value !=== $expected {
                                $is_sequential := False;
                                last;
                            }
                        } else {
                            $is_sequential := False;
                            last;
                        }
                    }
                    
                    if $is_sequential {
                        # Replace with range-based array constructor
                        return QAST::Op.new(:op<array_from_range>, 
                                           QAST::IVal.new(:value($start_val)),
                                           QAST::IVal.new(:value($start_val + $step * ($elem_count - 1))),
                                           QAST::IVal.new(:value($step)));
                    }
                }
            }
        }
        
        nqp::null()
    }
    
    # Optimize array access operations
    method optimize_array_access($access) {
        # Optimize direct array element access
        if $access.op eq 'atpos' && $access.elems >= 2 {
            my $array := $access[0];
            my $index := $access[1];
            
            # Case 1: Access to constant array with constant index
            if $array && nqp::istype($array, QAST::Op) && 
               ($array.op eq 'array' || $array.op eq 'constarray' || $array.op eq 'optimized_list') &&
               $index && nqp::istype($index, QAST::IVal) {
                
                my $idx_val := $index.value;
                my $array_size := nqp::elems($array);
                
                # Check if index is within bounds
                if $idx_val >= 0 && $idx_val < $array_size {
                    # Directly return the element at that index
                    return $array[$idx_val];
                }
                # For out-of-bounds access, we can't optimize (would throw)
            }
            
            # Case 2: Access to empty array
            elsif $array && nqp::istype($array, QAST::Op) && $array.op eq 'emptyarray' {
                # Accessing an empty array will always return null/throw
                return QAST::Op.new(:op<null>);
            }
            
            # Case 3: Common index patterns (first, last elements)
            # For first element (index 0)
            elsif $index && nqp::istype($index, QAST::IVal) && $index.value == 0 {
                # Could potentially inline first() method call
                # This would require more complex analysis
            }
            
            # Case 4: Optimize array slice operations
            # 1. For a slice with sequential indices (range), optimize to avoid creating intermediate arrays
            if $index && nqp::istype($index, QAST::Op) && $index.op eq 'range' && $index.elems >= 3 {
                # Extract range parameters
                my $start := $index[0];
                my $end := $index[1];
                my $step := $index[2] // QAST::IVal.new(:value(1));
                
                # If all range parameters are constants, we can optimize further
                if nqp::istype($start, QAST::IVal) && nqp::istype($end, QAST::IVal) && nqp::istype($step, QAST::IVal) {
                    my $start_val := $start.value;
                    my $end_val := $end.value;
                    my $step_val := $step.value;
                    
                    # Handle case when accessing a constant array with a constant range
                    if $array && nqp::istype($array, QAST::Op) && 
                       ($array.op eq 'array' || $array.op eq 'constarray' || $array.op eq 'optimized_list') {
                        my $array_size := nqp::elems($array);
                        my @result_elements;
                        
                        # Collect elements within the range that are within array bounds
                        my $current := $start_val;
                        while ($step_val > 0 && $current <= $end_val) || ($step_val < 0 && $current >= $end_val) {
                            if $current >= 0 && $current < $array_size {
                                @result_elements.push($array[$current]);
                            }
                            $current := $current + $step_val;
                        }
                        
                        # If we have valid elements, create a constant array
                        if @result_elements {
                            my $const_array := QAST::Op.new(:op<optimized_list>);
                            for @result_elements -> $elem { $const_array.push($elem) }
                            return $const_array;
                        }
                        # If no elements are in bounds, return empty array
                        elsif $start_val >= 0 || $end_val < $array_size {
                            return QAST::Op.new(:op<emptyarray>);
                        }
                    }
                }
            }
            # 2. For a slice that's a single element, convert to atpos
            elsif $index && nqp::istype($index, QAST::Op) && $index.op eq 'array' && 
                  $index.elems == 1 && nqp::istype($index[0], QAST::IVal) {
                # Convert single-element slice to atpos for better performance
                my $new_atpos := QAST::Op.new(:op<atpos>);
                $new_atpos.push($array);
                $new_atpos.push($index[0]);
                return $new_atpos;
            }
            # 3. Optimize slice with multiple constant indices on a constant array
            elsif $index && nqp::istype($index, QAST::Op) && $index.op eq 'array' && 
                  $array && nqp::istype($array, QAST::Op) && 
                  ($array.op eq 'array' || $array.op eq 'constarray' || $array.op eq 'optimized_list') {
                # Check if all indices are constants
                my $all_constants := True;
                my @indices;
                for $index.list() -> $idx {
                    if $idx && nqp::istype($idx, QAST::IVal) {
                        @indices.push($idx.value);
                    } else {
                        $all_constants := False;
                        last;
                    }
                }
                
                if $all_constants {
                    my $array_size := nqp::elems($array);
                    my @result_elements;
                    
                    # Collect elements at specified indices (ignoring out-of-bounds)
                    for @indices -> $idx_val {
                        if $idx_val >= 0 && $idx_val < $array_size {
                            @result_elements.push($array[$idx_val]);
                        }
                    }
                    
                    # Create optimized array with extracted elements
                    if @result_elements {
                        my $const_array := QAST::Op.new(:op<optimized_list>);
                        for @result_elements -> $elem { $const_array.push($elem) }
                        return $const_array;
                    }
                    # If no valid indices, return empty array
                    else {
                        return QAST::Op.new(:op<emptyarray>);
                    }
                }
            }
        }
        
        # Optimize slice operations
        elsif $access.op eq 'slice' && $access.elems >= 2 {
            my $array := $access[0];
            my $indices := $access[1];
            
            # Case: Slice of empty array
            if $array && nqp::istype($array, QAST::Op) && $array.op eq 'emptyarray' {
                return QAST::Op.new(:op<emptyarray>);
            }
            
            # Case: Slice with a single constant index
            if $indices && nqp::istype($indices, QAST::Op) && $indices.op eq 'array' && 
               $indices.elems == 1 && nqp::istype($indices[0], QAST::IVal) {
                # Convert to simple atpos access
                my $new_atpos := QAST::Op.new(:op<atpos>);
                $new_atpos.push($array);
                $new_atpos.push($indices[0]);
                return $new_atpos;
            }
        }
        
        nqp::null()
    }
    
    # Optimize collection operations
    method optimize_collection_operations($op) {
        # 1. Optimize immediately consumed temporary array append operations
        if $op && nqp::istype($op, QAST::Op) {
            # Optimize array append operation [1, 2, 3].append(4, 5)
            if $op.op eq 'call' && $op.name eq 'append' && $op[0] {
                my $invocant := $op[0];
                # Check if the invocant is an array literal or optimized array construction
                if $invocant && nqp::istype($invocant, QAST::Op) && 
                   ($invocant.op eq 'array' || $invocant.op eq 'constarray' || 
                    $invocant.op eq 'optimized_list' || $invocant.op eq 'emptyarray') {
                    # Extract existing elements and elements to append
                    my @elements := nqp::findmethod($invocant, 'list')();
                    # Get elements to append from the append call (skip invocant and method name)
                    for 2..^$op.elems -> $i {
                        @elements.push($op[$i]);
                    }
                    
                    # Create a new array construction node
                    my $new_array := QAST::Op.new(:op('array'));
                    for @elements -> $elem {
                        $new_array.push($elem);
                    }
                    
                    # Try to further optimize this new array
                    return self.optimize_array_construction($new_array) || $new_array;
                }
            }
            
            # 2. Optimize array concatenation operation ([1,2], [3,4], [5,6])
            if $op.op eq 'call' && $op.name eq 'infix:<,>' {
                my @all_arrays := [];
                my $all_arrays_are_arrays := True;
                
                # Check if all operands are array constructions
                for 0..^$op.elems -> $i {
                    next if $i == 1; # Skip operator name
                    my $arg := $op[$i];
                    if $arg && nqp::istype($arg, QAST::Op) && 
                       ($arg.op eq 'array' || $arg.op eq 'constarray' || 
                        $arg.op eq 'optimized_list' || $arg.op eq 'emptyarray') {
                        @all_arrays.push($arg);
                    } else {
                        $all_arrays_are_arrays := False;
                        last;
                    }
                }
                
                # If all parameters are arrays, merge them
                if $all_arrays_are_arrays && @all_arrays.elems > 1 {
                    my $new_array := QAST::Op.new(:op('array'));
                    for @all_arrays -> $arr {
                        for nqp::findmethod($arr, 'list')() -> $elem {
                            $new_array.push($elem);
                        }
                    }
                    
                    # Try to further optimize this new array
                    return self.optimize_array_construction($new_array) || $new_array;
                }
            }
            
            # 3. Optimize array repetition operation [1,2] xx 3
            if $op.op eq 'call' && $op.name eq 'infix:<xx>' && $op.elems >= 3 {
                my $array := $op[0];
                my $count := $op[2];
                
                # Check if it's an array literal repeated a fixed number of times
                if $array && nqp::istype($array, QAST::Op) && 
                   ($array.op eq 'array' || $array.op eq 'constarray') && 
                   $count && nqp::istype($count, QAST::IVal) {
                    my $repeat_count := $count.value;
                    
                    # Only optimize reasonable sizes of repetition to avoid generating oversized arrays
                    if $repeat_count >= 1 && $repeat_count <= 100 {
                        my $new_array := QAST::Op.new(:op('array'));
                        my @original_elements := nqp::findmethod($array, 'list')();
                        
                        # Repeatedly add elements
                        for 1..$repeat_count -> $i {
                            for @original_elements -> $elem {
                                $new_array.push($elem);
                            }
                        }
                        
                        # Try to further optimize this new array
                        return self.optimize_array_construction($new_array) || $new_array;
                    }
                }
            }
            
            # 4. Optimize array map operation when the mapping function is an identity function
            if $op.op eq 'call' && $op.name eq 'map' && $op.elems >= 3 {
                my $array := $op[0];
                my $block := $op[2];
                
                # Check if the map block is a simple identity operation: { $_ }
                if $array && $block && nqp::istype($block, QAST::Block) && 
                   $block.body && nqp::istype($block.body, QAST::Var) && 
                   $block.body.name eq '_' && $block.body.scope eq 'parameter' {
                    return $array; # Directly return the original array, no mapping needed
                }
            }
        }
        
        nqp::null() # Return null if no optimization pattern is applicable
    }
    
    # Optimize temporary result variables to reduce unnecessary allocations
    method optimize_temporary_variables($stmts) {
        # This optimization looks for patterns where a variable is bound to a value
        # and then immediately used once and never again, allowing us to eliminate the variable
        
        my @statements := $stmts.list();
        my $stmt_count := nqp::elems(@statements);
        
        if $stmt_count < 2 {
            return nqp::null(); # Not enough statements to optimize
        }
        
        # Track variable definitions and their usage
        my %temp_vars; # name => { definition_index, usage_count, last_usage_index }
        
        # First pass: Identify temporary variable bindings and their usage
        for @statements.kv -> $i, $stmt {
            # Check for variable binding (QAST::Bind)
            if nqp::istype($stmt, QAST::Bind) && 
               $stmt[0] && nqp::istype($stmt[0], QAST::Var) && 
               $stmt[0].scope eq 'lexical' {
                
                my $var_name := $stmt[0].name;
                %temp_vars{$var_name} := {
                    definition_index => $i,
                    usage_count => 0,
                    last_usage_index => $i,
                    definition => $stmt
                };
            }
            
            # Check for variable usage (QAST::Var)
            self.track_variable_usage($stmt, %temp_vars, $i);
        }
        
        # Second pass: Identify variables that can be eliminated
        my %to_eliminate;
        for %temp_vars.kv -> $name, $info {
            # Criteria for elimination:
            # 1. Used exactly once
            # 2. Only used after definition
            # 3. Not used in any control structures that might complicate things
            if $info<usage_count> == 1 && 
               $info<last_usage_index> > $info<definition_index> {
                %to_eliminate{$name} := $info;
            }
        }
        
        # If we found variables to eliminate, create a new statement list
        if %to_eliminate.elems > 0 {
            my @new_statements;
            my %replacement_map;
            
            # Create replacement map for variables
            for %to_eliminate.kv -> $name, $info {
                %replacement_map{$name} := $info<definition>[1]; # RHS of binding
            }
            
            # Build new statement list
            for @statements.kv -> $i, $stmt {
                # Skip the definition of variables we're eliminating
                my $skip := False;
                if nqp::istype($stmt, QAST::Bind) && 
                   $stmt[0] && nqp::istype($stmt[0], QAST::Var) && 
                   %to_eliminate{$stmt[0].name} &&
                   $i == %to_eliminate{$stmt[0].name}<definition_index> {
                    $skip := True;
                }
                
                unless $skip {
                    # Replace variable usages with their values
                    my $new_stmt := self.replace_variable_with_value($stmt, %replacement_map);
                    @new_statements.push($new_stmt);
                }
            }
            
            # Create a new statements node
            my $new_stmts := QAST::Stmts.new();
            for @new_statements -> $stmt {
                $new_stmts.push($stmt);
            }
            
            return $new_stmts;
        }
        
        nqp::null()
    }
    
    # Helper method to track variable usage within an expression
    method track_variable_usage($node, %temp_vars, $current_index) {
        # Base case: node is a variable
        if nqp::istype($node, QAST::Var) && $node.scope eq 'lexical' {
            my $name := $node.name;
            if %temp_vars{$name} {
                %temp_vars{$name}<usage_count> := %temp_vars{$name}<usage_count> + 1;
                if $current_index > %temp_vars{$name}<last_usage_index> {
                    %temp_vars{$name}<last_usage_index> := $current_index;
                }
            }
            return;
        }
        
        # Recursively check children
        if $node && nqp::can($node, 'list') {
            for $node.list() -> $child {
                self.track_variable_usage($child, %temp_vars, $current_index);
            }
        }
    }
    
    # Helper method to replace variable references with their values
    method replace_variable_with_value($node, %replacement_map) {
        # Base case: node is a variable that needs replacement
        if nqp::istype($node, QAST::Var) && $node.scope eq 'lexical' && %replacement_map{$node.name} {
            return self.clone_node(%replacement_map{$node.name});
        }
        
        # For composite nodes, create a new node and replace children
        if $node && nqp::can($node, 'list') {
            # Create a new node of the same type
            my $new_node := QAST::Node.new(type => $node.type);
            
            # Copy attributes
            for nqp::getattr($node, QAST::Node, '$!attrs').hash() -> $key, $value {
                nqp::setattr($new_node, QAST::Node, '$!attrs')<{$key}> := $value;
            }
            
            # Replace children
            for $node.list() -> $child {
                $new_node.push(self.replace_variable_with_value($child, %replacement_map));
            }
            
            return $new_node;
        }
        
        # Return original node if no replacement needed
        return $node;
    }
    
    # Optimize method chains to reduce temporary object creation with advanced pattern recognition
    method optimize_method_chain($chain) {
        # This optimization targets method chains like: obj.method1().method2().method3()
        # The goal is to reduce temporary object allocations and improve performance
        
        # First, check if this is a method chain at all
        unless nqp::istype($chain, QAST::Op) && $chain.op eq 'callmethod' && $chain.elems >= 1 {
            return nqp::null();  # Not a method chain we can optimize
        }
        
        # Get the inner method call (first method in the chain)
        my $inner_call := $chain[0];
        
        # Handle different chain lengths
        if nqp::istype($inner_call, QAST::Op) && $inner_call.op eq 'callmethod' && $inner_call.elems >= 2 {
            # Two-method chain optimization
            my $outer_method := $chain.name;
            my $inner_method := $inner_call.name;
            
            # Case 1: String method chains with expanded optimization patterns
            if self.is_string_operation_chain($inner_method, $outer_method) {
                # Comprehensive optimized chains for string operations
                my %optimized_chains = (
                    'substr' => {
                        'uc' => 'substr_uc', 'lc' => 'substr_lc', 'tc' => 'substr_tc', 'fc' => 'substr_fc',
                        'chars' => 'substr_chars', 'ords' => 'substr_ords', 'flip' => 'substr_flip',
                        'trim' => 'substr_trim', 'triml' => 'substr_triml', 'trimr' => 'substr_trimr',
                        'starts-with' => 'substr_starts_with', 'ends-with' => 'substr_ends_with',
                        'contains' => 'substr_contains', 'index' => 'substr_index', 'rindex' => 'substr_rindex'
                    },
                    'uc' => {
                        'chars' => 'uc_chars', 'ords' => 'uc_ords', 'flip' => 'uc_flip',
                        'trim' => 'uc_trim', 'triml' => 'uc_triml', 'trimr' => 'uc_trimr'
                    },
                    'lc' => {
                        'chars' => 'lc_chars', 'ords' => 'lc_ords', 'flip' => 'lc_flip',
                        'trim' => 'lc_trim', 'triml' => 'lc_triml', 'trimr' => 'lc_trimr'
                    },
                    'tc' => {'chars' => 'tc_chars', 'ords' => 'tc_ords', 'flip' => 'tc_flip'},
                    'fc' => {'chars' => 'fc_chars', 'ords' => 'fc_ords', 'flip' => 'fc_flip'},
                    'flip' => {
                        'uc' => 'flip_uc', 'lc' => 'flip_lc', 'tc' => 'flip_tc', 'fc' => 'flip_fc',
                        'trim' => 'flip_trim'
                    },
                    'trim' => {
                        'uc' => 'trim_uc', 'lc' => 'trim_lc', 'tc' => 'trim_tc', 'fc' => 'trim_fc',
                        'chars' => 'trim_chars', 'ords' => 'trim_ords', 'flip' => 'trim_flip'
                    },
                    'triml' => {'uc' => 'triml_uc', 'lc' => 'triml_lc', 'chars' => 'triml_chars'},
                    'trimr' => {'uc' => 'trimr_uc', 'lc' => 'trimr_lc', 'chars' => 'trimr_chars'}
                );
                
                if %optimized_chains{$inner_method} && %optimized_chains{$inner_method}{$outer_method} {
                    # Create a combined operation that avoids the intermediate string
                    my $combined_op := QAST::Op.new(:op<callmethod>, :name(%optimized_chains{$inner_method}{$outer_method}));
                    
                    # Copy the original receiver (the first argument of the inner call)
                    $combined_op.push($inner_call[0]);
                    
                    # Copy arguments from both method calls
                    for 2..^$inner_call.elems -> $i {
                        $combined_op.push($inner_call[$i]);
                    }
                    for 2..^$chain.elems -> $i {
                        $combined_op.push($chain[$i]);
                    }
                    
                    # Log the optimization with detailed information
                    my $extra_info := "String chain: $inner_method → $outer_method";
                    self.log_optimization('string_method_chain', $chain, $combined_op, $extra_info);
                    
                    return $combined_op;
                }
            }
            
            # Case 2: Array method chains with more comprehensive optimizations
            elsif self.is_array_operation_chain($inner_method, $outer_method) {
                # Implementation of common array method chain optimizations
                my %array_optimizations = (
                    'grep' => {
                        'map' => 'grep_map',        # Optimized grep+map combination
                        'grep' => 'grep_then_grep',   # Combine two grep operations
                        'elems' => 'grep_elems',      # Count matches directly
                        'first' => 'grep_first',      # Find first match without intermediate list
                        'last' => 'grep_last',        # Find last match without intermediate list
                        'any' => 'grep_any',          # Check if any match exists
                        'all' => 'grep_all',          # Check if all elements match
                        'one' => 'grep_one'           # Check if exactly one element matches
                    },
                    'map' => {
                        'map' => 'map_then_map',     # Combine two map operations
                        'grep' => 'map_grep',        # Optimized map+grep combination
                        'elems' => 'map_elems',      # Count mapped elements directly
                        'first' => 'map_first',      # Find first mapped element
                        'last' => 'map_last',        # Find last mapped element
                        'any' => 'map_any',          # Check if any mapped element satisfies condition
                        'all' => 'map_all'           # Check if all mapped elements satisfy condition
                    },
                    'first' => {'elems' => 'first_elems', 'defined' => 'first_defined'},
                    'last' => {'elems' => 'last_elems', 'defined' => 'last_defined'},
                    'sort' => {
                        'grep' => 'sort_grep',      # Sort then grep
                        'map' => 'sort_map',        # Sort then map
                        'first' => 'sort_first',    # Get first element after sorting
                        'last' => 'sort_last'       # Get last element after sorting
                    },
                    'reverse' => {
                        'grep' => 'reverse_grep',   # Reverse then grep
                        'map' => 'reverse_map',     # Reverse then map
                        'sort' => 'reverse_sort'    # Reverse then sort
                    }
                );
                
                if %array_optimizations{$inner_method} && %array_optimizations{$inner_method}{$outer_method} {
                    # Create a combined operation for array methods
                    my $combined_op := QAST::Op.new(:op<callmethod>, 
                                                   :name(%array_optimizations{$inner_method}{$outer_method}));
                    
                    # Copy the original receiver
                    $combined_op.push($inner_call[0]);
                    
                    # Copy arguments - need to carefully handle block arguments
                    # for grep and map operations
                    for 2..^$inner_call.elems -> $i {
                        $combined_op.push($inner_call[$i]);
                    }
                    for 2..^$chain.elems -> $i {
                        $combined_op.push($chain[$i]);
                    }
                    
                    # Log the optimization
                    my $extra_info := "Array chain: $inner_method → $outer_method";
                    self.log_optimization('array_method_chain', $chain, $combined_op, $extra_info);
                    
                    return $combined_op;
                }
            }
            
            # Case 3: General method chaining optimization for identity transformations
            # If outer method is identity (like .Str on a string), we can skip it
            if self.is_identity_method($outer_method, $inner_call[0]) {
                # Just return the inner call result directly
                return $inner_call;
            }
        }
        
        # Check for longer method chains (3 or more methods)
        if $chain.op eq 'callmethod' && $chain.elems >= 2 && $inner_call.op eq 'callmethod' {
            # Get the receiver of the inner call
            my $receiver := $inner_call[0];
            
            # Check if we can collapse the chain
            if nqp::istype($receiver, QAST::Op) && $receiver.op eq 'callmethod' {
                # This is a 3-method chain - try to optimize it
                my $outer_method := $chain.name;
                my $middle_method := $inner_call.name;
                my $first_method := $receiver.name;
                
                # Enhanced handling of three-method combinations
                # Define more common three-method string optimizations
                my %three_method_str_optimizations = (
                    'substr' => {
                        'trim' => {
                            'uc' => 'substr_trim_uc',
                            'lc' => 'substr_trim_lc'
                        },
                        'uc' => {
                            'chars' => 'substr_uc_chars'
                        },
                        'lc' => {
                            'chars' => 'substr_lc_chars'
                        }
                    },
                    'trim' => {
                        'substr' => {
                            'uc' => 'trim_substr_uc',
                            'lc' => 'trim_substr_lc'
                        },
                        'uc' => {
                            'chars' => 'trim_uc_chars'
                        }
                    }
                );
                
                # Check if we have an optimization for this specific three-method chain
                if %three_method_str_optimizations{$first_method} && 
                   %three_method_str_optimizations{$first_method}{$middle_method} && 
                   %three_method_str_optimizations{$first_method}{$middle_method}{$outer_method} {
                    
                    my $combined_op := QAST::Op.new(:op<callmethod>, 
                        :name(%three_method_str_optimizations{$first_method}{$middle_method}{$outer_method}));
                    $combined_op.push($receiver[0]);
                    
                    # Copy all arguments from all methods
                    for 2..^$receiver.elems -> $i { $combined_op.push($receiver[$i]); }
                    for 2..^$inner_call.elems -> $i { $combined_op.push($inner_call[$i]); }
                    for 2..^$chain.elems -> $i { $combined_op.push($chain[$i]); }
                    
                    # Log with detailed information
                    my $extra_info := "Three-method string chain: $first_method → $middle_method → $outer_method";
                    self.log_optimization('three_method_string_chain', $chain, $combined_op, $extra_info);
                    
                    return $combined_op;
                }
                
                # Handle array three-method chains
                my %three_method_array_optimizations = (
                    'grep' => {
                        'map' => {
                            'grep' => 'grep_map_grep',
                            'elems' => 'grep_map_elems'
                        }
                    },
                    'map' => {
                        'grep' => {
                            'map' => 'map_grep_map'
                        }
                    }
                );
                
                # Check if we have an array three-method optimization
                if %three_method_array_optimizations{$first_method} && 
                   %three_method_array_optimizations{$first_method}{$middle_method} && 
                   %three_method_array_optimizations{$first_method}{$middle_method}{$outer_method} {
                    
                    my $combined_op := QAST::Op.new(:op<callmethod>, 
                        :name(%three_method_array_optimizations{$first_method}{$middle_method}{$outer_method}));
                    $combined_op.push($receiver[0]);
                    
                    # Copy all arguments from all methods
                    for 2..^$receiver.elems -> $i { $combined_op.push($receiver[$i]); }
                    for 2..^$inner_call.elems -> $i { $combined_op.push($inner_call[$i]); }
                    for 2..^$chain.elems -> $i { $combined_op.push($chain[$i]); }
                    
                    # Log with detailed information
                    my $extra_info := "Three-method array chain: $first_method → $middle_method → $outer_method";
                    self.log_optimization('three_method_array_chain', $chain, $combined_op, $extra_info);
                    
                    return $combined_op;
                }
            }
        }
        
        nqp::null()
    }
    
    # Check if a method is an identity transformation for a given type
    method is_identity_method($method_name, $receiver) {
        # Check if the receiver is already of the type the method would return
        if $method_name eq 'Str' && nqp::istype($receiver, QAST::WVal) && \
           ($receiver.value ~~ Str || $receiver.value ~~ /^\s*\$/ || \
            $receiver.value ~~ /^\s*\'/) {
            return True;
        }
        elsif $method_name eq 'Int' && nqp::istype($receiver, QAST::WVal) && \
              ($receiver.value ~~ Int || $receiver.value ~~ /^\s*\d+$/) {
            return True;
        }
        
        False
    }
    
    # Helper method to check if two methods form an optimizable string operation chain
    method is_string_operation_chain($first_method, $second_method) {
        # Define common string method pairs that might benefit from optimization
        my %string_chains = (
            'substr' => {'uc', 'lc', 'chars', 'ords', 'flip', 'trim'},
            'uc' => {'chars', 'ords', 'flip', 'trim'},
            'lc' => {'chars', 'ords', 'flip', 'trim'},
            'flip' => {'uc', 'lc', 'chars', 'ords', 'trim'},
            'trim' => {'uc', 'lc', 'chars', 'ords', 'flip'}
        );
        
        %string_chains{$first_method} && %string_chains{$first_method}{$second_method}
    }
    
    # Helper method to check if two methods form an optimizable array operation chain
    method is_array_operation_chain($first_method, $second_method) {
        # Define common array method pairs that might benefit from optimization
        my %array_chains = (
            'grep' => {'map', 'grep', 'first', 'last', 'elems', 'size'},
            'map' => {'grep', 'map', 'first', 'last', 'elems', 'size'},
            'first' => {'elems', 'size', 'defined'},
            'last' => {'elems', 'size', 'defined'}
        );
        
        %array_chains{$first_method} && %array_chains{$first_method}{$second_method}
    }
    
    # Eliminate unnecessary scalar containers
    method eliminate_unnecessary_containers($bind) {
        # Only perform this optimization at higher optimization levels
        if $!level < 2 {
            return nqp::null();
        }
        
        my $lhs := $bind[0];
        my $rhs := $bind[1];
        
        # 1. Check if binding to a lexical variable
        if $lhs && nqp::istype($lhs, QAST::Var) && $lhs.scope eq 'lexical' {
            my $var_name := $lhs.name;
            
            # 2. Collect variable usage information
            my @usages_inner := $!block_var_stack.top.get_usages_inner($var_name) || [];
            my @usages_flat := $!block_var_stack.top.get_usages_flat($var_name) || [];
            
            # 3. Check if there are only read operations and no write operations
            my $has_write := False;
            my $has_container_op := False;
            
            # Check inner usage (same scope)
            for @usages_inner -> $usage {
                if nqp::istype($usage, QAST::Bind) && $usage[0] && nqp::istype($usage[0], QAST::Var) && 
                   $usage[0].name eq $var_name && $usage !=== $bind {
                    $has_write := True;
                    last;
                }
                elsif nqp::istype($usage, QAST::Op) {
                    # Check for container operations (such as .= or postfix:<++>, etc.)
                    if $usage.op eq 'assign' || $usage.op eq 'addassign' || 
                       $usage.op eq 'subtractassign' || $usage.op eq 'multiplyassign' || 
                       $usage.op eq 'divideassign' || $usage.op eq 'postfix:<++>' || 
                       $usage.op eq 'postfix:<-->' || $usage.op eq 'prefix:<++>' || 
                       $usage.op eq 'prefix:<-->' || $usage.op eq 'callmethod' && 
                       ($usage.name eq 'STORE' || $usage.name eq 'ASSIGN-POS' || 
                        $usage.name eq 'ASSIGN-KEY') {
                        $has_container_op := True;
                        last;
                    }
                }
            }
            
            # 4. If there are only reads and no writes, and no container operations, we can eliminate the scalar container
            if !$has_write && !$has_container_op {
                # 5. Check if the right-hand side is already a container type (avoid duplicate containers)
                my $needs_container := False;
                
                # If the right-hand side is an array, hash, or other container type, we might need to keep the container
                if $rhs && nqp::istype($rhs, QAST::Op) {
                    if $rhs.op eq 'array' || $rhs.op eq 'hash' || $rhs.op eq 'constarray' || 
                       $rhs.op eq 'consthash' || $rhs.op eq 'emptyarray' || $rhs.op eq 'emptyhash' {
                        $needs_container := False; # These types are containers themselves, no need for an additional scalar container
                    }
                }
                
                # 6. Create a new bind operation marked as not needing a scalar container
                if !$needs_container {
                    my $new_bind := QAST::Bind.new();
                    
                    # Copy properties and children of the original bind
                    $new_bind.copy_ann($bind);
                    
                    # Create a new variable reference marked as non-container
                    my $new_var := QAST::Var.new(:name($var_name), :scope<lexical>, :non_scalar(1));
                    $new_var.copy_ann($lhs);
                    
                    $new_bind.push($new_var);
                    $new_bind.push($rhs);
                    
                    # Log the optimization
                    if $!debug {
                        note("Eliminated scalar container for variable '$var_name'");
                    }
                    
                    return $new_bind;
                }
            }
        }
        
        nqp::null()
    }

    # Utility method to check if an op is a core operator
    method op_eq_core($op, $name) {
        $op.name eq $name && $!symbols.is_from_core: $name
    }
}

# Class to track and report problems found during optimization
class RakuAST::Optimizer::Problems {
    has $!symbols;
    has @!problems;
    
    method new($symbols) {
        my $obj := nqp::create(self);
        $obj.$!symbols := $symbols;
        $obj.@!problems := [];
        $obj
    }
    
    method add($problem) {
        nqp::push(@!problems, $problem);
    }
    
    # Generate a comprehensive optimization report
    method report() {
        # Initialize report data structure
        my %report = (
            optimizations_applied => [],
            optimization_statistics => {},
            potential_issues => [],
            unoptimized_areas => []
        );
        
        # Process logged optimizations
        if $!optimization_logs {
            for $!optimization_logs.list() -> $log_entry {
                # Extract optimization type from log entry
                if $log_entry ~~ /^Optimization:\s+(\w+)/ {
                    my $opt_type := ~$0;
                    
                    # Add to applied optimizations list
                    %report<optimizations_applied>.push($log_entry);
                    
                    # Update statistics
                    if %report<optimization_statistics>{$opt_type} {
                        %report<optimization_statistics>{$opt_type}++;
                    } else {
                        %report<optimization_statistics>{$opt_type} = 1;
                    }
                }
            }
        }
        
        # Check for potential issues
        # This would be expanded with actual issue detection logic
        if $!level < 2 {
            %report<potential_issues>.push("Low optimization level ({$!level}) may result in suboptimal performance");
        }
        
        # Check for unoptimized complex expressions
        if $!unoptimized_complex_expressions {
            for $!unoptimized_complex_expressions.list() -> $expr {
                %report<unoptimized_areas>.push("Complex expression could not be optimized: $expr");
            }
        }
        
        # Generate human-readable report
        my @report_lines = ["=== Optimization Report ==="];
        
        # Optimization level information
        @report_lines.push("Optimization Level: {$!level}");
        @report_lines.push("");
        
        # Applied optimizations
        @report_lines.push("Applied Optimizations:");
        if %report<optimizations_applied>.elems > 0 {
            for %report<optimizations_applied>.list() -> $opt {
                @report_lines.push("  - $opt");
            }
        } else {
            @report_lines.push("  None");
        }
        @report_lines.push("");
        
        # Optimization statistics
        @report_lines.push("Optimization Statistics:");
        for %report<optimization_statistics>.kv -> $type, $count {
            @report_lines.push("  $type: $count");
        }
        if %report<optimization_statistics>.elems == 0 {
            @report_lines.push("  None");
        }
        @report_lines.push("");
        
        # Potential issues
        @report_lines.push("Potential Issues:");
        if %report<potential_issues>.elems > 0 {
            for %report<potential_issues>.list() -> $issue {
                @report_lines.push("  - $issue");
            }
        } else {
            @report_lines.push("  None");
        }
        @report_lines.push("");
        
        # Unoptimized areas
        @report_lines.push("Unoptimized Areas:");
        if %report<unoptimized_areas>.elems > 0 {
            for %report<unoptimized_areas>.list() -> $area {
                @report_lines.push("  - $area");
            }
        } else {
            @report_lines.push("  None");
        }
        @report_lines.push("");
        @report_lines.push("=== End of Report ===");
        
        # Return the report as a string
        @report_lines.join("\n");
    }
}

# Export the optimizer class
our sub EXPORT() {
    { RakuAST::Optimizer => RakuAST::Optimizer }
}