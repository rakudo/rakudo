use NQPP6Regex;

# This powers the optimization pass. It takes place after we've done all
# of the stuff in the grammar and actions, which means CHECK time is over.
# Thus we're allowed to assume that lexpads are immutable, declarations are
# over and done with, multi candidate lists won't change and so forth.
class Perl6::Optimizer {
    has @!block_stack;
    
    # Entry point for the optimization process.
    method optimize($past, *%adverbs) {
        my $unit := $past<UNIT>;
        unless $unit.isa(PAST::Block) {
            pir::die("Optimizer could not find UNIT");
        }
        @!block_stack := [];
        self.visit_block($unit);
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
        if $block.blocktype eq 'immediate' {
            my $outer := @!block_stack[+@!block_stack - 1];

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
            # XXX We may lose other important stuff in $block[0]. Needs
            # to be fixed before we start using this...
            # XXX Need to save $_ either side of this.
            # XXX We can also check for lack of colliding symbols and
            # do something in that case.
            if +@sigsyms == 0 {
                $block.blocktype('declaration');
                $outer[0].push($block);
                return $block[1];
            }
        }
        
        $block
    }
    
    # Called when we encounter a PAST::Op in the tree. Produces either
    # the op itself or some replacement opcode to put in the tree.
    method visit_op($op) {
        self.visit_children($op);
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
}
