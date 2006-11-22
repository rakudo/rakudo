=head1 TITLE

past_perl6.pir - PAST support routines for perl6

=head2 DESCRIPTION

This file contains support routines for PAST nodes in the perl6
compiler.

=item init_perl6([ child1, child2, ... , ] [ 'attr1' => value1, ... ])

Initialize a C<PAST::Block> node as a perl6 block.  This routine
initializes the block's symbol table with a copy of any outer block's
symbol table, and creates a C<mydecl> hash to keep track of
symbols defined locally in the block.

=cut

.namespace [ 'PAST::Block' ]

.sub 'init_perl6' :method
    .param pmc children        :slurpy
    .param pmc adverbs         :slurpy :named

    #   Create an empty children array if we didn't get one
    unless null children goto have_children
    children = new .ResizablePMCArray
  have_children:

    #   If we're not given an existing symbol table, then
    #   copy the outer block's symbol table or create a new one.
    $I0 = exists adverbs['symtable']
    if $I0 goto have_symtable
    .local pmc outerblock, symtable
    outerblock = get_hll_global ['Perl6::PAST::Grammar'], '$?BLOCK'
    if null outerblock goto new_symtable
    symtable = outerblock.'symtable'()
    symtable = clone symtable
    adverbs['symtable'] = symtable
    goto have_symtable
  new_symtable:
    symtable = new .Hash
    adverbs['symtable'] = symtable
  have_symtable:

    #   Initialize a new mydecl hash
    .local pmc mydecl
    mydecl = new .Hash
    self['mydecl'] = mydecl

    #   Call self.'init'(...) to perform normal PAST::Block initializations.
    .return self.'init'(children :flat, adverbs :flat :named)
.end


=item init_lexicals( )

Initialize the block with lexicals that are defined in every
block, including C<$_>, C<$/>, and C<$!>.

=cut

.sub 'init_lexicals' :method
    #    Create a new topic lexical (C<$_>) if it doesn't exist.
    $I0 = self.'mydecl'('$_')
    if $I0 goto have_topic
    .local pmc topicpast
    self.'push_new'('PAST::Var', 'name'=>'$_', 'scope'=>'lexical', 'ismy'=>1)
    self.'mydecl'('$_', 1)
  have_topic:

    #    Create a new match variable (C<$/>) if it doesn't exist.
    $I0 = self.'mydecl'('$/')
    if $I0 goto have_match
    self.'push_new'('PAST::Var', 'name'=>'$/', 'scope'=>'lexical', 'ismy'=>1)
    self.'mydecl'('$_', 1)
  have_match:

    .return (self)
.end


=item mydecl(name, [flag])

Gets/sets the current declaration status for C<name>, if C<flag> is
true, then the block has already declared a local symbol called
C<name>.

=cut

.sub 'mydecl' :method
    .param string name
    .param int value           :optional
    .param int has_value       :opt_flag

    .local pmc mydecl
    mydecl = self['mydecl']
    if has_value goto set_flag
    value = mydecl[name]
    .return (value)
  set_flag:
    mydecl[name] = value
    .return (value)
.end
