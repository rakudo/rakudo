## $Id$

=head1 TITLE

methods.pir - Methods on Perl6::Grammar.

=head2 DESCRIPTION

STD.pm contains various methods. This file implements some of them.

=over 4

=item C<add_type(type_name)>

Registers a type in the namespace.

=cut

.namespace [ "Perl6";"Grammar" ]
.sub "add_type" :method
    .param string name
    
    # :: is really anonymous, so do nothing.
    if name != '::' goto non_anon
    .return ()
  non_anon:

    # Parse name.
    .local pmc ns
    .local string short_name
    $P0 = compreg 'Perl6'
    ns = $P0.'parse_name'(name)
    short_name = pop ns

    # Check if the symbol already exists in the NS; if so we record it as
    # an existing type.
    $P0 = get_hll_global ns, short_name
    if null $P0 goto no_namespace
    $S0 = typeof $P0
    unless $S0 == 'NameSpace' goto type_exists

  no_namespace:
    # Work outwards to find a block defining a package and put the type
    # there. XXX This makes it too visible for lexical types, but if we
    # assume lexical rather than package scope then we will fail various
    # tests/code.
    .local pmc blocks, it, cur_block
    blocks = get_hll_global ['Perl6';'Grammar';'Actions'], '@?BLOCK'
    it = iter blocks
  it_loop:
    unless it goto it_loop_end
    cur_block = shift it
    $P0 = cur_block['sym']
    if null $P0 goto it_loop
    if $P0 == '' goto it_loop
  it_loop_end:
    $P0 = cur_block.'symbol'(name)
    if $P0 goto type_exists
    cur_block.'symbol'(name, 'does_abstraction'=>1)

    # We also need to register it under it's fully qualified name at the outermost
    # block.
    .local pmc bottom_block
    $I0 = elements blocks
    dec $I0
    bottom_block = blocks[$I0]
    $P0 = get_hll_global ['Perl6';'Grammar';'Actions'], '@?NS'
    unless $P0 goto no_ns
    $S0 = $P0[0]
    concat $S0, '::'
    name = concat $S0, name
    $P0 = bottom_block.'symbol'(name)
    if $P0 goto type_exists
  no_ns:
    bottom_block.'symbol'(name, 'does_abstraction'=>1)

    # Record that a type was added or already existed.
    $P0 = box 0
    goto set_redecl
  type_exists:
    $P0 = box 1
  set_redecl:
    setprop self, '$!type_redecl', $P0
.end


=item C<is_type(name)>

Checks if the name we have been passed represents a type.

=cut

.sub 'is_type' :method
    .param string full_name

    # Get blocks.
    .local pmc blocks
    blocks = get_hll_global [ 'Perl6' ; 'Grammar' ; 'Actions' ], '@?BLOCK'

    # If it starts with ::, it's a declaration; note it in the block.
    $S0 = substr full_name, 0, 2
    if $S0 != '::' goto not_decl
    $S0 = substr full_name, 2
    $P0 = blocks[0]
    $P0.'symbol'($S0, 'does_abstraction'=>1)
    goto type_ok
  not_decl:

    # Look in @?BLOCK first.
    .local pmc block_it, block, sym_info
    block_it = iter blocks
    block_it_loop:
    unless block_it goto block_it_loop_end
    block = shift block_it
    sym_info = block.'symbol'(full_name)
    if null sym_info goto block_it_loop
    $P0 = sym_info['does_abstraction']
    if null $P0 goto block_it_loop
    unless $P0 goto block_it_loop
    goto type_ok
    block_it_loop_end:

    # Parse name and look for the symbol in the namespace, then check if
    # it's a type.
    .local pmc compiler_obj, check_ns, check_symbol
    .local string short_name
    compiler_obj = get_hll_global [ 'Perl6' ], 'Compiler'
    check_ns = compiler_obj.'parse_name'(full_name)
    short_name = pop check_ns
    check_symbol = get_hll_global check_ns, short_name
    if null check_symbol goto fail_it
    $I0 = does check_symbol, 'Abstraction'
    if $I0 goto type_ok
    # XXX The following should be covered by a check for does Abstraction
    $I0 = isa check_symbol, 'P6protoobject'
    if $I0 goto type_ok
    $I0 = isa check_symbol, 'Perl6Role'
    if $I0 goto type_ok
    $P0 = class check_symbol
    $P0 = getprop 'enum', $P0
    if null $P0 goto not_enum
    if $P0 goto type_ok
  not_enum:
    goto fail_it

      type_ok:
        .return (1)
      fail_it:
        .return (0)
.end


=item type_redeclaration

Checks if the most recently added type was a re-declaration.

=cut

.sub 'type_redeclaration' :method
    $P0 = getprop '$!type_redecl', self
    .return ($P0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
