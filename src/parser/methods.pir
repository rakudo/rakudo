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

    # Parse name.
    .local pmc ns
    .local string short_name
    $P0 = compreg 'Perl6'
    ns = $P0.'parse_name'(name)
    short_name = pop ns

    # Check if the symbol already exists in the NS; if so we're done.
    $P0 = get_hll_global ns, short_name
    unless null $P0 goto done

    # Add name to the current block's symbols.
    .local pmc cur_block
    cur_block = get_hll_global ['Perl6';'Grammar';'Actions'], '@?BLOCK'
    cur_block = cur_block[0]
    cur_block.'symbol'(name, 'does_abstraction'=>1)

  done:
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
    $I0 = isa check_symbol, 'Role'
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

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
