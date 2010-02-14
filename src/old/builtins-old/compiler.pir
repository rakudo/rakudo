## $Id$

=head1 NAME

src/builtins/compiler.pir - various Perl6::Compiler methods

=head1 Methods

=over 4

=cut

.namespace ['Perl6';'Compiler']

.sub 'import' :method
    .param pmc exportns
    .param pmc symbols         :slurpy
    .param pmc options         :slurpy :named

    $P0 = self.'parse_name'(exportns)
    exportns = get_hll_namespace $P0
    if null exportns goto end

    .local pmc importns
    importns = options['import_to']
    if null importns goto import_caller_ns
    $P0 = self.'parse_name'(importns)
    importns = get_hll_namespace $P0
    goto have_importns
  import_caller_ns:
    $P0 = getinterp
    $P0 = $P0['sub';1]
    importns = $P0.'get_namespace'()
  have_importns:

    .local pmc symbols_it
    symbols_it = iter symbols
  symbols_loop:
    unless symbols_it goto symbols_done
    .local string symtag
    symtag = shift symbols_it
    $S0 = substr symtag, 0, 1
    if $S0 == ':' goto symbols_tag
    $P0 = exportns[$S0]
    importns[$S0] = $P0
    goto symbols_loop

  symbols_tag:
    symtag = substr symtag, 1
    .local pmc tagns
    tagns = exportns.'get_name'()
    push tagns, 'EXPORT'
    push tagns, symtag
    tagns = get_root_namespace tagns
    if null tagns goto tagns_done
    .local pmc tagns_it
    tagns_it = iter tagns
  tagns_loop:
    unless tagns_it goto tagns_done
    $S0 = shift tagns_it
    $P0 = tagns[$S0]
    importns[$S0] = $P0
    goto tagns_loop
  tagns_done:
    goto symbols_loop
  symbols_done:
  end:
.end

=back

=cut
