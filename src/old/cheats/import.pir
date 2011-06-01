.namespace ['Perl6';'Compiler']

.sub 'import' :method
    .param pmc exportname

    exportname = self.'parse_name'(exportname)

    .local pmc importns
    $P0 = getinterp
    importns = $P0['namespace';1]

    .local pmc tagns, tagns_it
    $P0 = clone exportname
    push $P0, 'EXPORT'
    push $P0, 'DEFAULT'
    tagns = get_hll_namespace $P0
    if null tagns goto tagns_done
    tagns_it = iter tagns
  tagns_loop:
    unless tagns_it goto tagns_done
    $S0 = shift tagns_it
    $P0 = tagns[$S0]
    importns[$S0] = $P0
    goto tagns_loop
  tagns_done:
.end
    
