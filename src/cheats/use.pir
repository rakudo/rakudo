.namespace []
.sub '!use'
    .param string name

    .local string filename
    filename = concat name, '.pir'
    load_bytecode filename

    .local pmc targetns
    $P0 = getinterp
    targetns = $P0['namespace';1]

    .local pmc loadedpkg, exportns, exportns_it
    $P0 = split '::', name
    push $P0, 'EXPORT'
    push $P0, 'DEFAULT'
    exportns  = get_hll_namespace $P0
    exportns_it = iter exportns
  export_loop:
    unless exportns_it goto export_done
    $S0 = shift exportns_it
    $P0 = exportns[$S0]
    targetns[$S0] = $P0
    goto export_loop
  export_done:
    .return (1)
.end

