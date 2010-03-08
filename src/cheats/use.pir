.namespace []
.sub '!use'
    .param string name
    .param pmc adverbs :named :slurpy

    # Use module loader to locate which module to load.
    .local pmc inc, locator
    .local string pm_file
    inc = get_hll_global '@INC'
    locator = get_hll_global ['Perl6';'Module'], 'Locator'
    pm_file = locator.'find_module_no_conditions'(name, inc)
    if pm_file == '' goto module_not_found

    # For now, we require pre-compiled .pir file.
    .local string filename
    $I0 = length pm_file
    $I0 -= 2
    filename = substr pm_file, 0, $I0
    filename = concat filename, 'pir'
    load_bytecode filename

    .local pmc targetns
    $P0 = getinterp
    targetns = $P0['namespace';1]

    .local pmc loadedpkg, exportns, exportns_it
    $P0 = split '::', name
    push $P0, 'EXPORT'
    push $P0, 'DEFAULT'
    exportns  = get_hll_namespace $P0
    if null exportns goto export_done
    exportns_it = iter exportns
  export_loop:
    unless exportns_it goto export_done
    $S0 = shift exportns_it
    $P0 = exportns[$S0]
    targetns[$S0] = $P0
    goto export_loop
  export_done:
    .return (1)
    
  module_not_found:
    $S0 = concat 'Unable to locate module ', name
    '&die'($S0)
.end
