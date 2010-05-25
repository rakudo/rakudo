.namespace []
.sub '!MAKE_REGEX'
    .param pmc var

    $I0 = does var, 'invokable'
    if $I0 goto done

    .local pmc p6regex
    p6regex = compreg 'Regex::P6Regex'

  var_list:
    .local pmc var_it, elem
    $P0 = '&list'(var)
    var_it = iter $P0
    var = new ['ResizablePMCArray']
  var_loop:
    unless var_it goto done
    elem = shift var_it
    $I0 = does elem, 'invokable'
    if $I0 goto var_next
    elem = p6regex.'compile'(elem)
  var_next:
    push var, elem
    goto var_loop

  done:
    .return (var)
.end

