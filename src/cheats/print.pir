.namespace []
.sub '&print'
    .param pmc values :slurpy
  loop:
    unless values goto done
    $P0 = shift values
    print $P0
    goto loop
  done:
    .return (1)
.end


.sub '&say'
    .param pmc values :slurpy
    '&print'(values :flat, "\n")
    .return (1)
.end
