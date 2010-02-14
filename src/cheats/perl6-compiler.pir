.namespace ['Perl6';'Compiler']

.sub 'get_lexinfo' :method
    .param pmc context

    .local pmc lexinfo, lexpad, lexpad_it
    lexinfo = root_new ['parrot';'Hash']

  context_loop:
    if null context goto context_done
    lexpad = getattribute context, 'lex_pad'
    lexpad_it = iter lexpad
  lexpad_loop:
    unless lexpad_it goto lexpad_done
    $S0 = shift lexpad_it
    $I0 = exists lexinfo[$S0]
    if $I0 goto lexpad_loop
    $P0 = lexpad[$S0]
    lexinfo[$S0] = $P0
    goto lexpad_loop
  lexpad_done:
    context = getattribute context, 'outer_ctx'
    goto context_loop
  context_done:
    .return (lexinfo)
.end

