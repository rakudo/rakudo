## $Id$

=head1 NAME

src/builtins/eval.pir - Perl6 evaluators

=head1 DESCRIPTION

This file implements methods and functions that evaluate code,
such as C<evalfile>, C<require>, and C<use>. The function C<eval>
itself can be found in src/builtins/control.pir.

=head1 Methods

=over 4

=cut

.namespace []
.sub 'onload' :anon :init :load
    $P0 = get_hll_namespace ['Any']
    '!EXPORT'('evalfile', 'from'=>$P0)
.end


.namespace ['Any']
.sub 'evalfile' :method :multi(_)
    .param pmc options         :slurpy :named

    .local string filename
    filename = self

    .local string lang
    lang = options['lang']
    if lang == 'Parrot' goto lang_parrot
    if lang goto lang_compile
    lang = 'Perl6'
  lang_compile:
    .local pmc compiler
    compiler = compreg lang
    .tailcall compiler.'evalfiles'(filename)

  lang_parrot:
    ##  load_bytecode currently doesn't accept non-ascii filenames (TT #65)
    ##  so we'll force it to ascii for now.
    $I0 = find_charset 'ascii'
    filename = trans_charset filename, $I0
    load_bytecode filename
    .return (1)
.end


.namespace []
.sub 'require' :multi(_)
    .param string name
    .param pmc options         :named :slurpy

    .local int ismodule
    .local pmc module
    ismodule = 0
    module = options['module']
    if null module goto have_name
    ismodule = istrue module
    unless ismodule goto have_name

    ##  convert '::' to '/'
    name = clone name
  slash_convert:
    $I0 = index name, '::'
    if $I0 < 0 goto have_name
    substr name, $I0, 2, '/'
    goto slash_convert

  have_name:
    ##  see if we loaded this already
    .local pmc inc_hash
    inc_hash = get_hll_global '%INC'
    $I0 = exists inc_hash[name]
    unless $I0 goto require_name
    $I0 = defined inc_hash[name]
    .return ($I0)

  require_name:
    ##  loop through @INC
    .local pmc inc_it
    $P0 = get_hll_global '@INC'
    inc_it = iter $P0
  inc_loop:
    unless inc_it goto inc_end
    .local string basename, realfilename
    $S0 = shift inc_it
    basename = concat $S0, '/'
    basename .= name
    if ismodule goto try_module
    realfilename = basename
    $I0 = stat realfilename, 0
    if $I0 goto eval_perl6
    goto inc_loop
  try_module:
    realfilename = concat basename, '.pbc'
    $I0 = stat realfilename, 0
    if $I0 goto eval_parrot
    realfilename = concat basename, '.pir'
    $I0 = stat realfilename, 0
    if $I0 goto eval_parrot
    realfilename = concat basename, '.pm'
    $I0 = stat realfilename, 0
    if $I0 goto eval_perl6
    goto inc_loop
  inc_end:
    $S0 = concat "Can't find ", basename
    concat $S0, ' in @INC'
    'die'($S0)
    .return (0)

  eval_parrot:
    .local pmc result
    inc_hash[name] = realfilename
    result = 'evalfile'(realfilename, 'lang'=>'Parrot')
    goto done

  eval_perl6:
    .local pmc outer_ns_chain, outer_blocks
    outer_ns_chain = get_hll_global ['Perl6';'Grammar';'Actions'], '@?NS'
    outer_blocks = get_hll_global ['Perl6';'Grammar';'Actions'], '@?BLOCK'
    $P0 = new 'List'
    set_hll_global ['Perl6';'Grammar';'Actions'], '@?NS', $P0
    $P0 = new 'List'
    set_hll_global ['Perl6';'Grammar';'Actions'], '@?BLOCK', $P0
    inc_hash[name] = realfilename
    result = 'evalfile'(realfilename, 'lang'=>'Perl6')
    set_hll_global ['Perl6';'Grammar';'Actions'], '@?NS', outer_ns_chain
    set_hll_global ['Perl6';'Grammar';'Actions'], '@?BLOCK', outer_blocks

  done:
    .return (result)
.end


.sub 'use'
    .param string module
    .param pmc args            :slurpy
    .param pmc options         :slurpy :named

    # Require module.
    .local pmc retval
    retval = 'require'(module, 'module'=>1)
    unless null retval goto have_retval
    retval = '!FAIL'()
  have_retval:

    # This is a first cut of import. It's essentially wrong, since it's meant
    # by default to put stuff into the lexical pad rather than the namespace.
    # However, it works as a first cut, and lexical stuff isn't quite there
    # enough in Rakudo yet.

    # See if we've had a namespace name passed in.
    .local pmc import_ns
    .local pmc compiler_obj
    compiler_obj = compreg 'Perl6'
    $P0 = options['import_to']
    if null $P0 goto use_caller_ns
    $S0 = $P0
    if $S0 == "" goto use_hll_root_ns
    $P1 = compiler_obj.'parse_name'($S0)
    $S0 = pop $P1
    import_ns = get_hll_global $P1, $S0
    goto got_import_ns
  use_hll_root_ns:
    import_ns = get_hll_namespace
    goto got_import_ns
  use_caller_ns:
    $P0 = new 'ParrotInterpreter'
    $P0 = $P0['sub'; 1]
    import_ns = $P0.'get_namespace'()
  got_import_ns:

    # Look up symbols to import by default.
    .local pmc export_ns
    $P0 = compiler_obj.'parse_name'(module)
    push $P0, 'EXPORT'
    export_ns = get_hll_global $P0, 'DEFAULT'
    if null export_ns goto done_import
    
    # Iterate over them and import.
    .local pmc it
    it = iter export_ns
  it_loop:
    unless it goto it_loop_end
    $S0 = shift it
    $P0 = export_ns[$S0]
    import_ns[$S0] = $P0
    goto it_loop
  it_loop_end:

  done_import:
    .return (retval)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:

