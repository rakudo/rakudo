# Copyright (C) 2009, Patrick R. Michaud

=head1 NAME

Perl6::Compiler - Perl6 compiler

=head1 DESCRIPTION

=cut

# Set RAKUDO_HLL to 'perl6' to have Rakudo compile in its own HLL namespace.
# MUST BE LOWERCASE
.macro_const RAKUDO_HLL 'perl6'

.HLL .RAKUDO_HLL

.loadlib 'perl6_group'
.loadlib 'perl6_ops'

.sub '' :anon :load :init
    load_bytecode 'P6Regex.pbc'

    # Init Rakudo dynops.
    rakudo_dynop_setup

    # Set up Object. XXX Stop calling it Perl6Object.
    .local pmc objproto, p6meta
    $P0 = get_root_global ['parrot'], 'P6metaclass'
    objproto = $P0.'new_class'('Perl6Object', 'name'=>'Object')
    p6meta = objproto.'HOW'()
    set_hll_global ['Perl6Object'], '$!P6META', p6meta
    set_hll_global '$!OBJECTREF', objproto

    ## Bring in PAST, PCT, HLL, and NQP namespaces from parrot hllns
    .local pmc hllns, parrotns, imports
    hllns = get_hll_namespace
    parrotns = get_root_namespace ['parrot']
    imports = split ' ', 'PAST PCT HLL Regex'
    parrotns.'export_to'(hllns, imports)

    # Tell the actions compiler what hllns we're using
    $P0 = box .RAKUDO_HLL
    set_hll_global ['Perl6';'Actions'], '$?RAKUDO_HLL', $P0
.end

.include 'src/gen/builtins.pir'
.include 'src/gen/cheats.pir'
.include 'src/gen/signature_pm.pir'
.include 'src/gen/parameter_pm.pir'
.include 'src/gen/perl6-grammar.pir'
.include 'src/gen/perl6-actions.pir'

.namespace ['Perl6';'Compiler']

.sub '' :anon :load :init
    # Set up parser/actions.
    .local pmc p6meta, nqpproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    nqpproto = p6meta.'new_class'('Perl6::Compiler', 'parent'=>'HLL::Compiler')
    nqpproto.'language'('Perl6')
    $P0 = get_hll_global ['Perl6'], 'Grammar'
    nqpproto.'parsegrammar'($P0)
    $P0 = get_hll_global ['Perl6'], 'Actions'
    nqpproto.'parseactions'($P0)
    $P0 = split ' ', 'e=s help|h target=s dumper=s trace|t=s encoding=s output|o=s combine version|v parsetrace'
    setattribute nqpproto, '@cmdoptions', $P0
.end

.sub 'main' :main
    .param pmc args_str

    $P0 = compreg 'Perl6'
    $P1 = $P0.'command_line'(args_str, 'encoding'=>'utf8', 'transcode'=>'ascii iso-8859-1')
    exit 0
.end

.sub 'parse_name'
    .param string name
    ##  remove any type parameterization for now
    .local string type_param
    type_param = ''
    $I0 = index name, '['
    if $I0 == -1 goto type_param_done
    type_param = substr name, $I0
    name = substr name, 0, $I0
  type_param_done:
    ##  divide name based on ::
    .local pmc list
    list = split '::', name
    ##  move any leading sigil to the last item
    .local string sigil
    $S0 = list[0]
    sigil = substr $S0, 0, 1
    $I0 = index '$@%&', $S1
    if $I0 < 0 goto sigil_done
    substr $S0, 0, 1, ''
    list[0] = $S0
    $S0 = list[-1]
    $S0 = concat sigil, $S0
    list[-1] = $S0
  sigil_done:
    ##  remove any empty items from the list
    $P0 = iter list
    list = new 'ResizablePMCArray'
  iter_loop:
    unless $P0 goto iter_done
    $S0 = shift $P0
    unless $S0 goto iter_loop
    push list, $S0
    goto iter_loop
  iter_done:
    if type_param == '' goto no_add_type_param
    $S0 = pop list
    concat $S0, type_param
    push list, $S0
  no_add_type_param:
    .return (list)
.end

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
