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

.include 'src/cheats/print-say.pir'
.include 'src/gen/perl6-grammar.pir'
.include 'src/gen/perl6-actions.pir'
.include 'src/gen/signature_pm.pir'
.include 'src/gen/parameter_pm.pir'
.include 'src/classes/Object.pir'
.include 'src/cheats/print-say.pir'

.namespace ['Perl6';'Compiler']

.sub '' :anon :load :init
    # Set up parser/actions.
    .local pmc p6meta, nqpproto
    p6meta = get_root_global ['parrot'], 'P6metaclass'
    nqpproto = p6meta.'new_class'('Perl6::Compiler', 'parent'=>'HLL::Compiler')
    nqpproto.'language'('Perl6')
    $P0 = get_hll_global ['Perl6'], 'Grammar'
    nqpproto.'parsegrammar'($P0)
    $P0 = get_hll_global ['Perl6'], 'Actions'
    nqpproto.'parseactions'($P0)
    $P0 = split ' ', 'e=s help|h target=s dumper=s trace|t=s encoding=s output|o=s combine version|v parsetrace'
    setattribute nqpproto, '@cmdoptions', $P0

    # Init Rakudo dynops.
    rakudo_dynop_setup

    # Set up Object. XXX Stop calling it Perl6Object.
    p6meta.'new_class'('Perl6Object', 'name'=>'Object')
.end

.sub 'main' :main
    .param pmc args_str

    $P0 = compreg 'Perl6'
    $P1 = $P0.'command_line'(args_str, 'encoding'=>'utf8', 'transcode'=>'ascii iso-8859-1')
    exit 0
.end

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
