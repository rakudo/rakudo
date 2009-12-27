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
.loadlib 'math_ops'

.sub '' :anon :load :init
    load_bytecode 'P6Regex.pbc'

    # Init Rakudo dynops.
    rakudo_dynop_setup

    # Set up Mu.
    .local pmc objproto, p6meta
    $P0 = get_root_global ['parrot'], 'P6metaclass'
    objproto = $P0.'new_class'('Mu')
    p6meta = objproto.'HOW'()
    set_hll_global ['Mu'], '$!P6META', p6meta

    set_hll_global '$!OBJECTREF', objproto

    ## Also want _dumper for now.
    load_bytecode 'dumper.pbc'

    ## Bring in PAST, PCT, HLL, and NQP namespaces from parrot hllns
    .local pmc hllns, parrotns, imports
    hllns = get_hll_namespace
    parrotns = get_root_namespace ['parrot']
    imports = split ' ', 'PAST PCT HLL _dumper'
    parrotns.'export_to'(hllns, imports)
    .local pmc regexns
    regexns = hllns.'make_namespace'('Regex')
    $P0 = get_root_namespace ['parrot';'Regex';'Cursor']
    regexns.'add_namespace'('Cursor', $P0)
    $P0 = get_root_global ['parrot';'Regex'], 'Cursor'
    regexns['Cursor'] = $P0
    $P0 = get_root_namespace ['parrot';'Regex';'Match']
    regexns.'add_namespace'('Match', $P0)
    $P0 = get_root_global ['parrot';'Regex'], 'Match'
    regexns['Match'] = $P0
    $P0 = get_root_namespace ['parrot';'Regex';'P6Regex']
    regexns.'add_namespace'('P6Regex', $P0)
    $P0 = get_root_global ['parrot';'Regex'], 'P6Regex'
    regexns['P6Regex'] = $P0
    # Tell the actions compiler what hllns we're using
    $P0 = box .RAKUDO_HLL
    set_hll_global ['Perl6';'Actions'], '$?RAKUDO_HLL', $P0
.end

.sub 'DEBUG'
    .param string msg
    printerr msg
    printerr "\n"
.end

.include 'src/gen/builtins.pir'
.include 'src/gen/signature_pm.pir'
.include 'src/gen/parameter_pm.pir'
.include 'src/gen/package_pm.pir'
.include 'src/gen/module_pm.pir'
.include 'src/gen/role_pm.pir'
.include 'src/gen/perl6-grammar.pir'
.include 'src/gen/perl6-actions.pir'

.namespace ['Perl6';'Compiler']

.sub '' :anon :load :init
    # Set up parser/actions.
    .local pmc p6meta, nqpproto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    nqpproto = p6meta.'new_class'('Perl6::Compiler', 'parent'=>'HLL::Compiler')
    nqpproto.'language'('perl6')
    $P0 = get_hll_global ['Perl6'], 'Grammar'
    nqpproto.'parsegrammar'($P0)
    $P0 = get_hll_global ['Perl6'], 'Actions'
    nqpproto.'parseactions'($P0)
    $P0 = split ' ', 'e=s help|h target=s dumper=s trace|t=s encoding=s output|o=s combine version|v parsetrace'
    setattribute nqpproto, '@cmdoptions', $P0
.end

.sub load_module :method
    .param string name
    .local string base, filename
    .local pmc namelist, module
    namelist = self.'parse_name'(name)
    base = join '/', namelist
    push_eh no_precompiled
    filename = concat base, '.pir'
    load_bytecode filename
    pop_eh
    goto done
  no_precompiled:
    pop_eh
    filename = concat base, '.pm'
    self.'evalfiles'(filename)
  done:
    module = self.'get_module'(name)
    .return (module)
.end

.sub 'main' :main
    .param pmc args_str
    $P0 = compreg 'perl6'
    $P1 = $P0.'command_line'(args_str, 'encoding'=>'utf8', 'transcode'=>'ascii iso-8859-1')
    '!fire_phasers'('END')
    exit 0
.end

.include 'src/gen/core.pir'

# Cheats go at the end, because some of them are in the 'parrot' HLL
# namespace.
.include 'src/gen/cheats.pir'

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
