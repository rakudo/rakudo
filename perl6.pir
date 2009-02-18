=head1 TITLE

perl6.pir - The Rakudo Perl 6 compiler.

=head2 Description

This is the base file for the Rakudo Perl 6 compiler.

=cut

.loadlib 'perl6_group'
.loadlib 'perl6_ops'
.include 'src/gen_builtins.pir'

=head2 Functions

=over 4

=item onload()

Creates the Perl 6 compiler by subclassing a C<PCT::HLLCompiler> object.

=cut

.namespace [ 'Perl6';'Compiler' ]

.sub 'onload' :load :init :anon
    load_bytecode 'PCT.pbc'

    .local pmc p6meta, perl6
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    perl6 = p6meta.'new_class'('Perl6::Compiler', 'parent'=>'PCT::HLLCompiler')

    load_bytecode 'config.pbc'

    perl6.'language'('Perl6')
    perl6.'parsegrammar'('Perl6::Grammar')
    perl6.'parseactions'('Perl6::Grammar::Actions')

    ##  set the compilation stages in the @stages attribute
    $P0 = split ' ', 'parse past check_syntax post pir evalpmc'
    setattribute perl6, '@stages', $P0

    ##  set the command line options
    $P0 = split ' ', 'c e=s help|h target=s trace|t=s encoding=s output|o=s version|v'
    setattribute perl6, '@cmdoptions', $P0

    ##  set the $usage attribute
    $P0 = new 'String'
    $P0 = <<'USAGE'
Usage: perl6 [switches] [--] [programfile] [arguments]
  -c                   check syntax only (runs BEGIN and CHECK blocks)
  -e program           one line of program
  -h, --help           display this help text
  --target=[stage]     specify compilation stage to emit
  -t, --trace=[flags]  enable trace flags
  --encoding=[mode]    specify string encoding mode
  -o, --output=[name]  specify name of output file
  -v, --version        display version information
USAGE
    setattribute perl6, '$usage', $P0

    ##  set the $version attribute
    .local pmc cfg
    $P0  = new 'String'
    $P0  = 'This is Rakudo Perl 6'
    push_eh _handler

    # currently works in the build tree, but not in the install tree
    cfg  = _config()
    $P0 .= ', revision '
    $S0  = cfg['revision']
    $P0 .= $S0
    $P0 .= ' built on parrot '
    $S0  = cfg['VERSION']
    $P0 .= $S0
    $S0  = cfg['DEVEL']
    $P0 .= $S0
    $P0 .= "\n"
    $P0 .= 'for '
    $S0  = cfg['archname']
    $P0 .= $S0
  _handler:
    pop_eh
    $P0 .= ".\n\nCopyright 2006-2008, The Perl Foundation.\n"
    setattribute perl6, '$version', $P0

    ##  create a list for holding the stack of nested blocks
    $P0 = new ['List']
    set_hll_global ['Perl6';'Grammar';'Actions'], '@?BLOCK', $P0

    ## create a list for holding the stack of nested package
    ## declarators
    $P0 = new 'List'
    set_hll_global ['Perl6';'Grammar';'Actions'], '@?PKGDECL', $P0

    ## create a list for holding the stack of nested package
    ## namespaces (we store the namespace as a flat, ::
    ## separated string for now, for handing to .parse_name)
    $P0 = new 'List'
    set_hll_global ['Perl6';'Grammar';'Actions'], '@?NS', $P0

    ## create a (shared) metaclass node
    $P0 = get_hll_global ['PAST'], 'Var'
    $P0 = $P0.'new'( 'name'=>'metaclass', 'scope'=>'register' )
    set_hll_global ['Perl6';'Grammar';'Actions'], '$?METACLASS', $P0

    ## create the $?CLASSMAP hash
    $P0 = new ['Hash']
    set_hll_global ['Perl6';'Grammar';'Actions'], '%?CLASSMAP', $P0

    ##  create a list of END blocks to be run
    $P0 = new 'List'
    set_hll_global ['Perl6'], '@?END_BLOCKS', $P0

    ##  tell PAST::Var how to encode Perl6Str and Str values
    $P0 = get_hll_global ['PAST';'Compiler'], '%valflags'
    $P0['Perl6Str'] = 'e'
    $P0['Str'] = 'e'
.end


.namespace ['Perl6';'Compiler']

=item check_syntax(source [, "option" => value, ...])

Check the syntax of C<source> after PAST tree has been built,
to ensure C<BEGIN> and C<CHECK> blocks have been executed.

=cut

.sub 'check_syntax' :method
    .param pmc source
    .param pmc adverbs      :slurpy :named

    $I0 = adverbs['c']
    if $I0 goto check_syntax
    .return ()
  check_syntax:
    ## if we're here, then syntax is OK
    say 'syntax OK'
    exit 0
.end


=item main(args :slurpy)  :main

Start compilation by passing any command line C<args>
to the Perl 6 compiler.

=cut

.sub 'main' :main
    .param pmc args_str

    $S0 = args_str[1]
    if $S0 != '-le' goto not_harness
    exit 0
  not_harness:

    $P0 = compreg 'Perl6'
    $P1 = $P0.'command_line'(args_str, 'encoding'=>'utf8', 'transcode'=>'ascii')

    .include 'iterator.pasm'
    .local pmc iter
    $P0 = get_hll_global ['Perl6'], '@?END_BLOCKS'
    iter = new 'Iterator', $P0
    iter = .ITERATE_FROM_END
  iter_loop:
    unless iter goto iter_end
    $P0 = pop iter
    $P0()
    goto iter_loop
  iter_end:
.end


.sub 'parse_name' :method
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

=back

=cut

.include 'src/gen_setting.pir'
.include 'src/gen_grammar.pir'
.include 'src/parser/expression.pir'
.include 'src/parser/methods.pir'
.include 'src/parser/quote_expression.pir'
.include 'src/gen_actions.pir'
.include 'src/gen_metaop.pir'
.include 'src/gen_junction.pir'

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
