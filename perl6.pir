=head1 TITLE

perl6.pir - A Perl 6 compiler.

=head2 Description

This is the base file for the Perl 6 compiler.

This file includes the parsing and grammar rules from
the src/ directory, loads the relevant PGE libraries,
and registers the compiler under the name 'Perl6'.

=head2 Functions

=over 4

=item onload()

Creates the Perl6 compiler by subclassing a C<PCT::HLLCompiler> object.

=cut

.include 'src/gen_builtins.pir'

.namespace [ 'Perl6::Compiler' ]

.loadlib 'perl6_group'

.sub 'onload' :load :init :anon
    load_bytecode 'PCT.pbc'
    load_bytecode 'Protoobject.pbc'

    $P0 = get_hll_global 'Protomaker'
    $P1 = get_class ['PCT::HLLCompiler']
    $P0.'new_subclass'($P1, 'Perl6::Compiler')
.end

.sub 'init' :vtable :method
    load_bytecode 'config.pbc'

    self.'language'('Perl6')
    self.'parsegrammar'('Perl6::Grammar')
    self.'parseactions'('Perl6::Grammar::Actions')

    ##  set the $usage attribute
    $P0 = new 'String'
    $P0 = <<'USAGE'
Usage: perl6 [switches] [--] [programfile] [arguments]
  -h                   display help text
  --target=[stage]     specify compilation stage to emit
  -t, --trace=[flags]  enable trace flags
  --encoding=[mode]    specify string encoding mode
  -o, --output=[name]  specify name of output file
  -v, --version        display version information
USAGE
    setattribute self, '$usage', $P0

    ##  set the $version attribute
    .local pmc cfg
    cfg  = _config()
    $P0  = new 'String'
    $P0  = 'This is perl6, revision '
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
    $P0 .= ".\n\nCopyright 2006-2008, The Perl Foundation.\n"
    setattribute self, '$version', $P0

    ##  create a list for holding the stack of nested blocks
    $P0 = new 'List'
    set_hll_global ['Perl6';'Grammar';'Actions'], '@?BLOCK', $P0

    ##  create a list of END blocks to be run
    $P0 = new 'List'
    set_hll_global ['Perl6'], '@?END_BLOCKS', $P0

    ##  tell PAST::Var how to encode Perl6Str and Str values
    $P0 = get_hll_global ['PAST::Compiler'], '%valflags'
    $P0['Perl6Str'] = 'e'
    $P0['Str'] = 'e'
.end


.namespace ['Perl6::Compiler']

=item command_line(PMC args)

Method for compiler invoked from a shell command line.

=cut

.sub 'command_line' :method
    .param pmc args
    .param pmc adverbs         :slurpy :named

    load_bytecode 'Getopt/Obj.pbc'
    load_bytecode 'dumper.pbc'
    load_bytecode 'PGE/Dumper.pbc'

    ##   perform option processing of command-line args
    .local string arg0
    arg0 = shift args
    .local pmc getopts, opts
    getopts = new 'Getopt::Obj'
    getopts.'notOptStop'(1)
    $P0 = getattribute self, '@cmdoptions'
    .local pmc iter
    iter = new 'Iterator', $P0
  getopts_loop:
    unless iter goto getopts_end
    $S0 = shift iter
    push getopts, $S0
    goto getopts_loop
  getopts_end:
    opts = getopts.'get_options'(args)

    ##   merge command-line args with defaults passed in from caller
    .local pmc iter
    iter = new 'Iterator', opts
  mergeopts_loop:
    unless iter goto mergeopts_end
    $S0 = shift iter
    $P0 = opts[$S0]
    adverbs[$S0] = $P0
    goto mergeopts_loop
  mergeopts_end:

    $I0 = adverbs['help']
    if $I0 goto usage

    $I0 = adverbs['version']
    if $I0 goto version

    .local pmc result
    result = new 'String'
    result = ''

  no_check_syntax:

    push_eh eh_err
    unless args goto interactive
    $I0 = adverbs['combine']
    if $I0 goto combine
    $S0 = shift args
    result = self.'evalfiles'($S0, args :flat, adverbs :flat :named)
    pop_eh
    goto save_output
  combine:
    result = self.'evalfiles'(args, adverbs :flat :named)
    pop_eh
    goto save_output
  interactive:
    self.'interactive'(args :flat, adverbs :flat :named)
    pop_eh

  save_output:
    if null result goto end
    unless result goto end
    .local string target
    target = adverbs['target']
    target = downcase target
    if target != 'pir' goto end
    .local string output
    .local pmc ofh
    ofh = getstdout
    output = adverbs['output']
    if output == '' goto save_output_1
    if output == '-' goto save_output_1
    ofh = open output, '>'
    unless ofh goto err_output
  save_output_1:
    print ofh, result
    close ofh
  end:
    .return ()

  err_output:
    .return self.'panic'('Error: file cannot be written: ', output)
  usage:
    self.'usage'(arg0)
    goto end
  version:
    self.'version'()
    goto end
  eh_err:
    .get_results($P0, $S0)
.end


=item main(args :slurpy)  :main

Start compilation by passing any command line C<args>
to the Perl6 compiler.

=cut

.sub 'main' :main
    .param pmc args_str

    ##  create @ARGS global.  We could possibly use the args pmc
    ##  coming directly from Parrot, but currently Parrot provides
    ##  it as a ResizableStringArray and we need Undefs for
    ##  non-existent elements (RSA gives empty strings).
    .local pmc args, iter
    args = new 'List'
    iter = new 'Iterator', args_str
  args_loop:
    unless iter goto args_end
    $P0 = shift iter
    push args, $P0
    goto args_loop
  args_end:
    set_hll_global '@ARGS', args

    $P0 = compreg 'Perl6'
    $P1 = $P0.'command_line'(args)

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


.include 'src/gen_grammar.pir'
.include 'src/parser/quote_expression.pir'
.include 'src/gen_actions.pir'


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
