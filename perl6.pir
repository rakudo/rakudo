=head1 TITLE

perl6.pir - The Rakudo Perl 6 compiler.

=head2 Description

This is the base file for the Rakudo Perl 6 compiler.

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

    ##  set the compilation stages in the @stages attribute
    $P0 = split ' ', 'parse past check_syntax post pir evalpmc'
    setattribute self, '@stages', $P0

    ##  set the command line options
    $P0 = split ' ', 'c e=s help|h target=s trace|t=s encoding=s output|o=s combine each version|v'
    setattribute self, '@cmdoptions', $P0

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
    setattribute self, '$usage', $P0

    ##  set the $version attribute
    .local pmc cfg
    $P0  = new 'String'
    $P0  = 'This is Rakudo Perl 6'
    push_eh _handler
    cfg  = _config()    # currently works in the build tree, but not in the install tree
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
    $P0 .= ".\n\nCopyright 2006-2008, The Perl Foundation.\n"
    setattribute self, '$version', $P0

    ##  create a list for holding the stack of nested blocks
    $P0 = new 'List'
    set_hll_global ['Perl6';'Grammar';'Actions'], '@?BLOCK', $P0

    ## create a list for holding the stack of nested packages
    ## (that may be roles, modules, classes or grammars).
    $P0 = new 'List'
    set_hll_global ['Perl6';'Grammar';'Actions'], '@?PACKAGE', $P0

    ## create a list for holding the stack of nested classes
    $P0 = new 'List'
    set_hll_global ['Perl6';'Grammar';'Actions'], '@?CLASS', $P0

    ## create a list for holding the stack of nested roles
    $P0 = new 'List'
    set_hll_global ['Perl6';'Grammar';'Actions'], '@?ROLE', $P0

    ##  create a list of END blocks to be run
    $P0 = new 'List'
    set_hll_global ['Perl6'], '@?END_BLOCKS', $P0

    ##  tell PAST::Var how to encode Perl6Str and Str values
    $P0 = get_hll_global ['PAST::Compiler'], '%valflags'
    $P0['Perl6Str'] = 'e'
    $P0['Str'] = 'e'
.end


.namespace ['Perl6::Compiler']

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
# vim: expandtab shiftwidth=4 ft=pir:
