=head1 TITLE

perl6.pir - A Perl 6 compiler.

=head2 Description

This is the base file for the Perl 6 compiler.

This file includes the parsing and grammar rules from
the src/ directory, loads the relevant PGE libraries,
and registers the compiler under the name 'Perl6'.

=head2 Functions

=over 4

=item __onload()

Creates the Perl6 compiler using a C<PCT::HLLCompiler>
object.

=cut

.include 'src/gen_builtins.pir'

.namespace [ 'Perl6::Compiler' ]

.loadlib 'perl6_group'

.sub '__onload' :load :init
    load_bytecode 'PCT.pbc'

    $P0 = get_hll_global ['PCT'], 'HLLCompiler'
    $P1 = $P0.'new'()
    $P1.'language'('Perl6')
    $P1.'parsegrammar'('Perl6::Grammar')
    $P1.'parseactions'('Perl6::Grammar::Actions')

    $P0 = new 'String'
    $P0 = <<'    USAGE'
    -h                   display help text
    --target=[stage]     specify compilation stage to emit
    -t, --trace=[flags]  enable trace flags
    --encoding=[mode]    specify string encoding mode
    -o, --output=[name]  specify name of output file
    USAGE
    setattribute $P1, '$usage', $P0

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


.sub 'usage' :method
    .param string name     :optional
    .param int    has_name :opt_flag

    unless has_name goto got_name
    name = 'perl6'
  got_name:
    print 'Usage: '
    print name
    $P0 = getattribute self, '$usage'
    say $P0
    exit 0
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
