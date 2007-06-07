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

Loads the PGE libraries needed for running the parser,
and registers the Perl6 compiler using a C<HLLCompiler>
object.

=cut

.namespace [ 'Perl6::Compiler' ]

.loadlib 'perl6_group'

.sub '__onload' :load :init
    load_bytecode 'PGE.pbc'
    load_bytecode 'PGE/Text.pbc'
    load_bytecode 'PGE/Util.pbc'
    load_bytecode 'Parrot/HLLCompiler.pbc'
    load_bytecode 'PAST-pm.pbc'

    $P0 = subclass 'PGE::Match', 'Match'
    $P0 = subclass 'Match', 'Grammar'
    $P0 = subclass 'Grammar', 'Perl6::Grammar'

    $P0 = new [ 'HLLCompiler' ]
    $P0.'language'('Perl6')
    $P0.'parsegrammar'('Perl6::Grammar')
    $P0.'astgrammar'('Perl6::PAST::Grammar')
.end


=item main(args :slurpy)  :main

Start compilation by passing any command line C<args>
to the Perl6 compiler.

=cut

.const int SEVERITY_SLOT = 2 # _severity

.sub 'main' :main
    .param pmc args
 
    $P0 = new .ResizablePMCArray
    $P1 = new .Hash
    $P1['END'] = $P0
    store_global '_perl6', '%BLOCKS', $P1

    $P0 = compreg 'Perl6'

    push_eh exit_handler
      $P1 = $P0.'command_line'(args)
    clear_eh

    goto do_END_blocks

# Run all the END blocks that have been registered.

exit_handler:
    .get_results($P0,$S0)     
    .include 'except_severity.pasm'
    $I0 = $P0[SEVERITY_SLOT]
    if $I0 != .EXCEPT_EXIT goto rethrow_error

do_END_blocks:
    .include 'iterator.pasm'

    $P0 = find_global '_perl6', '%BLOCKS'
    if null $P0 goto done
    $P0 = $P0['END']
    if null $P0 goto done
    $P1 = new .Iterator, $P0
    $P1 = .ITERATE_FROM_END
loop_blocks:
    unless $P1 goto done
    $P2 = pop $P1
    $P2() 
    goto loop_blocks
done:
    end

rethrow_error:
    rethrow $P0
.end

.include 'src/parser/expression.pir'
.include 'src/parser/quote.pir'
.include 'src/parser/regex.pir'

.include 'src/builtins_gen.pir'

.include 'src/PAST/Perl6.pir'

.namespace [ 'Perl6::Grammar' ]
.include 'src/parser/grammar_gen.pir'

.include 'src/PAST/Grammar_gen.pir'

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
