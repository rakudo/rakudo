=head1 TITLE

perl6_s1.pir - The Rakudo Perl 6 Stage 1 compiler.

=head2 Description

This is the base file for the Rakudo Perl 6 Stage 1 compiler. We build this
in order to compile the setting; we then build the main compiler out of this
bundled with the setting.

=cut

.loadlib 'perl6_group'
.loadlib 'perl6_ops'
.include 'src/gen_builtins.pir'
.include 'src/cli.pir'
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
