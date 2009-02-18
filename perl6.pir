=head1 TITLE

perl6.pir - The Rakudo Perl 6 compiler.

=head2 Description

This is the base file for the Rakudo Perl 6 compiler. Essentially, it just
includes a bunch of other PIR files.

=cut

.loadlib 'perl6_group'
.loadlib 'perl6_ops'
.include 'src/gen_builtins.pir'
.include 'src/cli.pir'
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
