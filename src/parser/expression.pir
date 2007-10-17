## $Id$

=head1 TITLE

expression.pir - Parsing of <expression> and <listop_expression> subrules

=head2 DESCRIPTION

This file contains the grammar subrules for <expression> and
<listop_expression>.  These have special parsing requirements,
and are therefore written in PIR instead of as a standard
Perl 6 rule statement.

=over 4

=item C<expression(PMC mob)>

The C<expression> subroutine implements the Perl6::Grammar
<expression> subrule.  It accepts a match object representing
the current state of the parse, passes the match object
to the operator precedence parser to obtain an expression,
and returns the result to the caller.

=cut

.namespace [ "Perl6::Grammar" ]

.include "cclass.pasm"

.sub "expression"
    .param pmc mob
    .param string stoptoken    :optional
    .param int has_stoptoken   :opt_flag
    .param pmc adverbs         :slurpy :named
    .local pmc optable
    .local pmc ws

    optable = find_global 'Perl6::Grammar', "$optable"
    ws = find_global 'Perl6::Grammar', 'ws'
    setattribute optable, "&!ws", ws
    if has_stoptoken > 0 goto expression_1
    stoptoken = ''
  expression_1:
    .return optable."parse"(mob, 'stop'=> stoptoken)
.end


=item C<listop_expression>

Parse a listop expression -- i.e., the tokens that follow
a listop.  This limits the parse to tokens that are tighter
than the listop precedence level, nominally indicated by C<< infix:<== >>.

=cut

.sub 'listop_expression'
    .param pmc mob
    .param pmc adverbs         :slurpy :named
    .local pmc optable, ws
    optable = find_global 'Perl6::Grammar', "$optable"
    ws = find_global 'Perl6::Grammar', 'ws'
    setattribute optable, "&!ws", ws
    .return optable.'parse'(mob, 'tighter'=>'infix:<==')
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
