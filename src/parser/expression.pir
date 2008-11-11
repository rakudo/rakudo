## $Id$

=head1 TITLE

expression.pir - Parsing of <expression> and <listop_expression> subrules

=head2 DESCRIPTION

This file contains the grammar subrules for <expression> and
<listop_expression>.  These have special parsing requirements,
and are therefore written in PIR instead of as a standard
Perl 6 rule statement.

=over 4

=item C<EXPR([tighter])>

The C<EXPR> method implements the Perl6::Grammar <EXPR> subrule.
It forwards the match object (invocant) to the operator
precedence parser to obtain an expression, and returns the
result to the caller.  Any C<tighter> option is passed as a
corresponding option to the operator precedence parser, which
parses expressions of tighter precedence.

(FIXME Parrot bug RT#53296 prevents us from using :optional
on the C<tighter> argument along with :slurpy :named parameters,
so we use :multi as a temporary workaround.)

=cut

.namespace [ "Perl6";"Grammar" ]

.include "cclass.pasm"

.sub "EXPR" :method :multi(_)
    .param pmc adverbs         :slurpy :named
    .local pmc optable

    optable = get_hll_global ['Perl6';'Grammar'], "$optable"
    .tailcall optable."parse"(self, 'rulename'=>'EXPR', adverbs :named :flat)
.end

.sub "EXPR" :method :multi(_,_)
    .param pmc tighter
    .param pmc adverbs         :slurpy :named
    .local pmc optable

    optable = get_hll_global ['Perl6';'Grammar'], "$optable"
    .tailcall optable."parse"(self, 'rulename'=>'EXPR', 'tighter'=>tighter, adverbs :named :flat)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
