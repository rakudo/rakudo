## $Id$

=head1 TITLE

parse.pir - Parsing support subroutines

=head2 DESCRIPTION

This file contains support subroutines for parsing Perl 6 programs.  
Specialized parsing subroutines will appear here as the parser grows.

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
    .param string stoptoken       :optional
    .param int has_stoptoken   :opt_flag
    .local pmc optable
    .local pmc ws

    optable = find_global 'Perl6::Grammar', "$optable"
    ws = find_global 'Perl6::Grammar', 'expression_ws'
    setattribute optable, "PGE::OPTable\x0&!ws", ws
    if has_stoptoken > 0 goto expression_1
    stoptoken = ''
  expression_1:
    .return optable."parse"(mob, 'stop'=> ';')
.end

## vim: expandtab sw=4
