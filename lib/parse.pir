=head1 TITLE

parse.pir - Parsing support subroutines

=head2 DESCRIPTION

This file contains numerous support subroutines for parsing
Perl 6 programs.  The most important is actually the "__onload"
sub, which creates an operator precedence parser for Perl 6
expressions.  Other specialized parsing subroutines will
appear here as the parser grows.

=over 4

=item C<opparse(PMC mob)>

The C<opparse> subroutine implements the Perl6::Grammar
<opparse> subrule.  It accepts a match object representing
the current state of the parse, passes the match object
to the operator precedence parser to obtain an expression,
and returns the result to the caller.

=cut

.namespace [ "Perl6::Grammar" ]

.include "cclass.pasm"

.sub "opparse" 
    .param pmc mob
    .local pmc optable
    .local pmc ws

    optable = find_global 'Perl6::Grammar', "$optable"
    ws = find_global 'Perl6::Grammar', 'ws'
    setattribute optable, "PGE::OPTable\x0&!ws", ws
    $P0 = optable."parse"(mob)
    .return ($P0)
.end

=item C<die(PMC mob, string message)>

The C<die> subroutine implements a special-purpose
<die> subrule.  The <die> subrule outputs a C<message>
indicating why the parse failed, displays the line
number and context of the failure, and returns a
PGE_CUT_MATCH result to abandon the parse altogether.

=cut

.sub "die"
    .param pmc mob
    .param string message      # XXX: TODO: :optional
    .local pmc newfrom
    .local string target
    .local pmc mfrom, mpos
    .local int pos, len, newline, lines
    newfrom = find_global "PGE::Match", "newfrom"
    (mob, target, mfrom, mpos) = newfrom(mob, 0)

    pos = mfrom
    newline = 0
    lines = 1
  newline_loop:
    $I0 = find_cclass .CCLASS_NEWLINE, target, newline, pos
    if $I0 >= pos goto print_message
    newline = $I0 + 1
    inc lines
    goto newline_loop

  print_message:
    print message
    print " at line "
    print lines
    print ", near \""
    $I0 = length target
    $I0 -= pos
    if $I0 < 10 goto print_message_1
    $I0 = 10
  print_message_1:
    $S0 = substr target, pos, $I0
    $S0 = escape $S0
    print $S0
    print "\"\n"
    mpos = -3
    .return (mob)
.end
   
=pod

=back

=cut
