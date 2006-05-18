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
    ws = find_global 'Perl6::Grammar', 'ws'
    setattribute optable, "PGE::OPTable\x0&!ws", ws
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
    setattribute optable, "PGE::OPTable\x0&!ws", ws
    .return optable.'parse'(mob, 'tighter'=>'infix:<==')
.end


=item C<quoted_literal>

Handles parsing of the various types of quoted literals.

=cut

.sub 'quoted_literal'
    .param pmc mob                                 # object to parse
    .param string delim                            # string delimiter (XXX)
    .param pmc adv             :slurpy :named      # adverbs

    ##   XXX: This is a temporary hack to set adverbs based
    ##   on the delimiter.  We'll remove this when we have full
    ##   qq[...] adverb capability.
    if delim == "'" goto q_string
    adv['double'] = 1
  q_string:
    adv['single'] = 1

    .local int adv_single, adv_backslash, adv_scalar
    adv_single = 1
    adv_backslash = 0
    adv_scalar = 0
    $I0 = exists adv['double']
    if $I0 == 0 goto with_double
    adv_backslash = 1
    adv_scalar = 1
  with_double:
    $I0 = exists adv['backslash']
    if $I0 == 0 goto with_backslash
    adv_backslash = adv['backslash']
  with_backslash:
    $I0 = exists adv['scalar']
    if $I0 == 0 goto with_scalar
    adv_scalar = adv['scalar']
  with_scalar:

    .local string target
    .local pmc mfrom, mpos
    .local int pos
    (mob, pos, target, mfrom, mpos) = mob.'new'(mob)

    .local int capt, lastpos, delimlen
    capt = 0
    lastpos = length target
    delimlen = length delim

    .local string lstop
    lstop = ''
    if adv_scalar == 0 goto lstop_1
    lstop .= '$'
  lstop_1:

  outer_loop:
    .local string literal
    .local int litfrom
    literal = ''
    litfrom = pos
    if pos >= lastpos goto fail
    $S0 = substr target, pos, delimlen
    if $S0 == delim goto outer_end
    if $S0 == '$' goto scan_scalar
    goto scan_literal

  scan_scalar:
    literal = $S0
    mpos = pos
    inc pos
    if adv_scalar == 0 goto scan_literal
    $P0 = find_global 'Perl6::Grammar', 'variable'
    $P1 = $P0(mob)
    unless $P1 goto scan_literal
    $P1['type'] = 'Perl6::Grammar::variable'
    mob[capt] = $P1
    inc capt
    pos = $P1.to()
    goto outer_loop

  scan_literal:
  scan_literal_loop:
    if pos >= lastpos goto fail
    $S0 = substr target, pos, delimlen
    if $S0 == delim goto scan_literal_end
    $S0 = substr target, pos, 1
    $I0 = index lstop, $S0
    if $I0 >= 0 goto scan_literal_end
    if adv_single == 0 goto scan_literal_1
    if $S0 != "\\" goto scan_literal_1
    if adv_backslash goto scan_literal_backslash
    $I0 = pos + 1
    $S1 = substr target, $I0, 1
    if $S1 == "\\" goto scan_literal_backslash
    if $S1 != delim goto scan_literal_1            # XXX: single-char delim
  scan_literal_backslash:
    inc pos
    $S0 = substr target, pos, 1
    # if $S0 == 'x' goto scan_backslash_x          # XXX: to-do
    # if $S0 == 'd' goto scan_backslash_d          # XXX: to-do
    # if $S0 == 'o' goto scan_backslash_o          # XXX: to-do
    $I0 = index "abefnrt", $S0
    if $I0 < 0 goto scan_literal_1
    $S0 = substr "\x07\x08\e\f\n\r\t", $I0, 1
  scan_literal_1:
    concat literal, $S0
    inc pos
    goto scan_literal_loop
  scan_literal_end:
    ($P0, $P1, $P2, $P3, $P4) = mob.'new'(mob)
    $P3 = litfrom
    $P4 = pos
    $P0.'value'(literal)
    $P0['type'] = 'str'
    mob[capt] = $P0
    inc capt
    goto outer_loop

  outer_end:
    mpos = pos
    .return (mob)
  fail:
    mpos = -1
    .return (mob)
.end


=item C<slash_regex(PMC mob)>

Handles parsing of "slash regexes" -- i.e., regexes that are 
terminated by a slash.  For this, we just call PGE's p6 regex 
parser, telling it to stop parsing on the closing slash.  

XXX: This is just a temporary sub to get things
working -- it will likely change.

=cut

.sub 'regex'
    .param pmc mob
    .param pmc args            :slurpy
    .param pmc adverbs         :slurpy :named
    .local string stop

    stop = ''
    if null adverbs goto with_stop_adverb
    stop = adverbs['stop']
    if stop > '' goto with_stop
  with_stop_adverb:
    unless args goto with_stop
    stop = shift args
  with_stop: 
    .include 'interpinfo.pasm'
    $P0 = interpinfo .INTERPINFO_NAMESPACE_ROOT
    $P0 = $P0['parrot';'PGE::Grammar';'regex']
    $P1 = $P0(mob, 'stop'=>stop)
    .return ($P1)
.end
