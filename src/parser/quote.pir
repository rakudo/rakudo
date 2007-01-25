## $Id$

=head1 TITLE

quote.pir - <quote_expression> subrule

=head2 DESCRIPTION

This file contains the grammar subrules for <quote_expression>,
which handles Perl 6's various quoting constructs.  Trying to
use a regular expression for parsing the various quote styles
and interpolations is a bit of a pain, so we write a
special-purpose parsing subroutine here.

=over 4

=item __onload()

Initialize the tables needed for quote parsing.

=cut

.namespace [ 'Perl6::Grammar' ]

.include "cclass.pasm"

.sub '__onload' :load :init
    .local pmc quotetable, keyadv

    ##  create the quotetable
    quotetable = new .Hash
    set_global '%!quotetable', quotetable

    ##  adverbs for single quotes
    keyadv = new .Hash
    keyadv['q'] = 1
    keyadv['closedelim'] = "'"
    quotetable["'"] = keyadv

    ## adverbs for double quotes
    keyadv = clone keyadv
    keyadv['qq'] = 1
    keyadv['s'] = 1
    keyadv['a'] = 1
    keyadv['h'] = 1
    keyadv['f'] = 1
    keyadv['c'] = 1
    keyadv['b'] = 1
    keyadv['closedelim'] = '"'
    quotetable['"'] = keyadv

    ## adverbs for qww
    keyadv = clone keyadv
    keyadv['qq'] = 0
    keyadv['ww'] = 1
    keyadv['closedelim'] = '>>'
    quotetable['<<'] = keyadv

    ## adverbs for qw
    keyadv = new .Hash
    keyadv['q'] = 1
    keyadv['w'] = 1
    keyadv['closedelim'] = '>'
    quotetable['<'] = keyadv
.end


=item quote_expression(match [, adverbs :slurpy :named])

Parse a quoted expression of some sort.  The $<KEY>
attribute of the incoming C<match> object identifies
the type of quoting to parse.  This is simply a lookup
into C<%!quotetable>, which contains the default adverb
settings for the type of quote.  Other adverbs are then
merged into the default, and quote parsing begins.

=cut

.sub 'quote_expression'
    .param pmc mob
    .param pmc adverbs         :slurpy :named

    .local string key
    key = mob['KEY']
    .local pmc quotetable, keyadverbs
    quotetable = get_global '%!quotetable'
    keyadverbs = quotetable[key]
    .local string closedelim
    closedelim = keyadverbs['closedelim']

    .local int advw, advww
    advw = keyadverbs['w']
    advww = keyadverbs['ww']
    $I0 = or advw, advww
    if $I0 goto quote_words
    .return mob.'quote_string'(closedelim, keyadverbs :flat :named)

  quote_words:
    .local string target
    .local int pos, delimlen, lastpos, ccount
    (mob, pos, target) = mob.'new'(mob)
    delimlen = length closedelim
    lastpos = length target
    lastpos -= delimlen
    ccount = 0

    pos = find_not_cclass .CCLASS_WHITESPACE, target, pos, lastpos
    if pos > lastpos goto fail
  quote_words_loop:
    if advww == 0 goto quote_words_unprotected
    .local string protectquote
    protectquote = substr target, pos, 1
    if protectquote == '"' goto quote_words_protected
    if protectquote != "'" goto quote_words_unprotected
  quote_words_protected:
    inc pos
    mob.'to'(pos)
    .local pmc cnode
    $P0 = quotetable[protectquote]
    cnode = mob.'quote_string'(protectquote, $P0 :flat :named)
    unless cnode goto fail
    pos = cnode.'to'()
    $S1 = substr target, pos, 1
    if $S1 != protectquote goto fail
    inc pos
    goto have_cnode
  quote_words_unprotected:
    mob.'to'(pos)
    .local pmc cnode
    cnode = mob.'quote_string'(closedelim, keyadverbs :flat :named)
    unless cnode goto fail
    pos = cnode.'to'()
  have_cnode:
    mob[ccount] = cnode
    inc ccount
    if pos > lastpos goto fail
    pos = find_not_cclass .CCLASS_WHITESPACE, target, pos, lastpos
    $S0 = substr target, pos, delimlen
    if $S0 == closedelim goto quote_words_end
    if pos >= lastpos goto fail
    goto quote_words_loop
  quote_words_end:
    if ccount > 1 goto return_list
    mob = mob[0]
    mob.'to'(pos)
    .return (mob)
  return_list:
    mob.'to'(pos)
    mob['type'] = 'quote_list'
    .return (mob)

  fail:
    mob.'to'(-1)
    .return (mob)
.end


.sub 'quote_string'
    .param pmc mob
    .param string closedelim
    .param pmc adverbs         :slurpy :named

    .local int advq, advqq, advw, advww, wsdelim
    advq = adverbs['q']
    advqq = adverbs['qq']
    advw = adverbs['w']
    advww = adverbs['ww']
    wsdelim = or advw, advww

    .local string interpolatechars
    interpolatechars = ''
    $I0 = adverbs['s']
    unless $I0 goto interp_array
    interpolatechars .= '$'
  interp_array:
    $I0 = adverbs['a']
    unless $I0 goto interp_hash
    interpolatechars .= '@'
  interp_hash:
    $I0 = adverbs['h']
    unless $I0 goto interp_sub
    interpolatechars .= '%'
  interp_sub:
    $I0 = adverbs['f']
    unless $I0 goto interp_closure
    interpolatechars .= '&'
  interp_closure:
    $I0 = adverbs['c']
    unless $I0 goto have_interpolatechars
    interpolatechars .= '{'
  have_interpolatechars:

    .local string target
    .local int pos, delimlen, lastpos, ccount
    (mob, pos, target) = mob.'new'(mob)
    delimlen = length closedelim
    lastpos = length target
    lastpos -= delimlen
    ccount = 0

  scan_new_literal:
    .local string literal, litchar
    .local int litfrom
    literal = ''
    litfrom = pos

  scan_loop:
    ##  see if we've reached a closing delimiter
    if pos > lastpos goto fail
    $S0 = substr target, pos, delimlen
    if $S0 == closedelim goto scan_end
    if pos >= lastpos goto fail
    if wsdelim == 0 goto scan_litchar
    $I0 = is_cclass .CCLASS_WHITESPACE, target, pos
    if $I0 goto scan_end

  scan_litchar:
    ##  get candidate character to add
    litchar = substr target, pos, 1

  scan_single:
    ##  handle backslash escapes (:q and :qq)
    if advq == 0 goto scan_interpolation
    if litchar != "\\" goto scan_interpolation
    $I0 = pos + 1
    $S1 = substr target, $I0, 1
    if $S1 != "\\" goto scan_delim
    pos += 2
    goto add_litchar
  scan_delim:
    $S2 = substr target, $I0, delimlen
    if $S2 != closedelim goto scan_double
    litchar = closedelim
    pos = $I0 + delimlen
    goto add_litchar
  scan_double:
    if advqq == 0 goto scan_interpolation
    litchar = $S1
    pos += 2
    ##  check for backslash metachars -- if the character following
    ##  the backslash isn't in the list below, we treat it as a literal.
    ##  otherwise, the offset is used to determine if the backslash
    ##  escape is an error, a x/d/o escape, or to obtain the equivalen char.
    $I0 = index "abefnrt0xdo123456789", litchar
    if $I0 < 0 goto add_litchar
    if $I0 >= 11 goto fail_backslash_num
    if $I0 >= 7 goto scan_xdo
    litchar = substr "\a\b\e\f\n\r\t\0", $I0, 1
    if $I0 < 7 goto add_litchar
    ##  check that \0 isn't followed by an octal digit
    $S0 = substr target, pos, 1
    $I0 = index '01234567', $S0
    if $I0 >= 0 goto fail_backslash_num
    goto add_litchar
  scan_xdo:
    ##  handle \x, \d, and \o escapes.  start by converting the
    ##  'o', 'd', or 'x' into 8, 10, or 16 (yes, it's a hack but
    ##  it works).  Then loop through the characters that follow
    ##  to compute the decimal value of the codepoint, and add
    ##  that codepoint to our literal.
    .local int base, decnum, isbracketed
    base = index '        o d     x', litchar
    decnum = 0
    $S0 = substr target, pos, 1
    isbracketed = iseq $S0, '['
    pos += isbracketed
  scan_xdo_char_loop:
    $S0 = substr target, pos, 1
    $I0 = index '0123456789abcdef', $S0
    if $I0 < 0 goto scan_xdo_char_end
    if $I0 >= base goto scan_xdo_char_end
    decnum *= base
    decnum += $I0
    inc pos
    goto scan_xdo_char_loop
  scan_xdo_char_end:
    $S1 = chr decnum
    concat literal, $S1
    unless isbracketed goto scan_xdo_end
    if $S0 == ']' goto scan_xdo_end
    if $S0 != ',' goto fail
    inc pos
    decnum = 0
    goto scan_xdo_char_loop
  scan_xdo_end:
    pos += isbracketed
    goto scan_loop

  scan_interpolation:
    ##  if it's an interpolative char, try that
    $I0 = index interpolatechars, litchar
    if $I0 >= 0 goto interpolation
    inc pos

  add_litchar:
    ##  litchar is part of a literal, so add it and keep scanning
    concat literal, litchar
    goto scan_loop

  interpolation:
    ##  we may be interpolating here, so try that.  if we don't
    ##  get back a valid interpolation, just treat it as
    ##  more of our literal and continue scanning
    .local pmc inode
    mob.'to'(pos)
    inode = mob.'quote_interpolation'()
    if inode goto interpolation_1
    inc pos
    goto add_litchar
  interpolation_1:
    ##  we have a valid interpolation, so close any existing literal
    ##  and add our interpolation
    if literal == '' goto interpolation_2
    $P0 = mob.'new'(mob)
    $P0.'from'(litfrom)
    $P0.'to'(pos)
    $P0.'result_object'(literal)
    $P0['type'] = 'quote_literal'
    mob[ccount] = $P0
    inc ccount
  interpolation_2:
    inode['type'] = 'quote_interpolation'
    mob[ccount] = inode
    inc ccount
    pos = inode.'to'()
    goto scan_new_literal
  scan_end:
    ## we're done scanning, set the end of this match object
    mob.'to'(pos)

    ## if count is zero, then we just return a single literal
    ## if there's a literal, we add it and return a concatenated result
    ## if we have multiple items, return the concatenated result
    ## else return the single item we already have
    if ccount == 0 goto return_literal
    if literal > '' goto add_literal
    if ccount > 1 goto return_concat
    mob = mob[0]
    .return (mob)
  return_literal:
    mob.'result_object'(literal)
    mob['type'] = 'quote_literal'
    .return (mob)
  add_literal:
    $P0 = mob.'new'(mob)
    $P0.'from'(litfrom)
    $P0.'to'(pos)
    $P0.'result_object'(literal)
    $P0['type'] = 'quote_literal'
    mob[ccount] = $P0
    inc ccount
  return_concat:
    mob['type'] = 'quote_concat'
    .return (mob)
  fail:
    mob.'to'(-1)
    .return (mob)
  fail_backslash_num:
    pos -= 2
    mob.'to'(pos)
    ##  FIXME: use 'syntax_error' rule when it accepts arguments
    .local pmc die
    die = get_hll_global ['PGE::Util'], 'die'
    die(mob, '\123 form deprecated, use \o123 instead')
    goto fail
.end

=back

=cut
