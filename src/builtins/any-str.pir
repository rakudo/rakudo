## $Id$

=head1 NAME

src/builtins/any-str.pir -  C<Str>-like functions and methods for C<Any>

=head1 DESCRIPTION

This file implements the methods and functions of C<Any> that
are most closely associated with the C<Str> class or role.
We place them here instead of F<src/classes/Any.pir> to keep
the size of that file down and to emphasize their generic,
"built-in" nature.

=head2 Methods

=over 4

=cut

.namespace []
.sub 'onload' :anon :init :load
    $P0 = get_hll_namespace ['Any']
    '!EXPORT'('chars index rindex substr', 'from'=>$P0)
.end


=item chars()

=cut

.namespace ['Any']
.sub 'chars' :method :multi(_)
    $S0 = self
    $I0 = length $S0
    .return ($I0)
.end

=item comb()

Partial implementation for now, returns a list of strings
(instead of a list of match objects).

=cut

.sub 'comb' :method :multi(_)
    .param pmc regex
    .param int count        :optional
    .param int has_count    :opt_flag
    .local pmc retv, match
    .local string s

    retv = new 'List'
    s = self

  do_match:
    match = regex.'ACCEPTS'(s)
    unless match goto done
    unless has_count goto skip_count
    dec count
    if count < 0 goto done
  skip_count:
    # shouldn't have to coerce to Str here, but see RT #55962
    $S0 = match
    retv.'push'($S0)
    $I0 = match.'to'()
    s = substr s, $I0
    goto do_match

  done:
    .return(retv)
.end

=item index()

=cut

.namespace ['Any']
.sub 'index' :method :multi(_)
    .param string substring
    .param int pos             :optional
    .param int has_pos         :opt_flag
    .local pmc retv

    if has_pos goto have_pos
    pos = 0
  have_pos:

    .local string s
    s = self

  check_substring:
    if substring goto substring_search
    $I0 = length s
    if pos < $I0 goto done
    pos = $I0
    goto done

  substring_search:
    pos = index s, substring, pos
    if pos < 0 goto fail

  done:
    $P0 = new 'Int'
    $P0 = pos
    .return ($P0)

  fail:
    $P0 = new 'Failure'
    .return ($P0)
.end

=item match()

=cut

.sub 'match' :method :multi(_)
    .param pmc x
    .local pmc match
    match = x(self)
    .return(match)
.end

=item rindex()

=cut

.namespace ['Any']
.sub 'rindex' :method :multi(_, _)
    .param string substring
    .param int pos             :optional
    .param int has_pos         :opt_flag
    .local pmc retv

  check_substring:
    if substring goto substring_search

    # we do not have substring return pos or length

    .local string s
    s = self
    $I0 = length s

    if has_pos goto have_pos
    pos = $I0
    goto done
  have_pos:
    if pos < $I0 goto done
    pos = $I0
    goto done

  substring_search:
    $I0 = self.'isa'('String')
    if $I0 goto self_string
    $P0 = new 'String'
    $S0 = self
    $P0 = $S0
    goto do_search
  self_string:
    $P0 = self
  do_search:
    pos = $P0.'reverse_index'(substring, pos)
    if pos < 0 goto fail

  done:
    $P0 = new 'Int'
    $P0 = pos
    .return ($P0)

  fail:
    $P0 = new 'Failure'
    .return ($P0)
.end

=item split(/PATTERN/)

Splits something on a regular expresion

=cut

.sub 'split' :method :multi(_, 'Sub')
    .param pmc regex 
    .local pmc match
    .local pmc retv
    .local int start_pos
    .local int end_pos

    $S0 = self
    retv = new 'List'
    start_pos = 0

    match = regex($S0)
    if match goto loop
    retv.'push'($S0)
    goto done

  loop:
    match = regex($S0, 'continue' => start_pos)
    end_pos = match.'from'()
    end_pos -= start_pos
    $S1 = substr $S0, start_pos, end_pos
    retv.'push'($S1)
    unless match goto done
    start_pos = match.'to'()
    goto loop

  done:
    .return(retv)
.end

=item substr()

=cut

.namespace ['Any']
.sub 'substr' :method :multi(_, _)
    .param int start
    .param int len             :optional
    .param int has_len         :opt_flag

    if has_len goto have_len
    len = self.'chars'()
  have_len:
    if len >= 0 goto len_done
    $I0 = self.'chars'()
    len += $I0
    len -= start
  len_done:
    $S0 = self
    $S1 = substr $S0, start, len
    .return ($S1)
.end

=item trans()

  Implementation of transliteration

=cut

.sub '!transtable' :multi(_)
    .param pmc r
    .local pmc retval
    retval = new 'ResizableStringArray'
  range_loop:
    unless r, done
    $S0 = r.'shift'()
    push retval, $S0
    goto range_loop
  done:
    .return(retval)
.end

.sub '!transtable' :multi('Perl6Str')
    .param string str
    .local pmc retval, prior, frm, to
    .local int start, end, len, ind, skipped, r_start, r_end, s_len
    .local string p
    retval = new 'ResizableStringArray'
    prior = new 'ResizableStringArray'
    start = 0
    skipped = 0
    len = length str
    end = len - 2
  next_index:
    ind = index str, '..' , start
    if ind == -1 goto last_string
    # ranges can only be after first position, before last one
    if ind == 0 goto skip_pos
    if ind >= end goto last_string
  init_range:
    r_start = ind - 1
    r_end = ind + 2
  range_frm:
    $S0 = substr str, r_start, 1
    $I0 = ord $S0
    # following code may be commented dep. on how we interpret
    # spaces within ranges, like 'a .. b'
    unless $S0 == ' ' goto range_to
    r_start -= 1
    if r_start < 0 goto illegal_range
    goto range_frm
  range_to:
    $S1 = substr str, r_end, 1
    $I1 = ord $S1
    # following code may be commented dep. on how we interpret
    # spaces within ranges, like 'a .. b'
    unless $S1 == ' ' goto prev_string
    r_end += 1
    if r_end == len goto illegal_range
    goto range_to
  prev_string:
    s_len = r_start - start
    s_len += skipped
    unless s_len, start_range
    p = substr str, start, s_len
    prior = split '', p
  process_pstring:
    unless prior, start_range
    $S2 = shift prior
    push retval, $S2
    goto process_pstring
  start_range:
    if $I0 > $I1 goto illegal_range
  make_range:
    # Here we're assuming the ordinal increments correctly for all chars.
    # This is a bit naive for now, it definitely needs some unicode testing.
    # If needed we can switch this over to use a true string Range
    if $I0 > $I1 goto next_loop
    $S2 = chr $I0
    push retval, $S2
    inc $I0
    goto make_range
  illegal_range:
    die "Illegal range used in transliteration operator"
  next_loop:
    start = r_end + 1
    goto next_index
  skip_pos:
    inc start
    inc skipped
    goto next_index
  last_string:
    s_len = len - start
    if s_len <= 0 goto done
    p = substr str, start, s_len
    prior = split '', p
  process_lstring:
    unless prior, done
    $S0 = shift prior
    push retval, $S0
    goto process_lstring
  done:
    .return(retval)
.end

.sub 'trans' :method
    .param pmc args :slurpy
    .param int del :named('d') :optional
    .param int comp :named('c') :optional
    .param int squash :named('s') :optional
    .local pmc pair
    .local pmc table
    .local pmc itable
    .local pmc pkey
    .local pmc pval
    .local pmc retv
    .local string tmps, k, v, lv, lastmatch, comp_match
    .local int len, klen, vlen, adjpos, pos, ind, nhits, isa_pair
    table = new 'Hash'
    itable = new 'Hash'
    retv = new 'Perl6Str'
    tmps = self
    lv = ''

  pair_loop:
    unless args, st_trans
    pair = shift args
    isa_pair = isa pair, 'Perl6Pair'
    unless isa_pair, pair_exception
    pkey = pair.'key'()
    pval = pair.'value'()
    pkey = '!transtable'(pkey)
    pval = '!transtable'(pval)
    vlen = elements pval
    if vlen goto comp_check
    if del goto comp_check
    pval = clone pkey
  comp_check:
    # for :c, I am using first element for replacing for now.  I can't find
    # many examples where this is used otherwise
    comp_match = pval[0]

  mapping:
    unless pkey, pair_loop
    k = shift pkey
    unless pval, get_prev1
    lv = shift pval
  get_prev1:
    unless del, get_prev2
    v = ''
  get_prev2:
    v = lv
    nhits = 0
    pos = 0
  index_loop:
    ind = index tmps, k, pos
    if ind == -1 goto check_elems
    inc nhits
    $S0 = itable[ind]
    unless $S0, new_hit
    # keep longest hit at that index
    $I0 = length $S0
    $I1 = length k
    if $I1 < $I0 goto next_hit
  new_hit:
    itable[ind] = k
  next_hit:
    pos = ind + 1
    goto index_loop
  check_elems:
    unless nhits, mapping
    table[k] = v
    goto mapping

  st_trans:
    len = length tmps
    pos = 0
    adjpos = 0
    v = ''
    k = ''
    lastmatch = ''

  table_loop:
    if pos >= len goto done
    k = itable[pos]
    klen = length k
    if comp goto complement
  normal:
    unless k, skip_pos
    v = table[k]
    goto check_squash
  complement:
    # may need to change dep. on how we want :c to work
    if k, skip_pos_comp
    v = comp_match
    klen = 1
  check_squash:
    unless squash, replace
    unless v == lastmatch goto replace
    vlen = 0
    substr tmps, adjpos, klen, ''
    goto next_pos
  replace:
    vlen = length v
    substr tmps, adjpos, klen, v
  next_pos:
    pos      += klen
    adjpos   += vlen
    lastmatch = v
    goto table_loop
  skip_pos:
    pos      += 1
    adjpos   += 1
    lastmatch = ''
    goto table_loop
  skip_pos_comp:
    pos      += klen
    adjpos   += klen
    lastmatch = ''
    goto table_loop

  done:
    retv = tmps
    .return(retv)

  pair_exception:
    die "Must pass a List of Pairs for transliteration"
.end

=item subst

 our Str method Str::subst ( Any $string: Any $substring, Any $replacement )
 our Str method Str::subst ( Any $string: Code $regexp,   Any $replacement )

Replaces the first occurrence of a given substring or a regular expression
match with some other substring.

Partial implementation. The :g modifier on regexps doesn't work, for example.

=cut

.sub subst :method :multi(_, _, _)
    .param string substring
    .param string replacement
    .local int pos
    .local int pos_after
    .local pmc retv

    retv = new 'Perl6Str'

    $S0 = self
    pos = index $S0, substring
    if pos < 0 goto no_match

    pos_after = pos
    $I0 = length substring
    add pos_after, $I0

    $S1 = substr $S0, 0, pos
    $S2 = substr $S0, pos_after
    concat retv, $S1
    concat retv, replacement
    concat retv, $S2

    goto done

  no_match:
    retv = self

  done:
    .return(retv)
.end

.sub subst :method :lex :multi(_, 'Sub', _)
    .param pmc regexp
    .param pmc replacement
    .local int pos
    .local int pos_after
    .local pmc retv
    .local pmc match

    retv = new 'Perl6Str'

    new $P14, "Perl6Scalar"
    .lex "$/", $P14

    match = regexp.'ACCEPTS'(self)
    unless match goto no_match
    pos = match.'from'()
    pos_after = match.'to'()

    $S0 = self
    $S1 = substr $S0, 0, pos
    $S2 = substr $S0, pos_after
    # pre-match
    concat retv, $S1

    # match
    $I0 = isa replacement, 'Sub'
    unless $I0 goto is_string

    $S3 = match.'text'()
    $S3 = replacement($S3)
    concat retv, $S3
    goto repl_done

  is_string:
    concat retv, replacement

  repl_done:
    # post-match
    concat retv, $S2

    goto done

  no_match:
    retv = self

  done:
    .return(retv)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
