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

.include 'cclass.pasm'

.namespace []
.sub 'onload' :anon :init :load
    $P0 = get_hll_namespace ['Any']
    '!EXPORT'('capitalize,chop,chomp,chars,:d,:e,:f,index,lc,lcfirst,rindex,ord,substr,trim,uc,ucfirst,unpack', 'from'=>$P0)
.end


=item chars()

=cut

.namespace ['Any']

=item capitalize

 our Str multi Str::capitalize ( Str $string )

Has the effect of first doing an C<lc> on the entire string, then performing a
C<s:g/(\w+)/{ucfirst $1}/> on it.

=cut

.sub 'capitalize' :method :multi(_)
    .local string tmps
    .local pmc retv
    .local int len

    retv = new 'Str'
    tmps = self

    len = length tmps
    if len == 0 goto done

    downcase tmps

    .local int pos
    .local string s1
    pos = 0
  next_word:
    pos = find_cclass .CCLASS_LOWERCASE, tmps, pos, len
    s1 = substr tmps, pos, 1
    upcase s1
    substr tmps, pos, 1, s1
    len = length tmps
    pos+=1
    if pos == len goto done
    pos = find_not_cclass .CCLASS_LOWERCASE, tmps, pos, len
    if pos == len goto done
    goto next_word

  done:
    retv = tmps
    .return (retv)
.end

.sub 'chars' :method :multi(_)
    $S0 = self
    $I0 = length $S0
    .return ($I0)
.end


=item chop

 our Str method Str::chop ( Str  $string: )

Returns string with one Char removed from the end.

=cut

.sub 'chop' :method :multi(_)
    .local string tmps
    .local pmc retv

    tmps = self
    chopn tmps, 1
    retv = new 'Str'
    retv = tmps
    .return(retv)
.end

=item chomp

 our Str method Str::chomp ( Str $string: )

 Returns string with newline removed from the end.  An arbitrary
 terminator can be removed if the input filehandle has marked the
 string for where the "newline" begins.  (Presumably this is stored
 as a property of the string.)  Otherwise a standard newline is removed.

=cut

.sub 'chomp' :method :multi(_)
    .local string tmps
    .local string lastchar
    .local pmc retv

    tmps = self
    lastchar = substr tmps,-1
    if lastchar != "\n" goto done
    chopn tmps, 1
    lastchar = substr tmps,-1
    if lastchar != "\r" goto done
    chopn tmps, 1
  done:
       retv = new 'Str'
       retv = tmps
       .return (retv)
.end

=item trim()

Remove leading and trailing whitespace from a string.

=cut

.sub 'trim' :method :multi(_)
    .local string s
    .local int start, end, temp, len
    .local int is_whitespace
    s = self
    start = 0
    end = length s
    if end == 0 goto donetrailing
  trimleading:
    is_whitespace = is_cclass .CCLASS_WHITESPACE, s, start
    unless is_whitespace goto doneleading
    inc start
    goto trimleading
  doneleading:
    temp = end
  trimtrailing:
    dec temp
    is_whitespace = is_cclass .CCLASS_WHITESPACE, s, temp
    unless is_whitespace goto donetrailing
    end = temp
    goto trimtrailing
  donetrailing:
    len = end - start
    s = substr s, start, len
    .return(s)
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
    .local int pos

    retv = 'list'()
    s = self

    pos = 0

  do_match:
    match = regex(s, 'continue' => pos)
    unless match goto done
    unless has_count goto skip_count
    dec count
    if count < 0 goto done
  skip_count:
    # shouldn't have to coerce to Str here, but see RT #55962
    $S0 = match
    retv.'push'($S0)
    $I0 = match.'to'()
    if pos == $I0 goto zero_width
    pos = $I0
    goto do_match

  zero_width:
    inc pos
    goto do_match
  done:
    .return(retv)
.end

=item ':d'()

 our Bool multi Str::':d' ( Str $filename )

Returns whether the file with the name indicated by the invocant is a
directory.

=cut

.sub ':d' :method :multi(_)
    .param int arg              :optional
    .param int has_arg          :opt_flag

    .local string filename
    filename = self

    push_eh not_a_dir
    $I0 = stat filename, 2
    if $I0 goto file_is_a_dir
  not_a_dir:
    $P0 = get_hll_global ['Bool'], 'False'
    .return ($P0)
  file_is_a_dir:
    $P0 = get_hll_global ['Bool'], 'True'
    .return ($P0)
.end

=item ':e'()

 our Bool multi Str::':e' ( Str $filename )

Returns whether the file with the name indicated by the invocant exists.

=cut

.sub ':e' :method :multi(_)
    .param int arg              :optional
    .param int has_arg          :opt_flag

    .local string filename
    filename = self

    $I0 = stat filename, 0
    if $I0 goto file_exists
    $P0 = get_hll_global ['Bool'], 'False'
    .return ($P0)
  file_exists:
    $P0 = get_hll_global ['Bool'], 'True'
    .return ($P0)
.end

=item ':f'()

 our Bool multi Str::':f' ( Str $filename )

Returns whether the file with the name indicated by the invocant is a plain
file.

=cut

.sub ':f' :method :multi(_)
    .param int arg              :optional
    .param int has_arg          :opt_flag

    .local string filename
    filename = self

    push_eh file_isnt_plain
    $I0 = stat filename, 2
    if $I0 goto file_isnt_plain
    $P0 = get_hll_global ['Bool'], 'True'
    .return ($P0)
  file_isnt_plain:
    $P0 = get_hll_global ['Bool'], 'False'
    .return ($P0)
.end

=item fmt

 our Str multi Any::fmt ( Str $format )

Returns the invocant formatted by an implicit call to C<sprintf>.

=cut

.sub 'fmt' :method :multi(_)
    .param string format
    .local pmc retv

    retv = 'sprintf'(format, self)

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


=item lc

 our Str multi Str::lc ( Str $string )

Returns the input string after converting each character to its lowercase
form, if uppercase.

=cut

.sub 'lc' :method :multi(_)
    .local string tmps
    .local pmc retv

    tmps = self
    downcase tmps

    retv = new 'Str'
    retv = tmps

    .return(retv)
.end

=item lcfirst

 our Str multi Str::lcfirst ( Str $string )

Like C<lc>, but only affects the first character.

=cut

.sub 'lcfirst' :method :multi(_)
    .local string tmps
    .local string fchr
    .local pmc retv
    .local int len

    retv = new 'Str'
    tmps = self

    len = length tmps
    if len == 0 goto done

    substr fchr, tmps, 0, 1
    downcase fchr

    concat retv, fchr
    substr tmps, tmps, 1
    concat retv, tmps

  done:
    .return(retv)
.end



=item match()

=cut

.sub 'match' :method :multi(_)
    .param pmc x
    .local pmc match
    match = x.'!invoke'(self)
    .return(match)
.end

=item reverse

=cut

.namespace []
.sub 'reverse' :multi()
    .param pmc values          :slurpy
    $I0 = elements values
    unless $I0 == 1 goto reverse_list
    $P0 = values[0]
    .tailcall $P0.'reverse'()
  reverse_list:
    values.'!flatten'()
    .tailcall values.'reverse'()
.end

.namespace ['Any']
.sub 'reverse' :method
    $P0 = self.'split'('')
    $P0 = $P0.'reverse'()
    .tailcall $P0.'join'('')
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

=item split

 our List multi Str::split ( Str $delimiter ,  Str $input = $+_, Int $limit = inf )
 our List multi Str::split ( Rule $delimiter = /\s+/,  Str $input = $+_, Int $limit = inf )
 our List multi Str::split ( Str $input :  Str $delimiter          , Int $limit = inf )
 our List multi Str::split ( Str $input : Rule $delimiter          , Int $limit = inf )

String delimiters must not be treated as rules but as constants.  The
default is no longer S<' '> since that would be interpreted as a constant.
P5's C<< split('S< >') >> will translate to C<.words> or some such.  Null trailing fields
are no longer trimmed by default.  We might add some kind of :trim flag or
introduce a trimlist function of some sort.

B<Note:> partial implementation only

=cut

.namespace[]
.sub 'split' :multi(_,_)
    .param pmc sep
    .param pmc target
    .tailcall target.'split'(sep)
.end

.namespace['Any']
.sub 'split' :method :multi(_, _)
    .param string delim
    .param int count        :optional
    .param int has_count    :opt_flag
    .local string objst
    .local pmc pieces
    .local pmc retv
    .local int len
    .local int pos
    .local int i

    retv = new 'List'

    # per Perl 5's negative LIMIT behavior
    unless has_count goto positive_count
    unless count < 1 goto positive_count
    has_count = 0

  positive_count:
    objst = self
    length $I0, delim
    split pieces, delim, objst
    len = pieces
    pos = 0
    i = 0

  loop:
    unless has_count goto skip_count
    dec count
    unless count < 1 goto skip_count
    $S0 = substr objst, pos
    retv.'push'($S0)
    goto done
  skip_count:
    if i == len goto done
    $S0 = pieces[i]
    retv.'push'($S0)
    length $I1, $S0
    pos += $I0
    pos += $I1
    inc i
    goto loop

  done:
    .return(retv)
.end

.sub 'split' :method :multi(_, 'Sub')
    .param pmc regex
    .param int count        :optional
    .param int has_count    :opt_flag
    .local pmc match
    .local pmc retv
    .local int start_pos
    .local int end_pos
    .local int zwm_start

    $S0 = self
    retv = new 'List'
    start_pos = 0

    # per Perl 5's negative LIMIT behavior
    unless has_count goto positive_count
    if count < 1 goto done

  positive_count:
    match = regex($S0)
    if match goto loop
    retv.'push'($S0)
    goto done

  loop:
    unless has_count goto skip_count
    dec count
    unless count < 1 goto skip_count
    $S1 = substr $S0, start_pos
    retv.'push'($S1)
    goto done
  next_zwm:
    zwm_start = start_pos
  inc_zwm:
    inc start_pos
    match = regex($S0, 'continue' => start_pos)
    end_pos = match.'from'()
    unless start_pos == end_pos goto inc_zwm
    start_pos = zwm_start
    end_pos -= start_pos
    goto add_str
  skip_count:
    match = regex($S0, 'continue' => start_pos)
    end_pos = match.'from'()
    $I99 = match.'to'()
    if $I99 == end_pos goto next_zwm
    end_pos -= start_pos
  add_str:
    $S1 = substr $S0, start_pos, end_pos
    retv.'push'($S1)
    unless match goto done
    $I0 = match.'to'()
    if $I0 == start_pos goto zero_width
    start_pos = $I0
    goto loop
  zero_width:
    inc start_pos
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
    if start < 0 goto neg_start
    $I0 = self.'chars'()
    len += $I0
  neg_start:
    len -= start
  len_done:
    $S0 = self
    push_eh fail
    $S1 = substr $S0, start, len
    pop_eh
    .return ($S1)
  fail:
    .get_results($P0)
    pop_eh
    .tailcall '!FAIL'($P0)
.end

=item trans()

  Implementation of transliteration

=cut

.sub '!transtable' :multi(_)
    .param pmc r
    .local pmc retval, tmps
    retval = new 'ResizablePMCArray'
    tmps = clone r
  range_loop:
    unless tmps, done
    $P0 = tmps.'shift'()
    push retval, $P0
    goto range_loop
  done:
    .return(retval)
.end

# Handles Regexes and Closures

.sub '!transtable' :multi('Sub')
    .param pmc r
    .local pmc retval
    retval = new 'ResizablePMCArray'
    push retval, r
    .return(retval)
.end

.sub '!transtable' :multi('String')
    .param string str
    .local pmc retval, prior, frm, to, next_str
    .local int start, end, len, ind, skipped, r_start, r_end, s_len
    .local string p
    retval = new 'ResizablePMCArray'
    prior = new 'ResizablePMCArray'
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
  range_to:
    $S1 = substr str, r_end, 1
    $I1 = ord $S1
  prev_string:
    s_len = r_start - start
    s_len += skipped
    unless s_len, start_range
    p = substr str, start, s_len
    prior = split '', p
  process_pstring:
    unless prior, start_range
    $S2 = shift prior
    next_str = new 'Str'
    next_str = $S2
    push retval, next_str
    goto process_pstring
  start_range:
    if $I0 > $I1 goto illegal_range
  make_range:
    # Here we're assuming the ordinal increments correctly for all chars.
    # This is a bit naive for now, it definitely needs some unicode testing.
    # If needed we can switch this over to use a true string Range
    if $I0 > $I1 goto next_loop
    $S2 = chr $I0
    next_str = new 'Str'
    next_str = $S2
    push retval, next_str
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
    if s_len <= 0 goto check_rval
    p = substr str, start, s_len
    prior = split '', p
  process_lstring:
    unless prior, check_rval
    $S0 = shift prior
    next_str = new 'Str'
    next_str = $S0
    push retval, next_str
    goto process_lstring
  check_rval:
    $I0 = elements retval
    if $I0 > 0 goto done
    push retval, ''
  done:
    .return(retval)
.end


.sub 'trans' :method
    .param pmc args :slurpy
    .param pmc adverbs         :slurpy :named
    .local int del, comp, squash
    $I0 = exists adverbs['d']
    $I1 = exists adverbs['delete']
    del = $I0 || $I1
    $I0 = exists adverbs['c']
    $I1 = exists adverbs['complement']
    comp = $I0 || $I1
    $I0 = exists adverbs['s']
    $I1 = exists adverbs['squash']
    squash = $I0 || $I1
    # TODO: unspec'd behavior: above arguments are similar
    # to p5 tr/// but are not described in S05, need some
    # clarification on whether these are implemented correctly
    .local pmc table, itable, retv, comp_match, by
    .local int len, klen, vlen, adjpos, pos, ind, nhits
    by = get_hll_global 'infix:<=>'
    # itable maps matching positions to key, value array
    itable = new 'Perl6Hash'
    retv = new 'Str'

  init_pair_loop:
    .local pmc pair, pkey, pval, pairlist
    .local int isatype
  pair_loop:
    unless args, init_trans
    pair = shift args
    # following is a cludge to get around list context issues
    # should be removed once that works
    isatype = isa pair, 'Perl6Pair'
    if isatype goto isa_pair
    isatype = isa pair, 'Hash'
    if isatype goto isa_hash
    isatype = isa pair, 'List'
    if isatype goto isa_list
    # change to Failure?
    die "Must pass a List of Pairs for transliteration"
  isa_hash:
    pairlist = pair.'pairs'()
    goto pairlist_loop
  isa_list:
    pairlist = clone pair
  pairlist_loop:
    unless pairlist, pair_loop
    pair = shift pairlist
    push args, pair
    goto pairlist_loop
  isa_pair:
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
    # any reliable examples where this is used otherwise
    comp_match = pval[0]

  init_mapping:
    .local pmc key, val, lastval, prev_val, prev_key
    .local string tmps
    .local int prev_pos, k_isa_regex
    tmps = self
  mapping:
    .local pmc match, km
    unless pkey, pair_loop
    key = shift pkey
    unless pval, get_prev1
    lastval = shift pval
  get_prev1:
    if del, get_prev2
    val = lastval
    goto init_index_loop
  get_prev2:
    val = new 'Str'
    val = ''
  init_index_loop:
    nhits = 0
    pos = 0
    prev_pos = 0
    # assume key is always a Str for now (will need to adjust for Regex)
    k_isa_regex = isa key, 'Sub' # should be Regex
    unless k_isa_regex, index_loop

  regex_loop:
    match = key(tmps, 'continue' => pos)
    unless match goto mapping
    ind = match.'from'()
    km = match
    inc nhits
    goto check_hit
  index_loop:
    km = key
    # change over to index method
    $S0 = key
    ind = index tmps, $S0, pos
    if ind == -1 goto mapping
    inc nhits
  check_hit:
    klen = km.'chars'()     # should work for Match, Str
    $I0 = exists itable[ind]
    unless $I0, new_hit
    prev_key = itable[ind;0]
    # keep longest hit at that index
    $I1 = prev_key.'chars'()
    if klen < $I1 goto next_hit
  new_hit:
    $P1 = new 'ResizablePMCArray'
    push $P1, km
    push $P1, val
    itable[ind] = $P1
  next_hit:
    prev_pos = ind
    pos = ind + klen
    prev_val = val
    unless k_isa_regex goto index_loop
    # Do we just grab the next match (which may backtrack), or only grab longest
    # match? This will affect closures ...
    goto regex_loop

  init_trans:
    .local pmc hit_set, inv_set, inv_table, it
    .local int kvdiff, llm, pr_pos, st, end
    .local string vs
    hit_set = new 'ResizableIntegerArray'
  normal_hits:
    hit_set = itable.'keys'()
    hit_set = hit_set.'sort'(by)
    unless comp, st_trans
  comp_hits:
    # if :c is indicated, rebuild complement set and use that instead
    # of original itable
    inv_table = new 'Perl6Hash'
    st = 0
    end = 0
    len = length tmps
    inv_set = new 'ResizableIntegerArray'
    it = hit_set.'iterator'()
  comp_loop1:
    unless it, fence_post
    end = shift it
    key = itable[end;0]
    klen = key.'chars'()
  comp_loop2:
    if st == len goto finish_comp
    if st == end goto comp_loop3
    # TODO: unspec'd behavior
    # depending on how we want to implement complement, we could
    # modify the following to replace the entire unmatched range once
    # or each char (latter implemented for now to match tests)
    push inv_set, st
    $P1 = new 'ResizablePMCArray'
    push $P1, 'x' # placeholder char; we can replace with substr if needed
    push $P1, comp_match
    inv_table[st] = $P1
    inc st
    goto comp_loop2
  comp_loop3:
    st += klen
    goto comp_loop1
  fence_post:
    end = len
    goto comp_loop2
  finish_comp:
    hit_set = inv_set
    itable = inv_table

  st_trans:
    .local int k_isa_match, v_isa_closure, pass_match
    .local pmc lastmatch, v
    lastmatch = new 'Str'
    lastmatch = ''
    pos = 0 # original unadjusted position
    pr_pos = 0 # prior unadjusted position
    adjpos = 0 # adjusted position
    kvdiff = 0 # key-value string length diff
    klen = 0 # key len
    vlen = 0 # val len
    llm = 0 # orig end marker for longest leftmost match
    tmps = self # reassig; workaround for [perl #59730]

  table_loop:
    unless hit_set, done
    pos = shift hit_set
    if pos < llm goto table_loop
    key = itable[pos;0]
    k_isa_match = isa key, ['PGE';'Match']
    klen = key.'chars'()
    # skip matches between pos and end of llm
    llm = pos + klen
    val = itable[pos;1]
    v_isa_closure = isa val, 'Sub'
    pass_match = k_isa_match && v_isa_closure
    unless v_isa_closure, not_closure
    unless pass_match, simple_closure
  regex_closure:
    val = val(key)
    goto not_closure
  simple_closure:
    val = val()
  not_closure:
    vlen = val.'chars'()
  check_squash:
    unless squash, replace
    # should these be stringified prior to squash?
    unless lastmatch goto replace
    unless val == lastmatch goto replace
    $I0 = pos - prev_pos
    unless $I0 == klen goto replace
    vlen = 0
    prev_pos = pos
    pos += adjpos
    substr tmps, pos, klen, ''
    goto next_pos
  replace:
    prev_pos = pos
    pos += adjpos
    $S0 = val
    substr tmps, pos, klen, $S0
  next_pos:
    kvdiff = klen - vlen
    adjpos -= kvdiff
    lastmatch = val
    goto table_loop

  done:
    retv = tmps
    .return(retv)
.end


=item subst

 our Str method Str::subst ( Any $string: Any $substring, Any $replacement )
 our Str method Str::subst ( Any $string: Code $regexp,   Any $replacement )

Replaces the first occurrence of a given substring or a regular expression
match with some other substring.

Partial implementation. The :g modifier on regexps doesn't work, for example.

=cut

.sub 'subst' :method :multi(_, _, _)
    .param string substring
    .param string replacement
    .param pmc options         :slurpy :named

    .local pmc global_flag
    global_flag = options['global']
    unless null global_flag goto have_global
    global_flag = options['g']
    unless null global_flag goto have_global
    global_flag = get_hll_global ['Bool'], 'False'
  have_global:

    .local int times                    # how many times to substitute
    times = 1                           # the default is to substitute once
    unless global_flag goto check_x
    times = -1                          # a negative number means all of them (:global)
  check_x:

    .local pmc x_opt
    x_opt = options['x']
    if null x_opt goto check_nth
    times = x_opt
    if times < 0 goto x_fail
  check_nth:

    .local pmc nth_opt
    nth_opt = options['nth']
    unless null nth_opt goto check_global
    nth_opt = get_hll_global ['Bool'], 'True'
  check_global:


    .local string result
    result = self
    result = clone result

    if times == 0 goto subst_done

    .local int startpos, pos, substringlen, replacelen
    startpos = 0
    pos = 0
    substringlen = length substring
    replacelen = length replacement
    .local int n_cnt, x_cnt
    n_cnt = 0
    x_cnt = 0
  subst_loop:
    pos = index result, substring, startpos
    startpos = pos + substringlen
    if pos < 0 goto subst_done

    n_cnt += 1
    $P0 = nth_opt.'ACCEPTS'(n_cnt)
    unless $P0 goto subst_loop

    if times < 0 goto skip_times

    x_cnt += 1
    if x_cnt > times goto subst_done
  skip_times:

    substr result, pos, substringlen, replacement
    startpos = pos + replacelen
    goto subst_loop
  subst_done:
    if null x_opt goto x_check_done
    if n_cnt >= times goto x_check_done
    .return (self)
  x_check_done:
    .return (result)

  nth_fail:
    'die'("Must pass a non-negative integer to :nth()")

  x_fail:
    'die'("Must pass a non-negative integer to :x()")
.end


.sub 'subst' :method :multi(_, 'Sub', _)
    .param pmc regex
    .param pmc replacement
    .param pmc options         :slurpy :named

    .local pmc global_flag
    global_flag = options['global']
    unless null global_flag goto have_global
    global_flag = options['g']
    unless null global_flag goto have_global
    global_flag = get_hll_global ['Bool'], 'False'
  have_global:


    .local int times                    # how many times to substitute
    times = 1                           # the default is to substitute once
    unless global_flag goto check_x
    times = -1                          # a negative number means all of them (:global)
  check_x:

    .local pmc x_opt
    x_opt = options['x']
    if null x_opt goto check_nth
    times = x_opt
    if times < 0 goto x_fail
  check_nth:

    .local pmc nth_opt
    nth_opt = options['nth']
    unless null nth_opt goto build_matches
    nth_opt = get_hll_global ['Bool'], 'True'

  build_matches:
    .local string source, result
    source = self
    result = clone source

    if times == 0 goto subst_done

    # build a list of matches
    .local pmc matchlist, match
    .local int n_cnt, x_cnt
    n_cnt = 0
    x_cnt = 0
    matchlist = new 'ResizablePMCArray'
    match = regex.'!invoke'(source)
    unless match goto matchlist_done

  matchlist_loop:
    n_cnt += 1
    $P0 = nth_opt.'ACCEPTS'(n_cnt)
    unless $P0 goto skip_push

    if times < 0 goto skip_times

    x_cnt += 1
    if x_cnt > times goto matchlist_done
  skip_times:

    push matchlist, match
  skip_push:

    $I0 = match.'to'()
    match = regex(match, 'continue'=>$I0)
    unless match goto matchlist_done
    goto matchlist_loop
  matchlist_done:

    # get caller's lexpad
    .local pmc lexpad
    $P0 = getinterp
    lexpad = $P0['lexpad';1]

    # now, perform substitutions on matchlist until done
    .local int offset
    offset = 0
  subst_loop:
    unless matchlist goto subst_done
    match = shift matchlist
    lexpad['$/'] = match
    # get substitution string
    .local string replacestr
    $I0 = isa replacement, 'Sub'
    if $I0 goto replacement_sub
    replacestr = replacement
    goto have_replacestr
  replacement_sub:
    replacestr = replacement(match)
  have_replacestr:
    # perform the replacement
    $I0 = match.'from'()
    $I1 = match.'to'()
    $I2 = $I1 - $I0
    $I0 += offset
    substr result, $I0, $I2, replacestr
    $I3 = length replacestr
    $I3 -= $I2
    offset += $I3
    goto subst_loop
  subst_done:
    if null x_opt goto x_check_done
    if n_cnt >= times goto x_check_done
    .return (self)
  x_check_done:
    .return (result)

  nth_fail:
    die "Must pass a non-negative integer to :nth()"

  x_fail:
    die "Must pass a non-negative integer to :x()"
.end


=item ord()

=cut

.namespace ['Any']
.sub 'ord' :method :multi(_)
    $S0 = self
    $I0 = ord $S0
    .return ($I0)
.end


=item uc

 our Str multi Str::uc ( Str $string )

Returns the input string after converting each character to its uppercase
form, if lowercase. This is not a Unicode "titlecase" operation, but a
full "uppercase".

=cut

.sub 'uc' :method :multi(_)
    .local string tmps
    .local pmc retv

    tmps = self
    upcase tmps

    retv = new 'Str'
    retv = tmps

    .return(retv)
.end

=item ucfirst

 our Str multi Str::ucfirst ( Str $string )

Performs a Unicode "titlecase" operation on the first character of the string.

=cut

.sub 'ucfirst' :method :multi(_)
    .local string tmps
    .local string fchr
    .local pmc retv
    .local int len

    retv = new 'Str'
    tmps = self

    len = length tmps
    if len == 0 goto done

    substr fchr, tmps, 0, 1
    upcase fchr

    concat retv, fchr
    substr tmps, tmps, 1
    concat retv, tmps

  done:
    .return(retv)
.end

=item unpack

 our List multi Str::unpack ( Str $template, Str $packval )

Takes a string and expands it out into a list of values.

=cut

.namespace['Any']
.sub 'unpack' :multi(_, _)
    .param string template
    .param string packval
    .local pmc retv
    .local int len

    retv = new 'List'

    len = length template
    if len == 0 goto done

    .local int pos
    .local int packpos
    pos = 0
    packpos = 0

  next_directive:
    $S0 = substr template, pos, 1
    if $S0 == 'A' goto ascii
    if $S0 == 'x' goto skip
    if $S0 == ' ' goto space
    goto fail

  ascii:
    pos += 1
    if pos == len goto fail
    $S0 = substr template, pos, 1
    $I0 = ord $S0
    $I0 -= 48
    $S1 = substr packval, packpos, $I0
    retv.'push'($S1)
    packpos += $I0
    pos += 1
    if pos == len goto done
    goto next_directive

  skip:
    pos += 1
    if pos == len goto fail
    $S0 = substr template, pos, 1
    $I0 = ord $S0
    $I0 -= 48
    $S1 = substr packval, packpos, $I0
    packpos += $I0
    pos += 1
    if pos == len goto done
    goto next_directive

  space:
    pos += 1
    if pos == len goto done
    goto next_directive

  done:
    .return(retv)

  fail:
    $P0 = new 'Failure'
    .return ($P0)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
