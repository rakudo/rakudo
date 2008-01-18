## $Id$

=head1 TITLE

Str - Perl 6 strings

=head1 DESCRIPTION

This file sets up the C<Perl6Str> PMC type (from F<src/pmc/perl6str.pmc>)
as the Perl 6 C<Str> class.

=cut

.namespace ['Perl6Str']

.include 'cclass.pasm'

.sub 'onload' :anon :init :load
    $P0 = get_hll_global ['Perl6Object'], 'make_proto'
    $P0('String', 'Str')
    $P0('Perl6Str', 'Str')
.end


.sub 'ACCEPTS' :method
    .param string topic
    .return 'infix:eq'(topic, self)
.end

.sub 'chars' :method
    .local pmc retv

    retv = new 'Integer'
    $S0  = self
    $I0  = length $S0
    retv = $I0

    .return (retv)
.end

.sub 'reverse' :method
    .local pmc retv

    retv = self.'split'('')
    retv = retv.'reverse'()
    retv = retv.join('')

    .return(retv)
.end

.sub split :method :multi('Perl6Str')
    .param string delim
    .local string objst
    .local pmc pieces
    .local pmc tmps
    .local pmc retv
    .local int len
    .local int i

    retv = new 'List'

    objst = self
    split pieces, delim, objst

    len = pieces
    i = 0
  loop:
    if i == len goto done

    tmps = new 'Perl6Str'
    tmps = pieces[i]

    retv.'push'(tmps)

    inc i
    goto loop
  done:
    .return(retv)
.end

.sub lc :method
    .local string tmps
    .local pmc retv

    tmps = self
    downcase tmps

    retv = new 'Perl6Str'
    retv = tmps

    .return(retv)
.end

.sub uc :method
    .local string tmps
    .local pmc retv

    tmps = self
    upcase tmps

    retv = new 'Perl6Str'
    retv = tmps

    .return(retv)
.end

.sub lcfirst :method
    .local string tmps
    .local string fchr
    .local pmc retv
    .local int len

    retv = new 'Perl6Str'
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

.sub ucfirst :method
    .local string tmps
    .local string fchr
    .local pmc retv
    .local int len

    retv = new 'Perl6Str'
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

.sub capitalize :method
    .local string tmps
    .local string fchr
    .local pmc retv
    .local int len

    retv = new 'Perl6Str'
    tmps = self

    len = length tmps
    if len == 0 goto done

    downcase tmps

    .local int pos, is_ws, is_lc
    pos = 0
    goto first_char
  next_grapheme:
    if pos == len goto done
    is_ws = is_cclass .CCLASS_WHITESPACE, tmps, pos
    if is_ws goto ws
  advance:
    pos += 1
    goto next_grapheme
  ws:
    pos += 1
  first_char:
    is_lc = is_cclass .CCLASS_LOWERCASE, tmps, pos
    unless is_lc goto advance
    $S1 = substr tmps, pos, 1
    upcase $S1
    substr tmps, pos, 1, $S1
    ## the length may have changed after replacement, so measure it again
    len = length tmps
    goto advance
  done:
    retv = tmps
    .return (retv)
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
