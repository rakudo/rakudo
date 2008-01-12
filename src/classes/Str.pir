## $Id$

=head1 TITLE

Str - Perl 6 strings

=head1 DESCRIPTION

This file sets up the C<Perl6Str> PMC type (from F<src/pmc/perl6str.pmc>)
as the Perl 6 C<Str> class.

=cut

.namespace ['Perl6Str']

.sub 'onload' :anon :init :load
    $P0 = get_hll_global ['Perl6Object'], 'make_proto'
    $P0('String', 'Str')
    $P0('Perl6Str', 'Str')
.end


.sub 'ACCEPTS' :method
    .param string topic
    .return 'infix:eq'(topic, self)
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

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
