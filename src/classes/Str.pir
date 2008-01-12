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
    .local string retv
    .local string tmps
    .local string ch
    .local int len

    retv = ""

    tmps = self
    len = length tmps
    if len == 0 goto done
    len = len - 1

  loop:
    if len < 0 goto done

    substr ch, tmps, len, 1
    concat retv, ch
	
    dec len
    goto loop

  done:	
    .return(retv)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
