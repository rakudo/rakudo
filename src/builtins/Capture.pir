## $Id$

=head1 TITLE

Capture - Perl 6 Capture class

=head1 DESCRIPTION

This file sets up the Perl 6 C<Capture> class.

=cut

.namespace ['Capture']

.sub 'onload' :anon :init :load
    .local pmc p6meta, captureproto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    captureproto = p6meta.'new_class'('Capture', 'parent'=>'Any', 'attr'=>'$!pos $!named')
.end


=head2 Methods

=over 4

=item new

Takes a bunch of positional and named arguments and builds a capture from
them.

=cut

.sub 'new' :method
    .param pmc pos_args   :slurpy
    .param pmc named_args :slurpy :named
    
    # Create capture.
    $P0 = get_hll_global 'Whatever'
    $P0 = self.'bless'($P0, '$!pos'=>pos_args, '$!named'=>named_args)
    .return ($P0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
