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
    $P0 = self.'CREATE'('P6opaque')
    setattribute $P0, '$!pos', pos_args
    setattribute $P0, '$!named', named_args
    .return ($P0)
.end


=item postcircumfix:<[ ]>

=cut

.sub 'postcircumfix:<[ ]>' :method :vtable('get_pmc_keyed_int')
    .param int i
    $P0 = getattribute self, '$!pos'
    $P0 = $P0[i]
    .return ($P0)
.end


=item postcircumfix:<{ }>

=cut

.sub 'postcircumfix:<{ }>' :method :vtable('get_pmc_keyed_str')
    .param pmc key
    $P0 = getattribute self, '$!named'
    $P0 = $P0[key]
    .return ($P0)
.end


=item elems

The number of positional elements in the Capture.

=cut

.sub 'elems' :method :vtable('elements')
    $P0 = getattribute self, '$!pos'
    $I0 = elements $P0
    .return ($I0)
.end


=item hash

XXX Returns Parrot Hash, not Perl 6 Hash.

=cut

.sub 'hash' :method
    $P0 = getattribute self, '$!named'
    .return ($P0)
.end


=item !PARROT_POSITIONALS

Gets a Parrot RPA that we can use :flat on.

=cut

.sub '!PARROT_POSITIONALS' :method
    $P0 = getattribute self, '$!pos'
    .return ($P0)
.end


=item !PARROT_NAMEDS

Gets a Parrot Hash that we can use :flat :named on.

=cut

.sub '!PARROT_NAMEDS' :method
    $P0 = getattribute self, '$!named'
    .return ($P0)
.end

=back

=head2 Functions

=over 4

=item !snapshot_capture

Snapshots the current capture cursor.

Well, akshually...until we implement Capture Cursor it just kinda pretends
to. :-) Hands back a Capture containing the snapshot.

=cut

.namespace []
.sub '!snapshot_capture'
    .param pmc capture
    .param int pos_position
    .param pmc nameds_unbound
    die "Capture snapshots in signatures not yet implemented."
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
