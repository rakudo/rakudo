=head1 TITLE

Proxy - Perl 6 Proxy elements

=head1 DESCRIPTION

A Proxy is used to represent non-existent elements in aggregates.
However, it also provides lvalue semantics to bind itself to the
aggregate if written to.

=head2 Methods

=over 4

=cut

.namespace ['Proxy']
.sub 'onload' :anon :init :load
    .local pmc p6meta, listproto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    listproto = p6meta.'new_class'('Proxy', 'parent'=>'parrot;Undef Any', 'attr'=>'$!base $!key &!vivibase')
.end

=back

=head2 Private Methods

=over 4

=item !STORE

Same as Object.!STORE, but first binds itself into its base 
container.

=cut

.sub '!STORE' :method
    .param pmc source
    .local pmc base, key
    base = getattribute self, '$!base'
    key  = getattribute self, '$!key'

    # If the base is undefined, we need to vivify it as well
    # according to the code in &!vivibase.
    $I0 = defined base
    if $I0 goto base_done
    .local pmc vivibase
    vivibase = getattribute self, '&!vivibase'
    vivibase(base)
  base_done:

    # now bind self into the base container
    base[key] = self

    # get the item to be stored
    source = descalarref source
    $I0 = can source, 'item'
    unless $I0 goto have_source
    source = source.'item'()
  have_source:

    # convert self into a scalar
    $P0 = new ['ObjectRef'], source
    copy self, $P0
    $P0 = get_hll_global ['Bool'], 'True'
    setprop self, 'scalar', $P0

    .return (self)
.end


=item defined

Before we're assigned to, we're not defined.

=cut

.sub 'defined' :method
    $P0 = get_hll_global ['Bool'], 'False'
    .return ($P0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
