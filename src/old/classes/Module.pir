## $Id$

=head1 TITLE

Code - Perl 6 Module class

=head1 DESCRIPTION

This file sets up the Perl 6 C<Module> class.

=cut

.namespace ['Module']

.sub 'onload' :anon :load :init
    .local pmc p6meta, moduleproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    moduleproto = p6meta.'new_class'('Module', 'parent'=>'parrot;NameSpace Any')
    p6meta.'register'('NameSpace', 'parent'=>moduleproto, 'protoobject'=>moduleproto)
.end


=head1 METHODS

=over 4

=item WHAT

Gets the proto-object for this module.

=cut

.sub 'WHAT' :method
    # The usual approach of .WHAT doesn't work for us here, because get_class
    # is overridden in the NameSpace PMC.
    $P0 = get_hll_global 'Module'
    .return ($P0)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
