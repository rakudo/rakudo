## $Id$

=head1 NAME

src/classes/ConcreteRole.pir - methods for the ConcreteRole class

=head1 Description

A concrete role is a role that has been selected from an abstract role by
providing a (possibly empty) set of role parameters. This class for now
is primarily just designed to know how to match one of them in a multiple
dispatch.

=head1 Methods

=over 4

=cut

.namespace ['ConcreteRole']

.sub 'onload' :anon :init :load
    .local pmc p6meta, proto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    proto = p6meta.'new_class'('ConcreteRole', 'parent'=>'Any')
.end

.sub 'PROTOOVERRIDES' :method
    .return ('new', 'ACCEPTS')
.end

.sub 'ACCEPTS' :method
    .param pmc topic
    $I0 = isa topic, 'P6role'
    $P0 = '&prefix:<?>'($I0)
    .return ($P0)
.end

=back

=cut
