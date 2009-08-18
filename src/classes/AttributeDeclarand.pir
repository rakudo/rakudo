## $Id$

=head1 NAME

src/classes/AttributeDeclarand.pir - Class specifying an attribute declaration

=head1 DESCRIPTION

This is the class that gets created and passed to a trait_mod to
describe a declaration of an attribute container in a class.

=cut

.namespace []

.sub '' :anon :load :init
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('AttributeDeclarand', 'parent'=>'ContainerDeclarand', 'attr'=>'$!how')
.end

.sub 'how' :method
    $P0 = getattribute self, '$!how'
    .return ($P0)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
