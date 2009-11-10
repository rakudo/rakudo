## $Id$

=head1 NAME

src/classes/ContainerDeclarand.pir - Class specifying a declaration

=head1 DESCRIPTION

This is the class that gets created and passed to a trait_mod to
describe a declaration of a container.

=cut

.namespace ['ContainerDeclarand']

.sub '' :anon :load :init
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('ContainerDeclarand', 'parent'=>'Any', 'attr'=>'$!container $!name')
.end

.sub 'new' :method
    .param pmc container :named('container')
    .param pmc name      :named('name')
    $P0 = new ['ContainerDeclarand']
    setattribute $P0, '$!container', container
    setattribute $P0, '$!name', name
    .return ($P0)
.end

.sub 'container' :method
    $P0 = getattribute self, '$!container'
    .return ($P0)
.end

.sub 'name' :method
    $P0 = getattribute self, '$!name'
    .return ($P0)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
