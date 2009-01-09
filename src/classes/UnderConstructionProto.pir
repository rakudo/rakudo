## $Id$

=head1 TITLE

UnderConstructionProto.pir - the UnderConstructionProto class

=head1 DESCRIPTION

Represents a class in the namespace that we're still in the process of
compiling.

=cut

.namespace ['Perl6' ; 'Compiler' ; 'UnderConstructionProto' ]

.sub 'onload' :anon :init :load
    .local pmc p6meta, proto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    proto = p6meta.'new_class'('Perl6::Compiler::UnderConstructionProto', 'parent'=>'Perl6Object', 'attr'=>'@!ns $!short_name')
    $P0 = get_hll_global 'Abstraction'
    p6meta.'add_role'($P0, 'to'=>proto)
.end


=head1 METHODS

=over

=item WHAT

Returns self.

=cut

.sub 'WHAT' :method
    .return (self)
.end


=item get_string (vtable method)

Returns the name of the class under construction.

=cut

.sub '' :vtable('get_string')
    $P0 = getattribute self, '$!short_name'
    .return ($P0)
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
