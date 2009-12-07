=head1 TITLE

Perl6::Metamodel::ParrotBacked - helps us with things backed by Parrot classes

=head1 DESCRIPTION

This is a low-level role (at the Parrot level) that helps us support having
metaclasses that use a Parrot-level class or role as backing for the metaclass.
It assumes it's stored in the attribute parrotclass. We use this for both
ClassHOW and RoleHOW, which back onto Parrot's Class and Role PMCs.

=cut

.namespace ['Perl6';'Metamodel';'ParrotBackend']

.sub 'init' :load :init :anon
    $P0 = get_hll_namespace ['Perl6';'Metamodel';'ParrotBackend']
    $P1 = root_new ['parrot';'Hash']
    $P1['namespace'] = $P0
    $P1 = new 'Role', $P1
.end


=item add_method(meta, name, code_ref)

Add a method to the given meta.

=cut

.sub 'add_method' :method
    .param pmc meta
    .param string name
    .param pmc meth
    $P0 = getattribute meta, 'parrotclass'
    addmethod $P0, name, meth
.end
