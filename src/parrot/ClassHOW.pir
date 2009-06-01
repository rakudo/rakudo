## $Id$

=head1 TITLE

ClassHOW - default metaclass

=head1 DESCRIPTION

This file for now actually just adds a method or two into P6metaclass. In the
long run, we probably need to subclass that, and make sure we have all of the
methods in here that are defined in the HOW API.

=head2 Methods on P6metaclass

=over

=item does(object, role)

Tests role membership.

=cut

.namespace ['P6metaclass']
.sub 'does' :method
    .param pmc obj
    .param pmc type

    # Check if we have a Perl6Role - needs special handling.
    $I0 = isa type, 'Perl6Role'
    unless $I0 goto not_p6role
    .tailcall type.'ACCEPTS'(obj)
  not_p6role:
    $I0 = does obj, type
    .const 'Sub' $P1 = 'prefix:?'
    .tailcall $P1($I0)
.end


=item parents()

Gets a list of this class' parents.

=cut

.sub 'parents' :method
    .param pmc obj
    .param pmc local         :named('local') :optional
    .param pmc hierarchical  :named('hierarchical') :optional
    
    .local pmc parrot_class, result_list, parrot_list, it
    result_list = get_root_global [.RAKUDO_HLL], 'Array'
    result_list = result_list.'new'()
    parrot_class = self.'get_parrotclass'(obj)
    
    # Fake top of Perl 6 hierarchy
    $S0 = parrot_class.'name'()
    if $S0 != 'Perl6Object' goto not_object
    unless null local goto done
    $P0 = get_hll_global 'Object'
    result_list.'push'($P0)
    goto done
  not_object:

    # If it's local or default, can just use inspect.
    unless null hierarchical goto do_hierarchical
    if null local goto all_parents
    parrot_list = inspect parrot_class, 'parents'
    goto have_list
  all_parents:
    parrot_list = inspect parrot_class, 'all_parents'
  have_list:
    it = iter parrot_list
  it_loop:
    unless it goto it_loop_end
    $P0 = shift it
    parrot_class = self.'get_parrotclass'($P0)
    $S0 = parrot_class.'name'()
    if $S0 == 'P6object' goto done
    $P0 = getprop 'metaclass', $P0
    $P0 = $P0.'WHAT'()
    result_list.'push'($P0)
    goto it_loop
  it_loop_end:
    goto done

  do_hierarchical:
    'die'(':hierarchical not yet implemented')

  done:
    .return (result_list)
.end


=item methods(object)

Gets a list of methods.

XXX Spec and implement various flags it takes, which currently aren't in S12.

XXX Fix bugs with introspecting some built-in classes (List, Str...)

=cut

.sub 'methods' :method
    .param pmc obj

    .local pmc parrot_class, method_hash, result_list, it, cur_meth
    parrot_class = self.'get_parrotclass'(obj)

    # Create array to put results in.
    result_list = get_root_global [.RAKUDO_HLL], 'Array'
    result_list = result_list.'new'()

    # Get methods hash and build list of methods.
    method_hash = inspect parrot_class, "methods"
    it = iter method_hash
  it_loop:
    unless it goto it_loop_end
    $S0 = shift it
    cur_meth = method_hash[$S0]
    result_list.'push'(cur_meth)
    goto it_loop
  it_loop_end:

    .return (result_list)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
