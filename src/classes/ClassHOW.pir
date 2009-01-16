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
    .tailcall 'prefix:?'($I0)
.end


=item dispatch(obj, name, ...)

Dispatches to method of the given name on this class or one of its parents.

=cut

.sub 'dispatch' :method
    .param pmc obj
    .param string name
    .param pmc pos_args  :slurpy
    .param pmc name_args :slurpy :named

    # Get MRO and an interator on it.
    .local pmc parrotclass, mro, mro_it, cur_class, methods, candidate
    parrotclass = getattribute self, 'parrotclass'
    mro = inspect parrotclass, 'all_parents'
    mro_it = iter mro

    # Iterate MRO and check it's methods.
    .local int have_pmc_proxy
    have_pmc_proxy = 0
  mro_loop:
    unless mro_it goto mro_loop_end
    cur_class = shift mro_it
    $S0 = typeof cur_class
    if $S0 == 'PMCProxy' goto pmc_proxy
    methods = cur_class.'methods'()
    candidate = methods[name]
    if null candidate goto check_handles

    # If we're not in the current class, need a submethod check.
    eq_addr cur_class, parrotclass, submethod_check_done
    $I0 = isa candidate, 'Submethod'
    if $I0 goto check_handles
  submethod_check_done:

    # Got a method that we can call. XXX Set up exception handlers for if we
    # have to do auto-threading of junctional arguments, additionally if we
    # get a control expection for callsame or nextsame etc. Won't be able to
    # be tailcall then...
    .tailcall obj.candidate(pos_args :flat, name_args :flat :named)

  check_handles:
    # XXX This is where we will insert logic to run any regex or more complex
    # 'handles' things to try and find a handler.
    goto mro_loop

  pmc_proxy:
    # If we inherit from a PMC, we'll try doing the call directly later on.
    # XXX Odd issues if we try and do it by introspective methods...
    have_pmc_proxy = 1
    goto mro_loop

  mro_loop_end:
    # If we get here, we didn't find anything to dispatch to; error unless a
    # PMC can provide it.
    unless have_pmc_proxy goto error
    ($P0 :slurpy, $P1 :slurpy :named) = obj.name(pos_args :flat, name_args :flat :named)
    .return ($P0 :flat, $P1 :named :flat)
  error:
    $P0 = getattribute self, 'longname'
    'die'("Could not locate a method '", name, "' to invoke on class '", $P0, "'.")
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
