## $Id$

=head1 TITLE

Object - Perl 6 Object class

=head1 DESCRIPTION

This file sets up the base classes and methods for Perl 6's
object system.  Differences (and conflicts) between Parrot's
object model and the Perl 6 model means we have to do a little
name and method trickery here and there, and this file takes
care of much of that.

=head2 Functions

=over

=item onload()

Perform initializations and create the base classes.

=cut

.sub 'onload' :anon :init :load
    .local pmc p6meta
    load_bytecode 'P6object.pbc'
    $P0 = get_hll_global 'P6metaclass'
    $P0.'new_class'('Perl6Object', 'name'=>'Object')
    p6meta = $P0.'HOW'()
    set_hll_global ['Perl6Object'], '$!P6META', p6meta
.end

=item infix:=(source)  (assignment method)

Assigns C<source> to C<target>.  We use the 'item' method to allow Lists
and Mappings to be converted into Array(ref) and Hash(ref).

=cut

.namespace ['Perl6Object']
.sub 'infix:=' :method
    .param pmc source
    $P0 = source.'item'()

    .local pmc type
    getprop type, 'type', self
    if null type goto do_assign
    $I0 = type.'ACCEPTS'(source)
    if $I0 goto do_assign
    die "Type mismatch in assignment."

  do_assign:
    eq_addr self, $P0, end
    copy self, $P0
  end:
    .return (self)
.end


=back

=head2 Object methods

=over 4

=item hash()

Return the scalar as a Hash.

=cut

.namespace ['Perl6Object']

.sub 'hash' :method
    $P0 = self.'list'()
    .return $P0.'hash'()
.end

=item item()

Return the scalar component of the invocant.  For most objects,
this is simply the invocant itself.

=cut


.sub 'item' :method
    .return (self)
.end


=item list()

Return the list component of the invocant.  For most (Scalar)
objects, we create a List containing the invocant.

=cut

.sub 'list' :method
    $P0 = new 'List'
    push $P0, self
    .return ($P0)
.end


=item new()

Create a new object having the same class as the invocant.

=cut

.sub 'new' :method
    .param pmc init_parents :slurpy
    .param pmc init_this    :named :slurpy

    # Instantiate.
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    $P0 = p6meta.get_parrotclass(self)
    $P1 = new $P0

    # If this proto object has a WHENCE auto-vivification, we should use
    # put any values it contains but that init_this does not into init_this.
    .local pmc whence
    whence = self.'WHENCE'()
    unless whence goto no_whence
    .local pmc this_whence_iter
    this_whence_iter = new 'Iterator', whence
  this_whence_iter_loop:
    unless this_whence_iter goto no_whence
    $S0 = shift this_whence_iter
    $I0 = exists init_this[$S0]
    if $I0 goto this_whence_iter_loop
    $P2 = whence[$S0]
    init_this[$S0] = $P2
    goto this_whence_iter_loop
  no_whence:

    # Now we will initialize each attribute in the class itself and it's
    # parents with an Undef or the specified initialization value. Note that
    # the all_parents list includes ourself.
    .local pmc all_parents, class_iter
    all_parents = inspect $P0, "all_parents"
    class_iter = new 'Iterator', all_parents
  class_iter_loop:
    unless class_iter goto class_iter_loop_end
    .local pmc cur_class
    cur_class = shift class_iter

    # If this the current class?
    .local pmc init_attribs
    eq_addr cur_class, $P0, current_class

    # If it's not the current class, need to see if we have any attributes.
    # Go through the provided init_parents to see if we have anything that
    # matches.
    .local pmc ip_iter, cur_ip
    ip_iter = new 'Iterator', init_parents
  ip_iter_loop:
    unless ip_iter goto ip_iter_loop_end
    cur_ip = shift ip_iter

    # We will check if their HOW matches.
    $P2 = p6meta.'get_parrotclass'(cur_ip)
    eq_addr cur_class, $P2, found_parent_init

    goto found_init_attribs
  ip_iter_loop_end:

    # If we get here, found nothing.
    init_attribs = new 'Hash'
    goto parent_init_search_done

    # We found some parent init data, potentially.
  found_parent_init:
    init_attribs = cur_ip.WHENCE()
    $I0 = 'defined'(init_attribs)
    if $I0 goto parent_init_search_done
    init_attribs = new 'Hash'
  parent_init_search_done:
    goto found_init_attribs

    # If it's the current class, we will take the init_this hash.
  current_class:
    init_attribs = init_this
  found_init_attribs:

    # Now go through attributes of the current class and iternate over them.
    .local pmc attribs, iter
    attribs = inspect cur_class, "attributes"
    iter = new 'Iterator', attribs
  iter_loop:
    unless iter goto iter_end
    $S0 = shift iter
    $S1 = substr $S0, 2
    $I0 = exists init_attribs[$S1]
    if $I0 goto have_init_value
    $P2 = new 'Undef'
    goto init_done
  have_init_value:
    $P2 = init_attribs[$S1]
    delete init_attribs[$S1]
  init_done:
    push_eh set_attrib_eh
    setattribute $P1, cur_class, $S0, $P2
set_attrib_eh:
    goto iter_loop
  iter_end:

    # Do we have anything left in the hash? If so, unknown.
    $I0 = elements init_attribs
    if $I0 == 0 goto init_attribs_ok
    'die'("You passed an initialization parameter that does not have a matching attribute.")
  init_attribs_ok:

    # Next class.
    goto class_iter_loop
  class_iter_loop_end:

    .return ($P1)
.end

=item WHENCE()

Return the invocant's auto-vivification closure.

=cut

.sub 'WHENCE' :method
    $P0 = self.'WHAT'()
    $P1 = $P0.'WHENCE'()
    .return ($P1)
.end

=item REJECTS(topic)

Define REJECTS methods for objects (this would normally
be part of the Pattern role, but we put it here for now
until we get roles).

=cut

.sub 'REJECTS' :method
    .param pmc topic
    $P0 = self.'ACCEPTS'(topic)
    n_not $P0, $P0
    .return ($P0)
.end

=item true()

Defines the .true method on all objects via C<prefix:?>.

=cut

.sub 'true' :method
 .return 'prefix:?'(self)
.end

=item print()

=item say()

Print the object

=cut

.sub 'print' :method
    $P0 = get_hll_global 'print'
    .return $P0(self)
.end

.sub 'say' :method
    $P0 = get_hll_global 'say'
    .return $P0(self)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
