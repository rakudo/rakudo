## $Id$

=head1 NAME

src/classes/Role.pir - methods for the Role class

=head1 Description

This class represents a role in Perl 6. It is not substitutable for a Parrot
role, nor does it subclass it. Instead, it provides a way to get at a Parrot
level role through a multiple dispatch (or perhaps from a cache). You can see
it as a kind of "role factory", which manufactures roles of a particular
short name for a particular set of parameters.

=head1 Methods

=over 4

=cut

.namespace ['Perl6Role']

.sub 'onload' :anon :init :load
    .local pmc p6meta, roleproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    roleproto = p6meta.'new_class'('Perl6Role', 'parent'=>'Any', 'name'=>'Role', 'attr'=>'$!selector @!created')
    p6meta.'register'('Role', 'proto'=>'roleproto')
.end


=item !add_variant

Adds a parameterized variant of the role.

=cut

.sub '!add_variant' :method
    .param pmc variant
    .local pmc selector
    selector = getattribute self, '$!selector'
    unless null selector goto have_selector
    selector = root_new ['parrot';'Perl6MultiSub']
    setattribute self, '$!selector', selector
  have_selector:
    push selector, variant
.end


=item !select

Selects a variant of the role to do based upon the supplied parameters.

=cut

.sub '!select' :method
    .param pmc pos_args  :slurpy
    .param pmc name_args :slurpy :named

    # @!created is an array of hashes describing role instantiations that have
    # already taken place. This means that we always hand back, for Foo[Int],
    # the same Parrot-level role rather than creating one each time we ask for
    # a Foo[Int]. The hash contains:
    #  * pos_args - array of the positional arguments for the instantiation
    #  * role - the Parrot role object
    # Note that since named parameters don't participate in a multiple dispatch,
    # they cannot form part of the long name for the role, so we don't need to
    # check those.
    .local pmc result, created_list, ins_hash, it, test_pos_args
    .local int num_pos_args, num_name_args, i
    created_list = getattribute self, '@!created'
    unless null created_list goto got_created_list
    created_list = root_new ['parrot';'ResizablePMCArray']
    setattribute self, '@!created', created_list
    goto select_role
  got_created_list:
    num_pos_args = elements pos_args
    it = iter created_list
  it_loop:
    unless it goto it_loop_end
    ins_hash = shift it

    # Compare positional counts.
    test_pos_args = ins_hash["pos_args"]
    $I0 = elements test_pos_args
    if $I0 != num_pos_args goto it_loop

    # Counts match, now look at positionals.
    i = 0
  pos_loop:
    if i >= num_pos_args goto pos_loop_end
    $P0 = pos_args[i]
    $P1 = test_pos_args[i]
    $I0 = 'infix:==='($P0, $P1)
    unless $I0 goto it_loop
    inc i
    goto pos_loop
  pos_loop_end:

    # If we get here, we've found a match, so return it.
    result = ins_hash["role"]
    .return (result)
  it_loop_end:

    # First time we've had these parameters, so need to create the role
    # taking them.
  select_role:
    .local pmc selector
    selector = getattribute self, '$!selector'
    result = selector(pos_args :flat, name_args :flat :named)
    ins_hash = root_new ['parrot';'Hash']
    ins_hash["pos_args"] = pos_args
    ins_hash["role"] = result
    push created_list, ins_hash

    # Also need to annotate that role with its parameters and the "factory"
    # that generated it.
    setprop result, "$!owner", self
    setprop result, "@!type_args", pos_args
    .return (result)
.end


=item ACCEPTS(topic)

Checks if the given topic does the role.

=cut

.sub 'ACCEPTS' :method
    .param pmc topic

    # If the topic is the same as self, then we're done.
    $I0 = 1
    topic = '!DEREF'(topic)
    eq_addr self, topic, done
    $I0 = 0

    # Go over the roles we've created and see if one of them is done.
    .local pmc created, it
    created = getattribute self, '@!created'
    if null created goto it_loop_end
    it = iter created
  it_loop:
    unless it goto it_loop_end
    $P0 = shift it
    $P0 = $P0["role"]
    $I0 = does topic, $P0
    if $I0 == 0 goto it_loop
  it_loop_end:

    # Undef is always OK.
    if $I0 goto done
    $I0 = isa topic, 'Failure'

  done:
    $P0 = 'prefix:?'($I0)
    .return ($P0)
.end


=item postcircumfix:<[ ]>

Selects a role based upon type.

=cut

.sub 'postcircumfix:[ ]' :method
    .param pmc pos_args  :slurpy
    .param pmc name_args :slurpy :named

    # Need to unwrap the arguments (they are wrapped by postcircumfix:[ ]
    # multi), then call !select.
    pos_args = pos_args[0]
    .tailcall self.'!select'(pos_args :flat, name_args :flat :named)
.end


=item elements (vtable method)

Gives the number of possible parameterized roles we can select from (but really
just here so postcircumfix:[ ] doesn't explode).

=cut

.sub 'elements' :vtable
    $P0 = getattribute self, '$!selector'
    $I0 = elements $P0
    .return ($I0)
.end


=item perl

=cut

.sub 'perl' :method
    $P0 = getprop '$!shortname', self
    .return ($S0)
.end


=item WHICH

=cut

.sub 'WHICH' :method
    $I0 = get_addr self
    .return ($I0)
.end


=item WHAT

=cut

.sub 'WHAT' :method
    .return (self)
.end


=item Str (vtable get_string)

=cut

.sub 'Str' :method :vtable('get_string')
    $P0 = getprop '$!shortname', self
    $S0 = $P0
    concat $S0, '()'
    .return ($S0)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
