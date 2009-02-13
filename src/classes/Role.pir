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
    selector = new 'Perl6MultiSub'
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
    created_list = new 'ResizablePMCArray'
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

    # First time we've had these parameters, so need to instantiate them.
  select_role:
    .local pmc selector
    selector = getattribute self, '$!selector'
    result = selector(pos_args :flat, name_args :flat :named)
    ins_hash = new 'Hash'
    ins_hash["pos_args"] = pos_args
    ins_hash["role"] = result
    push created_list, ins_hash
    .return (result)
.end


=item ACCEPTS(topic)

Checks if the given topic does the role.

=cut

.sub 'ACCEPTS' :method
    .param pmc topic

    # Since we aren't re-blessing code objects yet, need to get and test their
    # proto-object instead.
    $I0 = topic.'isa'('Code')
    unless $I0 goto no_proto
    topic = topic.'WHAT'()
  no_proto:

    # Now go over the roles we've created and see if one of them is done.
    .local pmc created, it
    created = getattribute self, '@!created'
    if null created goto it_loop_end
    it = iter created
    $I0 = 0
  it_loop:
    unless it goto it_loop_end
    $P0 = shift it
    $P0 = $P0["role"]
    $I0 = does topic, $P0
    if $I0 == 0 goto it_loop
  it_loop_end:

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


=item new

Puns the role and instantiates the punned class.

=cut

.sub 'new' :method
    .param pmc pos_args  :slurpy
    .param pmc name_args :slurpy :named

    # Must be argument-less case of the role; select that and then tailcall
    # it's new.
    $P0 = self.'!select'()
    .tailcall $P0.'new'(pos_args :flat, name_args :flat :named)
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
    .return ($P0)
.end


=back

=head1 Methods on Parrot Roles

We also add some methods to the Parrot roles.

=item !pun

Puns the role to a class and returns that class.

=cut

.namespace ["Role"]
.sub '!pun' :method
    # See if we have already created a punned class; use it if so.
    .local pmc pun
    pun = getprop '$!pun', self
    if null pun goto make_pun
    .return (pun)
  make_pun:

    # Otherwise, need to create a punned class.
    .local pmc p6meta, metaclass, proto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    metaclass = new ['Class']
    $P0 = box 'class'
    setprop metaclass, 'pkgtype', $P0
    metaclass.'add_role'(self)
    # XXX Would be nice to call !meta_compose here; for some reason, Parrot
    # ends up calling the wrong multi-variant. Something to investigate, when
    # I/someone has the energy for it.
    '!compose_role_attributes'(metaclass, self)
    proto = p6meta.'register'(metaclass, 'parent'=>'Any')

    # Stash it away, then instantiate it.
    setprop self, '$!pun', proto
    .return (proto)
.end


=item new

Puns the role to a class and instantiates it.

=cut

.sub 'new' :method
    .param pmc pos_args    :slurpy
    .param pmc name_args   :slurpy :named
    .local pmc pun
    pun = self.'!pun'()
    .tailcall pun.'new'(pos_args :flat, name_args :flat :named)
.end


=item ACCEPTS

=cut

.sub 'ACCEPTS' :method
    .param pmc topic
    $I0 = does topic, self
    $P0 = 'prefix:?'($I0)
    .return ($P0)
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
    .return ($P0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
