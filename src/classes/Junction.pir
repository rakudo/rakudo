## $Id$

=head1 NAME

src/classes/Junction.pir - Perl 6 Junction and related functions

=cut

# Constants for types of junctions.
.const int JUNCTION_TYPE_ANY  = 1
.const int JUNCTION_TYPE_ONE  = 2
.const int JUNCTION_TYPE_ALL  = 3
.const int JUNCTION_TYPE_NONE = 4

.namespace []
.sub 'onload' :anon :load :init
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('Junction', 'parent'=>'Perl6Object', 'attr'=>'@!eigenstates $!type')
.end

=head2 Methods

=over 4

=item perl()

Return perl representation.  (This should actually be autothreaded.)

=cut

.namespace ['Junction']
.sub 'perl' :method
    .local int type
    type = self.'!type'()

    .local string res
    if type == JUNCTION_TYPE_ANY goto any
    if type == JUNCTION_TYPE_ONE goto one
    if type == JUNCTION_TYPE_ALL goto all
    if type == JUNCTION_TYPE_NONE goto none
  any:
    res = 'any('
    goto type_done
  one:
    res = 'one('
    goto type_done
  all:
    res = 'all('
    goto type_done
  none:
    res = 'none('
  type_done:

    .local pmc it
    $P0 = self.'!eigenstates'()
    it = iter $P0
    unless it goto states_done
    $P0 = shift it
    $S0 = $P0.'perl'()
    concat res, $S0
  states_loop:
    unless it goto states_done
    $P0 = shift it
    $S0 = $P0.'perl'()
    concat res, ', '
    concat res, $S0
    goto states_loop
  states_done:
    concat res, ')'
    .return (res)
.end


=item true()

Evaluate Junction as a boolean.

=cut

.namespace ['Junction']
.sub 'true' :method
    .local pmc eigenstates, it
    .local int type
    eigenstates = self.'!eigenstates'()
    it = iter eigenstates
    type = self.'!type'()
    if type == JUNCTION_TYPE_NONE goto none
    if type == JUNCTION_TYPE_ALL goto all

  any_one:
    unless it goto false
    $P0 = shift it
    unless $P0 goto any_one
    if type == JUNCTION_TYPE_ANY goto true
    # fall through

  none:
    unless it goto true
    $P0 = shift it
    if $P0 goto false
    goto none

  all:
    unless it goto true
    $P0 = shift it
    if $P0 goto all
    # fall through

  false:
    $P0 = get_hll_global ['Bool'], 'False'
    .return ($P0)

  true:
    $P0 = get_hll_global ['Bool'], 'True'
    .return ($P0)
.end

=item ACCEPTS

Smart-matching for junctions, short-circuiting.

=cut

.namespace ['Junction']
.sub 'ACCEPTS' :method
    .param pmc topic
    .local pmc eigenstates, it, state
    .local int type
    eigenstates = self.'!eigenstates'()
    it = iter eigenstates
    type = self.'!type'()
    if type == JUNCTION_TYPE_NONE goto none
    if type == JUNCTION_TYPE_ALL goto all

  any_one:
    unless it goto false
    state = shift it
    $P0 = state.'ACCEPTS'(topic)
    unless $P0 goto any_one
    if type == JUNCTION_TYPE_ANY goto true
    # fall through

  none:
    unless it goto true
    state = shift it
    $P0 = state.'ACCEPTS'(topic)
    if $P0 goto false
    goto none

  all:
    unless it goto true
    state = shift it
    $P0 = state.'ACCEPTS'(topic)
    if $P0 goto all

  false:
    $P0 = get_hll_global ['Bool'], 'False'
    .return ($P0)

  true:
    $P0 = get_hll_global ['Bool'], 'True'
    .return ($P0)
.end
    

=item !type()

Return the type of the Junction.

=item !eigenstates()

Return the components of the Junction.

=cut

.namespace ['Junction']
.sub '!type' :method
    $P0 = getattribute self, '$!type'
    .return ($P0)
.end

.sub '!eigenstates' :method
    $P0 = getattribute self, '@!eigenstates'
    .return ($P0)
.end


=back

=head2 VTABLE functions

=cut

.namespace ['Junction']
.sub '' :method :vtable('get_bool')
    $I0 = self.'true'()
    .return ($I0)
.end


=back

=head2 Helper functions

=cut

.namespace []
.sub '!MAKE_JUNCTION'
    .param pmc type
    .param pmc eigenstates

    .local pmc junc
    $P0 = get_hll_global 'Junction'
    junc = $P0.'new'()
    setattribute junc, '$!type', type
    setattribute junc, '@!eigenstates', eigenstates
    .return (junc)
.end
    
.sub '!DISPATCH_JUNCTION'
    .param pmc the_sub
    .param pmc args            :slurpy

    ##  lookup a sub by name if needed
    $I0 = isa the_sub, 'Sub'
    if $I0 goto have_sub
    $S0 = the_sub
    the_sub = find_name $S0
  have_sub:

    ##  Look for the left-most junction.
    .local int argc, index, index_save
    argc = args
    index = 0
  left_loop:
    unless index < argc goto left_done
    .local pmc junc
    junc = args[index]
    $I0 = isa junc, 'Junction'
    if $I0 goto left_done
    inc index
    goto left_loop
  left_done:
    ##  If it's an all/none junction, we're good
    .local int type
    type = junc.'!type'()
    if type >= JUNCTION_TYPE_ALL goto have_index
    ##  one/any junction, so look through the remaining args for all/none
    index_save = index
    inc index
  all_loop:
    unless index < argc goto all_done
    junc = args[index]
    $I0 = isa junc, 'Junction'
    unless $I0 goto all_next
    type = junc.'!type'()
    if type >= JUNCTION_TYPE_ALL goto have_index
  all_next:
    inc index
    goto all_loop
  all_done:
    index = index_save
    junc = args[index]
    type = junc.'!type'()
  have_index:

    .local pmc eigenstates, it, results
    eigenstates = junc.'!eigenstates'()
    it = iter eigenstates
    results = 'list'()
  thread_loop:
    unless it goto thread_done
    $P0 = shift it
    args[index] = $P0
    $P0 = the_sub(args :flat)
    push results, $P0
    goto thread_loop
  thread_done:
    .tailcall '!MAKE_JUNCTION'(type, results)
.end

=head2 Functions

=over 4

=item any(), infix:<|>()

=cut

.namespace []
.sub 'any'
    .param pmc args            :slurpy
    args.'!flatten'()
    .tailcall '!MAKE_JUNCTION'(JUNCTION_TYPE_ANY, args)
.end

.sub 'infix:|'
    .param pmc args            :slurpy
    .tailcall '!MAKE_JUNCTION'(JUNCTION_TYPE_ANY, args)
.end

=item one(), infix:<^>()

=cut

.namespace []
.sub 'one'
    .param pmc args            :slurpy
    args.'!flatten'()
    .tailcall '!MAKE_JUNCTION'(JUNCTION_TYPE_ONE, args)
.end

.sub 'infix:^'
    .param pmc args            :slurpy
    .tailcall '!MAKE_JUNCTION'(JUNCTION_TYPE_ONE, args)
.end


=item all(), infix:<&>()

=cut

.namespace []
.sub 'all'
    .param pmc args            :slurpy
    args.'!flatten'()
    .tailcall '!MAKE_JUNCTION'(JUNCTION_TYPE_ALL, args)
.end

.sub 'infix:&'
    .param pmc args            :slurpy
    .tailcall '!MAKE_JUNCTION'(JUNCTION_TYPE_ALL, args)
.end


=item none()

=cut

.namespace []
.sub 'none'
    .param pmc args            :slurpy
    args.'!flatten'()
    .tailcall '!MAKE_JUNCTION'(JUNCTION_TYPE_NONE, args)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
